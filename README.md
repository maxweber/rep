# rep

A REPL without the loop. A caller sends Clojure code in an HTTP POST. The
server evaluates it in a [babashka](https://babashka.org) process that
belongs to the caller's session and answers the printed text. The loop stays
with the caller.

The caller I have in mind is a coding agent such as Claude Code. It already
knows how to send an HTTP request and how to read bash output. With rep that
is enough to work on another machine: run code there, keep definitions between
requests, run bash, move files in and out.

This is an experiment and it runs on babashka only.

## Try it

Start a server. It prints a token.

```
bb serve --port 8730 --dir work
```

Talk to it with the client, which reads the code from standard input.

```
export REP_URL=http://127.0.0.1:8730 REP_TOKEN=<the token>

bin/rep <<'EOF'
(def files (clojure.string/split-lines (slurp "list.txt")))
(count files)
EOF
```

Or with curl.

```
SESSION=$(uuidgen)

curl -s -H "Authorization: Bearer $REP_TOKEN" \
  --data-binary '(+ 1 2)' \
  "$REP_URL/rep/$SESSION/$(uuidgen)"
```

`(help)` tells what a session offers.

## The protocol

There is one call.

```
POST /rep/<session>/<id>
Authorization: Bearer <token>

<Clojure code>
```

`<session>` and `<id>` are UUIDs the caller makes up. The answer is plain
text, in the order things happened: what the code printed, then the value of
its last form as EDN. As with `bb -e`, a `nil` value prints nothing. An error
ends the request at its first failing form and prints as babashka prints it,
with line and column.

The outcome is in the headers, so the text needs no frame around it.

| Header | Values |
|---|---|
| `Rep-Status` | `running`, `done`, `failed`, `interrupted` |
| `Rep-Exit` | `0` when done, `1` when failed |
| `Rep-Session` | `started`, `continued`, `restarted` |
| `Rep-Evaluation` | `new`, `replayed` |

A POST waits up to a second for the evaluation. Most finish in that time and
answer with `Rep-Status: done`. A longer one answers `Rep-Status: running` and
streams its output until it ends. The same POST again, with
`?from=<bytes received>`, then gives the final headers.

Other answers: 401 without the token, 400 for ids that are no UUIDs, 409 for an
id that was used with other code, 405 for anything but a POST.

Two more routes move files, see below. `GET /` answers a short text that
explains all of this to an agent.

## Sessions

A session is a babashka process of its own, started on the session's first
request. Definitions stay in it. Every request starts in the namespace `user`.
It is full babashka: `slurp`, `spit`, `future`, atoms, `Thread/sleep`,
`babashka.fs`, `babashka.process`, `babashka.http-client`. Its working
directory is the work folder.

rep adds four functions to `user`: `sh`, `upload-url`, `download-url` and
`help`.

A session takes a second request while the first still runs. They share all
definitions.

A session lives in memory. When its process ends, or the server restarts, the
next answer carries `Rep-Session: restarted` and the client says that the
earlier definitions are gone. Keep what must survive in files.

On my laptop a session starts in about 0.3 seconds and holds about 95 MB. A
later request takes about 50 ms.

While a session runs, babashka keeps the port of its nREPL server in the file
`.nrepl-port` in the work folder. An editor can attach there and see the same
definitions. With several sessions the file names the newest one.

## Bash

A model learned bash from raw bash output. So the output of a script goes into
the answer as bash wrote it, standard error included and in order, with no
JSON and no Clojure string around it.

```
bin/rep <<'EOF'
(sh "ffmpeg -version | head -1")
EOF
```

`(sh script)` answers `{:exit <code>}`. A script that fails is a value, not an
error, so code can branch on it. Every call is a new bash process in the work
folder. A `cd` or a variable does not carry over to the next one.
`(with-out-str (sh script))` gives the output as a string.

On the way in, the script is a Clojure string, so the caller escapes `"` and
`\`. A script that is awkward to quote goes up as a file and runs from there:

```
URL=$(bin/rep <<'EOF'
(print (upload-url "scripts/cut.sh"))
EOF
)
curl -s -X PUT --data-binary @cut.sh "$URL"

bin/rep <<'EOF'
(sh "bash scripts/cut.sh")
EOF
```

## Long work

There is no call to follow or stop an evaluation. Clojure has what it takes.
Start the work in a future, keep the result in an atom, ask in a later
request.

```
bin/rep <<'EOF'
(def result (atom nil))
(def job (future (reset! result (with-out-str (sh "./render.sh")))))
EOF

bin/rep <<'EOF'
[(realized? job) @result]
EOF
```

`(future-cancel job)` ends it, and the script with it. What a future prints
after its request ended is lost, so keep it in the atom or in a file.

A request that hangs without a future is not lost either. A second request
can end the script with `(sh "pkill -f render.sh")`. The last resort is
`(System/exit 0)`. It ends the session's process, every request in it fails,
and the next one starts a new process.

The evaluation itself does not depend on the connection that started it. When
the caller hangs up, it runs on, and the same POST again follows it.

## Evaluation ids

The id in the URL makes a POST safe to repeat. The server keeps the request,
the output and the outcome of every evaluation on disk.

- The same id with the same code runs nothing. It answers the first output,
  and follows it if the evaluation still runs.
- The same id with other code is a 409.
- An evaluation that a restart of the server cut off answers
  `Rep-Status: interrupted`. The server cannot know how far it got and does not
  pretend to.

The client makes a fresh id for every call and reuses it when it asks again. A
model should not invent ids by hand. If it repeats one, an old answer comes
back as if it were new.

## Files

`(upload-url "in/clip.mp4")` answers a URL that takes a PUT.
`(download-url "out/final.mp4")` answers one for a GET. Both need no token and
expire after 15 minutes, or after the seconds given as a second argument. They
only reach into the work folder.

```
bin/rep <<'EOF'
(print (upload-url "in/clip.mp4"))
EOF
```

## The client

```
rep [--id ID] [--wait SECONDS] < code   evaluates standard input, prints the answer
rep session                             prints the session id
```

It reads `REP_URL` and `REP_TOKEN`. The session id comes from `REP_SESSION` or
from the file `.rep-session` in the current folder, which the client makes on
first use. One folder, one session.

It ends with 0 when the code ran through, 1 when it failed and 2 when the
server refused the request. With `--wait` it lets go after that many seconds
and ends with 124, the exit code of `timeout`. The evaluation runs on. The same
input again with the `--id` it names follows it to the end.

Link `bin/rep` into your `PATH`. It finds its sources next to itself.

## In your own babashka program

```clojure
(require '[rep.server :as rep])

(def system
  (rep/start! (rep/prepare {:rep/dir "work"
                            :rep/prelude "shop.clj"})))

(rep/stop! system)
```

`:rep/prelude` names a Clojure file that every session loads when it starts.
That is how a host gives the code more to call. With a `shop.clj` that defines
the namespace `shop`, a caller evaluates `(shop/price 21)`. See the docstring
of `rep.server/prepare` for the other options.

## Limits

rep runs the code it is sent. That is its job, so treat the token like a
shell login. A session is full babashka with the rights of the server's user.
Run rep on a machine you can throw away. It listens on 127.0.0.1 unless you
tell it otherwise.

- A session does not get the server's token. It does hold the key that signs
  file URLs, so its code can make such a URL for any file in the work folder.
- Sessions are never ended for being idle. Each one holds its 95 MB until the
  server stops.
- Code that loops without calling anything, such as `(loop [] (recur))`,
  cannot be stopped from inside the session. `(System/exit 0)` in a second
  request ends the whole session.
- http-kit holds a request body in memory. Uploads stop at 512 MB by default,
  which is too little for long footage.
- One evaluation may write 10 MB of output. Then the server interrupts it and
  it fails with a line that says so.
- Signed URLs work until they expire, more than once.

## What the experiment should answer

- Does an agent get by with this one call, or does it miss the file tools it
  was trained on?
- How often does bash inside a Clojure string go wrong? And does the agent
  find the way around it, a script uploaded as a file, on its own?
- Does the agent reach for a future and an atom when work takes long, or does
  it wait on one request until its own shell tool gives up?
- Is losing a session on restart a problem in practice?

## Tests

```
bb test
```

## License

MIT
