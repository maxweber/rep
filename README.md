# rep

A REPL without the loop. A caller sends Clojure code in an HTTP POST. The
server evaluates it in a [SCI](https://github.com/babashka/sci) context and
answers the printed text. The loop stays with the caller.

The caller I have in mind is a coding agent such as Claude Code. It already
knows how to send an HTTP request and how to read bash output. With rep that
is enough to work on another machine: run code there, keep definitions between
requests, run bash, move files in and out.

This is an experiment and it runs on [babashka](https://babashka.org) only.

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

`(help)` lists what the code may call.

## The protocol

| Request | What it does |
|---|---|
| `POST /rep/<session>/<id>` | Evaluates the body and answers its output |
| `GET /rep/<session>/<id>` | The output again. `?from=<bytes>` answers the rest |
| `DELETE /rep/<session>/<id>` | Stops the evaluation |
| `PUT /file/<path>?expires=…&signature=…` | Stores the body as a file in the work folder |
| `GET /file/<path>?expires=…&signature=…` | Answers a file of the work folder |
| `GET /` | A short text that explains all of this to an agent |

`<session>` and `<id>` are UUIDs the caller makes up. The `/rep` requests need
the header `Authorization: Bearer <token>`.

The answer is plain text, in the order things happened: what the code
printed, then its value as EDN. As with `bb -e`, a `nil` value prints nothing.
An error prints as `Error: <message>` with line and column when SCI knows
them.

The outcome is in the headers, so the text needs no frame around it.

| Header | Values |
|---|---|
| `Rep-Status` | `running`, `done`, `failed`, `stopped`, `interrupted` |
| `Rep-Exit` | `0` when done, `1` when failed, `130` when stopped. After bash, the exit code of bash |
| `Rep-Session` | `started`, `continued`, `restarted` |
| `Rep-Evaluation` | `new`, `replayed` |

A POST waits 100 ms for the evaluation. Most finish in that time and answer
with `Rep-Status: done`. A longer one answers `Rep-Status: running` and streams
its output until it ends. Then a `GET` with `?from=<bytes received>` gives the
final headers.

Other answers: 401 without the token, 400 for ids that are no UUIDs, 404 for an
id nobody used, 409 for a busy session or a reused id.

## Bash without quoting

A model learned bash from raw bash output. So the output of a script goes into
the answer byte for byte, standard error included, with no JSON and no Clojure
string around it.

```
bin/rep <<'EOF'
(sh "ffmpeg -version | head -1")
EOF
```

That still leaves quoting on the way in, bash inside a Clojure string. For a
plain script, skip Clojure and send the script as the body:

```
bin/rep --sh <<'EOF'
printf '%s\n' "it's a \"test\"" > note.txt
grep -n "test" note.txt
EOF
```

`--sh` sets `Content-Type: text/x-shellscript`. The exit code of bash becomes
`Rep-Exit` and the exit code of the client. Every script is a new bash process
in the work folder. A `cd` or a variable does not carry over to the next one.

In Clojure, `(sh script)` answers `{:exit <code>}`, so code can branch on it.

## Sessions

A session is one SCI context. Definitions stay in it. Every request starts in
the namespace `user`.

The context has no Java classes and no file or process access of its own. What
reaches the machine goes through six functions: `sh`, `slurp`, `spit`,
`upload-url`, `download-url` and `help`. `slurp` and `spit` only work inside
the work folder.

A session runs one evaluation at a time. A second POST while one runs gets a
409 that names the running id.

The contexts live in memory. After a restart of the server the first answer of
an old session carries `Rep-Session: restarted`, and the client says that the
earlier definitions are gone. Keep what must survive in files.

## Evaluation ids

The id in the URL makes a POST safe to repeat. The server keeps the request,
the output and the outcome of every evaluation on disk.

- The same id with the same body runs nothing. It answers the first output,
  and follows it if the evaluation still runs.
- The same id with another body is a 409.
- An evaluation that a restart cut off answers `Rep-Status: interrupted`. The
  server cannot know how far it got and does not pretend to.

The client makes a fresh id for every call and reuses it when it retries after
a broken connection. A model should not invent ids by hand. If it repeats
one, an old answer comes back as if it were new.

## Long work

An evaluation does not depend on the connection that started it. When the
caller hangs up, it runs on.

```
bin/rep --sh --wait 100 <<'EOF'
./render.sh
EOF
```

After 100 seconds the client prints `rep attach <id>` on standard error and
ends with 124, the exit code of `timeout`. `rep attach <id>` prints the output
from the start and follows it to the end. `rep stop <id>` ends it.

This is there because a coding agent's shell tool gives up after a couple of
minutes, and a render takes longer.

## Files

`(upload-url "in/clip.mp4")` answers a URL that takes a PUT.
`(download-url "out/final.mp4")` answers one for a GET. Both need no token and
expire after 15 minutes, or after the seconds given as a second argument.

```
bin/rep <<'EOF'
(print (upload-url "in/clip.mp4"))
EOF
```

## The client

```
rep [--sh] [--wait SECONDS] < code   evaluates standard input, prints the answer
rep attach ID                        follows an evaluation or prints it again
rep stop ID                          stops an evaluation
rep session                          prints the session id
```

It reads `REP_URL` and `REP_TOKEN`. The session id comes from `REP_SESSION` or
from the file `.rep-session` in the current folder, which the client makes on
first use. One folder, one session.

Link `bin/rep` into your `PATH`. It finds its sources next to itself.

## In your own babashka program

```clojure
(require '[rep.server :as rep])

(def system
  (rep/start!
   (rep/prepare {:rep/dir "work"
                 :rep/namespaces {'shop {'price (fn [amount] (* 2 amount))}}})))

(rep/stop! system)
```

`:rep/namespaces` is how a host gives the code more to call. A caller then
evaluates `(shop/price 21)`. See the docstring of `rep.server/prepare` for the
other options.

## Limits

rep runs the code it is sent. That is its job, so treat the token like a
shell login. The SCI context is closed, but `sh` is not a sandbox. Bash runs
with the rights of the server's user. Run rep on a machine you can throw away.
It listens on 127.0.0.1 unless you tell it otherwise.

- http-kit holds a request body in memory. Uploads stop at 512 MB by default,
  which is too little for long footage.
- A stop kills bash and interrupts the thread. Code that only computes, such as
  `(loop [] (recur))`, does not notice. The evaluation is recorded as stopped
  and its thread is left running.
- One evaluation may write 10 MB of output. Then it fails with a line that
  says so.
- Signed URLs work until they expire, more than once.

## What the experiment should answer

- Does an agent get by with this one tool, or does it miss the file tools it
  was trained on?
- How often does bash inside a Clojure string go wrong, compared with a script
  sent as the body?
- Does the agent use the session's definitions, or does it put everything into
  files anyway?
- Is losing a session on restart a problem in practice?

## Tests

```
bb test
```

## License

MIT
