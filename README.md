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

Start a server. It prints a login code that works once.

```
bb serve --port 8730 --dir work
```

Log in with the client, then talk to the server. The client reads the code to
evaluate from standard input.

```
export REP_URL=http://127.0.0.1:8730
bin/rep login 7K3M-9QWX-2B4T

bin/rep <<'EOF'
(def files (clojure.string/split-lines (slurp "list.txt")))
(count files)
EOF
```

Or with curl.

```
curl -s -X POST --data-binary "7K3M-9QWX-2B4T" "$REP_URL/login" -o ~/.rep-token
SESSION=$(uuidgen)

curl -s -H "Authorization: Bearer $(cat ~/.rep-token)" \
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

Two more routes move files and two log a caller in and out, see below.
`GET /` answers a short text that explains all of this to an agent.

## Logging in

A secret that passes through a chat should be worth little. So the server
hands out a login code that works once and for ten minutes. The caller
exchanges it for a token and keeps the token itself.

```
POST /login     body = the code, answer = the token and nothing else
POST /logout    ends the token it is sent with
```

`rep login <code>` does the exchange and stores the token in
`~/.config/rep/<host>-<port>.token`, readable by the user only. It never
prints the token and leaves nothing in the current folder. `rep logout` ends
the token and deletes the file. With curl, write the answer of `/login`
straight into a file, as in the example above, so that the token never shows
up in a transcript.

Only a POST looks at a code. A chat app that fetches a pasted link for a
preview burns nothing.

On the server's side:

```
bb code [--dir work]           makes another login code
bb tokens [--dir work]         lists the tokens that are still good
bb revoke <jti> [--dir work]   ends one
```

The token is a JWT, signed with HMAC-SHA256, good for 30 days. The server
keeps a small file per token and refuses a token without its file, which is
how `bb revoke` and `/logout` work. The signing key is the file `secret` in
the state folder, or `REP_SECRET`.

For tests and automation, set `REP_TOKEN` on the server. That static token is
then accepted as a bearer too, and the client sends it when it finds it in its
own environment.

## Sessions

A session is a babashka process of its own, started on the session's first
request. Definitions stay in it. Every request starts in the namespace `user`.
It is full babashka: `slurp`, `spit`, `future`, atoms, `Thread/sleep`,
`babashka.fs`, `babashka.process`, `babashka.http-client`. Its working
directory is the work folder. A server can also run one process for all
sessions, see "One shared image" below.

rep adds four functions to `user`: `sh`, `upload-url`, `download-url` and
`help`.

A session takes a second request while the first still runs. They share all
definitions.

A session lives in memory. When its process ends, or the server restarts, the
next answer carries `Rep-Session: restarted` and the client says that the
earlier definitions are gone. Keep what must survive in files.

On my laptop a session starts in about 0.3 seconds and holds about 95 MB. A
later request takes about 7 ms.

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

The server keeps an evaluation for seven days (`:rep/keep-days`). It deletes
older ones when it starts and once a day after that. An id that old is
forgotten: the same POST would run its code again. `:rep/keep-days 0` keeps
everything.

## Files

`(upload-url "in/clip.mp4")` answers a URL that takes a PUT.
`(download-url "out/final.mp4")` answers one for a GET. Both need no token and
expire after 15 minutes, or after the seconds given as a second argument. They
only reach into the work folder. What counts is where a path leads: a symbolic
link in the work folder that leads out of it, or into rep's own `.rep` folder
with the signing key, is as forbidden as the place it leads to.

```
bin/rep <<'EOF'
(print (upload-url "in/clip.mp4"))
EOF
```

The server holds a request body in memory before it looks at a route or a
credential. So it takes no body above 32 MB (`:rep/max-body`,
`bb serve --max-body-mb`) and answers a larger one with a 413. A larger file
comes in parts through the same signed URL:

```
PUT /file/<path>?expires=...&signature=...&offset=<n>
```

- `offset=0` starts the file over.
- An offset that is the file's size appends the body.
- Any other offset is a 409 that changes nothing.

Every answer says how many bytes the file holds, in its text and in the header
`Rep-File-Size`. A caller that lost an answer sends the part again, reads the
size from the 409 and goes on from there. A PUT without `offset` is the whole
file, as before.

Two more things keep an upload in parts safe:

- A file grows to 4096 MB at most (`:rep/max-file-mb`). A part that would pass
  that is a 413 with a sentence. A signed URL is good for many parts, so
  without the limit it could fill the disk.
- `&upload=<id>` names the upload a part belongs to, an id the sender makes up
  once per upload. The part at offset 0 puts the id on record, and every later
  part needs a record with its own id. A part with another id, a part whose
  upload is not on record, and a part without an id while an upload is on
  record are each a 409 without `Rep-File-Size` that says why. So two senders
  of one path do not mix their parts unnoticed. `&resume=true` on a later part
  takes the file over for its id. An upload that sent no part for 15 minutes
  is over and off the record. While no upload is on record, a part without an
  id is taken as before, and so is a plain PUT of a whole file.

A plain GET answers the whole file, however large, with its length in
`Content-Length`, and a HEAD answers the length alone. http-kit cannot send
such an answer: it refuses a file above 2 GB, maps a smaller one into memory
whole, and sends a stream chunked, without a `Content-Length`. So rep's server
has a listener of its own in front of http-kit, the door (`rep.door`). The
door answers a GET of a file itself, slice by slice, with a write that waits
for the client, and holds one megabyte of the file at a time
(`:rep/slice-bytes`). Every other request it passes on to http-kit as it is.

A GET with a `Range` header answers 206 with that part of the file, at most
8 MB of it, and `Content-Range` says how many bytes the file has. The range is
`bytes=<start>-<end>` or `bytes=<start>-`. Every other shape, and a range that
names no byte of the file, is a 416.

The client does both:

```
bin/rep upload clip.mp4 in/clip.mp4
bin/rep download out/final.mp4 final.mp4
```

It asks the session for the signed URL and sends or fetches the file in parts
of 8 MB. A signed URL holds for 15 minutes and a long transfer outlives it:
when the server refuses the URL in the middle, the client asks the session for
a fresh one and goes on at the same offset.

`--resume` goes on with a transfer that was cut:

```
bin/rep upload --resume clip.mp4 in/clip.mp4       # asks how many bytes the server has, sends the rest
bin/rep download --resume out/final.mp4 final.mp4  # goes on from the bytes that are here
```

A download writes to `final.mp4.part` and renames it at the end, so a download
that breaks leaves no half `final.mp4`. `--resume` goes on from the part file,
and without one it takes `final.mp4` itself for the part that came so far.

## The client

```
rep [--id ID] [--wait SECONDS] < code   evaluates standard input, prints the answer
rep upload [--resume] LOCAL PATH        sends a local file into the work folder, in parts
rep download [--resume] PATH LOCAL      fetches a file of the work folder, in parts
rep login CODE                          exchanges a login code for a token and stores it
rep logout                              ends the stored token
rep session                             prints the session id
```

It reads `REP_URL`. The token is `--token`, else `REP_TOKEN`, else the one
`rep login` stored for that server. The session id comes from `REP_SESSION` or
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
the namespace `shop`, a caller evaluates `(shop/price 21)`.

A prelude fails when its evaluation ends with an error, and only then. What
it prints, to standard error too, is no failure: the server writes it to its
own standard error, every line behind `rep setup:`. The setup of a fresh
process may take a minute (`:rep/setup-ms`). Past that the server ends the
process and the start counts as failed, so a prelude that waits forever does
not hang the server.

More options for a host. The docstring of `rep.server/prepare` has the rest.

| Option | What it does |
|---|---|
| `:rep/class-path` | A vector of paths that the session's babashka gets on its classpath, after the server's own. `bb serve --class-path a,b` sets it from the command line. |
| `:rep/setup-ms` | How long the setup of a fresh process may take, the prelude included. 60000 by default. |
| `:rep/shared-image?` | All sessions evaluate in one process, see the next section. |
| `:rep/after-evaluation` | `(fn [w evaluation])`. Runs after every evaluation of a POST. `evaluation` has `:evaluation/session`, `:evaluation/id`, `:evaluation/state`, which is `:done` or `:failed`, `:evaluation/exit` and `:evaluation/code`. |
| `:rep/on-image-start` | `(fn [w image])`. Runs after every start of the shared image that worked, its setup code included. `image` has `:image/generation`, `:image/pid`, `:image/port` and `:image/start-ms`. |

Both hooks run in the server's process. The server prints what a hook throws
to standard error, and the request goes on as if nothing happened.

A host with an HTTP server of its own does not call `rep/start!`. It mounts
the handler and passes on the requests that are rep's.

```clojure
(def w (rep/prepare {:rep/dir "work"}))

(defn handler [request]
  (if (rep/rep-path? (:uri request))
    (rep/handle w request)
    (my-own-page request)))

(rep/stop! w)
```

A host that serves files of the work folder under a credential of its own, a
cookie say, answers them with `(rep.files/serve w request path)`. It applies
the path checks of the signed route and answers a `Range` the same way.

A plain GET of a file above 2 GB needs the door in front of the host's
http-kit as well, because the body of that answer is the `File` and http-kit
cannot send it:

```clojure
(require '[rep.door :as door])

(def inner (http/run-server handler {:ip "127.0.0.1" :port 0 :legacy-return-value? false}))
(def entrance (door/start! {:door/ip "0.0.0.0"
                            :door/port 8080
                            :door/inner-port (http/server-port inner)
                            :door/handler handler
                            :door/own? rep/file-path?}))
```

`:door/own?` says for a path whether the door answers a GET of it itself, by
calling `handler`. Without the door such a GET gets a 400 that says to ask in
parts.

`rep-path?` is true for `/login`, `/logout` and everything below `/rep/` and
`/file/`. It is false for `/`, where the host has its own page. The host's
server has to be http-kit, because a long evaluation streams its output over
an http-kit channel. The host gives its server the limit for a request body
itself, `(:rep/max-body w)` is rep's. `(rep/start-sweeper! w)` starts the
daily deletion of old evaluations, which `rep/start!` does by itself.

## One shared image

Normally every session has a process of its own. A host that keeps a live
application in babashka wants one process that lasts, the image, and wants
every caller to evaluate in it. Then a function that one caller redefines is
redefined for all.

```
bb serve --shared-image
```

`REP_SHARED_IMAGE=1` and the option `:rep/shared-image? true` do the same.

- All session uuids evaluate in the one process. Each evaluation still has a
  connection and an nREPL session of its own, so two of them run at the same
  time and each starts in the namespace `user`.
- The image starts with the server, not with the first request. Its setup
  code, the `:rep/prelude` file included, runs once per start.
- The server watches the process. When it ends, by `(System/exit 0)`, a crash
  or a `kill`, the server starts the next one right away and waits for no
  request. On my laptop the new image answers about 0.2 seconds after the old
  one ended. An evaluation that ran in the old image fails with the same line
  as in a session's own process.
- An image that fails to start, or ends within five seconds, makes the server
  pause before the next try. The pause is 100 ms, then twice as long each
  time, up to five seconds. So a prelude that throws does not spin. A request
  in that time fails with the reason. `:rep/backoff-ms` and
  `:rep/backoff-max-ms` set the first and the longest pause. A prelude that
  only prints to standard error is no failed start, and one that does not come
  back within `:rep/setup-ms` is.
- Evaluations run side by side, and two callers may reach the server under two
  addresses. The address of each travels with its code, so `upload-url` and
  `download-url` hand every caller a URL for its own.
- `Rep-Session` keeps its meaning per session uuid. It says `started` for a
  uuid the server sees for the first time and `continued` afterwards. It says
  `restarted` when the image started again since that uuid's last evaluation,
  so its definitions are gone. The server tells by the generation of the
  image, the count of its starts.

The callers are not kept apart. Each one sees and can redefine what another
one defined. `(help)` and `GET /` say so.

The host itself evaluates in the image without HTTP.

```clojure
(require '[rep.image :as image])

(image/evaluate w "(shop/price 21)")
;; => {:out "" :value "42" :error nil}

(image/evaluate w "(shop/report)" {:timeout-ms 2000})

(image/status w)
;; => {:image/state :running :image/generation 1 :image/pid 4711 ...}
```

`:out` is what the code printed. `:value` is the value of the last form as
`pr-str` wrote it. `:error` is babashka's error text when the code threw, and
it also tells when the call ran into its timeout or the image is gone. The
call blocks for `:timeout-ms` at most, 10 seconds without the option. It
keeps nothing on disk, and it leaves its connection open for the next call,
so a call takes about 1.5 ms on my laptop. That is cheap enough to ask the
image after every evaluation, from the hook `:rep/after-evaluation`.

`rep.image/evaluate` throws without `:rep/shared-image?`.

`rep/start!` starts the image and comes back when it runs and
`:rep/on-image-start` is through. A host that mounts the handler in its own
HTTP server starts the image itself.

```clojure
(def w (image/start! (rep/prepare {:rep/dir "work" :rep/shared-image? true})))
```

`rep/stop!` ends the image and the watch over it in both cases.

## An example prelude: driving rragent

`examples/rragent.clj` lets a session remote-control the
[rragent](https://github.com/storrito/rragent) that runs on the same machine.
Start the server with `--prelude examples/rragent.clj`, then:

```
bin/rep <<'EOF'
(rragent/ask! "Which skills do you offer?")
EOF
```

`send!` puts a message into the bot's chat the way the page's composer does,
`ask!` also waits for the answer, `latest` shows the newest messages and
`busy?` tells whether the bot still works. The file reads rragent's SQLite
storage directly and posts its page command, so it follows rragent's internals
and breaks when they change. Treat it as a sketch of the idea.

## Limits

rep runs the code it is sent. That is its job, so treat the token like a
shell login. A session is full babashka with the rights of the server's user.
Run rep on a machine you can throw away. It listens on 127.0.0.1 unless you
tell it otherwise.

- An agent can read the token file that `rep login` stored. That is why a
  token only works for this one server, expires and can be revoked.
- The server hands a session neither a token nor the key that signs tokens.
  A session gets a key derived from it that signs file URLs and nothing else.
  But a session runs as the same user as the server, so its code can read the
  file `secret` in the state folder, or the server's environment. Keeping a
  session from that takes a second user or a sandbox, which rep does not set
  up.
- Sessions are never ended for being idle. Each one holds its 95 MB until the
  server stops.
- Code that loops without calling anything, such as `(loop [] (recur))`,
  cannot be stopped from inside the session. `(System/exit 0)` in a second
  request ends the whole session.
- http-kit holds a request body in memory, whoever sends it: the limit of
  32 MB holds before the token is checked. That is what one connection can
  make the server hold. A larger file has to come in parts, which
  `rep upload` does. `curl -T` with one PUT stops at the limit.
- One evaluation may write 10 MB of output. Then the server interrupts it and
  it fails with a line that says so.
- Signed URLs work until they expire, more than once.
- The door gives every connection a thread, and a second one once the
  connection talks to http-kit. It serves 1024 connections at once and closes
  one more. A client that stops reading in the middle of a download holds its
  thread and the open file until it hangs up.
- Behind the door http-kit sees every connection come from 127.0.0.1. rep
  never looks at a caller's address.
- The door answers a file request when it has read it. A client that sends a
  request before it has the answer to the one before, which HTTP allows and
  no browser does, could get the two answers out of order.

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
