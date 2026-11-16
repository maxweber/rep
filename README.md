# rep

A REPL without the loop. A caller sends Clojure code in an HTTP POST. The
server evaluates it in a [babashka](https://babashka.org) process that
belongs to the caller's session and answers the printed text. The loop stays
with the caller.

The caller I have in mind is a coding agent such as Claude Code. It already
knows how to send an HTTP request and how to read bash output. With rep that
is enough to work on another machine: run code there, keep definitions between
requests, run bash, move files in and out.

There is no client to install. The protocol is a POST with a token in a
header, so `curl` is the client.

This is an experiment and it runs on babashka only.

## Try it

Start a server. Without a public key it is a try on a laptop: it prints one
token that holds for a day.

```
bb serve --port 8730 --dir work
```

Copy the two `export` lines it prints into another terminal, then talk to the
server.

<!-- test: evaluate -->
```
curl -s -H "Authorization: Bearer $REP_TOKEN" --data-binary '(+ 1 2)' "$REP_URL/rep"
```

Code with quotes or more than one line goes through a heredoc, as you wrote
it.

<!-- test: define -->
```
curl -s -H "Authorization: Bearer $REP_TOKEN" --data-binary @- "$REP_URL/rep" <<'EOF'
(def greeting "it's \"quoted\",
on two lines")
(count greeting)
EOF
```

The definition is still there in the next call.

<!-- test: use -->
```
curl -s -H "Authorization: Bearer $REP_TOKEN" --data-binary '(println greeting)' "$REP_URL/rep"
```

`(help)` tells what a session offers.

## The protocol

There is one call.

```
POST /rep
Authorization: Bearer <token>

<Clojure code>
```

The answer is plain text, in the order things happened: what the code
printed, then the value of its last form as EDN. As with `bb -e`, a `nil`
value prints nothing. An error ends the request at its first failing form and
prints as babashka prints it, with line and column.

<!-- test: error -->
```
curl -si -H "Authorization: Bearer $REP_TOKEN" --data-binary '(/ 1 0)' "$REP_URL/rep"
```

The outcome is in the headers, so the text needs no frame around it. An agent
reads the text, a program reads the headers.

| Header | Values |
|---|---|
| `Rep-Status` | `running`, `done`, `failed`, `interrupted` |
| `Rep-Exit` | `0` when done, `1` when failed |
| `Rep-Session` | `started`, `continued`, `restarted` |
| `Rep-Evaluation` | `new`, `replayed` |

The session comes from the token, so one token is one REPL and the caller
keeps no session file. A caller that wants more says so in the path.

```
POST /rep/<session>         another session, a UUID the caller makes up
POST /rep/<session>/<id>    and an id of its own for the evaluation, a UUID too
```

Without an id the server makes one. With an id a POST is safe to send again,
see "Evaluation ids".

A POST waits up to a second for the evaluation. Most finish in that time and
answer with `Rep-Status: done`. A longer one answers `Rep-Status: running` and
streams its output until it ends.

Other answers: 401 without a token the server accepts, 400 for a session or an
id that is no UUID, 409 for an id that was used with other code, 405 for
anything but a POST.

Two more routes move files, see "Files". `GET /` answers a short text that
explains all of this to an agent.

<!-- test: usage -->
```
curl -s "$REP_URL/"
```

## Tokens

rep does not log anybody in. A token is a JWT that somebody else signed with
an Ed25519 private key, and the server verifies it against the public key. It
needs no secret for that and keeps nothing on disk. Whoever runs the machine
hands out the tokens: a host program, a script, a service somewhere else.

The server takes two settings, as options of `rep.server/prepare` or from the
environment.

| Option | Variable | What it is |
|---|---|---|
| `:rep/public-keys` | `REP_PUBLIC_KEYS` | The public keys of whoever signs the tokens. One or several. |
| `:rep/audience` | `REP_AUDIENCE` | The name of this machine. A token's `aud` has to hold it. |

A public key is written in one of two forms.

- The base64 of its X.509 SubjectPublicKeyInfo. That is 60 characters that
  start with `MCowBQYDK2VwAyEA`, the one line in the middle of a PEM file.
- The PEM file itself, `-----BEGIN PUBLIC KEY-----` and what follows, as
  `openssl pkey -pubout` prints it.

Several keys stand in one text, apart by commas, blanks or line breaks. Two
keys are for the day an issuer changes its own. The server refuses to start
with a text that is no Ed25519 public key, and with keys but no audience.

```
openssl genpkey -algorithm ed25519 -out issuer.pem
REP_PUBLIC_KEYS="$(openssl pkey -in issuer.pem -pubout)" REP_AUDIENCE=my-machine bb serve
```

What the server checks, and nothing else lets a token in:

- The token has three parts in base64url without padding and is no longer
  than 4096 characters.
- The header's `alg` is exactly `EdDSA`. A token with `none` or `HS256` is
  refused whatever its signature says.
- One of the configured keys verifies the signature. A key never comes from
  a token: `jwk`, `jku`, `x5u` and `x5c` are ignored. A header with `crit`
  is refused.
- `exp` is there and lies ahead. `nbf`, when it is there, lies behind. The
  server allows no slack for a clock that is off, unless a host sets
  `:rep/leeway`.
- `aud` is there and is the server's audience, or an array that has it.

A host that mounts the handler in its own program (`rep.server/prepare`) can
ask for more, and none of it has a variable of the environment:

- **Keys with names.** `:rep/public-keys` as a map of a name to its key, or
  as a function of a name that answers the key or nil. Then the header's
  `kid` chooses the key, among the server's own and nowhere else, and a
  token whose `kid` names none is refused. `rep.auth/jwks` reads the JWK set
  an issuer publishes, `{"keys":[{"kty":"OKP","crv":"Ed25519","kid":...,
  "x":...}]}`, into such a map, and `rep.auth/key-id` names a key: the
  base64url of the first 8 bytes of the SHA-256 of its 32 raw bytes. The
  function is for a host whose list comes from an address and changes: the
  host fetches and keeps it, rep asks once for every token.
  `rep.token/sign` takes the `kid` as its third argument.
- **`:rep/issuer`**: what `iss` has to be. A server with an issuer takes
  that issuer's tokens, and such a token says who asks and when it was
  made: `sub` has to be a text that is not blank, and `iat` a number that
  does not lie ahead, `:rep/leeway` counted in. A server without an issuer
  reads neither.
- **`:rep/claims`**: more claims a token has to have, a map of a claim's
  name to its value.
- **`:rep/leeway`**: for how many seconds past its `exp` a token still
  counts.
- **A host that vouches.** A host that let a caller in by a check of its own
  puts `:rep/vouched`, a map of claims, into the request it hands to
  `rep.server/handle`. No token is asked for then. No HTTP server puts that
  key into a request, so only the host's code can.

Two more claims name the session of a `POST /rep`: `sid` when the token has
one, else `sub`. A text that is a UUID is the session as it stands. Any other
text becomes one, the name-based UUID of its UTF-8 bytes
(`java.util.UUID/nameUUIDFromBytes`). So `sub` gives every person a REPL, and
`sid` gives one person two. A token with neither works for
`POST /rep/<session>` only.

Every refused request gets the same 401 and the same sentence. The reason
goes to the server's standard error, one line without anything of the request
in it.

A host on babashka signs with `rep.token`. It is not part of the server and
the server never calls it, except for the token of a try on a laptop.

```clojure
(require '[rep.token :as token])

(def pair (token/key-pair))
;; {:token/public-key "MCowBQYDK2VwAyEA..." :token/private-key "..."}

(token/sign {"aud" "my-machine"
             "sub" "max"
             "exp" (+ (quot (System/currentTimeMillis) 1000) 3600)}
            (:token/private-key pair))
```

`sign` also takes the PEM file of a private key. Any other tool that signs a
JWT with Ed25519 does as well: `test/rep/vectors.mjs` makes the tokens of the
tests with Node.

`bb serve` without a public key makes a key pair in memory, prints one token
for the session `laptop` that holds for a day, and forgets the private key.
Nothing of it is on disk and a restart ends the token. It does that on
127.0.0.1 only: with `--ip 0.0.0.0` and no public key it does not start, so a
server that forgot its key prints no working token into a log. With a public
key the server never makes a token.

## Sessions

A session is a babashka process of its own, started on the session's first
request. Definitions stay in it. Every request starts in the namespace `user`.
It is full babashka: `slurp`, `spit`, `future`, atoms, `Thread/sleep`,
`babashka.fs`, `babashka.process`, `babashka.http-client`. Its working
directory is the work folder. A server can also run one process for all
sessions, see "One shared image" below.

rep adds two functions to `user`: `sh` and `help`.

A session takes a second request while the first still runs. They share all
definitions.

A session lives in memory. When its process ends, or the server restarts, the
next answer carries `Rep-Session: restarted`, and the earlier definitions are
gone. Keep what must survive in files.

On my laptop a session starts in about 0.3 seconds and holds about 95 MB. A
later request takes about 7 ms.

While a session runs, babashka keeps the port of its nREPL server in the file
`.nrepl-port` in the work folder. An editor can attach there and see the same
definitions. With several sessions the file names the newest one.

## Bash

A model learned bash from raw bash output. So the output of a script goes into
the answer as bash wrote it, standard error included and in order, with no
JSON and no Clojure string around it.

<!-- test: bash -->
```
curl -s -H "Authorization: Bearer $REP_TOKEN" --data-binary @- "$REP_URL/rep" <<'EOF'
(sh "uname -s; ls nothing-here")
EOF
```

`(sh script)` answers `{:exit <code>}`. A script that fails is a value, not an
error, so code can branch on it. Every call is a new bash process in the work
folder. A `cd` or a variable does not carry over to the next one.
`(with-out-str (sh script))` gives the output as a string.

On the way in, the script is a Clojure string, so the caller escapes `"` and
`\`. A script that is awkward to quote goes up as a file and runs from there.

<!-- test: script -->
```
curl -s -H "Authorization: Bearer $REP_TOKEN" -T cut.sh "$REP_URL/file/scripts/cut.sh"

curl -s -H "Authorization: Bearer $REP_TOKEN" --data-binary '(sh "bash scripts/cut.sh")' "$REP_URL/rep"
```

## Long work

There is no call to follow or stop an evaluation. Clojure has what it takes.
Start the work in a future, keep the result in an atom, ask in a later
request.

<!-- test: future -->
```
curl -s -H "Authorization: Bearer $REP_TOKEN" --data-binary @- "$REP_URL/rep" <<'EOF'
(def result (atom nil))
(def job (future (reset! result (with-out-str (sh "./render.sh")))))
EOF
```

<!-- test: ask -->
```
curl -s -H "Authorization: Bearer $REP_TOKEN" --data-binary '[(realized? job) @result]' "$REP_URL/rep"
```

`(future-cancel job)` ends it, and the script with it. What a future prints
after its request ended is lost, so keep it in the atom or in a file.

A request that hangs without a future is not lost either. A second request
can end the script with `(sh "pkill -f render.sh")`. The last resort is
`(System/exit 0)`. It ends the session's process, every request in it fails,
and the next one starts a new process.

The evaluation itself does not depend on the connection that started it. When
the caller hangs up, it runs on. An agent that expects a long answer or a
shaky line gives the POST two UUIDs of its own.

<!-- test: ids -->
```
SESSION=$(uuidgen); ID=$(uuidgen)
```

<!-- test: long -->
```
curl -s -H "Authorization: Bearer $REP_TOKEN" --data-binary @job.clj "$REP_URL/rep/$SESSION/$ID"
```

Did the connection drop? Send the same command again, with the same id.
Nothing runs twice: the answer comes from its first byte and follows the
evaluation to its end. `?from=<bytes you have>` asks for the rest only.

A session the path names is not the token's session. What a `POST /rep`
defined is not in it, so an agent that works this way stays with its
`$SESSION`.

## Evaluation ids

An id in the URL makes a POST safe to repeat. The server keeps the request,
the output and the outcome of every evaluation on disk.

- The same id with the same code runs nothing. It answers the first output,
  and follows it if the evaluation still runs.
- The same id with other code is a 409.
- An evaluation that a restart of the server cut off answers
  `Rep-Status: interrupted`. The server cannot know how far it got and does not
  pretend to.

Make a fresh id for every new piece of code, with `uuidgen`. A model should
not invent ids by hand. If it repeats one, an old answer comes back as if it
were new, and only the header `Rep-Evaluation: replayed` says so.

A POST without an id gets one from the server. Nobody else knows it, so the
same POST again is a new evaluation and runs its code again.

The server keeps an evaluation for seven days (`:rep/keep-days`). It deletes
older ones when it starts and once a day after that. An id that old is
forgotten: the same POST would run its code again. `:rep/keep-days 0` keeps
everything.

## Files

Two routes move a file of the work folder, with the same header as an
evaluation.

```
PUT /file/<path>    the body becomes the file
GET /file/<path>    answers the file, HEAD its length alone
```

<!-- test: put -->
```
curl -s -H "Authorization: Bearer $REP_TOKEN" -T clip.mp4 "$REP_URL/file/in/clip.mp4"
```

<!-- test: get -->
```
curl -s -H "Authorization: Bearer $REP_TOKEN" -o final.mp4 "$REP_URL/file/out/final.mp4"
```

They only reach into the work folder. What counts is where a path leads: a
symbolic link in the work folder that leads out of it, or into rep's own
`.rep` folder with the output of every caller's evaluations, is as forbidden
as the place it leads to. A request without a token gets its 401 before the
server says anything about a path.

The server holds a request body in memory before it looks at a route or a
token. So it takes no body above 32 MB (`:rep/max-body`,
`bb serve --max-body-mb`) and answers a larger one with a 413. A larger file
comes in parts.

```
PUT /file/<path>?offset=<n>
```

- `offset=0` starts the file over.
- An offset that is the file's size appends the body.
- Any other offset is a 409 that changes nothing.

Every answer says how many bytes the file holds, in its text and in the header
`Rep-File-Size`. So the sender needs no count of its own. This loop sends
8 MB a request and takes the next offset from the server.

<!-- test: parts-up -->
```
SIZE=$(wc -c < clip.mp4); OFFSET=0
while [ "$OFFSET" -lt "$SIZE" ]; do
  OFFSET=$(tail -c +$((OFFSET + 1)) clip.mp4 | head -c 8388608 |
    curl -sf -H "Authorization: Bearer $REP_TOKEN" -T - -o /dev/null \
      -w '%header{rep-file-size}' "$REP_URL/file/in/clip.mp4?offset=$OFFSET") || break
done
```

It stops at the first part the server refuses. An upload that was cut goes on
where it stopped: start the loop with `OFFSET` at the bytes the server has,
which `(.length (java.io.File. "in/clip.mp4"))` tells.

Two more things keep an upload in parts safe:

- A file grows to 4096 MB at most (`:rep/max-file-mb`). A part that would pass
  that is a 413 with a sentence. A path takes one part after the other, so
  without the limit a file could fill the disk.
- `&upload=<id>` names the upload a part belongs to, an id the sender makes up
  once per upload. The part at offset 0 puts the id on record, and every later
  part needs a record with its own id. A part with another id, a part whose
  upload is not on record, and a part without an id while an upload is on
  record are each a 409 without `Rep-File-Size` that says why. So two senders
  of one path do not mix their parts unnoticed. `&resume=true` on a later part
  takes the file over for its id. An upload that sent no part for 15 minutes
  is over and off the record. While no upload is on record, a part without an
  id is taken, and so is a plain PUT of a whole file.

A plain GET answers the whole file with its length in `Content-Length`, and a
HEAD answers the length alone. That holds for a file up to 2 GB. http-kit
sends the answer and sets the limit: it puts the body into one buffer, and a
buffer ends at 2 GB. A plain GET of a larger file gets a 400 that names the
size and says to fetch the file in parts. A HEAD of such a file gets the same
400. The door, below, lifts the limit for whoever turns it on.

A GET with a `Range` header answers 206 with that part of the file, at most
8 MB of it, and `Content-Range` says how many bytes the file has. That works
for a file of any size. The range is `bytes=<start>-<end>` or
`bytes=<start>-`. Every other shape, and a range that names no byte of the
file, is a 416. `curl -r` sends the header, and this loop fetches a file of
any size with it.

<!-- test: parts-down -->
```
: > final.mp4; OFFSET=0; SIZE=1
while [ "$OFFSET" -lt "$SIZE" ]; do
  SIZE=$(curl -sf -H "Authorization: Bearer $REP_TOKEN" -r "$OFFSET-" -o part \
    -w '%header{content-range}' "$REP_URL/file/out/final.mp4") || break
  SIZE=${SIZE##*/}; cat part >> final.mp4; OFFSET=$(($(wc -c < final.mp4)))
done
```

A download that was cut goes on the same way: leave out the `: > final.mp4`
and start with `OFFSET` at the bytes that came. Both loops need curl 7.84 or
newer for `%header{}`.

### The door, for a file above 2 GB in one GET

```
bb serve --door
```

`REP_DOOR=1` and the option `:rep/door? true` do the same. With the door a
plain GET answers a file of any size in one request, with its length in
`Content-Length`, and the server holds one megabyte of the file at a time
(`:rep/slice-bytes`). That is for a `curl` that has to fetch a large file in
one request. The loop above needs no door.

The door (`rep.door`) is a listener of rep's own in front of http-kit. It has
the public port, and http-kit listens on a port of 127.0.0.1 behind it. The
door answers a GET or a HEAD of a file itself, slice by slice, with a write
that waits for the client. Every other request it passes on to http-kit as it
is. http-kit cannot send such an answer: it refuses a file above 2 GB, maps a
smaller one into memory whole, and sends a stream chunked, without a
`Content-Length`.

The door is off by default, and here is why: it is our own HTTP listener in
front of everything, and that job usually belongs to a proven server. With the
door on, every request to the public port passes code that has seen far less
traffic than http-kit. Without it nothing of `rep.door` is in the path of a
request. So turn it on when you need the one thing it does, and read its
limits under "Limits" first.

A review on 2026-11-13 found three problems in the door that are not fixed.
Do not put it on a port the internet reaches before the first two are:

- When the process runs out of file descriptors, the door stops accepting
  connections, and only a restart brings it back. Its cap of 1,024
  connections lies above the usual limit of 1,024 open files, and a
  connection can cost two.
- A connection has no time limit. Connections that say nothing can take
  every slot and keep everybody else out for as long as they stay.
- A protocol upgrade, a WebSocket for example, cannot pass. Neither rep nor
  nchat uses one.

## In your own babashka program

A program that calls rep speaks HTTP itself. This is all of it.

<!-- test: babashka -->
```clojure
(require '[babashka.http-client :as http])

(defn rep
  "Evaluates `code` at the server. Answers its text and how it ended."
  [code]
  (let [{:keys [status headers body]}
        (http/post (str (System/getenv "REP_URL") "/rep")
                   {:headers {"Authorization" (str "Bearer " (System/getenv "REP_TOKEN"))}
                    :body code
                    :throw false})]
    {:text body
     :status (get headers "rep-status" (str "HTTP " status))}))

(rep "(+ 1 2)")
;; => {:text "3\n", :status "done"}
```

A program that hosts rep starts it with its options.

```clojure
(require '[rep.server :as rep])

(def system
  (rep/start! (rep/prepare {:rep/dir "work"
                            :rep/public-keys "MCowBQYDK2VwAyEA..."
                            :rep/audience "my-machine"
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
| `:rep/public-keys` | The public keys of whoever signs the tokens, see "Tokens". Without one the server refuses every request. |
| `:rep/audience` | The name of this machine that a token's `aud` has to hold. |
| `:rep/class-path` | A vector of paths that the session's babashka gets on its classpath, after the server's own. `bb serve --class-path a,b` sets it from the command line. |
| `:rep/setup-ms` | How long the setup of a fresh process may take, the prelude included. 60000 by default. |
| `:rep/shared-image?` | All sessions evaluate in one process, see the next section. |
| `:rep/door?` | Puts `rep.door` in front of http-kit, for a plain GET of a file above 2 GB. Off by default, see "The door". |
| `:rep/after-evaluation` | `(fn [w evaluation])`. Runs after every evaluation of a POST. `evaluation` has `:evaluation/session`, `:evaluation/id`, `:evaluation/state`, which is `:done` or `:failed`, `:evaluation/exit`, `:evaluation/code` and `:evaluation/claims`, all claims of the token the POST came with. |
| `:rep/on-image-start` | `(fn [w image])`. Runs after every start of the shared image that worked, its setup code included. `image` has `:image/generation`, `:image/pid`, `:image/port` and `:image/start-ms`. |

Both hooks run in the server's process. The server prints what a hook throws
to standard error, and the request goes on as if nothing happened.

A host that hands out the tokens of its machine makes the key pair itself,
keeps the private key to itself and gives rep the public one. It puts what it
wants to know later into the claims, a `run` next to a person's `sub` for
example. `(rep.auth/claims w token)` answers all claims of a token the server
accepts, and nil for every other.

A host with an HTTP server of its own does not call `rep/start!`. It mounts
the handler and passes on the requests that are rep's.

```clojure
(def w (rep/prepare {:rep/dir "work"
                     :rep/public-keys "MCowBQYDK2VwAyEA..."
                     :rep/audience "my-machine"}))

(defn handler [request]
  (if (rep/rep-path? (:uri request))
    (rep/handle w request)
    (my-own-page request)))

(rep/stop! w)
```

A browser sends no `Authorization` header with a link or an upload form. So a
host that moves files of the work folder for a page does it under a
credential of its own, a cookie say, on a route of its own. It answers a
download with `(rep.files/serve w request path)` and takes an upload with
`(rep.files/store! w request path)`. Both apply the path checks of rep's own
routes, and they answer a `Range` and take an `offset` the same way.

A plain GET through the host's http-kit sends a file up to 2 GB. A larger one
gets a 400 that says to fetch it in parts, and a `Range` works for any size. A
host that wants a file above 2 GB in one GET puts the door in front of its
http-kit, as `bb serve --door` does. Read "The door" above first, it is off by
default for a reason:

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
calling `handler`.

`rep-path?` is true for `/rep` and everything below `/rep/` and `/file/`. It
is false for `/`, where the host has its own page. The host's server has to
be http-kit, because a long evaluation streams its output over an http-kit
channel. The host gives its server the limit for a request body itself,
`(:rep/max-body w)` is rep's. `(rep/start-sweeper! w)` starts the daily
deletion of old evaluations, which `rep/start!` does by itself.

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
Start the server with `--prelude examples/rragent.clj`, then evaluate:

```clojure
(rragent/ask! "Which skills do you offer?")
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

- There is no revocation. A token is good until its `exp`, and the server
  cannot end one before. The issuer has two answers to a token that got out.
  The first is a short life: minutes for a run, hours for a person. The second
  is a new key pair, which ends every token at once.
- A token is good for every session of its server. `sid` and `sub` say where
  a `POST /rep` goes, they keep no caller out of another caller's session.
- The server holds no secret. A session's code can read the public key in the
  server's environment, and a public key signs nothing. What the code can
  still read is a token that a caller left on the machine, in a file or a
  variable. A session does not inherit the variable `REP_TOKEN` from the
  server's environment, and that is all the hiding there is: session and
  server run as one user.
- The server writes one line to standard error for every request it refuses.
  Whoever reaches the port can make that log grow.
- Sessions are never ended for being idle. Each one holds its 95 MB until the
  server stops.
- Code that loops without calling anything, such as `(loop [] (recur))`,
  cannot be stopped from inside the session. `(System/exit 0)` in a second
  request ends the whole session.
- http-kit holds a request body in memory, whoever sends it: the limit of
  32 MB holds before the token is checked. That is what one connection can
  make the server hold. A larger file has to come in parts, see "Files".
  `curl -T` with one PUT stops at the limit.
- One evaluation may write 10 MB of output. Then the server interrupts it and
  it fails with a line that says so.
- A plain GET sends a file up to 2 GB. A larger one comes in parts with
  `Range`, or in one GET with the door on.
- The door, when it is on, is an HTTP listener of our own in front of every
  request. Its tests are all the traffic it has seen, where a proven server
  has had years of it. That is why it is off by default.
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
- Does an agent get by with `curl` alone, or does it miss a command that
  follows a long answer and sends a file in parts for it?

## Tests

```
bb test
```

One of them takes the commands out of this file and runs them with `curl`
against a real server, so the README cannot drift from the routes.

## License

MIT
