// Makes test/rep/vectors.json: Ed25519 public keys and tokens for
// rep.auth-test, signed with Node's crypto. rep verifies with the JDK's
// Ed25519, so the vectors check it against a second implementation.
//
//   node test/rep/vectors.mjs > test/rep/vectors.json
//
// The private keys live in this run only and are written nowhere. A new
// run gives new keys and new tokens. The test holds the clock at "now",
// a second of 2023: by the real clock every token here has expired.
import crypto from "node:crypto";

const now = 1700000000;
const audience = "rep-test";

const b64url = (data) => Buffer.from(data).toString("base64url");
const part = (value) => b64url(typeof value === "string" ? value : JSON.stringify(value));
const pair = () => crypto.generateKeyPairSync("ed25519");
const der = (key) => key.export({ type: "spki", format: "der" });
const pem = (key) => key.export({ type: "spki", format: "pem" });

const eddsa = { alg: "EdDSA", typ: "JWT" };
const good = { aud: audience, sub: "max", exp: now + 3600 };

// A token of header and claims, signed with Ed25519 whatever the header says.
const signed = (header, claims, privateKey) => {
  const content = `${part(header)}.${part(claims)}`;
  return `${content}.${b64url(crypto.sign(null, Buffer.from(content), privateKey))}`;
};

// The classic hole: a token "signed" with HMAC, the public key as the secret.
const hmac = (secret) => {
  const content = `${part({ alg: "HS256", typ: "JWT" })}.${part(good)}`;
  return `${content}.${b64url(crypto.createHmac("sha256", secret).update(content).digest())}`;
};

const first = pair();
const second = pair();
const attacker = pair();
const whole = signed(eddsa, good, first.privateKey);
const [header, payload, signature] = whole.split(".");

const accepted = (name, keys, token, claims) => ({ name, keys, token, claims });
const refused = (name, reason, token, keys = ["first"]) => ({ name, keys, token, refused: reason });

const tokens = [
  accepted("one key", ["first"], whole, good),
  accepted("the second of two keys", ["first", "second"], signed(eddsa, good, second.privateKey), good),
  accepted("the first of two keys", ["first", "second"], whole, good),
  ...[{ ...good, aud: ["another-machine", audience] }].map((claims) =>
    accepted("aud as an array", ["first"], signed(eddsa, claims, first.privateKey), claims)),
  ...[{ ...good, sid: "desk", nbf: now, iat: now - 5, run: true, exp: now + 0.5 }].map((claims) =>
    accepted("nbf at this second, exp as a fraction, claims rep does not read", ["first"],
      signed(eddsa, claims, first.privateKey), claims)),
  accepted("a kid in the header, which chooses nothing", ["first"],
    signed({ ...eddsa, kid: "no-such-key" }, good, first.privateKey), good),

  refused("alg none without a signature", "malformed", `${part({ alg: "none" })}.${payload}.`),
  refused("alg none, signed all the same", "algorithm", signed({ alg: "none" }, good, first.privateKey)),
  refused("alg in lower case", "algorithm", signed({ alg: "eddsa" }, good, first.privateKey)),
  refused("no alg", "algorithm", signed({ typ: "JWT" }, good, first.privateKey)),
  refused("HS256 over the public key's PEM text", "algorithm", hmac(pem(first.publicKey))),
  refused("HS256 over the public key's DER", "algorithm", hmac(der(first.publicKey))),
  refused("HS256 over the public key's base64", "algorithm", hmac(der(first.publicKey).toString("base64"))),
  refused("HS256 over the public key's 32 bytes", "algorithm", hmac(der(first.publicKey).subarray(12))),
  refused("a cut signature", "signature", `${header}.${payload}.${signature.slice(0, -2)}`),
  refused("a changed payload", "signature", `${header}.${part({ ...good, sub: "root" })}.${signature}`),
  refused("another key", "signature", signed(eddsa, good, attacker.privateKey)),
  refused("the second key where only the first is configured", "signature", signed(eddsa, good, second.privateKey)),
  refused("a jwk in the header that holds the attacker's key", "signature",
    signed({ ...eddsa, jwk: attacker.publicKey.export({ format: "jwk" }) }, good, attacker.privateKey)),
  refused("a jku, an x5u and an x5c in the header", "signature",
    signed({ ...eddsa, jku: "https://attacker.example/keys.json", x5u: "https://attacker.example/cert.pem",
      x5c: [der(attacker.publicKey).toString("base64")] }, good, attacker.privateKey)),
  refused("a kid in the header and the attacker's key", "signature",
    signed({ ...eddsa, kid: "first" }, good, attacker.privateKey)),
  refused("crit in the header", "critical", signed({ ...eddsa, crit: ["exp"] }, good, first.privateKey)),
  refused("expired", "expired", signed(eddsa, { ...good, exp: now - 1 }, first.privateKey)),
  refused("exp at this second", "expired", signed(eddsa, { ...good, exp: now }, first.privateKey)),
  refused("no exp", "no-exp", signed(eddsa, { aud: audience, sub: "max" }, first.privateKey)),
  refused("exp as a string", "no-exp", signed(eddsa, { ...good, exp: String(now + 3600) }, first.privateKey)),
  refused("not yet valid", "not-yet-valid", signed(eddsa, { ...good, nbf: now + 60 }, first.privateKey)),
  refused("nbf that is no number", "not-yet-valid", signed(eddsa, { ...good, nbf: "soon" }, first.privateKey)),
  refused("no aud", "no-audience", signed(eddsa, { sub: "max", exp: now + 3600 }, first.privateKey)),
  refused("another aud", "audience", signed(eddsa, { ...good, aud: "another-machine" }, first.privateKey)),
  refused("an aud that only starts like this machine's", "audience",
    signed(eddsa, { ...good, aud: `${audience}-2` }, first.privateKey)),
  refused("an aud array without this machine", "audience",
    signed(eddsa, { ...good, aud: ["another-machine", null] }, first.privateKey)),
  refused("four parts", "malformed", `${whole}.${part("more")}`),
  refused("two parts", "malformed", `${header}.${payload}`),
  refused("padding on the signature", "malformed", `${header}.${payload}.${signature}==`),
  refused("a header that is no JSON", "malformed", signed("not json", good, first.privateKey)),
  refused("a payload that is no JSON object", "claims", signed(eddsa, [1, 2, 3], first.privateKey)),
  refused("a huge token", "too-long", signed(eddsa, { ...good, pad: "x".repeat(6000) }, first.privateKey)),
];

console.log(JSON.stringify({
  "made-with": `node ${process.version}, crypto on OpenSSL ${process.versions.openssl}`,
  now,
  audience,
  keys: {
    first: der(first.publicKey).toString("base64"),
    second: der(second.publicKey).toString("base64"),
    "first-pem": pem(first.publicKey),
  },
  tokens,
}, null, 1));
