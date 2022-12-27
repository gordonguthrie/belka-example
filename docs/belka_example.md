# Belka Example Application

```erlang

-module(belka_example).

```

This is a normal Erlang OTP application - it exposes the callbacks
which are defined in the `application` behaviour.

```erlang

-behaviour(application).

```

## Public API

Nothing unusual the normal application API

```erlang

-export([start/2, stop/1]).

```

The `start` function starts the secure socket layer (`ssl`) which Gemini needs

We define the port using the default `gemini://` port number og `1965`
(the year of the Mercury mission fact fans).

In theory this port is conventional - you could change it, but some `gemini://` clients
don't accept URLs with non-default port numbers in, so YMMV

`start` also picks up certificates and keys which define this server. `gemini://`
conventional uses both self-signed keys and self-signed certificates and
a new set for your server can be generated with the batch file `generate_self_signed_certs.sh`

***Remember:*** you gotta edit that batch file with your org name, the URL you are
serving `gemini://` no and your contact details and stuff

```erlang

start(_StartType, _StartArgs) ->
    ok = ssl:start(),
    Port = 1965,
    CertFile = "/belka-example/priv/keys/server.crt",
    KeyFile  = "/belka-example/priv/keys/server.key",
    _PID = belka:start(Port, CertFile, KeyFile, {belka_example_callbacks, simpleRouter}),
    belka_example_sup:start_link().

stop(_State) ->
    ok.

```

There are no private functions
