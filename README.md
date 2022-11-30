# Belka Example  🚀🐕

An example site using the [Belka Gemini Server](https://github.com/gordonguthrie/belka)

# A better example

This example is deliberately the simplest implementation with a dummy router where URLs are processed in a single multi-head function.

A more sophisticated system would use a router like the [Belka Router](https://github.com/gordonguthrie/bleka-router).

This example also comingles the layout/UI (what the user sees) with the code required to produce it.

A more sophisticated system would use a templating system like [Belka Templates](https://github.com/gordonguthrie/bleka-templates).

An example of an app using both of them can be seen at [Vega and Altair](https://github.com/gordonguthrie/vega_and_altair).

# How to play with Belka/Belka Example

This method assumes you have [Docker](https://www.docker.com/get-started/) installed on your desktop. And doesn't require Erlang to be installed on your machine.

In one terminal:

```
git clone git@github.com:gordonguthrie/belka-example.git
cd belka-example/erlang
./generate_self_signed_certs.sh
cd ../
docker-compose up
```

You now have Belka Example running in a terminal, open another terminal:

```
cd belka-example/docker/scripts
./start_belka_example
```

This will bind that terminal to the running docker instance and log you in.

The directory with the erlang code on your machine is mounted into the container.

Any changes you make on your host will be reflected in the container.

In that terminal:

```
cd /belka-example
rebar3 shell
```

You now have `belka-example` running in a shell

Use your favourite Gemini client to attach to `gemini://localhost` and away you go:
* on Android in the App store [Deedum](https://play.google.com/store/apps/details?id=ca.snoe.deedum&hl=en_GB&gl=US&pli=1)
* on Mac, Windows, Linux, iOS (testflight), Android (beta) [Lagrange](https://gmi.skyjake.fi/lagrange/)

# In production

In production you will need to rejig the certificates as the SSL connection is signed for the URL so gently frig `generate_self_signed_certs.sh`.

At a minimum replace `localhost` with the URL you are deploying to, but it would be polite to replace Belka as well.

The certificate files are read from hard code paths in the docker setup - you will need to fetch them in an appropriate manner for your application.

Also the nonce needs to be salted or it can be trivially spoofed. The salt is hard coded in `belka_example.erl`.

