# Belka Example

## Introduction To The Belka Family

Belka is a family of components that you can use to build [Gemini](https://gemini.circumlunar.space/) servers.

`gemini://` is a comms protocol a bit more sophisticated, modern, secure and privacy aware than `gopher://` and a lot less sophisticated and snoop-on-able than `http://`.

This makes it ideal to build services that are smol, fun, lightweight.

It also makes it an ideal teaching protocol. If you are teaching very new developers then can architect and build their own lightweight apps using `gemini://` whilst reading and understanding the entire codebase.

Belka is designed to support a pedagogic approach based on simplicity, profound understanding and quick results.

## The Belka Example

This repository is a very basic Gemini application which uses every aspect of the Gemini protocol and is designed to help you understand how to build and extend Gemini servers.

The Belka Server (which is included in this repository as a dependency, and is invoked the `rebar.config` which fetches dependencies for the application build.

The Belka Server does all the plumbing to run a Gemini server for you and if you wish to understand in detail how it works, please see the [Belka Documentation](https://gordonguthrie.github.io/belka).

## How It Works

This example application starts a Belka Server and passes in a callback function.

We registered application is

The `belka` server does all the set up and invokes the callback function with a data structure called a route.

It has the following map:

```erlang

#{id       => Id,
  path     => Path,
  querykvs => QueryKVs,
  frag     => Frag}

```

where an id can be one of:

* the atom `no_identity`
* a map with the keys `name` and `key`
    * `key` is a cryptographic public key for the user (and will be unique)
    * `name` is what the users wishes to be called and might not be unique
^

A generic URL looks like this:

```erlang

gemini://example.com:1965/some/path?key1=value1&key2=value2#fragment
------   ----------- ---- --------- ----------------------- --------
scheme      site     port    path              kvs            frag

```

The `belka` server makes sure the `scheme`, `site` and `port` are fine and passes the rest up to the application.

## How to read the code:

* start with the [belka_example](./belka_example.html) module - this is how the application starts
* then read the [belka_example_callbacks](belka_example_callbacks.html) - this is how this example handles the various actions
* finally run the application and poke about the home page in a gemini browser and see how what you do causes which callback to be invoked
^

 <div>
 {% for item in site.data.contents.toc %}
     <h3>{{ item.title }}</h3>
       <ul>
         {% for entry in item.subfolderitems %}
           <li><a href="{{ entry.url }}">{{ entry.page }}</a></li>
         {% endfor %}
       </ul>
   {% endfor %}
 </div>
