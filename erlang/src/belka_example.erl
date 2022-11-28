%%%-------------------------------------------------------------------
%% @doc belka example public API
%% @end
%%%-------------------------------------------------------------------

-module(belka_example).

-behaviour(application).

-define(SALT, "second dog in space").

%% normal application API
-export([start/2, stop/1]).

%% function exported so it can be passed as a handler
%% to the Belka server
%% see the documentation for [Belka](https://github.com/gordonguthrie/belka/blob/main/src/belka.erl)
%% for an explanation of what happens under the hood
-export([dummyRouter/1]).

start(_StartType, _StartArgs) ->
    ok = ssl:start(),
    Port = 1965,
    CertFile = "/belka-example/priv/keys/server.crt",
    KeyFile  = "/belka-example/priv/keys/server.key",
    _PID = belka:start(Port, CertFile, KeyFile, {belka_example, dummyRouter}),
    belka_example_sup:start_link().

stop(_State) ->
    ok.

dummyRouter(#{path := ["test", "input"], querykvs := [{Something, true}]} = Route) ->
    io:format("handler (1) got route ~p~n", [Route]),
    [
        "20 text/gemini\r\n you inputted: ",
        Something,
        "\r\n"
    ];
dummyRouter(#{path := ["test", "input"]} = Route) ->
    io:format("handler (2) got route ~p~n", [Route]),
    [
        "10 What's your name?\r\n"
    ];
dummyRouter(#{path := ["test", "password"], querykvs := [{Pwd, true}]} = Route) ->
    io:format("handler (3) got route ~p~n", [Route]),
    [
        "20 text/gemini\r\n your password is ",
        Pwd,
        "\r\n"
    ];
dummyRouter(#{path := ["test", "password"]} = Route) ->
    io:format("handler (4) got route ~p~n", [Route]),
    [
        "11 password plz\r\n"
    ];
dummyRouter(#{path := [], id := Id} = Route) ->
    io:format("handler (5) got route ~p~n", [Route]),
    Top = [
        "20 text/gemini\r\n",
        "=> /test/input test input (status 10)\r\n",
        "=> /test/password test password input (status 11)\r\n",
        "=> /test/redirect/temporary test temporary redirects (status 30)\r\n",
        "=> /test/redirect/permanent test permanent redirects (status 31)\r\n",
        "=> /test/failure/temporary test temporary failure (status 40)\r\n",
        "=> /test/failure/permanent test permanent failure (status 50)\r\n",
        "=> /test/certificate test mandatory certificates (status 60)\r\n"
    ],
    Bottom = [
        "# Header 1\r\n",
        "## Header 2\r\n",
        "### Header 3\r\n",
        "* bingo\r\n",
        "* bongo\r\n",
        "# Chess\r\n",
        "``` alt text\r\n",
        "   1  2  3  4  5  6  7  8  \r\n",
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"a ░♜░ ♞ ░♝░ ♚ ░♛░ ♝ ░♞░ ♜  \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"b  ♟︎ ░♟︎░ ♟︎ ░♟︎░ ♟︎ ░♟︎░ ♟︎ ░♟︎░ \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"c ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"d    ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"e ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"f    ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"g ░♙░ ♙ ░♙░ ♙ ░♙░ ♙ ░♙░ ♙  \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"h  ♖ ░♘░ ♗ ░♕░ ♔ ░♗░ ♘ ░♖░ \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        "```\r\n"
    ],
    case Id of
        no_identity -> Top ++ Bottom;
        _           -> URL = "/test/nonce/",
                       NewURL = make_nonce(URL, Id),
                       Middle = ["=> ", NewURL, " test actions with nonces\r\n"],
                       Top ++ Middle ++ Bottom
    end;
dummyRouter(#{path := ["test", "redirect", "temporary"]} = Route) ->
    io:format("handler (6) got route ~p~n", [Route]),
    [
        "30 /test/redirect/success\r\n"
    ];
dummyRouter(#{path := ["test", "redirect", "permanent"]} = Route) ->
    io:format("handler (7) got route ~p~n", [Route]),
    [
        "31 /test/redirect/success\r\n"
    ];
dummyRouter(#{path := ["test", "redirect", "success"]} = Route) ->
    io:format("handler (8) got route ~p~n", [Route]),
    [
        "20 text/gemini\r\n",
        "successfully redirected\r\n"
    ];
dummyRouter(#{path := ["test", "failure", "temporary"]} = Route) ->
    io:format("handler (9) got route ~p~n", [Route]),
    [
        "40 temporary failure\r\n"
    ];
dummyRouter(#{path := ["test", "failure", "permanent"]} = Route) ->
    io:format("handler (10) got route ~p~n", [Route]),
    [
        "50 permanent failure\r\n"
    ];
dummyRouter(#{path := ["test", "certificate"], id := no_identity} = Route) ->
    io:format("handler (11) got route ~p~n", [Route]),
    [
        "60 need certificate\r\n"
    ];
dummyRouter(#{path := ["test", "certificate"], id := #{key := K, name := N}} = Route) ->
    io:format("handler (12) got route ~p~n", [Route]),
    [
        "20 text/gemini\r\n",
        "you can see it because you are logged in as\r\n",
        N,
        "\r\nwith key\r\n",
        K,
        "\r\n"
    ];
dummyRouter(#{path := ["test", "nonce", _Nonce], id := no_identity} = Route) ->
    io:format("handler (13) got route ~p~n", [Route]),
    [
        "60 need certificate\r\n"
    ];
dummyRouter(#{path := ["test", "nonce", Nonce], id := Id} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    "/test/nonce/" ++ CorrectNonce = make_nonce("/test/nonce/", Id),
    case Nonce of
        CorrectNonce ->
            [
                "20 text/gemini\r\n",
                "your nonce for this page is correct\r\n"
            ];
        _ ->
            [
                "20 text/gemini\r\n",
                "your nonce for this page is wrong:\r\ngot: ",
                Nonce,
                "\r\nexp: ",
                CorrectNonce,
                "\r\n"
            ]
    end;
dummyRouter(Route) ->
    io:format("handler (14) got route ~p~n", [Route]),
    [
    "20 text/gemini\r\n",
    "404\r\n"
    ].

make_nonce(URL, #{key := K}) ->
    Nonce = crypto:hash(md5, list_to_binary([?SALT, URL, integer_to_list(K)])),
    SafeNonce = binary:encode_hex(Nonce),
    URL ++ binary_to_list(SafeNonce).