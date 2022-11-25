%%%-------------------------------------------------------------------
%% @doc laika example public API
%% @end
%%%-------------------------------------------------------------------

-module(laika_example).

-behaviour(application).

%% normal application API
-export([start/2, stop/1]).

%% function exported so it can be passed as a handler
%% to the Laika server
%% see the documentation for [Laika](https://github.com/gordonguthrie/laika/blob/main/src/laika.erl)
%% for an explanation of what happens under the hood
-export([dummyHandler/1]).

start(_StartType, _StartArgs) ->
    ok = ssl:start(),
    Port = 1965,
    CertFile = "/laika-example/priv/keys/server.crt",
    KeyFile  = "/laika-example/priv/keys/server.key",
    _PID = laika:start(Port, CertFile, KeyFile, {laika_example, dummyHandler}),
    laika_example_sup:start_link().

stop(_State) ->
    ok.

dummyHandler(#{path := ["test", "input"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"10 What's your name\r\n">>
    ];
dummyHandler(#{path := ["test", "password"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"11 password plz\r\n">>
    ];
dummyHandler(#{path := [], id := Id} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    Top = [
        <<"20 text/gemini\r\n">>,
        <<"=> /test/input test input (status 10)\r\n">>,
        <<"=> /test/password test password input (status 11)\r\n">>,
        <<"=> /test/redirect/temporary test temporary redirects (status 30)\r\n">>,
        <<"=> /test/redirect/permanent test permanent redirects (status 31)\r\n">>,
        <<"=> /test/failure/temporary test temporary failure (status 40)\r\n">>,
        <<"=> /test/failure/permanent test permanent failure (status 50)\r\n">>,
        <<"=> /test/certificate test mandatory certificates (status 60)\r\n">>
    ],
    Bottom = [
        <<"# Header 1\r\n">>,
        <<"## Header 2\r\n">>,
        <<"### Header 3\r\n">>,
        <<"* bingo\r\n">>,
        <<"* bongo\r\n">>,
        <<"# Chess\r\n">>,
        <<"``` alt text\r\n">>,
        <<"   1  2  3  4  5  6  7  8  \r\n">>,
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
        <<"```\r\n">>
    ],
    case Id of
        no_identity -> Top ++ Bottom;
        _           -> URL = <<"/test/nonce/">>,
                       NewURL = make_nonce(URL, Id),
                       Middle = [list_to_binary([<<"=> ">>, NewURL, <<" test actions with nonces\r\n">>])],
                       io:format("Middle is ~s~n", Middle),
                       Top ++ Middle ++ Bottom
    end;
dummyHandler(#{path := ["test", "redirect", "temporary"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"30 /test/redirect/success\r\n">>
    ];
dummyHandler(#{path := ["test", "redirect", "permanent"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"31 /test/redirect/success\r\n">>
    ];
dummyHandler(#{path := ["test", "redirect", "success"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"20 text/gemini\r\n">>,
        <<"successfully redirected\r\n">>
    ];
dummyHandler(#{path := ["test", "failure", "temporary"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"40 temporary failure\r\n">>
    ];
dummyHandler(#{path := ["test", "failure", "permanent"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"50 permanent failure\r\n">>
    ];
dummyHandler(#{path := ["test", "certificate"], id := no_identity} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"60 need certificate\r\n">>
    ];
dummyHandler(#{path := ["test", "certificate"], id := #{key := K, name := N}} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    io:format("name is ~p~nkey is ~p~n", [N, K]),
    [
        <<"20 text/gemini\r\n">>,
        <<"you can see it because you are logged in\r\n">>
    ];
dummyHandler(#{path := ["test", "nonce", _Nonce], id := no_identity} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"60 need certificate\r\n">>
    ];
dummyHandler(#{path := ["test", "nonce", Nonce], id := Id} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    <<"/test/nonce/", CorrectNonce/binary>> = make_nonce("/test/nonce/", Id),
    case list_to_binary(Nonce) of
        CorrectNonce ->
            [
                <<"20 text/gemini\r\n">>,
                <<"your nonce for this page is correct\r\n">>
            ];
        _ ->
            [
                <<"20 text/gemini\r\n">>,
                <<"your nonce for this page is wrong:\r\ngot: ">>,
                Nonce,
                <<"\r\nexp: ">>,
                CorrectNonce,
                <<"\r\n">>
            ]
    end;
dummyHandler(Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
    <<"20 text/gemini\r\n">>,
    <<"404\r\n">>
    ].

make_nonce(URL, #{key := K}) ->
    Nonce = crypto:hash(md5, list_to_binary([URL, integer_to_list(K)])),
    SafeNonce = binary:encode_hex(Nonce),
    list_to_binary([URL, SafeNonce]).