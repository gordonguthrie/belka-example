% # Belka Example

% The example is basically just a module with an exported function.

% The `belka` server handles the connection, identifies the client does all the SSL and so on, but it doesn't know what to do with particular requests, how to handle them

% This is where the callbacks come in. The `belka` server has a `module`/`function` pair passed in when it starts and whenever it gets a valid request it simply calls that callback with a `route`

-module(belka_example_callbacks).

% We use the salt to season our crypto - make it unique to this site.

% #### if you base your server on this example CHANGE THE SALT TO SOMETHING ELSE

-define(SALT, "second dog in space").

% ## Public API

%% There is only one function exported so it can be passed as a handler to the Belka server
%% see the documentation for [Belka](https://github.com/gordonguthrie/belka/blob/main/src/belka.erl) for an explanation of what happens under the hood
-export([simpleRouter/1]).

% ## `case` Statements And Erlang `function heads`

% If you don't know Erlang this code will read a bit odd. You probably know what a case statement is tho, right?
% ```erlang
% function(c)
% ...
%   case c of
%     something      -> do_something(c)
%     something_else -> do_something_else(c)
%     default        -> handle_default(c)
%   end
% end
% ```

% Well Erlang is a pattern matching language and so you can push the case statements into what are called function heads. This app could be written as a single function call with a case statement for each of the 15 different cases handled. Instead its written as 15 different function definitions with a pattern match in each. When the function is called these act like a case statement:
%
% * if the parameters the function is being called with match the pattern match in the 1st clause then the first clause runs, if not the 2nd clause is checked.
% * if the parameters mactch the 2nd, its invoked, if not, carry on
% * and so on and so forth
% ^

% Just like in a case statement, the paramaters might match on multiple patterns so
% the order in which you write the cases matters.

% ## The Complete `gemini://` Protocol Implementation

% When you are reading this next bit of code start with Route 5 and compare it to the home page then look at the URLs on the home page in the browser and find the clause they match with here.

% ### Route 1

% The first route is actually the path that responds to the 2nd route.  It is used to handle input - which appear as query `key`/`value` pairs. This route's response value `20 text/gemini` which is just a successful request to a gemini server with a response type of `20` and a response consisting of `gemini text` for the client to display
simpleRouter(#{path := ["test", "input"], querykvs := [{Something, true}]} = Route) ->
    io:format("handler (1) got route ~p~n", [Route]),
    [
        "20 text/gemini\r\n you inputted: ",
        Something,
        "\r\n"
    ];

% ### Route 2

% In this route we want the user to provide us with data so we return a response type of `10` with a text of 10. This user prompt that the user gets is 'What's your name?'
simpleRouter(#{path := ["test", "input"]} = Route) ->
    io:format("handler (2) got route ~p~n", [Route]),
    [
        "10 What's your name?\r\n"
    ];

% ### Route 3

% This path respondes to the next, 4th route. In that route a return type of `11` is used.  The client takes this as a request to do privacy and typically hides what the user is inputting - in the case of the next, 4th, invocation we are asking for a password so hidding it makes sense. For didact reasons we echo back what the user posted as plain text
simpleRouter(#{path := ["test", "password"], querykvs := [{Pwd, true}]} = Route) ->
    io:format("handler (3) got route ~p~n", [Route]),
    [
        "20 text/gemini\r\n your password is ",
        Pwd,
        "\r\n"
    ];

% ### Route 4

% This is the path that uses the reponse code `11` to prompt the user for  password
simpleRouter(#{path := ["test", "password"]} = Route) ->
    io:format("handler (4) got route ~p~n", [Route]),
    [
        "11 password plz\r\n"
    ];

% ### Route 5

% This is the home page - when you navigate to this page is shows you links that we use to generate all the use cases - to understand it you have to map the URLs exposed to the user with the function heads that pattern match to them.
%
% Good luck ;-)
%
% Couple of things to notice:
%
% * the home page is different depending on whether you have
% provided an identity or not
%      * if you think about it, there is no way to demonstrate the use of cryptographic identities without allowing you to use the sight both with and without a cryptographic identity.
% * this function head also introduces another critical concept - a `nonce`
%      * please see the private function `make_nonce/2` for details
% * the top of the page exposes URLs to test all the return types
% * the bottom of the pages shows what features of the (very limited) markdown supported by the MIME type `text/gemini` are handled by your client
%      * implementation of markdown is left to clients but this set is the canonical set
% ^
simpleRouter(#{path := [], id := Id} = Route) ->
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

% ### Route 6

% This route tests that redirects work - hit this URL and a code `30` will be issued with a path the clients's job is to redirect to that path - in this case the one handled by clause 8 - this is expected to be a temporary redirect
simpleRouter(#{path := ["test", "redirect", "temporary"]} = Route) ->
    io:format("handler (6) got route ~p~n", [Route]),
    [
        "30 /test/redirect/success\r\n"
    ];

% ### Route 7

% This route tests that redirects work - same as previous clause except a permanent redirect not a temporary one marked by response type `31`
simpleRouter(#{path := ["test", "redirect", "permanent"]} = Route) ->
    io:format("handler (7) got route ~p~n", [Route]),
    [
        "31 /test/redirect/success\r\n"
    ];

% ### Route 8

% This is the success page for the 2 previous clauses
simpleRouter(#{path := ["test", "redirect", "success"]} = Route) ->
    io:format("handler (8) got route ~p~n", [Route]),
    [
        "20 text/gemini\r\n",
        "successfully redirected\r\n"
    ];

% ### Route 9

% A type `40` response is when something has gone wrong with the server ***its not your fault, its ours***. Nothing has gone wrong here, but we are pretending something has
simpleRouter(#{path := ["test", "failure", "temporary"]} = Route) ->
    io:format("handler (9) got route ~p~n", [Route]),
    [
        "40 temporary failure\r\n"
    ];

% ### Route 10

% A type `50` response is when the user has mucked up in someway, something is missing from their request
simpleRouter(#{path := ["test", "failure", "permanent"]} = Route) ->
    io:format("handler (10) got route ~p~n", [Route]),
    [
        "50 permanent failure\r\n"
    ];

% ### Route 11

% A type '51' is a user error, used when the path is not being handled - you have ended up in **Area 51** the X-Files zone ;-)
simpleRouter(#{path := ["test", "certificate"], id := no_identity} = Route) ->
    io:format("handler (11) got route ~p~n", [Route]),
    [
        "60 need certificate\r\n"
    ];
simpleRouter(#{path := ["test", "certificate"], id := #{key := K, name := N}} = Route) ->
    io:format("handler (12) got route ~p~n", [Route]),
    [
        "20 text/gemini\r\n",
        "you can see it because you are logged in as\r\n",
        N,
        "\r\nwith key\r\n",
        K,
        "\r\n"
    ];

% ### Route 12

% In route 5  - the home page - we checked if the user had provided us with an ID If they had we exposed this route. In this clause we check that they are logged in, if not we issue a code `60` **go to jail crimi!**
simpleRouter(#{path := ["test", "nonce", _Nonce], id := no_identity} = Route) ->
    io:format("handler (13) got route ~p~n", [Route]),
    [
        "60 need certificate\r\n"
    ];

% ### Route 13

% This route is the twin of Route 12 - where we check the user is logged in and has supplied the correct nonce
simpleRouter(#{path := ["test", "nonce", Nonce], id := Id} = Route) ->
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

% ### Route 14

% This is the default route - normally this is where you would put a `51` response.
simpleRouter(Route) ->
    io:format("handler (14) got route ~p~n", [Route]),
    [
    "20 text/gemini\r\n",
    "404\r\n"
    ].

% ## Private Functions

% ### The Nonce

% We use nonces to stop cross-site attacks. I want to user to do stuff:
%
% * create something
% * delete something
% * supply some info
% ^

% These actions are bound to a URL. If a bad hat knows the URL structure they can create a dummy link on another `gemini://` site and try and trick the user to click knowing it will have an effect on this one.

% To counter that we `hide` all actions that we want the user to take behind URLs that require them to have provided an idenity and then we decorate our action URLs with `nonce` segements generated from the base of the URL and the user's public key.
%
% This gives us ***unguessable*** action URLs.
%
% We use the salt previously defined as a macro at the top of this module to make this unique.

make_nonce(URL, #{key := K}) ->
    Nonce = crypto:hash(md5, list_to_binary([?SALT, URL, integer_to_list(K)])),
    SafeNonce = binary:encode_hex(Nonce),
    URL ++ binary_to_list(SafeNonce).