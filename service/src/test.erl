-module(test).

-export([
         hello/0
        ]).

hello() -> io:format("~p ~p!", ["hello", test_dep:hello()]).
