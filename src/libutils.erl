%%% Module Description:
%%% A collection of useful utility functions
-module(libutils).
-author([{"Vereis", "Chris Bailey"}]).
-define(VERSION, "1.0.0").

-vsn(?VERSION).

%% Export all functions if test build
-ifdef(TEST).
    -compile(export_all).
-else.
    -export([
        get_input/1,
        wildcard/1,
        linebreak/2
    ]).
-endif.





%%% -------------------------------------------------------------------------------------------- %%%
%%% - FILE MANIPULATION ------------------------------------------------------------------------ %%%
%%% -------------------------------------------------------------------------------------------- %%%

%% Performs filepath wildcards for any potential wildcard containing paths in a list of paths.
%% Any non-matching wildcards are also returned.
-spec wildcard([file:filename_all()]) -> [file:filename_all()].
wildcard([]) ->
    [];
wildcard(FileList) ->
    lists:foldl(fun(PotentialWildcard, Accumulator) ->
        case filelib:wildcard(PotentialWildcard) of
            [] ->
                Accumulator ++ [PotentialWildcard];
            Matches ->
                Accumulator ++ Matches
        end
    end, [], FileList).





%%% -------------------------------------------------------------------------------------------- %%%
%%% - STRING MANIPULATION ---------------------------------------------------------------------- %%%
%%% -------------------------------------------------------------------------------------------- %%%

%% Splits a long binary or list based string into a list of list based strings where
%% each split string has a maximum length of LineLength + LongestWordLength.
-spec linebreak(binary() | string(), pos_integer()) -> [string()].
linebreak(String, LineLength) when is_binary(String) ->
    linebreak(binary_to_list(String), LineLength);
linebreak(String, LineLength) when is_list(String) ->
    [W | Ws] = string:tokens(String, " "),
    [lists:flatten([Line, "\n"]) || Line <- linebreak(Ws, LineLength, [W], [])].

-spec linebreak(string(), pos_integer(), iolist(), [iolist()]) -> iolist().
linebreak([], _, L, Ls) ->
    Ls ++ [L];
linebreak([W | Ws], LineLength, L, Ls) when length(L) + length(W) >= LineLength ->
    linebreak(Ws, LineLength, [W], Ls ++ [L]);
linebreak([W | Ws], LineLength, L, Ls) ->
    linebreak(Ws, LineLength, lists:flatten(L ++ [" ", W]), Ls).





%%% -------------------------------------------------------------------------------------------- %%%
%%% - IO UTILITY FUNCTIONS --------------------------------------------------------------------- %%%
%%% -------------------------------------------------------------------------------------------- %%%

%% Wrapper around io:get_line which strips off the newline at the end.
-spec get_input(string()) -> string().
get_input(Prompt) ->
    [Char || Char <- io:get_line(Prompt), Char =/= $\n].





%%% -------------------------------------------------------------------------------------------- %%%
%%% - META FUNCTIONS --------------------------------------------------------------------------- %%%
%%% -------------------------------------------------------------------------------------------- %%%

-ifdef(TEST).
%% Start eunit testing for this module
-spec eunit() -> ok.
eunit() ->
    eunit:test({inparallel, ?MODULE}),
    init:stop().
-endif.