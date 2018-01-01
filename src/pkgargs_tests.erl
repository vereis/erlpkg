%%% Module Description:
%%% Test module for pkgargs
-module(pkgargs_tests).
-author([{"Vereis", "Chris Bailey"}]).
-define(VERSION, "1.0.0").

-vsn(?VERSION).

%% Include testing framework
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(ARGDEF, [
    {["-a", "--aaaa"], o_a, is_set, false, ""},
    {["-b", "--bbbb"], o_b, singleton, true, ""},
    {["-c", "--cccc", "something"], o_c, many, [1, 2, 3], ""},
    {["-d", "--dddd", "something_else"], o_d, 5, [], ""}
]).

%% Tell dialyzer to shut up about test generator things never matching.
-dialyzer(no_match).

%%% ---------------------------------------------------------------------------------------------%%%
%%% - TEST CASES --------------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

create_name_id_mapping_test_() ->
    % Expected output of create_name_id_mapping(?ARGDEF)
    ExpectedOutput = #{"--aaaa" => o_a,
                       "--bbbb" => o_b,
                       "--cccc" => o_c,
                       "--dddd" => o_d,
                       "-a" => o_a,
                       "-b" => o_b,
                       "-c" => o_c,
                       "-d" => o_d,
                       "something" => o_c,
                       "something_else" => o_d},

    ActualOutput = pkgargs:create_name_id_mapping(?ARGDEF),

    % Sanity check output
    Ids = maps:values(ActualOutput),
    UniqueIds = lists:usort(Ids),

    [?_assert(ActualOutput =:= ExpectedOutput),
     ?_assert(length(Ids) =:= length(maps:values(ExpectedOutput))),
     ?_assert(length(UniqueIds) =:= length(?ARGDEF))].

create_arg_defaults_test_() ->
    OpMap = pkgargs:create_name_id_mapping(?ARGDEF),

    ExpectedOutput = #{o_a => false,
                       o_b => true,
                       o_c => [1, 2, 3],
                       o_d => []},

    ActualOutput = pkgargs:create_arg_defaults(?ARGDEF, OpMap),

    [?_assert(ActualOutput =:= ExpectedOutput),
     ?_assert(length(maps:values(ActualOutput)) =:=
              length(lists:usort(maps:values(OpMap))))].

parse_args_test_() ->
    OpDefs = ?ARGDEF,
    OpMap = pkgargs:create_name_id_mapping(OpDefs),
    DefaultArgs = pkgargs:create_arg_defaults(OpDefs, OpMap),

    ArgStrings = [[],                                                                % 1
                  ["first", "second", "third", "fourth", "fifth"],                   % 2
                  ["first", "--random", "---c", "_.~~@", "52625"],                   % 3
                  ["first", "-a", "second", "fourth", "fifth"],                      % 4
                  ["first", "--aaaa", "second", "third", "fifth"],                   % 5
                  ["first", "-b", "SINGLETON", "second", "third"],                   % 6
                  ["first", "--bbbb", "SINGLETON", "second", "third"],               % 7
                  ["-c", "FIRST", "second", "third", "fourth", "fifth"],             % 8
                  ["--cccc", "FIRST", "second", "third", "fourth"],                  % 9
                  ["-c", "FIRST", "-c", "SECOND", "-c", "THIRD"],                    % 10
                  ["-c", "FIRST", "--cccc", "SECOND", "third", "fourth"],            % 11
                  ["-d", "first", "second", "third", "fourth", "fifth", "sixth"]],   % 12

    ExceptionCausingArgStrings = [["-d", "first", "second"]],

    Outputs = [maps:to_list(pkgargs:parse_args(Args, DefaultArgs, OpMap, OpDefs)) || Args <- ArgStrings],

    T = fun(Nth, Key) ->
        pkgargs:get(Key, lists:nth(Nth, Outputs))
    end,

    [?_assert(lists:nth(1, Outputs) =:= [{default, []} | maps:to_list(DefaultArgs)]),
     ?_assert(T(2, default) =:= ["first", "second", "third", "fourth", "fifth"]),
     ?_assert(T(2, o_a) =:= pkgargs:get_option_default(o_a, OpDefs)),
     ?_assert(T(2, o_b) =:= pkgargs:get_option_default(o_b, OpDefs)),
     ?_assert(T(2, o_c) =:= pkgargs:get_option_default(o_c, OpDefs)),
     ?_assert(T(2, o_d) =:= pkgargs:get_option_default(o_d, OpDefs)),
     ?_assert(T(3, o_a) =:= pkgargs:get_option_default(o_a, OpDefs)),
     ?_assert(T(3, o_b) =:= pkgargs:get_option_default(o_b, OpDefs)),
     ?_assert(T(3, o_c) =:= pkgargs:get_option_default(o_c, OpDefs)),
     ?_assert(T(3, o_d) =:= pkgargs:get_option_default(o_d, OpDefs)),
     ?_assert(T(4, o_a) =:= true),
     ?_assert(T(5, o_a) =:= true),
     ?_assert(T(6, o_b) =:= "SINGLETON"),
     ?_assert(T(7, o_b) =:= "SINGLETON"),
     ?_assert(T(8, o_c) =:= pkgargs:get_option_default(o_c, OpDefs) ++ ["FIRST"]),
     ?_assert(T(9, o_c) =:= pkgargs:get_option_default(o_c, OpDefs) ++ ["FIRST"]),
     ?_assert(T(10, o_c) =:= pkgargs:get_option_default(o_c, OpDefs) ++ ["FIRST", "SECOND", "THIRD"]),
     ?_assert(T(11, o_c) =:= pkgargs:get_option_default(o_c, OpDefs) ++ ["FIRST", "SECOND"]),
     ?_assert(T(12, o_d) =:= ["first", "second", "third", "fourth", "fifth"]),
     ?_assert(T(12, default) =:= ["sixth"]),
     ?_assertError(badarg,
                   pkgargs:parse_args(lists:nth(1, ExceptionCausingArgStrings), DefaultArgs, OpMap, OpDefs))].

parabreak_test_() ->
    String = lists:flatten(["Mundi vitae labitur ne per. Minim aeterno has et, dicat tation ",
                            "patrioque ne mel. Justo doctus commodo et mei. Eius adhuc in nam, ",
                            "semper labitur tincidunt duo ad. Magna quidam eos ne, sumo doctus ",
                            "lobortis ut eam. Dicit cetero imperdiet vis ad, mel inani eligendi ",
                            "antiopam te, solet phaedrum corrumpit ei eos! Mei at illud qualisque? ",
                            "Pri alii magna habemus id. His no nostrum patrioque, ex atqui ",
                            "indoctum qui? Vix malis dolor cu! Habemus facilisis est ex, has te ",
                            "omnis fabulas, qui ea scripta consetetur. Has lorem electram ex, ",
                            "ponderum eleifend cum te, id aliquip pertinax pro."]),

    BinString = list_to_binary(String),

    %% The parabreak function doesn't cut off words, allowing room for small amounts of overflow.
    %% Logic suggests the length of a line shouldn't overflow more than the lenghth of the longest
    %% word however, so we need to find out the length of the longest word.
    [AcceptedErr | _] = lists:sort(fun(A, B) -> A > B end,
                                   [length(Word) || Word <- string:tokens(String, " ")]),

    %% Test boilerplate
    T = fun (CharCount, Str) ->
        lists:all(fun(Line) ->
            length(Line) < CharCount + AcceptedErr
        end, pkgargs:parabreak(Str, CharCount))
    end,

    [?_assert(T(100, String) =:= true),
     ?_assert(T(90,  String) =:= true),
     ?_assert(T(80,  String) =:= true),
     ?_assert(T(70,  String) =:= true),
     ?_assert(T(60,  String) =:= true),
     ?_assert(T(50,  String) =:= true),
     ?_assert(T(40,  String) =:= true),
     ?_assert(T(30,  String) =:= true),
     ?_assert(T(20,  String) =:= true),
     ?_assert(T(10,  String) =:= true),
     ?_assert(T(100, BinString) =:= true),
     ?_assert(T(90,  BinString) =:= true),
     ?_assert(T(80,  BinString) =:= true),
     ?_assert(T(70,  BinString) =:= true),
     ?_assert(T(60,  BinString) =:= true),
     ?_assert(T(50,  BinString) =:= true),
     ?_assert(T(40,  BinString) =:= true),
     ?_assert(T(30,  BinString) =:= true),
     ?_assert(T(20,  BinString) =:= true),
     ?_assert(T(10,  BinString) =:= true)].