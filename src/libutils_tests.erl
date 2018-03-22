%%% Module Description:
%%% Test module for libutils
-module(libutils_tests).
-author([{"Vereis", "Chris Bailey"}]).
-define(VERSION, "1.0.0").

-vsn(?VERSION).

%% Include testing framework
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%% Tell dialyzer to shut up about test generator things never matching.
-dialyzer(no_match).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - TEST CASES --------------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

linebreak_test_() ->
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

    %% The linebreak function doesn't cut off words, allowing room for small amounts of overflow.
    %% Logic suggests the length of a line shouldn't overflow more than the lenghth of the longest
    %% word however, so we need to find out the length of the longest word.
    [AcceptedErr | _] = lists:sort(fun(A, B) -> A > B end,
                                   [length(Word) || Word <- string:tokens(String, " ")]),

    %% Test boilerplate
    T = fun (CharCount, Str) ->
        lists:all(fun(Line) ->
            length(Line) < CharCount + AcceptedErr
        end, libutils:linebreak(Str, CharCount))
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