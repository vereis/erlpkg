%%% Module Description:
%%% Test module for erlpkg
-module(erlpkg_tests).
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

default_pkg_name_test_() ->
    [?_assert(erlpkg:default_pkg_name([]) =:= "default_pkg_name.erlpkg"),
     ?_assert(erlpkg:default_pkg_name(["something", "something_else"]) =:= "something.erlpkg"),
     ?_assert(erlpkg:default_pkg_name([<<"something">>]) =:= "something.erlpkg"),
     ?_assert(erlpkg:default_pkg_name([<<"some/randomly/complex/../long/path/something">>])
                                        =:= "something.erlpkg"),
     ?_assert(erlpkg:default_pkg_name(["some/randomly/complex/../long/path/something"])
                                        =:= "something.erlpkg"),
     ?_assert(erlpkg:default_pkg_name(["another/really/../complex/path/something.extension"])
                                        =:= "something.erlpkg"),
     ?_assert(erlpkg:default_pkg_name([<<"another/really/../complex/path/something.extension">>])
                                        =:= "something.erlpkg"),
     ?_assertThrow({invalid, "package name"}, erlpkg:default_pkg_name([<<>>])),
     ?_assertThrow({invalid, "package name"}, erlpkg:default_pkg_name([""]))].

default_entrypoint_test_() ->
    [?_assert(erlpkg:default_entrypoint([]) =:= default),
     ?_assert(erlpkg:default_entrypoint(["something", "something_else"]) =:= "something"),
     ?_assert(erlpkg:default_entrypoint([<<"something">>]) =:= "something"),
     ?_assert(erlpkg:default_entrypoint([<<"some/randomly/complex/../long/path/something">>])
                                        =:= "something"),
     ?_assert(erlpkg:default_entrypoint(["some/randomly/complex/../long/path/something"])
                                        =:= "something"),
     ?_assert(erlpkg:default_entrypoint(["another/really/../complex/path/something.extension"])
                                        =:= "something"),
     ?_assert(erlpkg:default_entrypoint([<<"another/really/../complex/path/something.extension">>])
                                        =:= "something"),
     ?_assertThrow({invalid, "entry point"}, erlpkg:default_entrypoint([<<>>])),
     ?_assertThrow({invalid, "entry point"}, erlpkg:default_entrypoint([""]))].