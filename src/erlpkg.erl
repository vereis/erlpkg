%%% Module Description:
%%% Builds an escript package out of a list of given Erlang packages
-module(erlpkg).
-author([{"Vereis", "Chris Bailey"}]).
-vsn(1.0).

-export([
    main/1,
    build/1
]).

%% Entrypoint into erlpkg
%% Simply checks the length of arguments to determine whether or not we should show 
%% usage instructions or build.
main(Args) ->
    case length(Args) >= 2 of
        true ->
            build(Args);
        _ ->
            usage_instructions()
    end.

%% Takes a list of parameters, starting with a Package Name and then a list of files 
%% to include in said package, and builds an escript package.
build([Pkg_name | Files_to_include]) ->
    io:format("Preparing to build package: ~p...~n", [Pkg_name]),
    
    Pkg_header = build_header(),
    Pkg_contents = [build_file(File_to_include) || File_to_include <- Files_to_include],
    
    {ok, {_, Zipped_contents}} = zip:create(Pkg_name, Pkg_contents, [memory, {compress, all}]),
    Pkg = iolist_to_binary([Pkg_header, Zipped_contents]),
    file:write_file(Pkg_name, Pkg),

    io:format("Successfully built package: ~s~n~n", [Pkg_name]).

%% Shorthand for building escript headers
build_header() ->
    <<"#!/usr/bin/env escript\n%%\n%%! -A0 +sbtu\n">>.

%% Compiles a given filename and returns its binary data so we can add it to an escript
%% package.
build_file(Filename) ->
    io:format("==> Including file: ~s~n", [Filename]),
    {ok, _, Compiled_file} = compile:file(Filename, [binary]),
    io:format("~4cok.~n", [$ ]),
    {Filename, Compiled_file}.

%% Displays help information
usage_instructions() ->
    io:format("Usage: erlpkg PACKAGE_NAME FILES_TO_INCLUDE...~n~n").
