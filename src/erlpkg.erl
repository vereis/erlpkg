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
%%
%% Note : The given Pkg_name determines which module in an escript is assumed to be
%%        the main entrypoint said escript. I.e. running erlpkg on itself with
%%        'erlpkg erlpkg erlpkg.erl' will produce a working erlpkg escript whereas
%%        trying to create an escript with 'erlpkg erlpkg_escript erlpkg.erl' will not.
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
    <<"#!/usr/bin/env escript\n%%! -A0 +sbtu\n">>.

%% Processes a file and appends its data to an escript package. Determines how to process given file
%% by reading file info to check whether or not input is a directory, and if it isn't, reading file extension.
build_file(Filename) ->
    {ok, File_info} = file:read_file_info(Filename),
    case tuple_to_list(File_info) of
        [file_info, _, directory | _] ->
            build_file(dir, Filename);
        [file_info, _, regular | _] ->
            [$. | Type] = filename:extension(Filename),
            build_file(list_to_atom(Type), Filename)
    end.

%% Compiles a given erlang source file and returns its binary data so we can add it to an escript
%% package.
build_file(erl, Filename) ->
    io:format("==> Including Erlang source file: ~s~n", [Filename]),
    io:format("~4cCompiling Erlang source file in memory....~n", [$ ]),
    {ok, ModuleName, Compiled_file} = compile:file(Filename, [binary]),
    io:format("~4cok.~n", [$ ]),
    {atom_to_list(ModuleName) ++ ".beam", Compiled_file};

%% Zips up a directory and returns its binary data so we can add it to an escript package
build_file(dir, Filename) ->
    io:format("==> Including directory: ~s/~n", [Filename]),
    io:format("~4cCompressing and zipping directory in memory....~n", [$ ]),
    {ok, {_, Zipped_contents}} = zip:create(Filename ++ ".zip", [Filename], [memory, {compress, all}]),
    io:format("~4cok.~n", [$ ]),
    {Filename ++ ".zip", Zipped_contents};

%% Reads a given beam file and attaches its binary data so we can add it to an escript package
build_file(beam, Filename) ->
    io:format("==> Including Erlang bytecode file: ~s~n", [Filename]),
    {ok, File_data} = file:read_file(Filename),
    io:format("~4cok.~n", [$ ]),
    {Filename, File_data};

%% Blindly reads a given file for its binary data so we can add it to an escript package
build_file(_, Filename) ->
    io:format("==> Including unknown type file: ~s~n", [Filename]),
    {ok, File_data} = file:read_file(Filename),
    io:format("~4cok.~n", [$ ]),
    {Filename, File_data}.

%% Displays help information
usage_instructions() ->
    io:format("Usage: erlpkg PACKAGE_NAME FILES_TO_INCLUDE...~n~n").
