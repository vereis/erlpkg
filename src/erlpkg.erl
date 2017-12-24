%%% Module Description:
%%% Builds an escript package out of a list of given Erlang packages
-module(erlpkg).
-author([{"Vereis", "Chris Bailey"}]).
-vsn("2.0.0").

-export([
    main/1,
    build/3
]).

-define(DEFAULT_ARGS, #{
    "-output" => default,
    "-entrypoint" => default
}).

%% Entrypoint into erlpkg
%% Processes arguments with pkrargs, and sets the variables 'Files_to_include', 'Output_dir',
%% 'Escript_mode' and 'Entrypoint' to whatever is set in the Args, or whatever the default is.
main(Args) ->
    Processed_args = pkgargs:parse(Args, ?DEFAULT_ARGS),
    Files_to_include = maps:get("normal_args", Processed_args),

    % Display usage instructions or error message, or build package.
    case length(Files_to_include) >= 1 of
        true ->
            % Set the variables we'll need to build
            case maps:get("-entrypoint", Processed_args) of
                default ->
                    [Entrypoint | _] = Files_to_include;
                Custom_Entrypoint ->
                    Entrypoint = Custom_Entrypoint
            end,
            case maps:get("-output", Processed_args) of
                default ->
                    Pkg_name = [filename:rootname(Entrypoint), ".erlpkg"];
                Custom_Pkg_name ->
                    Pkg_name = Custom_Pkg_name
            end,

            % and proceed to build escript
            build(Entrypoint, Files_to_include, Pkg_name);
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
build(Entrypoint, Files_to_include, Pkg_name) ->
    io:format("Preparing to build package: ~s...~n", [Pkg_name]),
    
    Pkg_header = build_header(Entrypoint),
    Pkg_contents = [build_file(File_to_include) || File_to_include <- Files_to_include],
    
    {ok, {_, Zipped_contents}} = zip:create(Pkg_name, Pkg_contents, [memory, {compress, all}]),
    Pkg = iolist_to_binary([Pkg_header, Zipped_contents]),
    file:write_file(Pkg_name, Pkg),

    io:format("Successfully built package: ~s~n~n", [Pkg_name]).

%% Shorthand for building escript headers
build_header(Entrypoint) ->
    [
        <<"#!/usr/bin/env escript\n%%! -A0 +sbtu">>, 
        <<" ">>,
        [<<"-escript main ">>, filename:rootname(Entrypoint)], 
        <<"\n">>
    ].

%% Processes a file and appends its data to an escript package. Determines how to process given file
%% by reading file info to check whether or not input is a directory, and if it isn't, reading file extension.
build_file(Filename) when is_binary(Filename) ->
    build_file(binary_to_list(Filename));
build_file(Filename) when is_list(Filename) ->
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
    io:format(
        "Usage:  ~s FILES... [-entrypoint <module>] [-output <filename>]~n" ++
        "Entrypoint is the first file argument without the file's file extension unless otherwise specified~n" ++
        "Output file is the same as the entrypoint with the file extension '.erlpkg' unless otherwise specified~n~n",
        [filename:basename(escript:script_name())]
    ).
