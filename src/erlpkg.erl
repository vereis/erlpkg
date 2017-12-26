%%% Module Description:
%%% Builds an escript package out of a list of given Erlang packages
-module(erlpkg).
-author([{"Vereis", "Chris Bailey"}]).
-vsn("3.0.0").

-export([
    main/0,
    main/1,
    build/3
]).

-define(DEFAULT_ARGS, [
    {["-e", "--entrypoint"], o_entry,  singleton, default, "Sets the entry point for the erlpkg which is the " ++
                                                           "module we start the erlpkg from. The default value for" ++
                                                           "this is the first module argument provided."},
    {["-o", "--output"],     o_output, singleton, default, "Sets the output name for the erlpkg. The default " ++
                                                           "value for this is the first module argument provided " ++
                                                           "with the extension '.erlpkg'"},
    {["-h", "--help"],       o_help,   is_set,    false,   "Displays this help message and exits."}
]).

-define(COND(Cond, A, B), [
    case Cond of true -> A; false -> B end
]).


%% Entrypoint into erlpkg
%% Processes arguments with pkrargs, and sets the variables 'Files', 'Output_dir',
%% 'Escript_mode' and 'Entrypoint' to whatever is set in the Args, or whatever the default is.
main() ->
    main(init:get_plain_arguments()).

main(Args) ->
    ParsedArgs      = pkgargs:parse(Args,   ?DEFAULT_ARGS),
    ShowHelp        = pkgargs:get(o_help,   ParsedArgs),

    case ShowHelp of
        true ->
            help();
        false ->
            Files = pkgargs:get(default,  ParsedArgs),
            case length(Files) >= 1 of
                true ->
                    % Set entrypoint to either a specified entrypoint or the default value for it
                    Entrypoint = ?COND(pkgargs:get(o_entry, ParsedArgs) =:= default,
                                          filename:rootname(filename:basename(lists:nth(1, Files))),
                                          pkgargs:get(o_entry, ParsedArgs)),

                    % Set pkg_name to either a specified pkg_name or the default value for it
                    PkgName   = ?COND(pkgargs:get(o_output, ParsedArgs) =:= default,
                                          [filename:rootname(Entrypoint), ".erlpkg"],
                                          pkgargs:get(o_output, ParsedArgs)),

                    % Begin to build
                    build(Entrypoint, Files, PkgName);
                _ ->
                    help()
            end
    end,

    % Clean up and stop
    init:stop().


%% Takes a list of parameters, starting with a Package Name and then a list of files
%% to include in said package, and builds an escript package.
%%
%% Note : The given PkgName determines which module in an escript is assumed to be
%%        the main entrypoint said escript. I.e. running erlpkg on itself with
%%        'erlpkg erlpkg erlpkg.erl' will produce a working erlpkg escript whereas
%%        trying to create an escript with 'erlpkg erlpkg_escript erlpkg.erl' will not.
build(Entrypoint, Files, PkgName) ->
    io:format("Preparing to build package: ~s...~n", [PkgName]),
    PkgHeader = build_header(Entrypoint),
    PkgContents = [build_file(File) || File <- Files],

    {ok, {_, Zip}} = zip:create(PkgName, PkgContents, [memory, {compress, all}]),
    Pkg = iolist_to_binary([PkgHeader, Zip]),
    ok = file:write_file(PkgName, Pkg),

    io:format("Successfully built package: ~s~n~n", [PkgName]).


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
build_file(Filename) when is_atom(Filename) ->
    build_file(atom_to_list(Filename));
build_file(Filename) when is_binary(Filename) ->
    build_file(binary_to_list(Filename));
build_file(Filename) when is_list(Filename) ->
    {ok, FileInfo} = file:read_file_info(Filename),
    case tuple_to_list(FileInfo) of
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
    {ok, ModuleName, CompiledFile} = compile:file(Filename, [binary]),
    io:format("~4cok.~n", [$ ]),
    {atom_to_list(ModuleName) ++ ".beam", CompiledFile};

%% Zips up a directory and returns its binary data so we can add it to an escript package
build_file(dir, Filename) ->
    io:format("==> Including directory: ~s~n", [Filename]),
    io:format("~4cCompressing and zipping directory in memory....~n", [$ ]),
    {ok, {_, Zip}} = zip:create(filename:basename(Filename) ++ ".zip", [Filename], [memory, {compress, all}]),
    io:format("~4cok.~n", [$ ]),
    {filename:basename(Filename) ++ ".zip", Zip};

%% Reads a given beam file and attaches its binary data so we can add it to an escript package
build_file(beam, Filename) ->
    io:format("==> Including Erlang bytecode file: ~s~n", [Filename]),
    {ok, Data} = file:read_file(Filename),
    io:format("~4cok.~n", [$ ]),
    {filename:basename(Filename), Data};

%% Blindly reads a given file for its binary data so we can add it to an escript package
build_file(_, Filename) ->
    io:format("==> Including unknown type file: ~s~n", [Filename]),
    {ok, Data} = file:read_file(Filename),
    io:format("~4cok.~n", [$ ]),
    {filename:basename(Filename), Data}.


%% Displays help information
help() ->
    io:format(
        "Usage: ~s FILE... [-e <module>] [-o <filename>]~n" ++
        "Generate an Erlang Escript with the FILEs you specify.~n" ++
        "Example: ~s calc.erl sci_calc.erl stats_calc.erl -e calc -o calculator.erlpkg~n~n" ++
        "Configuration Parameters:~n" ++
        "~s~n",
        [
            filename:basename(escript:script_name()),
            filename:basename(escript:script_name()),
            pkgargs:create_help_string(?DEFAULT_ARGS, 1, 21)
        ]
    ).
