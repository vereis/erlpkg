%%% Module Description:
%%% Builds an escript package out of a list of given Erlang packages
-module(erlpkg).
-author([{"Vereis", "Chris Bailey"}]).

-define(VERSION, "3.0.0").

-vsn(?VERSION).

%% Export all functions if test build
-ifdef(TEST).
    -compile(export_all).
-else.
    -export([
        main/0,
        main/1,
        build/4
    ]).
-endif.

-define(DEFAULT_ARGS, [
    {["-e", "--entrypoint"], o_entry, singleton, default, "Sets the entry point for the erlpkg which is the " ++
                                                          "module we start the erlpkg from. The default value for " ++
                                                          "this is the first module argument provided."},

    {["-o", "--output"], o_output, singleton, default, "Sets the output name for the erlpkg. The default " ++
                                                       "value for this is the first module argument provided " ++
                                                       "with the extension '.erlpkg'"},

    {["-h", "--help"], o_help, is_set, false, "Displays this help message and exits."},

    {["-v", "--version"], o_vsn, is_set, false, "Displays current build version."},

    {["--no-utils"], o_no_pkg_utils, is_set, false, "Disable automatic inclusion of erlpkg pkg* modules to " ++
                                                    "packages built by erlpkg. If unset, all packages built " ++
                                                    "by erlpkg will include pkgargs and pkgutil modules for " ++
                                                    "convenience."}
]).

-define(COND(Cond, A, B), [
    try Cond of _ -> case Cond of true -> A; false -> B end catch _ -> B end
]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - PUBLIC FUNCTIONS --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Entrypoint into erlpkg
main() ->
    main(init:get_plain_arguments()).

main(Args) ->
    % Normalize args to strings if they're atoms.
    NArgs = [case is_atom(Arg) of true -> atom_to_list(Arg); false -> Arg end || Arg <- Args],

    % Parse Normalized args
    ParsedArgs = pkgargs:parse(NArgs, ?DEFAULT_ARGS),

    % Set ShowHelp to true or false depending on whether or not it was specified in args
    ShowHelp = pkgargs:get(o_help, ParsedArgs),

    % Set ShowVsn to true or false depending on whether or not it was specified in args
    ShowVsn = pkgargs:get(o_vsn, ParsedArgs),

    % Set NoAttachPkgs to true or false depending on whether or not it was specified in args
    AttachPkgs = pkgargs:get(o_no_pkg_utils, ParsedArgs) =:= false,

    % Get a list of files from args to include in escript pkg
    Files = perform_wildcard_matches(pkgargs:get(default,  ParsedArgs)),

    % Set entrypoint to either a specified entrypoint if set in args, or
    % to the default value for it
    Entrypoint = ?COND(pkgargs:get(o_entry, ParsedArgs) =:= default,
                       default_entrypoint(Files),
                       pkgargs:get(o_entry, ParsedArgs)),

    % Set pkg_name to either a specified pkg_name if set in args, or
    % to the default value for it
    PkgName = ?COND(pkgargs:get(o_output, ParsedArgs) =:= default,
                    default_pkg_name(Files),
                    pkgargs:get(o_output, ParsedArgs)),

    try branch(Files, Entrypoint, PkgName, ShowHelp, ShowVsn, AttachPkgs) of
        _ -> ok
    catch
        throw:usage ->
            usage(),
            help();
        throw:help ->
            help();
        throw:vsn ->
            version();
        throw:{bad_filename, File} ->
            io:format("~s: Error - ~s is not a valid filename or could not be found.~nAborting build.~n~n",
                      [pkgutils:pkg_name(), File]);
        throw:{invalid, Thing} ->
            io:format("~s: Invalid ~s given.~nAborting build.~n~n",
                      [pkgutils:pkg_name(), Thing]);
        E ->
            SelfName = pkgutils:pkg_name(),
            io:format("~s: Unknown Error - ~s~n", [SelfName, E]),
            usage(),
            io:format("Try '~s --help' for more information.", [SelfName])
    end,

    % Clean up and stop
    init:stop().





%%% ---------------------------------------------------------------------------------------------%%%
%%% - ENTRYPOINT CODE ---------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Determines which function to run depending on arguments given.
branch(Files, Entrypoint, PkgName,
       _ShowHelp = false, _ShowVsn = false, AttachPkgs) when length(Files) > 0 ->
    build(Files, Entrypoint, PkgName, AttachPkgs);
branch(_, _, _, _ShowHelp = false, _ShowVsn = false, _) ->
    throw(usage);
branch(_, _, _, _ShowHelp = true, _ShowVsn = false, _) ->
    throw(help);
branch(_, _, _, _ShowHelp = false, _ShowVsn = true, _) ->
    throw(vsn);
branch(_, _, _, _, _, _) ->
    throw(unknown).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - BUILDING ERLPKG ---------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Takes a list of parameters, starting with a Package Name and then a list of files
%% to include in said package, and builds an escript package.
%%
%% Note : The given PkgName determines which module in an escript is assumed to be
%%        the main entrypoint said escript. I.e. running erlpkg on itself with
%%        'erlpkg erlpkg erlpkg.erl' will produce a working erlpkg escript whereas
%%        trying to create an escript with 'erlpkg erlpkg_escript erlpkg.erl' will not.
build(Files, Entrypoint, PkgName, AttachPkgs) ->
    io:format("Preparing to build package: ~s...~n", [PkgName]),

    % Include erlpkg utilities in Files before proceeding if we are being run as an erlpkg
    % ourselves
    case pkgutils:pkg_is_erlpkg() and AttachPkgs of
        true ->
            PkgFiles = [pkgutils:pkg_extract_file("pkgargs.beam"),
                        pkgutils:pkg_extract_file("pkgutils.beam") | Files];
        _ ->
            io:format("Note - Not attaching erlpkg utility modules~n"),
            PkgFiles = Files
    end,

    % Generate data we need to put into escript
    PkgHeader = build_header(Entrypoint),
    PkgContents = [build_file(File) || File <- PkgFiles],

    {ok, {_, Zip}} = zip:create(PkgName, PkgContents, [memory, {compress, all}]),
    Pkg = iolist_to_binary([PkgHeader, Zip]),
    ok = file:write_file(PkgName, Pkg),

    % Clean up pkgutil tmp directory
    pkgutils:pkg_clean_tmp_dir(),

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
    case file:read_file_info(Filename) of
        {ok, _} ->
            case filelib:is_dir(Filename) of
                true ->
                    build_file(dir, Filename);
                _ ->
                    [$. | Type] = filename:extension(Filename),
                    build_file(list_to_atom(Type), Filename)
            end;
        _ ->
            throw({bad_filename, Filename})
    end.

%% Compiles a given erlang source file and returns its binary data so we can add it to an escript
%% package.
build_file(erl, Filename) ->
    io:format("==> Including Erlang source file: ~s~n", [Filename]),
    io:format("~4cCompiling Erlang source file in memory....~n", [$ ]),
    {ok, ModuleName, CompiledFile} = compile:file(Filename, [binary, {d, 'ERLPKG'}]),
    io:format("~4cok.~n", [$ ]),
    {atom_to_list(ModuleName) ++ ".beam", CompiledFile};

%% Reads a given directory
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




%%% ---------------------------------------------------------------------------------------------%%%
%%% - MISC FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Displays usage information
usage() ->
    SelfName = pkgutils:pkg_name(),
    io:format("Usage: ~s FILE... [-e <module>] [-o <filename>]~n" ++
              "Generates an Erlang Escript with the FILEs you specify.~n" ++
              "Example: ~s src/*.erl -o ebin/example.erlpkg~n~n",
              [SelfName,
               SelfName]).

%% Displays help information
help() ->
    io:format("Configuration Parameters:~n" ++
              "~s~n",
              [pkgargs:create_help_string(?DEFAULT_ARGS, 1, 55)]).

%% Displays version information
version() ->
    io:format("Current ~s version: v~s~n~n",
              [pkgutils:pkg_name(),
              ?VERSION]).

%% Generates a default package name
default_pkg_name([FirstFile | _]) when is_binary(FirstFile) ->
    default_pkg_name([binary_to_list(FirstFile)]);
default_pkg_name([FirstFile | _]) when is_list(FirstFile) , length(FirstFile) =:= 0 ->
    throw({invalid, "package name"});
default_pkg_name([FirstFile | _]) when is_list(FirstFile) ->
    filename:basename(filename:rootname(FirstFile)) ++ ".erlpkg";
default_pkg_name([]) ->
    "default_pkg_name.erlpkg".

%% Generates a default entrypoint
default_entrypoint([FirstFile | _]) when is_binary(FirstFile) ->
    default_entrypoint([binary_to_list(FirstFile)]);
default_entrypoint([FirstFile | _]) when is_list(FirstFile) , length(FirstFile) =:= 0 ->
    throw({invalid, "entry point"});
default_entrypoint([FirstFile | _]) when is_list(FirstFile) ->
    filename:basename(filename:rootname(FirstFile));
default_entrypoint([]) ->
    default.

%% Looks through a list of file names and expands any wildcards that may exist.
%% None wildcards will just be added to the accumulator so that we can crash gracefully
%% later on.
perform_wildcard_matches([]) ->
    [];
perform_wildcard_matches(FileList) ->
    lists:foldl(fun(PotentialWildcard, Accumulator) ->
        case filelib:wildcard(PotentialWildcard) of
            [] ->
                Accumulator ++ [PotentialWildcard];
            Matches ->
                Accumulator ++ Matches
        end
    end, [], FileList).


%%% ---------------------------------------------------------------------------------------------%%%
%%% - META FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

-ifdef(TEST).
%% Start eunit testing for this module
eunit() ->
    eunit:test({inparallel, ?MODULE}),
    init:stop().
-endif.