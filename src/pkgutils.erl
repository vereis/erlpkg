%%% Module Description:
%%% Utility package for erlpkg, allows easy extraction of erlpkg components, cleanup of components and
%%% more.
%%%
%%% Note that this module is only intended to be run as an included module of an erlpkg.
%%% I'm trying to make this work as best as possible in the shell but I'm unsure polyfilling functions
%%% which semantically make no sense in a given context is a good idea.
-module(pkgutils).
-author([{"Vereis", "Chris Bailey"}]).

-define(VERSION, "1.1.0").

-vsn(?VERSION).

%% Export all functions if test build
-ifdef(TEST).
    -compile(export_all).
-else.
    -export([
        pkg_is_erlpkg/0,
        pkg_extract/0,
        pkg_extract/1,
        pkg_extract_file/1,
        pkg_extract_file/2,
        pkg_extract_dir/1,
        pkg_extract_dir/2,
        pkg_extract_zip/1,
        pkg_extract_zip/2,
        pkg_ls/0,
        pkg_name/0,
        pkg_rel_path/0,
        pkg_abs_path/0,
        pkg_dir_path/0,
        pkg_reflect/0,
        pkg_reflect/1,
        pkg_tmp_dir/0,
        pkg_clean_tmp_dir/0
    ]).
-endif.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - PKG INFO ----------------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Determines whether or not this is being run from an erlpkg
-ifdef(ERLPKG).
pkg_is_erlpkg() ->
    true.
-else.
pkg_is_erlpkg() ->
    false.
-endif.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - PKG EXTRACTION ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Extracts all files included in erlpkg. If no argument is given we automatically extract
%% to /tmp/pkg_name()/
pkg_extract() ->
    pkg_extract(pkg_tmp_dir()).

pkg_extract(ExtractPath) ->
    % If we're an erlpkg, just get all the files in the current zip pkg and put them in a list,
    % If we're not an erlpkg, get all the files in the current directory relative to this module
    % and read those files into a list.
    case pkg_is_erlpkg() of
        true ->
            Self = pkg_open(),
            {ok, Files} = zip:zip_get(Self),
            pkg_close(Self);
        _ ->
            ReadFile = fun(FileName) ->
                {ok, Data} = file:read_file(FileName),
                {filename:basename(FileName), Data}
            end,
            Files = [ReadFile(File) || File <- pkg_ls()]
    end,

    % Prepare to extract files to Extract path
    ExtractFile = fun({DataName, DataBinary}) ->
        file:write_file(lists:flatten([ExtractPath, "/", DataName]), DataBinary)
    end,

    [ExtractFile(File) || File <- Files],
    ok.

%% Extracts a file which is included in erlpkg. If no argument for ExtractPath is given we
%% automatically extract to /tmp/pkg_name()/
pkg_extract_file(FileName) ->
    pkg_extract_file(FileName, pkg_tmp_dir()).

pkg_extract_file(FileName, ExtractPath) ->
    case pkg_is_erlpkg() of
        true ->
            Self = pkg_open(),
            {ok, {DataName, DataBinary}} = zip:zip_get(FileName, Self),
            pkg_close(Self);
        _ ->
            {ok, Data} = file:read_file(FileName),
            {DataName, DataBinary} = {filename:basename(FileName), Data}
    end,
    ok = file:write_file(lists:flatten([ExtractPath, "/", DataName]), DataBinary).

%% Directories in erlpkgs are zipped and thus, when extracting a directory from an erlpkg we need
%% to perform extra extraction steps. If no ExtractPath is given we extract to /tmp/pkg_name()/
pkg_extract_dir(FileName) ->
    pkg_extract_zip(FileName).

pkg_extract_dir(FileName, ExtractPath) ->
    pkg_extract_zip(FileName, ExtractPath).

pkg_extract_zip(FileName) ->
    pkg_extract_zip(FileName, pkg_tmp_dir()).

pkg_extract_zip(FileName, ExtractPath) ->
    % Open our erlpkg, making sure we deal with all the extracted stuff in memory so that we can
    % read the data we need and extract as needed
    Self = pkg_open(),

    % Extract requested dir zip
    {ok, {_, DirZipBinary}} = zip:zip_get(FileName, Self),

    % Extract dir zip to specified directory
    {ok, _} = zip:extract(DirZipBinary, [{cwd, lists:flatten(ExtractPath)}]),

    % Close our erlpkg
    pkg_close(Self).

%% Returns path to temp dir for this erlpkg, also ensures tmp dir exists
pkg_tmp_dir() ->
    Path = lists:flatten(["/tmp/", filename:rootname(pkg_name())]),
    ok = filelib:ensure_dir(Path ++ "/"),
    Path.

%% Deletes temp dir for this erlpkg, because del_dir only works when a given directory is empty,
%% we will need to delete all the files in said directory first.
pkg_clean_tmp_dir() ->
    TmpDir = pkg_tmp_dir(),
    {ok, Files} = file:list_dir(TmpDir),
    [ok = file:delete(lists:flatten([TmpDir, "/", File])) || File <- Files],
    file:del_dir(pkg_tmp_dir()).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - PKG METADATA ------------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Lists files currently included in erlpkg
%% When run when not included in an erlpkg, we list files in the current directory relative to
%% to this module.
pkg_ls() ->
    case pkg_is_erlpkg() of
        true ->
            % Read our own data and extract the archive, open it
            Self = pkg_open(),
            {ok, SelfData} = zip:zip_list_dir(Self),

            % Produce a list of filenames that exist within the opened archive
            GetFileName = fun({_, File, _, _, _, _}) -> File end,
            Files = [GetFileName(F) || F <- SelfData, size(F) > 2],

            % Close open archive
            pkg_close(Self);
        _ ->
            Files = filelib:wildcard(lists:flatten([pkg_dir_path(), "/*"]))
    end,
    lists:sort(Files).

%% Returns the current package name
pkg_name() ->
    filename:basename(pkg_rel_path()).

%% Returns the current package location relative to current working directory
%% escript:script_name() is undefined behaviour when called from outside an escript so
%% we need to ifdef here to polyfill the behaviour for testing sake on the shell.
pkg_rel_path() ->
    case pkg_is_erlpkg() of
        true -> escript:script_name();
        _    -> code:which(?MODULE)
    end.

%% Returns the current absolute package location
pkg_abs_path() ->
    filename:absname(pkg_rel_path()).

%% Returns the directory containing current package
pkg_dir_path() ->
    filename:dirname(pkg_abs_path()).

%% Returns binary data for escript
pkg_reflect() ->
    {ok, [_, _, {emu_args, EmuArgs}, {archive, Archive}]} = escript:extract(pkg_rel_path(), []),
    {EmuArgs, Archive}.

pkg_reflect(emu_args) ->
    {EmuArgs, _} = pkg_reflect(),
    EmuArgs;
pkg_reflect(archive) ->
    {_, Archive} = pkg_reflect(),
    Archive.

%% Opens the current erlpkg so that we can get data from it
pkg_open() ->
    {ok, Handle} = zip:zip_open(pkg_reflect(archive)),
    Handle.

%% Closes the given handle to an erlpkg
pkg_close(Handle) ->
    zip:zip_close(Handle).


%%% ---------------------------------------------------------------------------------------------%%%
%%% - META FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

-ifdef(TEST).
%% Start eunit testing for this module
eunit() ->
    eunit:test({inparallel, ?MODULE}),
    init:stop().
-endif.