%%% Module Description:
%%% Utility package for erlpkg, allows easy extraction of erlpkg components, cleanup of components and
%%% more.
%%%
%%% Note that this module is only intended to be run as an included module of an erlpkg.
%%% I'm trying to make this work as best as possible in the shell but I'm unsure polyfilling functions
%%% which semantically make no sense in a given context is a good idea.
-module(pkgutils).
-author([{"Vereis", "Chris Bailey"}]).

-define(VERSION, "1.2.0").

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
        pkg_clean_tmp_dir/0,
        pkg_open/0,
        pkg_close/1
    ]).
-endif.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - TYPE DEFINITIONS --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

-type emu_args() :: {emu_args, string()}.
-type pkg_archive() :: {archive, binary()}.
-type escript_archive() :: {emu_args(), pkg_archive()}.

-export_type([
    emu_args/0,
    pkg_archive/0,
    escript_archive/0
]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - PKG INFO ----------------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Determines whether or not this is being run from an erlpkg
-ifdef(ERLPKG).
-spec pkg_is_erlpkg() -> true.
pkg_is_erlpkg() ->
    true.
-else.
-spec pkg_is_erlpkg() -> false.
pkg_is_erlpkg() ->
    false.
-endif.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - PKG EXTRACTION ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Extracts all files included in erlpkg. If no argument is given we automatically extract
%% to /tmp/pkg_name()/
%% If we're an erlpkg, just get all the files in the current zip pkg and put them in a list,
%% If we're not an erlpkg, get all the files in the current directory relative to this module
%% and read those files into a list.
-spec pkg_extract() -> [file:filename_all()].
-spec pkg_extract(file:filename_all()) -> [file:filename_all()].
pkg_extract() ->
    pkg_extract(pkg_tmp_dir()).

-ifdef(ERLPKG).
pkg_extract(ExtractPath) ->
    % Ensure ExtractPath exists before proceeding
    ok = filelib:ensure_dir(ExtractPath),

    Self = pkg_open(),
    {ok, Files} = zip:zip_get(Self),
    pkg_close(Self),

    % Prepare to extract files to Extract path
    ExtractFile = fun({DataName, DataBinary}) ->
        OutName = lists:flatten([ExtractPath, "/", DataName]),
        ok = file:write_file(OutName, DataBinary),
        OutName
    end,

    [ExtractFile(File) || File <- Files].
-else.
pkg_extract(ExtractPath) ->
    % Ensure ExtractPath exists before proceeding
    ok = filelib:ensure_dir(ExtractPath),

    ReadFile = fun(FileName) ->
        {ok, Data} = file:read_file(FileName),
        {filename:basename(FileName), Data}
    end,
    Files = [ReadFile(File) || File <- pkg_ls()],

    % Prepare to extract files to Extract path
    ExtractFile = fun({DataName, DataBinary}) ->
        OutName = lists:flatten([ExtractPath, "/", DataName]),
        ok = file:write_file(OutName, DataBinary),
        OutName
    end,

    [ExtractFile(File) || File <- Files].
-endif.

%% Extracts a file which is included in erlpkg. If no argument for ExtractPath is given we
%% automatically extract to /tmp/pkg_name()/
-spec pkg_extract_file(file:filename_all()) -> file:filename_all().
-spec pkg_extract_file(file:filename_all(), file:name_all()) -> file:filename_all().
pkg_extract_file(FileName) ->
    pkg_extract_file(FileName, pkg_tmp_dir()).

-ifdef(ERLPKG).
pkg_extract_file(FileName, ExtractPath) ->
    % Ensure ExtractPath exists before proceeding
    ok = filelib:ensure_dir(ExtractPath),

    Self = pkg_open(),
    {ok, {DataName, DataBinary}} = zip:zip_get(FileName, Self),
    pkg_close(Self),

    OutName = lists:flatten([ExtractPath, "/", DataName]),
    file:write_file(OutName, DataBinary),
    OutName.
-else.
pkg_extract_file(FileName, ExtractPath) ->
    % Ensure ExtractPath exists before proceeding
    ok = filelib:ensure_dir(ExtractPath),

    {ok, Data} = file:read_file(FileName),
    {DataName, DataBinary} = {filename:basename(FileName), Data},

    OutName = lists:flatten([ExtractPath, "/", DataName]),
    file:write_file(OutName, DataBinary),
    OutName.
-endif.

%% Directories in erlpkgs are zipped and thus, when extracting a directory from an erlpkg we need
%% to perform extra extraction steps. If no ExtractPath is given we extract to /tmp/pkg_name()/
-spec pkg_extract_dir(file:filename_all()) -> ok | {error, einval}.
-spec pkg_extract_zip(file:filename_all()) -> ok | {error, einval}.
-spec pkg_extract_dir(file:filename_all(), file:name_all()) -> ok | {error, einval}.
pkg_extract_dir(FileName) ->
    pkg_extract_zip(FileName).

pkg_extract_dir(FileName, ExtractPath) ->
    pkg_extract_zip(FileName, ExtractPath).

pkg_extract_zip(FileName) ->
    pkg_extract_zip(FileName, pkg_tmp_dir()).

-ifdef(ERLPKG).
pkg_extract_zip(FileName, ExtractPath) ->
    % Ensure ExtractPath exists before proceeding
    ok = filelib:ensure_dir(ExtractPath),

    % Open our erlpkg, making sure we deal with all the extracted stuff in memory so that we can
    % read the data we need and extract as needed
    Self = pkg_open(),

    try zip:zip_get(FileName, Self) of
        {ok, {_, DirZipBinary}} ->
            % Extract dir zip to specified directory
            {ok, _} = zip:extract(DirZipBinary, [{cwd, lists:flatten(ExtractPath)}])
    catch
        _ ->
            ok
    end,

    % Close our erlpkg
    pkg_close(Self).
-else.
pkg_extract_zip(_, _) ->
    ok.
-endif.

%% Returns path to temp dir for this erlpkg, also ensures tmp dir exists
-spec pkg_tmp_dir() -> file:filename_all().
pkg_tmp_dir() ->
    Path = lists:flatten(["/tmp/", filename:rootname(pkg_name())]),
    ok = filelib:ensure_dir(Path ++ "/"),
    Path.

%% Deletes temp dir for this erlpkg, because del_dir only works when a given directory is empty,
%% we will need to delete all the files in said directory first.
-spec pkg_clean_tmp_dir() -> ok | {error, _}.
pkg_clean_tmp_dir() ->
    TmpDir = pkg_tmp_dir(),

    % Get the files in TmpDir, and sort by the number of /'s, which equates to how deeply nested
    % each file is, as we need to delete these deepest first
    TmpDirFiles = lists:sort(fun(A, B) ->
        DepthA = length([X || X <- A, X =:= $/]),
        DepthB = length([X || X <- B, X =:= $/]),
        DepthA > DepthB
    end, filelib:wildcard(lists:flatten([TmpDir, "/**"]))),

    [ok = file:delete(File) || File <- TmpDirFiles],
    file:del_dir(pkg_tmp_dir()).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - PKG METADATA ------------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Lists files currently included in erlpkg
%% When run when not included in an erlpkg, we list files in the current directory relative to
%% to this module.
-spec pkg_ls() -> [file:filename_all() | file:name_all()].
-ifdef(ERLPKG).
pkg_ls() ->
    % Read our own data and extract the archive, open it
    Self = pkg_open(),
    {ok, SelfData} = zip:zip_list_dir(Self),

    % Produce a list of filenames that exist within the opened archive
    GetFileName = fun({_, File, _, _, _, _}) -> File end,
    Files = [GetFileName(F) || F <- SelfData, size(F) > 2],

    % Close open archive
    pkg_close(Self),
    lists:sort(Files).
-else.
pkg_ls() ->
    Files = filelib:wildcard(lists:flatten([pkg_dir_path(), "/*"])),
    lists:sort(Files).
-endif.

%% Returns the current package name
-spec pkg_name() -> file:name_all().
pkg_name() ->
    filename:basename(pkg_rel_path()).

%% Returns the current package location relative to current working directory
%% escript:script_name() is undefined behaviour when called from outside an escript so
%% we need to ifdef here to polyfill the behaviour for testing sake on the shell.
-spec pkg_rel_path() -> file:filename_all().
-ifdef(ERLPKG).
pkg_rel_path() ->
    escript:script_name().
-else.
pkg_rel_path() ->
    code:which(?MODULE).
-endif.

%% Returns the current absolute package location
-spec pkg_abs_path() -> file:filename_all().
pkg_abs_path() ->
    filename:absname(pkg_rel_path()).

%% Returns the directory containing current package
-spec pkg_dir_path() -> file:filename_all().
pkg_dir_path() ->
    filename:dirname(pkg_abs_path()).

%% Returns binary data for escript
-spec pkg_reflect() -> escript_archive().
-ifdef(ERLPKG).
pkg_reflect() ->
    {ok, [_, _, {emu_args, EmuArgs}, {archive, Archive}]} = escript:extract(pkg_rel_path(), []),
    {EmuArgs, Archive}.
-else.
pkg_reflect() ->
    {{emu_args, ""}, {archive, <<>>}}.
-endif.

-spec pkg_reflect(emu_args | archive) -> emu_args() | pkg_archive().
pkg_reflect(emu_args) ->
    {EmuArgs, _} = pkg_reflect(),
    EmuArgs;
pkg_reflect(archive) ->
    {_, Archive} = pkg_reflect(),
    Archive.

%% Opens the current erlpkg so that we can get data from it
-spec pkg_open() -> zip:handle() | ok.
-ifdef(ERLPKG).
pkg_open() ->
    {ok, Handle} = zip:zip_open(pkg_reflect(archive), [memory]),
    Handle.
-else.
pkg_open() ->
    ok.
-endif.

%% Closes the given handle to an erlpkg
-spec pkg_close(zip:handle()) -> ok | {error, einval}.
-ifdef(ERLPKG).
pkg_close(Handle) ->
    zip:zip_close(Handle).
-else.
pkg_close(_Handle) ->
    ok.
-endif.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - META FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

-ifdef(TEST).
%% Start eunit testing for this module
%% Note that we dont do this asynchronously because some tests make or remove files,
%% and clean up after themselves. This will cause most extraction tests to fail.
-spec eunit() -> ok.
eunit() ->
    eunit:test(?MODULE),
    init:stop().

%% Create a test function so we can test an erlpkg version of this module
-spec main([string() | atom()]) -> any().
main([Function]) ->
    Fn = list_to_atom(Function),
    apply(?MODULE, Fn, []);
main([Function | Args]) ->
    Fn = list_to_atom(Function),
    apply(?MODULE, Fn, Args).
-endif.