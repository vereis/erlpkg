%%% Module Description:
%%% Utility package for erlpkg, allows easy extraction of erlpkg components, cleanup of components and
%%% more.
-module(pkgutils).
-author([{"Vereis", "Chris Bailey"}]).

-define(VERSION, "1.0.0").

-vsn(?VERSION).

%% Export all functions if test build
-ifdef(TEST).
    -compile(export_all).
-else.
    -export([
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
        pkg_reflect/0,
        pkg_reflect/1,
        pkg_tmp_dir/0,
        pkg_clean_tmp_dir/0
    ]).
-endif.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - PKG EXTRACTION ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Extracts all files included in erlpkg. If no argument is given we automatically extract
%% to /tmp/pkg_name()/
pkg_extract() ->
    pkg_extract(pkg_tmp_dir()).

pkg_extract(ExtractPath) ->
    % Open our erlpkg, making sure we deal with all the extracted stuff in memory so that we can
    % read the data we need and extract as needed
    {ok, Self} = zip:zip_open(pkg_reflect(archive), [memory]),

    % Extract requested file and write to ExtractPath
    {ok, Files} = zip:zip_get(Self),

    ExtractFile = fun({DataName, DataBinary}) ->
        file:write_file(lists:flatten([ExtractPath, "/", DataName]), DataBinary)
    end,

    [ok = ExtractFile(File) || File <- Files],

    % Close our erlpkg
    zip:zip_close(Self).

%% Extracts a file which is included in erlpkg. If no argument for ExtractPath is given we
%% automatically extract to /tmp/pkg_name()/
pkg_extract_file(FileName) ->
    pkg_extract_file(FileName, pkg_tmp_dir()).

pkg_extract_file(FileName, ExtractPath) ->
    % Open our erlpkg, making sure we deal with all the extracted stuff in memory so that we can
    % read the data we need and extract as needed
    {ok, Self} = zip:zip_open(pkg_reflect(archive), [memory]),

    % Extract requested file and write to ExtractPath
    {ok, {DataName, DataBinary}} = zip:zip_get(FileName, Self),
    ok = file:write_file(lists:flatten([ExtractPath, "/", DataName]), DataBinary),

    % Close our erlpkg
    zip:zip_close(Self).

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
    {ok, Self} = zip:zip_open(pkg_reflect(archive), [memory]),

    % Extract requested dir zip
    {ok, {_, DirZipBinary}} = zip:zip_get(FileName, Self),

    % Extract dir zip to specified directory
    {ok, _} = zip:extract(DirZipBinary, [{cwd, lists:flatten(ExtractPath)}]),

    % Close our erlpkg
    zip:zip_close(Self).

%% Returns path to temp dir for this erlpkg
pkg_tmp_dir() ->
    lists:flatten(["/tmp/", pkg_name()]).

%% Deletes temp dir for this erlpkg
pkg_clean_tmp_dir() ->
    file:del_dir(pkg_tmp_dir()).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - PKG METADATA ------------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Lists files currently included in erlpkg
pkg_ls() ->
    % Read our own data and extract the archive, open it
    {ok, Self} = zip:zip_open(pkg_reflect(archive)),
    {ok, SelfData} = zip:zip_list_dir(Self),

    % Produce a list of filenames that exist within the opened archive
    GetFileName = fun({_, File, _, _, _, _}) -> File end,
    Files = [GetFileName(F) || F <- SelfData, size(F) > 2],

    % Close open archive
    zip:zip_close(Self),
    Files.

%% Returns the current package name
pkg_name() ->
    filename:basename(filename:absname(pkg_rel_path())).

%% Returns the current package location relative to current working directory
pkg_rel_path() ->
    escript:script_name().

%% Returns the current absolute package location
pkg_abs_path() ->
    filename:absname(escript:script_name()).

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





%%% ---------------------------------------------------------------------------------------------%%%
%%% - META FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

-ifdef(TEST).
%% Start eunit testing for this module
eunit() ->
    eunit:test({inparallel, ?MODULE}),
    init:stop().
-endif.