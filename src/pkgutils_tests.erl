%%% Module Description:
%%% Test module for pkgutils
-module(pkgutils_tests).
-author([{"Vereis", "Chris Bailey"}]).
-define(VERSION, "1.0.0").

-vsn(?VERSION).

%% Include testing framework
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%%% ---------------------------------------------------------------------------------------------%%%
%%% - TEST CASES (Non-Erlpkg) -------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

pkg_open_test_() ->
    [?_assert(pkgutils:pkg_open() =:= ok)].

pkg_close_test_() ->
    [?_assert(pkgutils:pkg_close(ok) =:= ok),
     ?_assert(pkgutils:pkg_close("String") =:= ok),
     ?_assert(pkgutils:pkg_close(<<"Binary">>) =:= ok),
     ?_assert(pkgutils:pkg_close(12453) =:= ok),
     ?_assert(pkgutils:pkg_close(453.553) =:= ok),
     ?_assert(pkgutils:pkg_close(#{}) =:= ok)].

pkg_reflect_test_() ->
    [?_assert(pkgutils:pkg_reflect() =:= {{emu_args, ""}, {archive, <<>>}}),
     ?_assert(pkgutils:pkg_reflect(emu_args) =:= {emu_args, ""}),
     ?_assert(pkgutils:pkg_reflect(archive) =:= {archive, <<>>})].

pkg_dir_path_test_() ->
    [?_assert(pkgutils:pkg_dir_path() =:= filename:dirname(code:which(pkgutils)))].

pkg_abs_path_test_() ->
    [?_assert(pkgutils:pkg_abs_path() =:= filename:absname(code:which(pkgutils)))].

pkg_rel_path_test_() ->
    [?_assert(pkgutils:pkg_rel_path() =:= code:which(pkgutils))].

pkg_name_test_() ->
    [?_assert(filename:rootname(pkgutils:pkg_name()) =:= "pkgutils")].

pkg_ls_test_() ->
    CurrentDirectory = filename:dirname(code:which(pkgutils)),
    LsCommand = lists:flatten(["ls ", CurrentDirectory]),

    FilesInCurrentDirectory = [filename:absname(F) || F <- string:tokens(os:cmd(LsCommand), "\n")],
    [?_assert(lists:sort(pkgutils:pkg_ls()) =:= lists:sort(FilesInCurrentDirectory))].

pkg_tmp_dir_test_() ->
    Path = pkgutils:pkg_tmp_dir(),
    Test1 = filelib:is_dir(Path),

    pkgutils:pkg_clean_tmp_dir(),
    Test2 = filelib:is_dir(Path),
    [?_assert(Test1 =:= true),
     ?_assert(Test2 =:= false)].

pkg_extract_file_test_() ->
    Path = pkgutils:pkg_tmp_dir(),
    pkgutils:pkg_extract_file(atom_to_list(?MODULE) ++ ".beam"),
    Test1 = filelib:is_file(lists:flatten([Path, "/", atom_to_list(?MODULE) ++ ".beam"])),

    Test2 = filelib:is_file(lists:flatten([Path, "/", "random_thing.erl"])),

    NewPath = "/tmp/abcdef774/",
    pkgutils:pkg_extract_file(atom_to_list(?MODULE) ++ ".beam", NewPath),
    Test3 = filelib:is_file(lists:flatten([NewPath, "/", atom_to_list(?MODULE) ++ ".beam"])),

    Test4 = filelib:is_file(lists:flatten([NewPath, "/", "random_thing.erl"])),

    % Clean up NewPath
    {ok, Files} = file:list_dir(NewPath),
    [ok = file:delete(lists:flatten([NewPath, "/", File])) || File <- Files],
    file:del_dir(NewPath),

    [?_assert(Test1 =:= true),
     ?_assert(Test2 =:= false),
     ?_assert(Test3 =:= true),
     ?_assert(Test4 =:= false)].

pkg_extract_test_() ->
    Path = pkgutils:pkg_tmp_dir(),

    pkgutils:pkg_extract(),
    Test1 = [filename:basename(F) || F <- filelib:wildcard(lists:flatten([Path, "/*"]))] =:=
            [filename:basename(F) || F <- filelib:wildcard(lists:flatten([pkgutils:pkg_dir_path(), "/*"]))],

    NewPath = "/tmp/pkgextracttest2/",
    pkgutils:pkg_extract(NewPath),
    Test2 = [filename:basename(F) || F <- filelib:wildcard(lists:flatten([NewPath, "/*"]))] =:=
            [filename:basename(F) || F <- filelib:wildcard(lists:flatten([pkgutils:pkg_dir_path(), "/*"]))],

    % Clean up NewPath
    {ok, Files} = file:list_dir(NewPath),
    [ok = file:delete(lists:flatten([NewPath, "/", File])) || File <- Files],
    file:del_dir(NewPath),

    [?_assert(Test1 =:= true),
     ?_assert(Test2 =:= true)].

pkg_is_erlpkg_test_() ->
    [?_assert(pkgutils:pkg_is_erlpkg() =:= false)].

%%% TODO: pkg_extract_zip_tests_()