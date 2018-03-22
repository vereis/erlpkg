%%% Module Description:
%%% A module which builds a boilerplate Erlpkg project
-module(boilerplate).
-author([{"Vereis", "Chris Bailey"}]).
-define(VERSION, "1.0.0").

-vsn(?VERSION).

%% Export all functions if test build
-ifdef(TEST).
    -compile(export_all).
-else.
    -export([
        generate/0
    ]).
-endif.






%%% ---------------------------------------------------------------------------------------------%%%
%%% - BOILERPLATE GEN ---------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% Gitignore content macro
-define(GITIGNORE,
string:join([
"### ERLANG STUFF ###",
".eunit",
"deps",
"*.o",
"*.beam",
"*.plt",
"erl_crash.dump",
".concrete/DEV_MODE",
".rebar",
".log",
"### END OF ERLANG STUFF ###",
"### ERLPKG STUFF ###",
"ebin/*",
"edebug/*",
"etesting/*",
"### END OF ERLPKG STUFF ###"],
"\n")
).

%% Creates a directory containing optional git information, including erlpkg and creating a simple
%% hello_world application.
-spec generate() -> ok.
generate() ->
    % Make directory to contain everything
    ProjectName = libutils:get_input("==> Name of Project? (Used for directory containing project)\n    "),
    file:make_dir(ProjectName),
    io:format("    ok.~n"),

    % Check if user is using git, and if so do a git init
    case libutils:get_input("==> Initialise with Git? (y/n)\n    ") of
        "y"  -> io:format("    ok.~n"),
                io:format("==> Setting up .git in ./~s/~n    ok.~n", [ProjectName]),
                os:cmd(lists:flatten(["git init ", ProjectName])),
                io:format("==> Creating .gitignore in ./~s/~n    ok.~n", [ProjectName]),
                file:write_file(lists:flatten([ProjectName, "/.gitignore"]), ?GITIGNORE);
        _    -> io:format("    ok.~n")
    end,

    % Make standard directories
    io:format("==> Creating standard directories~n"),
    Srcdir = ProjectName ++ "/src",
    Utildir = ProjectName ++ "/util",
    file:make_dir(Srcdir),
    file:make_dir(Utildir),
    io:format("    ok.~n"),

    % Extract 'Hello world' example to newly created project src dir
    io:format("==> Writing example module and eunit test in ./~s/~s/~n", [ProjectName, "src"]),
    pkgutils:pkg_extract_file("hello.src", Srcdir),
    pkgutils:pkg_extract_file("hello_tests.src", Srcdir),
    file:rename(lists:flatten(Srcdir, "/hello.src"), Srcdir ++ "/hello.erl"),
    file:rename(lists:flatten(Srcdir, "/hello_tests.src"), Srcdir ++ "/hello_tests.erl"),
    io:format("    ok.~n"),

    % Extract Elvis and Elvis.config to newly created project util dir
    io:format("==> Configuring 'Elvis' (Linter)~n"),
    pkgutils:pkg_extract_file("elvis", Utildir),
    pkgutils:pkg_extract_file("elvis.config", Utildir),
    io:format("    ok.~n"),

    % Copy a copy of erlpkg into utildir
    io:format("==> Configuring 'Erlpkg'~n"),
    file:copy(pkgutils:pkg_abs_path(), Utildir ++ "/" ++ pkgutils:pkg_name()),
    io:format("    ok.~n"),

    % Extract makefile
    io:format("==> Creating makefile~n"),
    pkgutils:pkg_extract_file("makefile", ProjectName),
    io:format("    ok.~n"),
    io:format("Project '~s' successfully built in ./~s/~n~n", [ProjectName, ProjectName]),

    ok.