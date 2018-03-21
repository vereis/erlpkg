%%% Module Description:
%%% Parses escript package arguments
-module(pkgargs).
-author([{"Vereis", "Chris Bailey"}]).
-define(VERSION, "3.1.0").

-vsn(?VERSION).

%% Export all functions if test build
-ifdef(TEST).
    -compile(export_all).
-else.
    -export([
        get/1,
        get/2,
        parse/2,
        create_help_string/2,
        create_help_string/3,

        get_option/2,
        get_option_names/2,
        get_option_type/2,
        get_option_desc/2,
        get_option_default/2
    ]).
-endif.

%% Process name for our parsed_arg process
-define(Q, parsed_arg_agent__).

%% Any arguments not matching a specified option_definition are returned as
%% a list keyed by the value of this macro.
-define(DEFAULT_ARG_ID, default).

%% Minimum size for the flag field of help description text
-define(MIN_FLAG_COL_SIZE, 16).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - TYPE DEFINITIONS --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

-type arg_name() :: [string()].
-type arg_id() :: atom().
-type arg_type() :: is_set | singleton | many | integer().
-type arg_default() :: any().
-type arg_description() :: string().

-type arg_def() :: {ArgName::arg_name(), ArgId::arg_id(), ArgType::arg_type(),
                    ArgDefault::arg_default(), ArgDescription::arg_description()}.

-type arg_defs() :: [arg_def()].

-type parsed_arg_key_value() :: {arg_id(), any()}.
-type parsed_args_map() :: #{arg_id() => any()}.
-type parsed_args_list() :: [parsed_arg_key_value()].

-type arg_name_id_map() :: #{arg_name() => arg_id()}.

-type arg_parse_mode() :: default | {string(), arg_type()}.

-export_type([
    arg_name/0,
    arg_id/0,
    arg_type/0,
    arg_default/0,
    arg_description/0,

    arg_def/0,
    arg_defs/0,
    parsed_arg_key_value/0,
    parsed_args_list/0
]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - PUBLIC FUNCTIONS --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Parses a list of arguments based on the option_definitions passed in and
%% returns a tuple list containing whatever options were specified in the
%% arguments, or the default value for an option as specified in the option
%% definitions.
%%
%% Option definitions should look like:
%% [
%%     {[A, ...] = OpNames, OpId, OpType, OpDef, Description}
%%     ...
%% ]
%% Where:
%%       - OpNames is a list of strings used to specify options.
%%
%%       - OpId is an atom identifying the option. Returned parsed args will
%%         use OpId as a key.
%%
%%       - OpType is an atom/int which specifies how we handle the options.
%%             - the atom 'is_set' returns true or false based on whether the
%%               option is found in arguments
%%             - the atom 'singleton' returns the latest occurence of said
%%               option (i.e. the option string "-c hello -c world" will return
%%               "world" and not "hello")
%%             - the atom 'many' returns a list of all occurences of said option.
%%               the default value of this ought to be a list.
%%             - an integer specifies how many tokens of the arg string should be
%%               consumed for that option.
%%
%%       - Default is the default value of the option if it isn't found in args
%%
%%       - Description is a string which is used for generated help messages only.
%%
-spec parse([string()], arg_defs()) -> parsed_args_list().
parse(Args, OpDefs) ->
    OpMap = create_name_id_mapping(OpDefs),
    DefaultArgs = create_arg_defaults(OpDefs, OpMap),
    ParsedArgs  = maps:to_list(parse_args(Args, DefaultArgs, OpMap, OpDefs)),

    %% Spawn an agent for querying ParsedArgs
    register(?Q, spawn(fun() -> query_processor(OpDefs, ParsedArgs) end)),

    %% Return ParsedArgs anyway, in case user wants to manually traverse it
    ParsedArgs.

%% Gets the value for an entry of a parsed argument
-spec get(arg_id() | [arg_id()], parsed_args_map())  -> any();
         (arg_id() | [arg_id()], parsed_args_list()) -> any().
get(Key, ParsedArgs) when is_list(ParsedArgs) ->
    get(Key, maps:from_list(ParsedArgs));
get(Key, ParsedArgs) when is_map(ParsedArgs) ->
    case is_list(Key) of
        true ->
            [get(K, ParsedArgs) || K <- Key];
        _ ->
            case maps:is_key(Key, ParsedArgs) of
                true -> maps:get(Key, ParsedArgs);
                _    -> {err, requested_key_not_found}
            end
    end.

%% Queries the parsed_args agent for whatever is stored in a given key
-spec get(arg_id() | [arg_id()]) -> any().
get(Key) ->
    case lists:member(?Q, registered()) of
        false ->
            {err, parse_agent_not_found};
        _    ->
            ?Q ! {self(), Key},
            receive
                {?Q, Key, Value} ->
                    Value
            end,
            Value
    end.




%%% ---------------------------------------------------------------------------------------------%%%
%%% - ARGUMENT PARSING --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Parses a list of strings, Args, updating ArgsBuffer as we go. Looks up
%% OpMap and OpDefs while parsing to perform the correct action
%% on the input string.
-spec parse_args([string()], parsed_args_map(), arg_name_id_map(), arg_defs()) -> parsed_args_map().
parse_args(Args, ArgsBuffer, OpMap, OpDef) ->
    parse_args(Args, ArgsBuffer#{?DEFAULT_ARG_ID => []}, OpMap, OpDef, default).

%% When there are no arguments remaining to parse, just return ArgsBuffer.
-spec parse_args([string()], parsed_args_map(), arg_name_id_map(), arg_defs(), arg_parse_mode()) -> parsed_args_map().
parse_args([], ArgsBuffer, _, _, default) ->
    ArgsBuffer;

%% In the 'default' parse_args option_type, we check to see whether or not the CTok
%% is an option:
%% If it is, we don't do anything other than set the OpType to the type of said option
%% Otherwise, we keep the option_type as 'default' and merely append the current token to the
%% default key list in ArgsBuffer.
parse_args(Args, ArgsBuffer, OpMap, OpDef, default) ->
    [CTok | RToks] = Args,
    case maps:is_key(CTok, OpMap) of
        true ->
            OpId = maps:get(CTok, OpMap),
            OpType = {
                OpId,
                get_option_type(OpId, OpDef)
            },
            UpdatedArgsBuffer = ArgsBuffer;
        false ->
            OpType = default,
            UpdatedArgsBuffer = ArgsBuffer#{
                ?DEFAULT_ARG_ID := maps:get(?DEFAULT_ARG_ID, ArgsBuffer) ++ [CTok]
            }
    end,

    parse_args(RToks, UpdatedArgsBuffer, OpMap, OpDef, OpType);

%% When in the 'is_set' state, we simply update ArgsBuffer to set the PrevTokId
%% key to be true, then continue parsing in the normal state.
parse_args(Args, ArgsBuffer, OpMap, OpDef, {PrevTokId, is_set}) ->
    UpdatedArgsBuffer = ArgsBuffer#{
        PrevTokId := true
    },

    parse_args(Args, UpdatedArgsBuffer, OpMap, OpDef, default);

%% When in the 'singleton' state, we simply update ArgsBuffer to set the PrevTokId
%% key to be the same as the current token. Then we parse the remaining args in the default
%% state.
parse_args(Args, ArgsBuffer, OpMap, OpDef, {PrevTokId, singleton}) ->
    [CTok | RToks] = Args,
    UpdatedArgsBuffer = ArgsBuffer#{
        PrevTokId := CTok
    },

    parse_args(RToks, UpdatedArgsBuffer, OpMap, OpDef, default);

%% When in the 'many' state, we simply update ArgsBuffer to append the current token to
%% the PrevTokId's value which ought to be a list. Then we parse the remaining args in
%% the default state.
parse_args(Args, ArgsBuffer, OpMap, OpDef, {PrevTokId, many}) ->
    [CTok | RToks] = Args,
    UpdatedArgsBuffer = ArgsBuffer#{
        PrevTokId := maps:get(PrevTokId, ArgsBuffer) ++ [CTok]
    },

    parse_args(RToks, UpdatedArgsBuffer, OpMap, OpDef, default);

%% When in an integer state, we simply skip forward and consume the next N tokens from Args and
%% set PrevTokId's value to be the list of consumed tokens. We parse the remaining args in
%% the default state.
parse_args(Args, ArgsBuffer, OpMap, OpDef, {PrevTokId, N}) when is_integer(N)->
    {NToks, RToks} = lists:split(N, Args),
    UpdatedArgsBuffer = ArgsBuffer#{
        PrevTokId := NToks
    },

    parse_args(RToks, UpdatedArgsBuffer, OpMap, OpDef, default).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - UTILITY FUNCTIONS -------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% A process which responds to queries about whether or not args were set.
-spec query_processor(arg_defs(), parsed_args_list()) -> no_return().
query_processor(OpDefs, ParsedArgs) ->
    receive
        {Sender, Arg} when is_atom(Arg) ; is_list(Arg) ->
            case Arg of
                opdef -> Sender ! {?Q, opdef, OpDefs};
                _     -> Sender ! {?Q, Arg, get(Arg, ParsedArgs)}
            end;
        stop ->
            exit(self(), ok);
        _    ->
            ok
    end,
    query_processor(OpDefs, ParsedArgs).

-spec get_arg_defs() -> arg_defs().
get_arg_defs() ->
    case lists:member(?Q, registered()) of
        false ->
            {err, parse_agent_not_found};
        _    ->
            ?Q ! {self(), opdef},
            receive
                {?Q, opdef, Value} ->
                    Value
            end,
            Value
    end.

%% Returns a map which maps option_names to option_ids for easy lookup.
%% Note:
%%      - We assume that the same option_name won't be used for several
%%        option_ids and thus multiple definitions of the option_name will
%%        most likely break. This is untested and unintended behaviour.
-spec create_name_id_mapping(arg_defs()) -> arg_name_id_map().
create_name_id_mapping(OpDefs) ->
    lists:foldl(fun({OpNames, OpId, _, _, _}, Mapping) ->

        % We need to do an inner fold here because there is a possibility
        % of there being more than one option name needing to be added to
        % Mapping
        lists:foldl(fun(OpName, Mapping2) ->
            Mapping2#{OpName => OpId}
        end, Mapping, OpNames)

    end, #{}, OpDefs).

%% Returns a map of all option_ids with their default options already set.
-spec create_arg_defaults(arg_defs(), arg_name_id_map()) -> parsed_args_map().
create_arg_defaults(OpDefs, OpMapping) ->
    % Get a list of unique option_ids
    OpIds = lists:usort(maps:values(OpMapping)),

    % And fold over them, building up default_arg_container
    lists:foldl(fun(OpId, Mapping) ->
        Mapping#{
            OpId => get_option_default(OpId, OpDefs)
        }
    end, #{}, OpIds).

%% Takes an option_id and an option_definitions and returns the definition of
%% that option.
-spec get_option(arg_id(), arg_defs()) -> arg_def().
get_option(OpId, OpDefs) ->
    lists:keyfind(OpId, 2, OpDefs).

%% Takes an option_id and an option_definitions and returns the possible names of
%% that option
-spec get_option_names(arg_id(), arg_defs()) -> arg_name().
get_option_names(OpId, OpDefs) ->
    {OpNames, _, _, _, _} = get_option(OpId, OpDefs),
    OpNames.

%% Takes an option_id and an option_definitions and returns the type of that option
-spec get_option_type(arg_id(), arg_defs()) -> arg_type().
get_option_type(OpId, OpDefs) ->
    {_, _, OpType, _, _} = get_option(OpId, OpDefs),
    OpType.

%% Takes an option_id and an option_definitions and returns the default value of that
%% option
-spec get_option_default(arg_id(), arg_defs()) -> arg_default().
get_option_default(OpId, OpDefs) ->
    {_, _, _, OpDef, _} = get_option(OpId, OpDefs),
    OpDef.

%% Takes an option_id and an option_definitions and returns description of that option
-spec get_option_desc(arg_id(), arg_defs()) -> arg_description().
get_option_desc(OpId, OpDefs) ->
    {_, _, _, _, OpDesc} = get_option(OpId, OpDefs),
    OpDesc.

-spec create_help_string(non_neg_integer(), pos_integer()) -> [string()].
create_help_string(ColumnPadding, DescLenBound) ->
    create_help_string(get_arg_defs(), ColumnPadding, DescLenBound).

-spec create_help_string(arg_defs(), non_neg_integer(), pos_integer()) -> [string()].
create_help_string(OpDefs, ColumnPadding, DescLenBound) ->
    Options = lists:sort([{string:join(Flags, ", "), parabreak(Desc, DescLenBound)} ||
                          {Flags, _, _, _, Desc} <- OpDefs]),
    FlagColumnWidth = lists:max([length(Fs) || {Fs, _} <- Options] ++ [?MIN_FLAG_COL_SIZE]),

    lists:map(fun({FlagString, DescStrings}) ->
        FlagStringLen = length(FlagString),
        PaddingNeeded = FlagColumnWidth - FlagStringLen + ColumnPadding,

        FirstFormatString = lists:flatten(["~s", $~, integer_to_list(PaddingNeeded), "c~s"]),
        FirstLine = io_lib:format(FirstFormatString, [FlagString, $ , hd(DescStrings)]),

        OtherFormatString = lists:flatten([$~, integer_to_list(FlagColumnWidth + ColumnPadding), "c~s"]),
        OtherLines = [io_lib:format(OtherFormatString, [$ , Str]) || Str <- tl(DescStrings)],

        lists:flatten([FirstLine, OtherLines])
    end, Options).

%% Naively build a paragraph of lines Line_length long, seperated by new lines
-spec parabreak(binary() | string(), pos_integer()) -> [string()].
parabreak(String, MaxLen) when is_binary(String) ->
    parabreak(binary_to_list(String), MaxLen);
parabreak(String, MaxLen) when is_list(String) ->
    [W | Ws] = string:tokens(String, " "),
    [lists:flatten([Line, "\n"]) || Line <- parabreak(Ws, MaxLen, [W], [])].

-spec parabreak(string(), pos_integer(), iolist(), [iolist()]) -> iolist().
parabreak([], _, L, Ls) ->
    Ls ++ [L];
parabreak([W | Ws], MaxLen, L, Ls) when length(L) + length(W) >= MaxLen ->
    parabreak(Ws, MaxLen, [W], Ls ++ [L]);
parabreak([W | Ws], MaxLen, L, Ls) ->
    parabreak(Ws, MaxLen, lists:flatten(L ++ [" ", W]), Ls).




%%% ---------------------------------------------------------------------------------------------%%%
%%% - META FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

-ifdef(TEST).
%% Start eunit testing for this module
-spec eunit() -> ok.
eunit() ->
    eunit:test({inparallel, ?MODULE}),
    init:stop().
-endif.