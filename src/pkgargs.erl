%%% Module Description:
%%% Parses escript package arguments
-module(pkgargs).
-author([{"Vereis", "Chris Bailey"}]).
-vsn(2.0).

-export([
    get/2,
    parse/2,
    create_help_string/3
]).

%% Any arguments not matching a specified option_definition are returned as
%% a list keyed by the value of this macro.
-define(DEFAULT_ARG_ID, default).

-define(DESC_INDENT_AMOUNT, 16).

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
parse(Args, OpDefs) ->
    OpMap = create_name_id_mapping(OpDefs),
    DefaultArgs = create_arg_defaults(OpDefs, OpMap),
    maps:to_list(parse_args(Args, DefaultArgs, OpMap, OpDefs)).

%% Gets the value for an entry of a parsed argument
get(Key, ParsedArgs) ->
    {_Key, Value} = lists:keyfind(Key, 1, ParsedArgs),
    Value.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - ARGUMENT PARSING --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Parses a list of strings, Args, updating ArgsBuffer as we go. Looks up
%% OpMap and OpDefs while parsing to perform the correct action
%% on the input string.
parse_args(Args, ArgsBuffer, OpMap, OpDef) ->
    parse_args(Args, ArgsBuffer#{?DEFAULT_ARG_ID => []}, OpMap, OpDef, default).

%% When there are no arguments remaining to parse, just return ArgsBuffer.
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

%% Returns a map which maps option_names to option_ids for easy lookup.
%% Note:
%%      - We assume that the same option_name won't be used for several
%%        option_ids and thus multiple definitions of the option_name will
%%        most likely break. This is untested and unintended behaviour.
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
create_arg_defaults(OpDefs, Map) when is_map(Map) ->
    % Get a list of unique option_ids
    OpIds = lists:usort(maps:values(Map)),

    % And fold over them, building up default_arg_container
    lists:foldl(fun(OpId, Mapping) ->
        Mapping#{
            OpId => get_option_default(OpId, OpDefs)
        }
    end, #{}, OpIds).

%% Takes an option_id and an option_definitions and returns the definition of
%% that option.
get_option(OpId, OpDefs) ->
    lists:keyfind(OpId, 2, OpDefs).

%% Takes an option_id and an option_definitions and returns the possible names of
%% that option
% get_option_names(OpId, OpDefs) ->
%     {OpNames, _, _, _, _} = get_option(OpId, OpDefs),
%     OpNames.

%% Takes an option_id and an option_definitions and returns the type of that option
get_option_type(OpId, OpDefs) ->
    {_, _, OpType, _, _} = get_option(OpId, OpDefs),
    OpType.

%% Takes an option_id and an option_definitions and returns the default value of that
%% option
get_option_default(OpId, OpDefs) ->
    {_, _, _, OpDef, _} = get_option(OpId, OpDefs),
    OpDef.

%% Takes an option_id and an option_definitions and returns description of that option
% get_option_desc(OpId, OpDefs) ->
%     {_, _, _, _, Option_desc} = get_option(OpId, OpDefs),
%     Option_desc.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - W.I.P FUNCTIONS ---------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Generates a UNIX style help string like you might find on grep or ls
%% Option definitions should look like:
%% [
%%     {[A, ...] = OpNames, OpId, OpType, OpDef, Description}
%%     ...
%% ]
create_help_string(OpDefs, LPad, MaxDescLen) ->
    % Firstly, generate the name_strings which tell the user what commands are available
    NameStrs = lists:sort(
        lists:map(fun({OpNames, _, _, _, Desc}) ->
            case length(OpNames) > 1 of
                true ->
                    [Name | OtherNames] = OpNames,
                    NameStr = [
                        Name,
                        [io_lib:format(", ~s", [OtherName]) || OtherName <- OtherNames]
                    ];
                false ->
                    NameStr = OpNames
            end,
            {lists:flatten(NameStr), Desc}
        end, OpDefs)
    ),

    % The description of each command will begin in a certain column which is at minimum
    % the 16th column and at most 2 columns larger than the longest command
    DescBeginCol = lists:max([16, lists:max([length(S) || {S, _} <- NameStrs]) + 2]),
    LPadStr = [" " || _ <- lists:seq(1, LPad)],

    lists:map(fun({NameStr, Desc}) ->

        % We need this to correctly space the first line of the desc from the name_string
        NameColEnds = length(NameStr),
        NameDescSpace = DescBeginCol - NameColEnds,

        % but we also need this to break the desc into multiple lines if need be.
        DescStrs = parabreak(Desc, MaxDescLen),
        case length(DescStrs) > 1 of
            true ->
                SpaceDescBegins = [" " || _ <- lists:seq(1, DescBeginCol + LPad)],
                [FirstDescStr | OtherDescStrs] = DescStrs,
                ProcessedDescStr = [
                    [" " || _ <- lists:seq(1, NameDescSpace)],
                    FirstDescStr,
                    [
                        [[SpaceDescBegins, NthDescStr] || NthDescStr <- OtherDescStrs]
                    ]
                ];
            false ->
                ProcessedDescStr = [
                    [" " || _ <- lists:seq(1, NameDescSpace)],
                    DescStrs
                ]
        end,

        lists:flatten([
            LPadStr,
            NameStr,
            ProcessedDescStr
        ])
    end, NameStrs).

%% Naively build a paragraph of lines Line_length long, seperated by new lines
parabreak(String, MaxLen) ->
    [W | Ws] = string:tokens(String, " "),
    [lists:flatten([Line, "\n"]) || Line <- parabreak(Ws, MaxLen, [W], [])].

parabreak([], _, CurLine, Lines) ->
    Lines ++ [CurLine];
parabreak([W | Ws], MaxLen, L, Ls) when length(L) + length(W) >= MaxLen ->
    parabreak(Ws, MaxLen, [W], Ls ++ [L]);
parabreak([W | Ws], MaxLen, L, Ls) ->
    parabreak(Ws, MaxLen, L ++ [" ", W], Ls).