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
%%     {[A, ...] = Option_names, Option_id, Option_type, Option_default, Description}
%%     ...   
%% ]
%% Where:
%%       - Option_names is a list of strings used to specify options.
%%
%%       - Option_id is an atom identifying the option. Returned parsed args will
%%         use Option_id as a key.
%%
%%       - Option_type is an atom/int which specifies how we handle the options.
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
parse(Args, Option_definitions) ->
    Option_mapping = create_name_id_mapping(Option_definitions),
    Default_args   = create_arg_defaults(Option_definitions, Option_mapping),
    maps:to_list(parse_args(Args, Default_args, Option_mapping, Option_definitions)).

%% Gets the value for an entry of a parsed argument
get(Key, Parsed_args) ->
    {_Key, Value} = lists:keyfind(Key, 1, Parsed_args),
    Value.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - ARGUMENT PARSING --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Parses a list of strings, Args, updating Args_container as we go. Looks up 
%% Option_mapping and Option_definitions while parsing to perform the correct action 
%% on the input string.
parse_args(Args, Args_container, Option_mapping, Option_definition) ->
    parse_args(Args, Args_container#{?DEFAULT_ARG_ID => []}, Option_mapping, Option_definition, default).

%% When there are no arguments remaining to parse, just return Args_container.
parse_args([], Args_container, _, _, _) ->
    Args_container;

%% In the 'default' parse_args option_type, we check to see whether or not the Current_token 
%% is an option:
%% If it is, we don't do anything other than set the Option_type to the type of said option
%% Otherwise, we keep the option_type as 'default' and merely append the current token to the 
%% default key list in Args_container.
parse_args(Args, Args_container, Option_mapping, Option_definition, default) ->
    [Current_token | Remaining_args] = Args,
    case maps:is_key(Current_token, Option_mapping) of
        true ->
            Option_id = maps:get(Current_token, Option_mapping),
            Option_type = {
                Option_id,
                get_option_type(Option_id, Option_definition)
            },
            Updated_args_container = Args_container;
        false ->
            Option_type = default,
            Updated_args_container = Args_container#{
                ?DEFAULT_ARG_ID := maps:get(?DEFAULT_ARG_ID, Args_container) ++ [Current_token]
            }
    end,
    parse_args(Remaining_args, Updated_args_container, Option_mapping, Option_definition, Option_type);

%% When in the 'is_set' state, we simply update Args_container to set the Prev_token_id 
%% key to be true, then continue parsing in the normal state.
parse_args(Args, Args_container, Option_mapping, Option_definition, {Prev_token_id, is_set}) ->
    Updated_args_container = Args_container#{
        Prev_token_id := true
    },

    parse_args(Args, Updated_args_container, Option_mapping, Option_definition, default);

%% When in the 'singleton' state, we simply update Args_container to set the Prev_token_id 
%% key to be the same as the current token. Then we parse the remaining args in the default 
%% state.
parse_args(Args, Args_container, Option_mapping, Option_definition, {Prev_token_id, singleton}) ->
    [Current_token | Remaining_args] = Args,    
    Updated_args_container = Args_container#{
        Prev_token_id := Current_token
    },

    parse_args(Remaining_args, Updated_args_container, Option_mapping, Option_definition, default);

%% When in the 'many' state, we simply update Args_container to append the current token to 
%% the Prev_token_id's value which ought to be a list. Then we parse the remaining args in 
%% the default state.
parse_args(Args, Args_container, Option_mapping, Option_definition, {Prev_token_id, many}) ->
    [Current_token | Remaining_args] = Args,    
    Updated_args_container = Args_container#{
        Prev_token_id := maps:get(Prev_token_id, Args_container) ++ [Current_token]
    },

    parse_args(Remaining_args, Updated_args_container, Option_mapping, Option_definition, default);

%% When in an integer state, we simply skip forward and consume the next N tokens from Args and
%% set Prev_token_id's value to be the list of consumed tokens. We parse the remaining args in 
%% the default state.
parse_args(Args, Args_container, Option_mapping, Option_definition, {Prev_token_id, N}) when is_integer(N)->
    {Consumed_tokens, Remaining_args} = lists:split(N, Args),    
    Updated_args_container = Args_container#{
        Prev_token_id := Consumed_tokens
    },

    parse_args(Remaining_args, Updated_args_container, Option_mapping, Option_definition, default).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - UTILITY FUNCTIONS -------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Returns a map which maps option_names to option_ids for easy lookup.
%% Note:
%%      - We assume that the same option_name won't be used for several
%%        option_ids and thus multiple definitions of the option_name will
%%        most likely break. This is untested and unintended behaviour.
create_name_id_mapping(Option_definitions) ->
    lists:foldl(fun({Option_names, Option_id, _, _, _}, Mapping) ->

        % We need to do an inner fold here because there is a possibility
        % of there being more than one option name needing to be added to
        % Mapping
        lists:foldl(fun(Option_name, Mapping_2) ->
            Mapping_2#{Option_name => Option_id}
        end, Mapping, Option_names)

    end, #{}, Option_definitions).

%% Returns a map of all option_ids with their default options already set.
create_arg_defaults(Option_definitions, Name_id_map) when is_map(Name_id_map) ->
    % Get a list of unique option_ids
    Option_ids = lists:usort(maps:values(Name_id_map)),

    % And fold over them, building up default_arg_container
    lists:foldl(fun(Option_id, Default_arg_container) ->
        Default_arg_container#{
            Option_id => get_option_default(Option_id, Option_definitions)
        }
    end, #{}, Option_ids).

%% Takes an option_id and an option_definitions and returns the definition of 
%% that option.
get_option(Option_id, Option_definitions) ->
    lists:keyfind(Option_id, 2, Option_definitions).

%% Takes an option_id and an option_definitions and returns the possible names of 
%% that option
% get_option_names(Option_id, Option_definitions) ->
%     {Option_names, _, _, _, _} = get_option(Option_id, Option_definitions),
%     Option_names.

%% Takes an option_id and an option_definitions and returns the type of that option
get_option_type(Option_id, Option_definitions) ->
    {_, _, Option_type, _, _} = get_option(Option_id, Option_definitions),
    Option_type.

%% Takes an option_id and an option_definitions and returns the default value of that 
%% option
get_option_default(Option_id, Option_definitions) ->
    {_, _, _, Option_default, _} = get_option(Option_id, Option_definitions),
    Option_default.

%% Takes an option_id and an option_definitions and returns description of that option
% get_option_desc(Option_id, Option_definitions) ->
%     {_, _, _, _, Option_desc} = get_option(Option_id, Option_definitions),
%     Option_desc.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - W.I.P FUNCTIONS ---------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Generates a UNIX style help string like you might find on grep or ls
%% Option definitions should look like:
%% [
%%     {[A, ...] = Option_names, Option_id, Option_type, Option_default, Description}
%%     ...   
%% ]
create_help_string(Option_definitions, L_padding, Max_desc_len) ->
    % Firstly, generate the name_strings which tell the user what commands are available
    Name_strings = lists:sort(
        lists:map(fun({Option_names, _, _, _, Desc}) ->
            case length(Option_names) > 1 of
                true -> 
                    [Name | Other_names] = Option_names,
                    Name_string = [
                        Name, 
                        [io_lib:format(", ~s", [Other_name]) || Other_name <- Other_names]
                    ];
                false -> 
                    Name_string = Option_names
            end,
            {lists:flatten(Name_string), Desc}
        end, Option_definitions)
    ),

    % The description of each command will begin in a certain column which is at minimum
    % the 16th column and at most 2 columns larger than the longest command
    Desc_begins_at = lists:max([16, lists:max([length(S) || {S, _} <- Name_strings]) + 2]),
    L_padding_string = [" " || _ <- lists:seq(1, L_padding)],

    lists:map(fun({Name_string, Desc}) ->

        % We need this to correctly space the first line of the desc from the name_string
        Name_column_ends = length(Name_string),
        Space_between_name_and_desc = Desc_begins_at - Name_column_ends,

        % but we also need this to break the desc into multiple lines if need be.
        Desc_strings = parabreak(Desc, Max_desc_len),
        case length(Desc_strings) > 1 of
            true -> 
                Space_before_desc_begins = [" " || _ <- lists:seq(1, Desc_begins_at + L_padding)],
                [First_desc_string | Other_desc_strings] = Desc_strings,
                Processed_desc_string = [
                    [" " || _ <- lists:seq(1, Space_between_name_and_desc)],
                    First_desc_string,
                    [
                        [[Space_before_desc_begins, Nth_desc_string] || Nth_desc_string <- Other_desc_strings]
                    ]
                ];
            false -> 
                Processed_desc_string = [
                    [" " || _ <- lists:seq(1, Space_between_name_and_desc)],
                    Desc_strings
                ]
        end,

        lists:flatten([
            L_padding_string,
            Name_string, 
            Processed_desc_string
        ])
    end, Name_strings).

%% Naively build a paragraph of lines Line_length long, seperated by new lines
parabreak(String, Line_length) ->
    [Cur_word | Next_words] = string:tokens(String, " "),
    [lists:flatten([Line, "\n"]) || Line <- parabreak(Next_words, Line_length, [Cur_word], [])].

parabreak([], _, Cur_Line, Lines) ->
    Lines ++ [Cur_Line];
parabreak([Cur_word | Next_words], Line_len, Cur_line, Line_buffer) when length(Cur_line) + length(Cur_word) >= Line_len ->
    parabreak(Next_words, Line_len, [Cur_word], Line_buffer ++ [Cur_line]);
parabreak([Cur_word | Next_words], Line_len, Cur_line, Line_buffer) ->
    parabreak(Next_words, Line_len, Cur_line ++ [" ", Cur_word], Line_buffer).