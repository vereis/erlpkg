%%% Module Description:
%%% Parses escript package arguments
-module(pkgargs).
-author([{"Vereis", "Chris Bailey"}]).
-vsn(1.0).

-export([
    parse/1,
    parse/2
]).

%% Returns a map containing a single key, 'Args', containing a list of tokens in a given argument 
%% string
parse(Args) ->
    parse(Args, #{}).

%% Returns a map containing keys for any arguments we find in the given Args list.
%%
%% Depending on whether or not the map 'Format' supplied to this function was empty or not,
%% parsed tokens in the supplied Args list will be classified differently.
%%
%% If a key exists in the Format map, and it is found in the Args list, the token
%% directly following that will be stored as a value of that key. 
%% If the value of the key of the map contains a list, then the token to be saved will be appended 
%% to the list instead.
%%
parse(Args, Args_List) ->
    % Ensure we have a 'normal_args' key before continuing
    case maps:is_key("normal_args", Args_List) of
        true ->
            Processed_Args_List = Args_List;
        _ ->
            Processed_Args_List = maps:put("normal_args", [], Args_List)
    end,

    process_args(Args, Processed_Args_List, none).

%% Returns an updated Args_List after processing every token in a given Arguments list
%%   - When processing every token in Args, we check whether or not that token is a key
%%     found in Args_List. If it is then we update the current 'Key_To_Update' to that
%%     key for the next token which will update the value of said key in the following
%%     processing step.
%%
%%   - Based on the default value for the key to update, we update the value accordingly.
%%     Lists are appended to.
%%     Atoms, numbers, binary and others will be replaced.
%%   
%%   TODO: Add syntax {N} where N is an integer to specify max number of tokens for a given
%%         key
process_args([], Args_List, _) ->
    Args_List;
process_args([Current | Remaining_Args], Args_List, none) ->
    case maps:is_key(Current, Args_List) of
        true ->
            process_args(Remaining_Args, Args_List, Current);
        _ ->
            process_args(Remaining_Args, update_key("normal_args", Current, Args_List), none)
    end;
process_args([Current | Remaining_Args], Args_List, Key_To_Update) ->
    process_args(Remaining_Args, update_key(Key_To_Update, Current, Args_List), none).

%% Checks the type of the value of 'Key_To_Update' in 'Args_List' and performs the
%% map update differently based on the type of the found value.
%%     Lists are appended to.
%%     Atoms, numbers, binary and others will be replaced.
update_key(Key_To_Update, Value, Args_List) ->
    Current_Value = maps:get(Key_To_Update, Args_List),
    case is_list(Current_Value) of
        true ->
            Updated_Value = Current_Value ++ [list_to_binary(Value)];
        _ ->
            Updated_Value = list_to_binary(Value)
    end,
    maps:update(Key_To_Update, Updated_Value, Args_List).