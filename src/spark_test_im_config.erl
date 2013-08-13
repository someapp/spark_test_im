-module(spark_test_im_config).

-export([get_config/2,
         get_config/3,
         get_config/4,
         get_config/5,
         get_ct/1]).


get_config(Option, Config) ->
    get_config(Option, Config, undefined).

get_config(Option, Config, Default) ->
    case lists:keyfind(Option, 1, Config) of
        {Option, Value} ->
            Value;
        false ->
            case ct:get_config(Option) of
                undefined ->
                    Default;
                Value ->
                    Value
            end
    end.

get_config(USName, UserSpec, CName, Config) ->
    get_config(USName, UserSpec, CName, Config, undefined).

get_config(USName, UserSpec, CName, Config, Default) ->
    case lists:keyfind(USName, 1, UserSpec) of
        {USName, Value} ->
            Value;
        false ->
            get_config(CName, Config, Default)
    end.


get_ct(Required) when is_atom(Required) ->
    ct:get_config(Required);
get_ct(Required) when is_tuple(Required) ->
    TopList = ct:get_config(erlang:element(1, Required)),
    get_ct_recurse(Required, 2, TopList).


get_ct_recurse(Required, Pos, LevelVal) when Pos < size(Required) ->
    Key = erlang:element(Pos, Required),
    {_, NewLevelVal} = lists:keyfind(Key, 1, LevelVal),
    get_ct_recurse(Required, Pos+1, NewLevelVal);

get_ct_recurse(Required, Pos, LevelVal) when Pos == size(Required) ->
    Key = erlang:element(Pos, Required),
    {_, NewLevelVal} = lists:keyfind(Key, 1, LevelVal),
    NewLevelVal.
