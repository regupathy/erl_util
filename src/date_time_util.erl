%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2016 5:45 PM
%%%-------------------------------------------------------------------
-module(date_time_util).
-author("regupathy.b").

%% API
-export([get_timestamp/0]).

get_timestamp() -> calendar:datetime_to_gregorian_seconds(erlang:universaltime()).