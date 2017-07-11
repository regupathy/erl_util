%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2016 4:56 PM
%%%-------------------------------------------------------------------
-module(list_util).
-author("regupathy.b").

%% API
-export([swipe_merge/2,confrom_binary_items/1]).

%% ===============================================================
%% API functions
%% ===============================================================

swipe_merge(List1,List2) -> swipe_merge(List1,List2,[]).

confrom_binary_items(List) -> confrom_binary_items(List,[]).

%% ===============================================================
%% Internal functions
%% ===============================================================

swipe_merge([],[],Result) -> lists:reverse(Result);
swipe_merge([],Rest,R) -> lists:reverse(R) ++ Rest;
swipe_merge(Rest,[],R) -> lists:reverse(R) ++ Rest;
swipe_merge([H|T],[A|B],R) -> swipe_merge(T,B,[A,H|R]).

confrom_binary_items([],Result) -> lists:reverse(Result);
confrom_binary_items([X|Rest],Result) -> confrom_binary_items(Rest,[to_binary(X)|Result]).

%% ===============================================================
%% Helper functions
%% ===============================================================
to_binary(X) when is_binary(X)  -> X;
to_binary(X) when is_list(X)    -> list_to_binary(X);
to_binary(X) when is_atom(X)    -> list_to_binary(atom_to_list(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X)                    -> term_to_binary(X).
