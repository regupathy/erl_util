%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Jan 2016 12:01 PM
%%%-------------------------------------------------------------------
-module(binary_util).
-author("regupathy.b").

%% API
-export([strip/1,replace/3,remove/2,split/2,merge/1,merge/2,re/1,strip_list/1]).
-export([append/2]).

%% ===============================================================
%% API functions
%% ===============================================================

merge(Bin1,Bin2) -> <<Bin1/binary,Bin2/binary>>.

merge(ListOfBin) -> binary:list_to_bin(ListOfBin).

strip(Bin) -> strip(Bin,<<>>).

strip_list(List) -> [strip(T) || T <- List].

replace(Bin,Key,Newset) -> replace(Bin,Key,Newset,<<>>).

remove(Bin,Key) -> remove(Bin,Key,<<>>).

split(Bin,Key) -> split(Bin,Key,[<<>>]).

re(Bin) -> combine_re_values(re_loop(Bin,[]),[<<>>]).

append(AppendItem,List) -> append(List,AppendItem,<<>>).

%% ===============================================================
%% Internal functions
%% ===============================================================

append([],_,Acc) -> Acc;
append([Bin|[]],_ItemBin,Acc) -> <<Acc/binary,Bin/binary>>;
append([HBin|TBin],ItemBin,Acc) -> append(TBin,ItemBin,<<Acc/binary,HBin/binary,ItemBin/binary>>).

strip(<<>>,Result) -> Result;
strip(<<$ ,Rest/binary>>,Result) -> strip(Rest,Result);
strip(<<H,Rest/binary>>,Result) -> strip(Rest,<<Result/binary,H>>).

remove(<<>>,_,Result) -> Result;
remove(<<H,_/binary>> = Data,<<H,_/binary>> = Key,Cache) -> remove_operation(loop(Data,Key,<<>>),Key,Cache);
remove(<<H,Rest/binary>>,Key,Cache) -> remove(Rest,Key,<<Cache/binary,H>>).

remove_operation({<<"F">>,RestBin},Key,Cache) -> remove(RestBin,Key,Cache);
remove_operation({<<"NF">>,RestBin,Matched},Key,Cache) -> remove(RestBin,Key,<<Cache/binary,Matched/binary>>).

replace(<<>>,_,_,Result) -> Result;
replace(<<H,_/binary>> =Data,<<H,_/binary>> = Key,NewSet,Cache) -> replace_operation(loop(Data,Key,<<>>),Key,NewSet,Cache);
replace(<<H,Rest/binary>>,Key,NewSet,Cache) -> replace(Rest,Key,NewSet,<<Cache/binary,H>>).

replace_operation({<<"F">>,RestBin},Key,NewSet,Cache) -> replace(RestBin,Key,NewSet,<<Cache/binary,NewSet/binary>>);
replace_operation({<<"NF">>,RestBin,Matched},Key,NewSet,Cache) -> replace(RestBin,Key,NewSet,<<Cache/binary,Matched/binary>>).

split(<<>>,_,[<<>>|Result]) -> lists:reverse(Result);
split(<<>>,_,Result) -> lists:reverse(Result);
split(<<H,_/binary>> =Data,<<H,_/binary>> = Key,Cache) -> split_operation(loop(Data,Key,<<>>),Key,Cache);
split(<<H,Rest/binary>>,Key,[Last|Cache]) -> split(Rest,Key,[<<Last/binary,H>>|Cache]).

split_operation({<<"F">>,RestBin},Key,Cache) -> split(RestBin,Key,[<<>>|Cache]);
split_operation({<<"NF">>,RestBin,Matched},Key,[Last|Cache]) -> split(RestBin,Key,[<<Last/binary,Matched/binary>>|Cache]).

loop(Rest,<<>>,_Matched) -> {<<"F">>,Rest};
loop(<<>>,_,Matched) -> {<<"NF">>,<<>>,Matched};
loop(<<H,Rest/binary>>,<<H,KeyRest/binary>>,Matched) -> loop(Rest,KeyRest,<<Matched/binary,H>>);
loop(Rest,_,Matched) -> {<<"NF">>,Rest,Matched}.

combine_re_values([],Acc) -> lists:reverse(Acc);
combine_re_values([{lowercase,Chars}|Rest],[Bin|Acc]) -> combine_re_values(Rest,[<<Bin/binary,Chars/binary>>|Acc]);
combine_re_values([{upercase,Chars}|Rest],[Bin|Acc]) -> combine_re_values(Rest,[<<Bin/binary,Chars/binary>>|Acc]);
combine_re_values([{num,Num}|Rest],[<<>>|Acc]) -> combine_re_values(Rest,[<<>>|[Num|Acc]]);
combine_re_values([{num,Num}|Rest],Acc) -> combine_re_values(Rest,[<<>>|[Num|Acc]]);
combine_re_values([{sp,SP}|Rest],[<<>>|Acc]) -> combine_re_values(Rest,[<<>>|[SP|Acc]]);
combine_re_values([{sp,SP}|Rest],Acc) -> combine_re_values(Rest,[<<>>|[SP|Acc]]).


re_loop(<<>>,Stack) -> lists:reverse(Stack);

re_loop(<<$0,Rest/binary>>,Stack) -> number(Rest,[<<"0">>|Stack]);
re_loop(<<$1,Rest/binary>>,Stack) -> number(Rest,[<<"1">>|Stack]);
re_loop(<<$2,Rest/binary>>,Stack) -> number(Rest,[<<"2">>|Stack]);
re_loop(<<$3,Rest/binary>>,Stack) -> number(Rest,[<<"3">>|Stack]);
re_loop(<<$4,Rest/binary>>,Stack) -> number(Rest,[<<"4">>|Stack]);
re_loop(<<$5,Rest/binary>>,Stack) -> number(Rest,[<<"5">>|Stack]);
re_loop(<<$6,Rest/binary>>,Stack) -> number(Rest,[<<"6">>|Stack]);
re_loop(<<$7,Rest/binary>>,Stack) -> number(Rest,[<<"7">>|Stack]);
re_loop(<<$8,Rest/binary>>,Stack) -> number(Rest,[<<"8">>|Stack]);
re_loop(<<$9,Rest/binary>>,Stack) -> number(Rest,[<<"9">>|Stack]);

re_loop(<<$a,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"a">>|Stack]);
re_loop(<<$b,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"b">>|Stack]);
re_loop(<<$c,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"c">>|Stack]);
re_loop(<<$d,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"d">>|Stack]);
re_loop(<<$e,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"e">>|Stack]);
re_loop(<<$f,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"f">>|Stack]);
re_loop(<<$g,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"g">>|Stack]);
re_loop(<<$h,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"h">>|Stack]);
re_loop(<<$i,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"i">>|Stack]);
re_loop(<<$j,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"j">>|Stack]);
re_loop(<<$k,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"k">>|Stack]);
re_loop(<<$l,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"l">>|Stack]);
re_loop(<<$m,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"m">>|Stack]);
re_loop(<<$n,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"n">>|Stack]);
re_loop(<<$o,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"o">>|Stack]);
re_loop(<<$p,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"p">>|Stack]);
re_loop(<<$q,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"q">>|Stack]);
re_loop(<<$r,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"r">>|Stack]);
re_loop(<<$s,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"s">>|Stack]);
re_loop(<<$t,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"t">>|Stack]);
re_loop(<<$u,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"u">>|Stack]);
re_loop(<<$v,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"v">>|Stack]);
re_loop(<<$w,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"w">>|Stack]);
re_loop(<<$x,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"x">>|Stack]);
re_loop(<<$y,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"y">>|Stack]);
re_loop(<<$z,Rest/binary>>,Stack) -> lower_case_chars(Rest,[<<"z">>|Stack]);

re_loop(<<$A,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"A">>|Stack]);
re_loop(<<$B,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"B">>|Stack]);
re_loop(<<$C,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"C">>|Stack]);
re_loop(<<$D,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"D">>|Stack]);
re_loop(<<$E,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"E">>|Stack]);
re_loop(<<$F,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"F">>|Stack]);
re_loop(<<$G,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"G">>|Stack]);
re_loop(<<$H,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"H">>|Stack]);
re_loop(<<$I,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"I">>|Stack]);
re_loop(<<$J,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"J">>|Stack]);
re_loop(<<$K,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"K">>|Stack]);
re_loop(<<$L,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"L">>|Stack]);
re_loop(<<$M,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"M">>|Stack]);
re_loop(<<$N,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"N">>|Stack]);
re_loop(<<$O,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"O">>|Stack]);
re_loop(<<$P,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"P">>|Stack]);
re_loop(<<$Q,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"Q">>|Stack]);
re_loop(<<$R,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"R">>|Stack]);
re_loop(<<$S,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"S">>|Stack]);
re_loop(<<$T,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"T">>|Stack]);
re_loop(<<$U,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"U">>|Stack]);
re_loop(<<$V,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"V">>|Stack]);
re_loop(<<$W,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"W">>|Stack]);
re_loop(<<$X,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"X">>|Stack]);
re_loop(<<$Y,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"Y">>|Stack]);
re_loop(<<$Z,Rest/binary>>,Stack) -> upper_case_chars(Rest,[<<"Z">>|Stack]);

re_loop(<<H,Rest/binary>>,Stack) -> re_loop(Rest,[{sp,<<H>>}|Stack]).


number(<<>>,[Number|Stack]) -> re_loop(<<>>,[{num,Number}|Stack]);

number(<<$0,Rest/binary>>,[Bin|Stack]) -> number(Rest,[<<Bin/binary,<<"0">>/binary>>|Stack]);
number(<<$1,Rest/binary>>,[Bin|Stack]) -> number(Rest,[<<Bin/binary,<<"1">>/binary>>|Stack]);
number(<<$2,Rest/binary>>,[Bin|Stack]) -> number(Rest,[<<Bin/binary,<<"2">>/binary>>|Stack]);
number(<<$3,Rest/binary>>,[Bin|Stack]) -> number(Rest,[<<Bin/binary,<<"3">>/binary>>|Stack]);
number(<<$4,Rest/binary>>,[Bin|Stack]) -> number(Rest,[<<Bin/binary,<<"4">>/binary>>|Stack]);
number(<<$5,Rest/binary>>,[Bin|Stack]) -> number(Rest,[<<Bin/binary,<<"5">>/binary>>|Stack]);
number(<<$6,Rest/binary>>,[Bin|Stack]) -> number(Rest,[<<Bin/binary,<<"6">>/binary>>|Stack]);
number(<<$7,Rest/binary>>,[Bin|Stack]) -> number(Rest,[<<Bin/binary,<<"7">>/binary>>|Stack]);
number(<<$8,Rest/binary>>,[Bin|Stack]) -> number(Rest,[<<Bin/binary,<<"8">>/binary>>|Stack]);
number(<<$9,Rest/binary>>,[Bin|Stack]) -> number(Rest,[<<Bin/binary,<<"9">>/binary>>|Stack]);
number(Rest,[Num|Stack]) -> re_loop(Rest,[{num,Num}|Stack]).

lower_case_chars(<<>>,[Lowercase|Stack]) -> re_loop(<<>>,[{lowercase,Lowercase}|Stack]);

lower_case_chars(<<$a,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"a">>/binary>>|Stack]);
lower_case_chars(<<$b,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"b">>/binary>>|Stack]);
lower_case_chars(<<$c,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"c">>/binary>>|Stack]);
lower_case_chars(<<$d,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"d">>/binary>>|Stack]);
lower_case_chars(<<$e,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"e">>/binary>>|Stack]);
lower_case_chars(<<$f,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"f">>/binary>>|Stack]);
lower_case_chars(<<$g,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"g">>/binary>>|Stack]);
lower_case_chars(<<$h,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"h">>/binary>>|Stack]);
lower_case_chars(<<$i,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"i">>/binary>>|Stack]);
lower_case_chars(<<$j,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"j">>/binary>>|Stack]);
lower_case_chars(<<$k,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"k">>/binary>>|Stack]);
lower_case_chars(<<$l,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"l">>/binary>>|Stack]);
lower_case_chars(<<$m,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"m">>/binary>>|Stack]);
lower_case_chars(<<$n,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"n">>/binary>>|Stack]);
lower_case_chars(<<$o,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"o">>/binary>>|Stack]);
lower_case_chars(<<$p,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"p">>/binary>>|Stack]);
lower_case_chars(<<$q,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"q">>/binary>>|Stack]);
lower_case_chars(<<$r,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"r">>/binary>>|Stack]);
lower_case_chars(<<$s,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"s">>/binary>>|Stack]);
lower_case_chars(<<$t,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"t">>/binary>>|Stack]);
lower_case_chars(<<$u,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"u">>/binary>>|Stack]);
lower_case_chars(<<$v,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"v">>/binary>>|Stack]);
lower_case_chars(<<$w,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"w">>/binary>>|Stack]);
lower_case_chars(<<$x,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"x">>/binary>>|Stack]);
lower_case_chars(<<$y,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"y">>/binary>>|Stack]);
lower_case_chars(<<$z,Rest/binary>>,[Bin|Stack]) -> lower_case_chars(Rest,[<<Bin/binary,<<"z">>/binary>>|Stack]);
lower_case_chars(Rest,[Lowercase|Stack]) -> re_loop(Rest,[{lowercase,Lowercase}|Stack]).


upper_case_chars(<<>>,[UpperCase|Stack]) -> re_loop(<<>>,[{upercase,UpperCase}|Stack]);

upper_case_chars(<<$A,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"A">>/binary>>|Stack]);
upper_case_chars(<<$B,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"B">>/binary>>|Stack]);
upper_case_chars(<<$C,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"C">>/binary>>|Stack]);
upper_case_chars(<<$D,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"D">>/binary>>|Stack]);
upper_case_chars(<<$E,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"E">>/binary>>|Stack]);
upper_case_chars(<<$F,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"F">>/binary>>|Stack]);
upper_case_chars(<<$G,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"G">>/binary>>|Stack]);
upper_case_chars(<<$H,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"H">>/binary>>|Stack]);
upper_case_chars(<<$I,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"I">>/binary>>|Stack]);
upper_case_chars(<<$J,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"J">>/binary>>|Stack]);
upper_case_chars(<<$K,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"K">>/binary>>|Stack]);
upper_case_chars(<<$L,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"L">>/binary>>|Stack]);
upper_case_chars(<<$M,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"M">>/binary>>|Stack]);
upper_case_chars(<<$N,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"N">>/binary>>|Stack]);
upper_case_chars(<<$O,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"O">>/binary>>|Stack]);
upper_case_chars(<<$P,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"P">>/binary>>|Stack]);
upper_case_chars(<<$Q,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"Q">>/binary>>|Stack]);
upper_case_chars(<<$R,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"R">>/binary>>|Stack]);
upper_case_chars(<<$S,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"S">>/binary>>|Stack]);
upper_case_chars(<<$T,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"T">>/binary>>|Stack]);
upper_case_chars(<<$U,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"U">>/binary>>|Stack]);
upper_case_chars(<<$V,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"V">>/binary>>|Stack]);
upper_case_chars(<<$W,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"W">>/binary>>|Stack]);
upper_case_chars(<<$X,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"X">>/binary>>|Stack]);
upper_case_chars(<<$Y,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"Y">>/binary>>|Stack]);
upper_case_chars(<<$Z,Rest/binary>>,[Bin|Stack]) -> upper_case_chars(Rest,[<<Bin/binary,<<"Z">>/binary>>|Stack]);
upper_case_chars(Rest,[UpperCase|Stack]) -> re_loop(Rest,[{upercase,UpperCase}|Stack]).



