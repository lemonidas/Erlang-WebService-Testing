-module(contains_prop).

-include_lib("proper/include/proper.hrl").

-export([make_call/2]).

-define(WSDL, "http://localhost:8080/ContainsProj/services/Contains?WSDL").
-define(OP_NAME, "myContains").

make_call(List, Element) ->
  inets:start(),
  Wsdl = yaws_soap_lib:initModel(?WSDL),
  Result = yaws_soap_lib:call(Wsdl,?OP_NAME, [List,Element]),
  case Result of
    {ok, _, [{_,_,Bool}]} -> Bool;
    {error, Reason} -> io:format("~p~n",[Reason])
  end.

not_empty(T) -> ?SUCHTHAT(L,T,L=/=[]).

prop_list_contains_x() ->
  ?FORALL({List,Element}, {not_empty(list(integer())), integer()}, 
    lists:member(Element,List) =:= make_call(List,Element)).
