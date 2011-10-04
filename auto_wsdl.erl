-module(auto_wsdl).

-include_lib("proper/include/proper.hrl").

-export([test/0]).
-export([parse_wsdl/3]).

-define(CONTAINS_WSDL, "http://localhost:8080/ContainsProj/services/Contains?WSDL").

test() -> parse_wsdl(?CONTAINS_WSDL, "bar", "auto_prop").

parse_wsdl(WSDL_url, Prefix, Module_name) ->
  inets:start(),
  Wsdl = yaws_soap_lib:initModel(WSDL_url, Prefix),
  Operations = yaws_soap_lib:wsdl_operations(Wsdl),
  Model = yaws_soap_lib:wsdl_model(Wsdl),
  % FIXME: Now only first operation is handled
  [H | _Tail] = Operations,
  Op = yaws_soap_lib:wsdl_op_operation(H),
  Call_text = make_call_text(WSDL_url, Prefix, Op),
  Property = create_property(Model, Op, Prefix),
  Header = create_header (Module_name),
  FileName = Module_name ++ ".erl",
  Bytes = Header ++ Call_text ++ Property,
  file:write_file(FileName, Bytes).

create_property(Model, Operation, Prefix) ->
  {_model, Types, _Schemas, _Envelope, _} = Model,
  Atom_string = Prefix ++ ":" ++ Operation,
  Operation_atom = list_to_atom(Atom_string),
  Request_types = find_operation(Types, Operation_atom),
  Generators = create_generators(Request_types),
  {A1,A2} = create_args (Generators),
% Created Property is name prop_<op_name>()
% Variables are named X1, X2 (in reverse order)
% To make call we need to replace "tuple" structure of A1 to list
" 
prop_"++Operation ++"() ->
  ?FORALL(" ++ A1 ++ ", "++ A2 ++ ", 
    auto_make_call([" ++ (string:substr(A1,2, string:len(A1)-2)) ++ "]) == ok).
".

create_args(Gen) -> create_args (Gen, "}", "}", 1).
create_args([], A1 , A2, _) -> 
  {"{"++(string:substr(A1,3)), "{"++(string:substr(A2,3))};
create_args([{H}|T], A1, A2, N) ->
  create_args(T, (", X"++(integer_to_list(N))++A1), (", "++ H ++ A2), N+1).

% Creates the header of the produced file
create_header(Module_name) ->
"-module(" ++ Module_name ++ ").

-include_lib(\"proper/include/proper.hrl\").
".

% Creates the auto_make_call function that handles soapy things...
make_call_text(WSDL_url, Prefix, Op_name)->
" 
not_empty(T) -> ?SUCHTHAT(L,T,L=/=[]).

auto_make_call(Args) ->
  inets:start(),
  Wsdl = yaws_soap_lib:initModel(\""++WSDL_url++"\",\""++Prefix++"\"),
  Result = yaws_soap_lib:call(Wsdl, \""++ Op_name ++"\", Args, prefix, \""++Prefix++"\"),
  case Result of 
    {ok, _, _ } -> ok;
    {error, Reason} -> io:format(\"~p~n\", [Reason])
  end.
".


% Go throught the Types list to find the operation Op_atom and return the Types it expects
find_operation([H | T], Op_atom) ->
  case H of
    {type, Op_atom, _type, Data, _, _, _, _, _, _, _ ,_} ->
      Data;
    {type, _another_atom, _, _, _, _, _, _, _, _, _, _} ->
      find_operation(T, Op_atom)
  end.

create_generators(Request_types) -> create_generators(Request_types, []).

% Creates the PropEr generators for the specified Type
% Currently handles single nested complex types/sequences of ints/floats/bools
create_generators([], Acc) -> Acc;
create_generators([{el, [Type], Min_occurs, Max_occurs, _no}|T], Acc) ->
  Gen_type = extract_gen_type (Type),
  case {Min_occurs, Max_occurs} of
    {0, unbound} -> create_generators (T, [{"list("++Gen_type++")"} | Acc]);
    {1, unbound} -> create_generators (T, [{"not_empty(list("++Gen_type++"))"} | Acc]);
    {1, 1} -> create_generators (T, [ {Gen_type} | Acc]);
    _ -> not_handled_yet
  end.

% Extract the PropEr generator
extract_gen_type ({_alt, _name, {'#PCDATA', GenType}, _, _, _, _,_})-> 
  case GenType of
    integer -> "integer()";
    float -> "float()";
    bool -> "union([true,false])";
    _ -> not_handled_yet
  end.


  
  
