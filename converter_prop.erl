-module (converter_prop).

-include_lib("proper/include/proper.hrl").

-export([convert/2]).

-define(WSDL,"http://localhost:8080/ConverterProj/services/Converter?WSDL").
-define(CEL_TO_FAR, "celsiusToFarenheit").
-define(CEL_TO_FAR_RET, 'p:celsiusToFarenheitResponse').
-define(FAR_TO_CEL, "farenheitToCelsius").
-define(FAR_TO_CEL_RET, 'p:farenheitToCelsiusResponse').

convert(Mode,Data)->
  case Mode of 
    celsius -> 
      make_call(?CEL_TO_FAR, [integer_to_list(Data)]);
    fahrenheit -> 
      make_call(?FAR_TO_CEL, [integer_to_list(Data)])
  end.

make_call(Op_name, Data)->
  inets:start(),
  Wsdl = yaws_soap_lib:initModel(?WSDL),
  {ok, _, [{_, _, Result}]} = yaws_soap_lib:call(Wsdl, Op_name, Data),
  {Float, _} = string:to_float(Result),
  Float.

prop_convert_cel_to_far () ->
  ?FORALL(Celsius, integer(), 
    convert(celsius, Celsius) =:= Celsius * 1.8 + 32).
