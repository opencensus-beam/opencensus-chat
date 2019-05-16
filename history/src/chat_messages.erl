-module(chat_messages).

-export([from_json/1,
         to_json/1]).

from_json(JSON) ->
  {ok, jsx:decode(JSON, [return_maps])}.

to_json(Messages) ->
  jsx:encode(Messages).
