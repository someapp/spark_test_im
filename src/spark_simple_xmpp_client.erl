-module(spark_simple_xmpp_client).
-export([connect/4,
	     init_session/2,
	     disconnect/1]).
	     
-include_lib(exmpp-0.9.9/include/exmpp_jid.hrl).
-include_lib(exmpp-0.9.9/include/exmpp_client.hrl).
	     
connect(UserName, Password, Host, Port)->
  Session = exmpp_session:start(),
  Jid = exmpp_jid:make(UserName, 
  	   
  {ok, {Session, Jid}}.

disconnect(Session) ->
  catch(exmpp_session:stop(Session)),
  ok.

init_session(Session)->
  try 
     exmpp_session(Session) 
  catch
     throw:(auth_error, 'not-authorized') ->
     	{error, not_authorized}     
  end

send_presence(Session) ->
  AvailableStanza = 
  	  exmp_session:set_status(
  	     exmpp_presence:presence(available , chat)),
  
  exmpp_session:send_packet(Session, Available).
