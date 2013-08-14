-module(spark_simple_xmpp_client).
-export([connect/4,
	     init_session/2,
	     disconnect/1]).
	     
-include_lib(exmpp-0.9.9/include/exmpp_jid.hrl).
-include_lib(exmpp-0.9.9/include/exmpp_client.hrl).
	     
connect(UserName, Password, Host, Port)->
  Session = exmpp_session:start(),
  Jid = spark_xmpp_user:make_jid(UserName),
  case init_session(Session) of
  	 loggedin -> {ok, {Session, Jid}};
  	 not_authorized -> {ok, {Jid, not_authorized}}
  end.

disconnect(Session) ->
  catch(exmpp_session:stop(Session)),
  ok.

init_session(Session)->
  try 
     {ok, _} = exmpp_session(Session),
     {ok, loggedin}; 
  catch
     throw:(auth_error, 'not-authorized') ->
     	{error, not_authorized}     
  end

send_presence(_, not_authorized)-> {ok, not_authorized};
send_presence(Session, loggedin) ->
  AvailableStanza = 
  	  exmp_session:set_status(
  	     exmpp_presence:presence(available , chat)),
  exmpp_session:send_packet(Session, Available),
  {ok, ready_to_chat}.
