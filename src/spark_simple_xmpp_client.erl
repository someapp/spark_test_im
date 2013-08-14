-module(spark_simple_xmpp_client).
-export([connect/4,
		 send_presence/2
	     try_authenticate/3,
	     init_bosh_session/5,
	     disconnect/1]).
	     
-include_lib(exmpp-0.9.9/include/exmpp_jid.hrl).
-include_lib(exmpp-0.9.9/include/exmpp_client.hrl).
	     
connect(UserName, Password, Host, Port)->
  Session = exmpp_session:start({1,0}), 
  Jid = spark_xmpp_user:make_jid(UserName),
  case init_session(Session) of
  	 logged_in -> {ok, {Session, Jid}};
  	 not_authorized -> {ok, {Jid, not_authorized}}
  end.

disconnect(Session) ->
  catch(exmpp_session:stop(Session)),
  ok.

try_authenticate(Session, Jid, Password)->
  try exmpp_session:auth_basic(Session, Jid, Password) of
     {ok, _} -> {ok, logged_in} 
  catch
     throw:(auth_error, 'not-authorized') ->
     	{error, not_authorized}     
  end.
  
init_bosh_session(Session, 
				  HttpBind,
				  Host,
				  Port,
				  Jid)-> 
  {ok, StreamId, _Features} = 
  		exmpp_session:connect_BOSH(Session, 
        HttpBind, Host,[]),
  {ok, StreamId}.
  

send_presence(_, not_authorized)-> {ok, not_authorized};
send_presence(Session, loggedin) ->
  AvailableStanza = 
  	  exmp_session:set_status(
  	     exmpp_presence:presence(available , chat)),
  exmpp_session:send_packet(Session, Available),
  {ok, ready_to_chat}.
