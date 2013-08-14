-module(spark_xmpp_user).
-export([make_jid/1]).


-include_lib("exmpp-0.9.9/include/exmpp_jid.hrl").
-include_lib("exmpp-0.9.9/include/exmpp_client.hrl").
-include_lib("spark_test_im.hrl").

make_jid(#user{} = User) ->
  
  #jid{
      raw = User,
      resource = undefined
  }.
