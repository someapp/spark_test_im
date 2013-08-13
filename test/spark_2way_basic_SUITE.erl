%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(spark_2way_basic_SUITE).
-export([all/0,
		 suite/0,
		 init_per_group/2,
		 end_per_group/2,
		 init_per_suite/2,
		 end_per_suite/2,
		 init_per_testcase/2,
		 end_per_testcase/2
]).


-export([
	  aa2aa_2way_should_pass_story,
      aa2sub_2way_should_pass_story,
      aa2nonsub_2way_should_pass_story,
      sub2sub_2way_should_pass_story,
      sub2nonsub_2way_should_block_pass_story,
      nonsub2non_2way_should_block_pass_story,
      nonsub2sub_2way_should_block_pass_story,
      nonsub2aa_2way_should_block_pass_story           
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("spark_test_im.hrl").

-define(COMPONENT, get_conf_value(component)).
-define(SECRET, get_conf_value(secret)).
-define(SERVER_HOST, spark_test_im_config:get_conf_value(server_host)).
-define(SERVER_PORT, spark_test_im_config:get_conf_value(server_port)).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, messages}].

groups() ->
    [{messages, [sequence], 
     [aa2aa_2way_should_pass_story,
      aa2sub_2way_should_pass_story,
      aa2nonsub_2way_should_pass_story,
      sub2sub_2way_should_pass_story,
      sub2nonsub_2way_should_block_pass_story,
      nonsub2non_2way_should_block_pass_story,
      nonsub2sub_2way_should_block_pass_story,
      nonsub2aa_2way_should_block_pass_story           
      ]}].

suite() ->
    [{timetrap, {minutes, 1}}].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
	error_logger:info_msg("Init Test Suite with Config ~p~n",[Config]),
    ok = ensure_start().

end_per_suite(Config) ->
	error_logger:info_msg("End Test Suite with Config ~p~n",[Config])
	ok = ensure_stop().

init_per_group(GroupName, Config) ->
	error_logger:info_msg("Init Test Group ~p with Config ~p~n",[GroupName, Config]),
    ok.

end_per_group(GroupName, Config) ->
	error_logger:info_msg("End Test Group ~p with Config ~p~n",[GroupName, Config]),
    ok.

init_per_testcase(CaseName, Config) ->
	error_logger:info_msg("Init Test Case ~p with Config ~p~n",[CaseName, Config]),
    ok.

end_per_testcase(CaseName, Config) ->
	error_logger:info_msg("End Test Case ~p with Config ~p~n",[CaseName, Config]),
    ok.

ensure_start()->
    Apps = [
	    	inets,
	    	crypto, 
        	public_key,
        	ssl,
	    	restc,
	    	ets, 
	    	exmpp],
	    	
    app_util:start_apps(Apps),    
    ok.

ensure_stop()->
    Apps = [
	    	inets,
	    	crypto, 
        	public_key,
        	ssl,
	    	restc,
	    	ets, 
	    	exmpp],
	    	
    app_util:stop_apps(Apps),    
    ok.


%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------
permit_to_chat(UserA, UserB, Server)->
   {ok, Session1} = create_chat_session(UserAJid, 
   				    UserAEmail, UserAPassword, Server),
   {ok, Session2} = create_chat_Session(UserBJid, 
   					UserBEmail, UserBPassword, Server),
   {ok, sent} = chat_2_way_ok(Session1, Session2, UserA, UserB).

cannot_reply(UserA, UserB, Server)->
   {ok, Session1} = create_chat_session(UserAJid, 
   				    UserAEmail, UserAPassword, Server),
   {ok, Session2} = create_chat_Session(UserBJid, 
   					UserBEmail, UserBPassword, Server),
   {ok, sent_one_way} = chat_1_way_ok(Session1, Session2, UserA, UserB).

cannot_chat(UserA, UserB)->
   {nok, not_authorized} = create_chat_session(UserAJid, 
   				    UserAEmail, UserAPassword, Server),
   {nok, not_authorized} = create_chat_Session(UserBJid, 
   					UserBEmail, UserBPassword, Server).
 
aa2aa_2way_should_pass_story(Config) ->
    UserA = get_user_setting(allaccess1, Config),
    UserB = get_user_setting(allaccess2, Config),
	permit_to_chat(UserA, UserB).
   
aa2sub_2way_should_pass_story(Config) ->
    UserA = get_user_setting(allaccess1, Config),
    UserB = get_user_setting(subscribed1, Config),
 	permit_to_chat(UserA, UserB).  

aa2nonsub_2way_should_pass_story(Config) ->
    UserA = get_user_setting(allaccess1, Config),
    UserB = get_user_setting(non_subscribed1, Config),
	permit_to_chat(UserA, UserB).

sub2sub_2way_should_pass_story(Config) ->
    UserA = get_user_setting(subscribed1, Config),
    UserB = get_user_setting(subscribed2, Config),
	permit_to_chat(UserA, UserB).

sub2nonsub_2way_should_block_pass_story(Config) ->
    UserA = get_user_setting(subscribed1, Config),
    UserB = get_user_setting(non_subscribed1, Config),    
	cannot_reply(UserA, UserB).

nonsub2non_2way_should_block_pass_story(Config) ->
    UserA = get_user_setting(non_subscribed1, Config),
    UserB = get_user_setting(non_subscribed2, Config),
	cannot_chat(UserA, UserB).

nonsub2sub_2way_should_block_pass_story(Config) ->
    UserA = get_user_setting(non_subscribed1, Config),
    UserB = get_user_setting(subscribed1, Config),

	cannot_chat(UserA, UserB).

nonsub2aa_2way_should_block_pass_story(Config) ->           
    UserA = get_user_setting(non_subscribed1, Config),
    UserB = get_user_setting(allaccess1, Config),
	cannot_chat(UserA, UserB).
	

	
get_user_setting(UserType, []) -> ok;	
get_user_setting(UserType, Config) 
	when is_atom(UserType) ->
    Users = ct:get_config(spark_im_users);
    User = proplists:get_val(UserType, undefined),
    Membership = proplists:getvalue(membership, User, undefined),
    Jid = proplists:getvalue(username, User, undefined),
    Email = proplists:getvalue(email, User, undefined),
    Password = proplists:getvalue(login_password, User, undefined),
    AccessToken = spark_im_test_util:create_password(Config),
    #user {
	 membership = Membership,
	 jid = Jid, 
	 access_token = AccessToken,
 	 email = Email,
 	 password = Password}.

create_chat_session(Jid, Password, Server) ->
    Session = exmpp_session:start({1,0}), 
               % xmpp stream ver  1.0 for SASL used in http-bind
    MyJID = exmpp_jid:make("bosh", "localhost", random),
    exmpp_session:auth_basic(MySession, MyJID, Password),
    %% Connect in standard TCP:
    {ok, _StreamId, _Features} = exmpp_session:connect_BOSH(MySession,
"http://127.0.0.1:5280/http-bind",
"localhost", []),
    session(MySession, MyJID).
	
	
iso_8601_fmt(DateTime)->
   {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
   io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.B:~2.10.0B",
   	[Year, Month, Day, Hour, Min, Sec]).

timestamp_as_msg(Seed)->
   DateTime = erlang:localtime(),
   Str = iso_8601_fmt(DateTime),
   <<Str/binary,Seed/binary>>.
  
