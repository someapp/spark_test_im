-ifndef(SPARK_TEST_IM_HRL).
-define(SPARK_TEST_IM_HRL, true).
-record(user, {
	 membership = undefined :: atom(),
	 jid = <<"">> :: binary(), 
	 access_token = <<"">> ::binary(),
 	 email = <<"">> :: binary(),
 	 password = <<"">> :: binary()
}).
-endif.
