{application, pub_sub_app,
	[{vsn, "0.0.1"},
	{modules, [controller, pub_sub, pub_sub_sup, tcp_server]},
	{registered, [pub_sub_app]},
	{mod, {pub_sub_app, []}}
]}.