{application, 'backend', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['backend_app','backend_sup','hello_handler']},
	{registered, [backend_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {backend_app, []}},
	{env, []}
]}.