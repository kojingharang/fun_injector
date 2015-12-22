fun_injector
======
Type safe gen_server generator: It injects functions in a specified module to some empty gen_server by using parse_transform

Motivation
======
- Testing your gen_server is sometimes not easy.
- To avoid that, you might split your gen_server into a complicated logic module and a thin gen_server which calls the logic module.
- Then you might write exported functions which call gen_server:call and handle_call clauses which call corresponding funcion in the logic module, following some rules.
- .... that's buggy and really boring.
- fun_injector auto-generate those wrapper functions instead of you, Yay!

How to use
======
0. Add an entry to rebar.config in your project (like many other libraries).

          {fun_injector, ".*", {git, "git://github.com/kojingharang/fun_injector.git", {branch, "master"}}}

2. Add an empty gen_server implementation.
3. Add compile option like this:

          -compile([{parse_transform, fun_injector},
                    {fun_injector_extract_from, adder}]).

Now your gen_server have exported functions defined in adder.

See test directory for detail.


Status
======
the very alpha.

- TODOs
 - Docs
 - Error handling
 - Output deparsed source of generated AST
