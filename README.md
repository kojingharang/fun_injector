fun_injector
======
Injects functions in some module to specified gen_server by using parse_transform

Motivation
======
- Testing your gen_server is sometimes not easy.
- To avoid that, you might split gen_server into complicated logic module and a thin gen_server which calls the logic module.
- Then you might write exported functions which call gen_server:call, handle_call clauses which call corresponding funcion in the logic module, following some rules.
- .... that's buggy and really boring.
- fun_injector auto-generate those wrapper functions instead of you, Yay!

How to use
======
1. Add entry to rebar.config in your project.
```
{fun_injector, ".*", {git, "git://github.com/kojingharang/fun_injector.git", {branch, "master"}}}
```

2. Add an empty gen_server implementation.
4. Add compile option like this:
```
-compile([{parse_transform, fun_injector},
          {fun_injector_extract_from, fun_injector_sample_module_a}]).
```
Now your gen_server have exported functions defined in fun_injector_sample_module_a.

See test directory for detail.


Status
======
the very alpha.

- TODOs
 - Docs
 - Error handling
 - Output deparsed source of generated AST
