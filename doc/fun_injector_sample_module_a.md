

# Module fun_injector_sample_module_a #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Sample module.
Copyright (c) (C) 2015 Koji Hara. All Rights Reserved.


<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###



<pre><code>
state() = #fun_injector_sample_module_a{value = undefined | integer()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-2">add/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-2"></a>

### add/2 ###


<pre><code>
add(V::integer(), State::<a href="#type-state">state()</a>) -&gt; {integer(), <a href="#type-state">state()</a>}
</code></pre>
<br />


<a name="get-1"></a>

### get/1 ###

`get(State) -> any()`


<a name="init-1"></a>

### init/1 ###


<pre><code>
init(V::integer()) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />


