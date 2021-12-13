# emqx_plugrel

A rebar plugin to help create EMQ X plugin release package

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar.config:

```
{plugins, [
    {emqx_plugrel, {git, "https://host/user/emqx_plugrel.git", {tag, "0.1.0"}}}
]}.
```

Execute command:

```
rebar3 emqx_plugrel
```
