# emqx_plugrel

A rebar plugin to help create EMQ X plugin release package (.tar.gz)

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar.config:

```

%% Make use of this plugin
{plugins, [
    {emqx_plugrel, {git, "https://github.com/emqx/emqx_plugrel.git", {tag, "0.1.0"}}}
]}.

%% Add relx config to build a release for this plugin to work on
{relx, [ {release, {emqx_plugin_template, "0.1.0"}, %% this is the release version, different from app vsn in .app file
            [ emqx_plugin_template
            , map_sets
            ]}
       , {dev_mode, false}
       , {include_erts, false}
       ]}.

%% Additional information to describe more details about the plugin.
%% This part will be generated to a JSON file in the release package.
{emqx_plugrel,
    [ {authors, ["EMQ X Team"]}
    , {builder,
        [ {name, "EMQ X Team"}
        , {contact, "emqx-support@emqx.io"}
        , {website, "www.emqx.com"}
        ]}
    , {repo, "https://github.com/emqx/emqx-plugin-template"}
    , {functionality, ["Demo"]}
    , {compatibility,
        [ {emqx, "~> 5.0"}
        ]}
    , {description, "This is a demo plugin"}
    ]
}.

```

Execute command:

```
rebar3 emqx_plugrel tar
```

