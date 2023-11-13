# emqx_plugrel

A rebar plugin to help create EMQX plugin release package (.tar.gz)

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar.config:

```erlang
%% Make use of this plugin
{plugins, [
    {emqx_plugrel, {git, "https://github.com/emqx/emqx_plugrel.git", {tag, "0.1.0"}}}
]}.

%% Add relx config to build a release for this plugin to work on
%% Version "0.1.0" in this example is the release version
%% which is different from the application version (vsn in .app file)
{relx, [ {release, {emqx_plugin_template, "0.1.0"}, 
            [ emqx_plugin_template %% list the applications to be released
            , map_sets
            ]}
       , {include_erts, false} % no need to include erts
       ]}.

%% Additional information to describe more details about the plugin.
%% This part will be generated to a JSON file in the release package.
{emqx_plugrel,
    [ {authors, ["EMQX Team"]}
    , {builder,
        [ {name, "EMQX Team"}
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

