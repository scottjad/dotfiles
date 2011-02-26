register_user_stylesheet(
    "data:text/css,"+
        escape("@namespace url(\"http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul\");\n"+
               // "#minibuffer, .mode-line, .completions {"+
               // " font-family: Terminus; font-size: 12pt;"+
               // "}\n"+
               "code, pre { font-family: DejaVu Sans Mono !important; "+
               "font-size: 12px !important; }"+
               "span.__conkeror_hint {"+
               " line-height: 14px !important;"+
               " font-size: 14px !important;"+
               "}\n"));

// toggle dark mode, adapted from philjackson's darken function
color_theme_toggle = {};

function global_color_theme(name, key, styles) {
    interactive_cmd = "toggle-" + name + "-mode";
    color_theme_toggle[name] = false;
    interactive(interactive_cmd, "",
                function (I) {
                    if (color_theme_toggle[name]) {
                        for(x in styles) {
                            unregister_user_stylesheet(styles[x]);
                        }

                        color_theme_toggle[name] = false;                    
                    } else {
                        for(x in styles) {
                            register_user_stylesheet(styles[x]);
                        }
                        color_theme_toggle[name] = true;
                    }
                });
    define_key(default_global_keymap, key, interactive_cmd);
}

global_color_theme(
    "dark", "K",
    [make_css_data_uri(
         ['*:not(img) { background: #303030 !important; '+
          'color: #f6f3e8 !important; '+
          'font-family: MonteCarlo !important; '+
          'font-size: 10pt !important; '+
          '-moz-border-bottom-colors: #444 #444 !important; '+
          '-moz-border-top-colors: #444 #444  !important; '+
          '-moz-border-right-colors: #444 #444  !important; '+
          '-moz-border-left-colors: #444 #444  !important; '+
          '-moz-box-shadow: 0 0 0 black !important; '+
          'border-color: #444 !important; '+
          ' }',
          // 'p { line-height: 100% !important; }',
          // 'code, pre { font-family: Envy Code R !important }',
          ':link, :link * { color: #8ac6f2 !important }',
          ':visited, :visited * { color: #95e454 !important }',
          'em { background-color: #444444 ! important;}'
         ], $url_prefixes = ["http", "about"]),
     
     make_css_data_uri(
         ['.vh { foreground-color: white; '+
          'background-color: #444 ! important;}',
          '.TC { border-width: 0px ! important; }', // remove extra lines on inbox
          '.xY { height: 10pt !important; }', // keep message list from being too tall
          '.oZ-jd,.hF,.hG { background: red ! important;}' // make cursor red
         ], $domains = "mail.google.com"),

     make_css_data_uri(
         ['#current-entry { background-color: #888 !important;} '+
          '.entry { background-color: #444 !important; }'
         ], $url_prefixes = ["http://www.google.com/reader",
                             "https://www.google.com/reader"]),
     
     make_css_data_uri(
         ['.k { font-weight: bold ! important }', // keyword
          'a, h1 {text-shadow: black 0px 0px 0px !important; }', // ugly shadows
          // named builtin? .nb named function .nf
          // commented cause gh's clojure highlighting marks lots of the code with this
          '.nv { color: #ff5996 ! important } ', // function name/variable name
          '.s { color: #95e454 ! important } ', // string
          '.c1 { color: #99968b ! important }', // comment
          '.x { background-color: #444444 ! important }', // added/deleted portion
          '.gi, .gi .x { color: #1AFF84 ! important }', // inserted line
          '.gd, .gd .x { color: #ff0080 ! important }' // deleted line
         ], $domains = "github.com")]);

// global_color_theme("light", "L",
//                    ['*:not(img) { background: #edebe8 ! important; color: #2c2d32 !important }',
//                     ':link, :link * { color: #1856ba !important }',
//                     ':visited, :visited * { color: #95e454 !important }']);

// global_color_theme("grey", "A",
//                    ['*:not(img) { background: #4c4c4c ! important; color: #cccccc !important }',
//                     ':link, :link * { color: #8ac6f2 !important }',
//                     ':visited, :visited * { color: #95e454 !important }']);

// global_color_theme("grey", "A",
//                    ['*:not(img) { background: #535353 ! important; color: #ffffff !important }',
//                     ':link, :link * { color: #8ac6f2 !important }',
//                     ':visited, :visited * { color: #95e454 !important }']);

// global_color_theme("beige", "Q",
//                    ['*:not(img) { background: #f5deb3 ! important; color: #000000 !important }',
//                     ':link, :link * { color: #005f87 !important }',
//                     ':visited, :visited * { color: #d75f00 !important }']);

