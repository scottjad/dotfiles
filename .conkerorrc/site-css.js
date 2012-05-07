site_css_dir = "~/.conkerorrc/site-css/";

function site_css(filename, url_prefixes) {
    var styles = read_text_file(make_file(site_css_dir+filename+".css"));
    var stylesheet = make_css_data_uri([styles], $url_prefixes = url_prefixes);
    register_user_stylesheet(stylesheet);
    interactive("toggle-"+filename,"", function() {
                    unregister_user_stylesheet(stylesheet);
                });
}

site_css("hacker-news", ["http://news.ycombinator.com"]);
// site_css("google-reader", ["http://www.google.com/reader"]);

