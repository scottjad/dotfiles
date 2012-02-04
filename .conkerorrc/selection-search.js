// selection searches
function create_selection_search(webjump, key) {
    interactive(webjump+"-selection-search",
                "Search " + webjump + " with selection contents",
                "find-url-new-buffer",
		$browser_object = function (I) {
                    return webjump + " " + I.buffer.top_frame.getSelection();});
    define_key(content_buffer_normal_keymap, key.toUpperCase(), webjump + "-selection-search");

    interactive("prompted-"+webjump+"-search", null,
                function (I) {
                    var term = yield I.minibuffer.read_url($prompt = "Search "+webjump+":",
                                                           $initial_value = webjump+" ");
                    browser_object_follow(I.buffer, FOLLOW_DEFAULT, term);
                });
    define_key(content_buffer_normal_keymap, key, "prompted-" + webjump + "-search");
}

create_selection_search("g","l");
create_selection_search("lucky","/"); // _cool
create_selection_search("wikipedia","w");
create_selection_search("dictionary","d");
create_selection_search("myspace","y");
create_selection_search("amazon","a");
create_selection_search("youtube","u");
create_selection_search("maps","p");
// create_selection_search("mp3","p");
// create_selection_search("torrentz","o");

