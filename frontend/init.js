let token = (function() {
    r = document.cookie.match(new RegExp('XSRF-TOKEN=([^;]+)'))
    if (r) return r[1];)();
Elm.App.init(
    { node: document.getElementById("app")
    , flags : token
    });