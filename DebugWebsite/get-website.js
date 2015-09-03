var url = 'http://127.0.0.1:8023/';
var url2 = 'http://www.google.com/';

var page = require('webpage').create();
page.open(url, function () {
    page.evaluate(function(){

    });
    console.log(page.content);
    phantom.exit();
});
