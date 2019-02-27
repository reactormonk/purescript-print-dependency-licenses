'use strict'

var crawler = require('license-crawler');
var license = require('bower-license');

exports.npmCrawler = function() {
    return crawler.crawlLicenses({
        input: './',                  // input folder which contains package.json
        out: './tmpLicenses.json', // output file
        production: false,             // if true don't check devDependencies
        statistics: false,             // generate statistics
        exclude: [],
        sorted: 'package',            // 'license' or 'package'
        format: 'json',               // 'json' or 'txt'
        });
}

exports.bowerCrawlerImpl = function (left, right, callback) {
    return function() { 
        license.init('./', function(data, err) {
            callback((err || false) ? left(err) : right(data))();
        });
    }
}
