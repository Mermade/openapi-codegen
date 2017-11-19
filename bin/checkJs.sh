#!/bin/sh
cd out/nodejs
tsc -t ES6 -m system --allowJs --outFile _ts.js *.js
rm -f _ts.js
cd ../..
cd out/Javascript-Closure-Angular
tsc -t ES6 -m system --allowJs --outFile _ts.js *.js
rm -f _ts.js
cd ../..

