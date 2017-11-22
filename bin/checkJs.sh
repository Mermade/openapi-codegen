#!/bin/sh

set -e

echo nodejs
cd out/nodejs
tsc -t ES6 -m system --allowJs --outFile _ts.js *.js
rm -f _ts.js
cd ../..

echo Javascript-Closure-Angular
cd out/Javascript-Closure-Angular
tsc -t ES6 -m system --allowJs --outFile _ts.js *.js
rm -f _ts.js
cd ../..

echo Javascript
cd out/Javascript/src
tsc -t ES6 -m system --allowJs --outFile _ts.js *.js
rm -f _ts.js
cd ../..

