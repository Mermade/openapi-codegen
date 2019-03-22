#!/bin/sh

set -e

echo typescript-node
cd out/typescript-node
npm i
npx tsc -t ES6 -m commonjs *.ts
cd ../..

echo typescript-fetch
cd out/typescript-fetch
npm i
npx tsc -t ES6 -m commonjs *.ts
cd ../..

echo typescript-axios
cd out/typescript-axios
npm i
npx tsc -t ES6 -m commonjs *.ts
cd ../..

#echo typescript-angularjs
#cd out/typescript-angularjs
#npx tsc -t ES6 -m commonjs *.ts
#cd ../..

#echo typescript-jquery
#cd out/typescript-jquery
#npx tsc -t ES6 -m commonjs *.ts
#cd ../..

