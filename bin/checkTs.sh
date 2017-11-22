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

#cd out/typescript-angularjs
#npx tsc -t ES6 -m commonjs *.ts

