#!/bin/sh
cd out/typescript-node
npx tsc -t ES6 -m commonjs *.ts
cd ../..
cd out/typescript-fetch
npx tsc -t ES6 -m commonjs *.ts
cd ../..
#cd out/typescript-angularjs
#npx tsc -t ES6 -m commonjs *.ts

