#!/bin/sh

set -e

ruby --version

echo sinatra
cd out/sinatra
ruby -c *.rb
cd ../..

