#!/bin/sh

set -e

perl --version | head -n 2

cd out/perl
perl -c *.pl
cd ../..

