#!/bin/sh

set -e

python2 --version

cd out/python
cd swagger_client
python2 -m py_compile *.py
cd ../../..

cd out/flaskConnexion
python2 -m py_compile *.py
cd ../..
