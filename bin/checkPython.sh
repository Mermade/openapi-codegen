#!/bin/sh

set -e

python --version

cd out/python
cd swagger_client
python -m py_compile *.py
cd ../../..

cd out/flaskConnexion
python -m py_compile *.py
cd ../..
