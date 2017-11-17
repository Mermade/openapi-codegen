#!/bin/sh
export NODE_ENV=development
export DEBUG="swagger-tools:middleware:*"
node server/index.js
