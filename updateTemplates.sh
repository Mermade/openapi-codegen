#!/bin/sh
export MERGE_BASE=`cat MERGE_BASE`
#export SCBRANCH=3.0.0
export SCBRANCH=master
echo Using swagger-codegen@$MERGE_BASE
cd ../swagger-codegen
git checkout $SCBRANCH
git branch -D tmp
git pull
export NEWBASE=`git rev-parse HEAD`
git checkout -b tmp $MERGE_BASE
cp -pr ../openapi-codegen/templates/* modules/swagger-codegen/src/main/resources/
git diff
git add modules
git commit -m "Temporary commit"
git rebase $SCBRANCH
echo If ok:
echo cd ../swagger-codegen
echo cp -pr 'modules/swagger-codegen/src/main/resources/*' ../openapi-codegen/templates/
echo git checkout $SCBRANCH
echo git branch -D tmp
echo echo $NEWBASE '>' ../openapi-codegen/MERGE_BASE
