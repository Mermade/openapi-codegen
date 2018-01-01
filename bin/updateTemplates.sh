#!/bin/sh
export MERGE_BASE=`cat MERGE_BASE`
#export SCBRANCH=3.0.0
export SCBRANCH=master
echo Using swagger-codegen/$SCBRANCH@$MERGE_BASE
cd ../swagger-codegen
git checkout $SCBRANCH
git branch -D tmp 2> /dev/null
git pull
export NEWBASE=`git rev-parse HEAD`
git checkout -b tmp $MERGE_BASE
cp -pr ../openapi-codegen/templates/* modules/swagger-codegen/src/main/resources/
git diff
git add modules
git commit -m "Temporary commit"
git rebase $SCBRANCH
rc=$?
if [ $rc != 0 ]; then
  echo You need to complete the rebase...
  exit $rc
fi
cd ../swagger-codegen
cp -pr modules/swagger-codegen/src/main/resources/* ../openapi-codegen/templates/
git checkout $SCBRANCH
git branch -D tmp
echo $NEWBASE > ../openapi-codegen/MERGE_BASE
