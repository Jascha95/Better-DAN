#!/bin/sh

TMP=/tmp
REG_DIR=$TMP/chat-rmi

if [ -d $REG_DIR ] ; then
    rm -rf $REG_DIR
fi
mkdir $REG_DIR

cd $REG_DIR
echo "launching rmiregistry"
rmiregistry