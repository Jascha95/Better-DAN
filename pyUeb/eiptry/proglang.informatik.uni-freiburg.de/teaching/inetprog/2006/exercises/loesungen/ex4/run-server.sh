#!/bin/sh

if [ ! -e chat-server.jar -o ! -e chat_policy ] ; then
    echo "chat-server.jar or chat_policy not found!"
    exit 1
fi

SRV_DIR=$HOME/.public_html/RMI

cp chat-server.jar chat_policy $SRV_DIR
chmod 644 $SRV_DIR/chat-server.jar
cd $SRV_DIR
export CLASSPATH=""
java -Djava.rmi.server.ignoreStubClasses=true -Djava.security.policy=chat_policy -Djava.rmi.server.codebase=http://www.informatik.uni-freiburg.de/~wehr/RMI/chat-server.jar -jar chat-server.jar