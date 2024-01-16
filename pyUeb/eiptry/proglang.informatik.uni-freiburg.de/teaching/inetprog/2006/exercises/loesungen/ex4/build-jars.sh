#!/bin/sh

if [ ! -e ChatServer.java ] ; then
    echo "The script must be executed from within the directory where the"
    echo ".java and .class files reside."
    exit 1
fi

TOPDIR=../../../../../..
SRCDIR=de/uni_freiburg/informatik/proglang/inetprog/ex4

echo "Creating jar file for server"
jar cvmf server-manifest chat-server.jar -C $TOPDIR $SRCDIR/ChatServer.class -C $TOPDIR $SRCDIR/ChatClient.class -C $TOPDIR $SRCDIR/ChatServerImplementation.class

echo "Creating jar file for client"
jar cvmf client-manifest chat-client.jar chat_policy -C $TOPDIR $SRCDIR/ChatServer.class -C $TOPDIR $SRCDIR/ChatClient.class -C $TOPDIR $SRCDIR/ChatClientImplementation.class -C $TOPDIR $SRCDIR/'ChatClientImplementation$1.class' -C $TOPDIR $SRCDIR/'ChatClientImplementation$2.class' -C $TOPDIR $SRCDIR/'ChatClientImplementation$3.class' -C $TOPDIR $SRCDIR/SwingWorker.class -C $TOPDIR $SRCDIR/'SwingWorker$1.class' -C $TOPDIR $SRCDIR/'SwingWorker$2.class' -C $TOPDIR $SRCDIR/'SwingWorker$ThreadVar.class'

