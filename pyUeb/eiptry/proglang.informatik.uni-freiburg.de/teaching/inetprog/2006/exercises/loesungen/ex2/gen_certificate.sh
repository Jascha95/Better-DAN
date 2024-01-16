#!/bin/sh

# Generiert und exportiert das Serverzertifikat

KEYSTORE=serverkey.jks
SERVER_CERTIFICATE=sw.cer

echo "Generating new key store $KEYSTORE."
keytool -genkey -alias sw -keyalg RSA -validity 7 -keystore $KEYSTORE

echo "Exporting server certificate to $SERVER_CERTIFICATE"
keytool -export -keystore $KEYSTORE -alias sw -file $SERVER_CERTIFICATE
