#!/bin/sh

# Importiert das Serverzertifikat in den Truststore des Clients

SERVER_CERTIFICATE=sw.cer
TRUSTSTORE=trust.jks

keytool -import -alias sw -file $SERVER_CERTIFICATE -keystore $TRUSTSTORE

