#!/usr/bin/env bash

# Datum: 2007-11-06
# Autor: Stefan Wehr (wehr@informatik.uni-freiburg.de)
# Zweck: Beheben von häufig auftretenden Problem beim Login in die Umgebung
#        zum betreuten Programmieren.
#
# Hinweis: Die Benutzung dieses Skriptes geschieht auf eigenes Risiko.
#
# Benutzung: 
# * Mit der normalen Sitzung (nicht der Info-I Sitzung) einloggen
# * xterm starten
# * Skript durch Aufruf des Befehls "bash fix-info1-env.sh" ausführen
#
# Versionshistorie:
# * 2007-11-07: Behandlung von SSH-Schlüsseln mit Passwort verbessert
# * 2007-11-06: initiale Version

PUBKEY=$HOME/.ssh/id_dsa.pub
PRIVKEY=$HOME/.ssh/id_dsa
AUTHKEY=$HOME/.ssh/authorized_keys

GREP=`which ggrep`
if [ ! -x "$GREP" ] ; then
    GREP=grep
fi

ls -ld $HOME | grep '^d..x..x..x' > /dev/null
ecode=$?

if [ $ecode -eq 0 ] ; then
    echo "Berechtigungen fuer $HOME OK"
else
    echo "Berechtigungen fuer $HOME nicht OK"
    ls -ld $HOME
    echo "Aendere Berechtigungen fuer $HOME"
    chmod a+x $HOME
    chmod g+x $HOME
    chmod u+rwx $HOME
    echo "Aktuelle Berechtigungen fuer $HOME"
    ls -ld $HOME
fi

if [ ! -e $PUBKEY -o ! -e $AUTHKEY ] ; then
    echo "Fehlerhafte SSH Konfiguration, entweder $PUBKEY oder $AUTHKEY existieren nicht!!"
    exit 1
fi

$GREP -f $PUBKEY $AUTHKEY > /dev/null
ecode=$?

if [ $ecode -eq 0 ] ; then
    echo "SSH Konfiguration OK"
else
    echo "SSH Konfiguration nicht OK, dies wird jetzt geaendert"
    echo >> $AUTHKEY
    cat $PUBKEY >> $AUTHKEY
    echo "$PUBKEY ist jetzt in der Datei $AUTHKEY enthalten"
fi

echo "Falls Dein SSH Schluessel durch ein Passwort geschuetzt ist, sollte dieses jetzt durch ein"
echo "leeres Passwort ersetzen werden."
ssh-keygen -p -f $PRIVKEY

echo "Es wird jetzt versucht, per SSH auf dem Rechner abraxas einzuloggen. Dies muss "
echo "ohne Passwort möglich sein."
ssh abraxas.informatik.uni-freiburg.de
