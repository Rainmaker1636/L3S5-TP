#!/bin/bash
nomXML='([a-zA-Z:_])([a-zA-Z:_0-9.\-]*)'
refentite='&'$nomXML';'
valeurattribut='"([^<"&]|('$refentite'))*"'
baliseouvrante='<('$nomXML')([[:space:]]+('$nomXML'[[:space:]]*=[[:space:]]*'$valeurattribut'))*>'

egrep $baliseouvrante --color=auto tdm2_fichiers/html/contact.html
