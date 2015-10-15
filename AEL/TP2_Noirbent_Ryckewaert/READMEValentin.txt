# TP2-AEL
alias cegrep='egrep --color=auto'
1) cegrep --color=auto nez Cyrano.txt

2)cegrep  '\([[:alpha:][:punct:][:space:]]*\)'  Cyrano.txt

3) cegrep '([^[:alpha:]àâéàêîùûÀÂÉÈÊÎÔÙÛ]|^)([[:alpha:]àâéàêîùûÀÂÉÈÊÎÔÙÛ]{4})([^[:alpha:]àâéàêîùûÀÂÉÈÊÎÔÙÛ6]|$)' Cyrano.txt

4) Car dans " vous vous", egrep prend " vous ", il reste donc "vous " or ce cas là n'est pas considéré comme un mot car il n'est pas précédé par un caractère

Exercice 2

3) cegrep '((+33[[:space:]]\(0\)[[:space:]][0-9])|([0-9]{2}))(([.][0-9][0-9]){4})' tdm2_fichiers/html/contact.html
