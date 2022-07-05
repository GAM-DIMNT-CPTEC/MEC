#!/bin/ksh
# ==============================================================================
# Prepare the email to be sent at the end of the verification process
# ==============================================================================

cd /Users/jrmgarcia/ProjDocs/Science/Projects/interative/MEC-DEV
OPER=INFORMING
SERVER=`hostname -s`
echo "================================================================================"
echo "| STARTING ${OPER}"
echo "| DATE...: `date \"+%Y-%m-%d %H:%M:%S\"`"
echo "| SERVER.: ${SERVER}"
echo "| DIRECT.: `pwd`"
echo "| SCRIPT.: `basename "$0"`"
echo "--------------------------------------------------------------------------------"
            
mail_to="roberto.garcia@inpe.br roberto.garcia@inpe.br"
mail_subj="Avaliação diária MERGE vs. Modelos operacionais"
mail_body=AUX-HTML_Body.html

cat AUX-HTML_01_Header.html > ${mail_body}

echo " * Registering the last evaluation date."
echo "<h2>Última data alvo da avaliação</h2>" >> ${mail_body}
last_dt=$(<LAST_DT.txt)
echo "<h3>" >> ${mail_body}
echo "${last_dt:0:4}-${last_dt:4:2}-${last_dt:6:2} às ${last_dt:8:2}hs">> ${mail_body}
echo "</h3>" >> ${mail_body}

echo " * Appending errors messages."
echo "<h2 class=\"erro\">Mensagens de erro</h2>" >> ${mail_body}
echo "<pre>" >> ${mail_body}
cat REPPORTED_ERRORS.txt >> ${mail_body}
echo "</pre>" >> ${mail_body}

echo " * Sending the email itself."
echo "mail -s "$(echo -e "${mail_subj}\nContent-Type: text/html")" ${mail_to} < ${mail_body}"
mail -s "$(echo -e "${mail_subj}\nContent-Type: text/html")" ${mail_to} < ${mail_body}

echo "--------------------------------------------------------------------------------"
echo "| FINISHED ${OPER}"
echo "| DATE..: `date \"+%Y-%m-%d %H:%M:%S\"`"
echo "================================================================================"
echo 

