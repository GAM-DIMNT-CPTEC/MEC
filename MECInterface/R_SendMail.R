Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.131-3.b12.el7_3.x86_64")
#Sys.getenv("JAVA_HOME")
# ALSO..
# Put the following two jars into
# system.file("java", package = "mailR")
# https://mvnrepository.com/artifact/javax.activation/javax.activation-api/1.2.0
# https://mvnrepository.com/artifact/com.sun.activation/javax.activation/1.2.0

library(mailR)

# ==============================================================================
# Alimenta a lista de texto a ser mostrado
# ==============================================================================
f.Output = function(x) {
  output[[length(output)+1]] = x
  .GlobalEnv$output = output
}
args <- commandArgs(trailingOnly=T)

suppressMessages(Sys.setlocale("LC_ALL", "pt_BR.UTF-8"))

mail.from  = "roberto.garcia@inpe.br"
mail.to    = c("roberto.garcia@inpe.br")
mail.title = "Avaliação diária MERGE vs. Modelos operacionais"
mail.body  = "AUX-HTML_Body.html"

output = list()
f.Output(readLines("AUX-HTML_01_Header.html"))

f.Output(paste0("<h2>Última data alvo da avaliação</h2>"))
LAST_DT=readLines("LAST_DT.txt")
LAST_DT=as.POSIXct(LAST_DT, format="%Y%M%d%H")
LAST_DT=format(LAST_DT, format="%Y-%M-%d às %Hhs")
f.Output(paste0("<h3>", LAST_DT,"</h2>"))

f.Output(paste0("<h2 class=\"erro\">Mensagens de erro</h2>"))
f.Output("<pre>")
if (file.exists("REPPORTED_ERRORS.txt")) {
  f.Output(readLines("REPPORTED_ERRORS.txt"))
}
f.Output("</pre>")

# Save the email body
writeLines(unlist(output), con = mail.body)

suppressMessages(send.mail(
  from = mail.from,
  to = mail.to,
  subject = mail.title,
  body = mail.body,
  smtp = list(host.name="inpe.br", port=587,
              user.name=mail.from, passwd="RG123!@#", ssl=T),
  authenticate=T, send=T, html=T)
)

#suppressMessages(send.mail(
#  from = "garcia.cptec@gmail.com",
#  to = mailsTo,
#  subject = mail.title,
#  body = "AUX-HTML_Body.html",
#  smtp = list(host.name="smtp.gmail.com", port=25,
#              user.name="jrmg.caragua@gmail.com", passwd="JC123!@#", ssl=T),
#  authenticate=F, send=T, html=T)
#)
