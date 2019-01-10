rm(list = ls())
setwd("/home/andrefbm/Dropbox/Observatório 2018/Dados")

library(descr)
library(readxl)
library(data.table)

aux1 <- c("Domicilios", "Mortalidade", "Pessoas")
aux2 <- c("DOMI", "MORT", "PESS")

for(i in 1:length(aux2))
{
  dicionario        <- read_xls(path = paste0(getwd(), "/Dados/Documentação/Layout/Layout.xls"), sheet = aux2[i], skip = 1)[, c(1, 2, 8, 9)]
  names(dicionario) <- c("var", "nome" ,"inicial", "final")
  write.table(x = dicionario, file = paste0(tolower(aux1)[i], "_dicio.csv"), sep = ";", append = F, row.names = F)
  fwf2csv(fwffile = paste0(getwd(), "/Dados/", aux1[i], "/Amostra_", aux1[i], "_41.txt"), csvfile = paste0(tolower(aux1)[i], ".csv"), 
          names = dicionario$var, begin = dicionario$inicial, end = dicionario$final)
}


