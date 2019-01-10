rm(list = ls())

library(dplyr)

# setwd("/home/furriel/Dropbox/Observatório 2018/Dados")
setwd("/home/andrefbm/Dropbox/Observatório 2018/Dados")

domicilios <- read.csv("domiciliosRML.csv", sep = ";")
pessoas    <- read.csv("pessoasRML.csv", sep = ";")

domicilios$apond <- domicilios$apond%>%as.character()
pessoas$apond    <- pessoas$apond%>%as.character()

#=============================================================================================================================================
# TRANFORMACAO DAS VARIAVEIS

# Cálculo das variáveis de acordo com apond
## 1) Percentual de pessoas cujo domicílio possui rede geral de esgoto ou pluvial, ou fossa séptica;
esgoto <- with(domicilios, tapply(esgoto, apond, function(j) mean(j %in% c(1, 2), na.rm = T)))

## 2) Percentual de pessoas cujo domicílio possui água distribuída por rede geral de abastecimento;
agua <- with(domicilios, tapply(agua, apond, function(j) mean(j %in% c(1, 2), na.rm = T)))

## 3) Percentual de pessoas cujo domicílio possui coleta de lixo diretamente por serviço de limpeza ou em caçamba de serviço de limpeza;
lixo <- with(domicilios, tapply(lixo, apond, function(j) mean(j %in% c(1, 2), na.rm = T)))

## 4) Percentual de pessoas em domicílios com densidade de até dois moradores por dormitório;
domicilios$densidade <- domicilios$pessoas/domicilios$dormitorio
densi_dorm <- with(domicilios, tapply(densidade, apond, function(j) mean(j <= 2, na.rm = T)))

## 5) Mediana do rendimento domiciliar per capita;
rdpc <- with(domicilios, tapply(rendimento, apond, median, na.rm = T))

## 6) Razão de dependência de menores de 15 anos (Pessoas de 0 a 14 anos / Pessoas de 15 a 64 anos)
rdep <- with(pessoas, tapply(idade, apond, function(u) length(u[u >= 0 & u <= 14]) / length(u[u >= 15 & u <= 64])))

## 7) Percentual de pessoas sem instrução ou com fundamental incompleto e 18 anos ou mais de idade;
escolaridade1 <- with(pessoas[pessoas$idade >= 18,], tapply(instrucao, apond, function(k) mean(k == 1) ))

## 8) Percentual de pessoas com fundamental completo ou médio incompleto e 18 anos ou mais de idade;
escolaridade2 <- with(pessoas[pessoas$idade >= 18,], tapply(instrucao, apond, function(k) mean(k == 2) ))

## 9) Percentual de pessoas com médio completo ou superior incompleto e 18 anos ou mais de idade; 
escolaridade3 <- with(pessoas[pessoas$idade >= 18,], tapply(instrucao, apond, function(k) mean(k == 3) ))

## 10)  Percentual de pessoas com superior completo e 18 anos ou mais de idade;
escolaridade4 <- with(pessoas[pessoas$idade >= 18,], tapply(instrucao, apond, function(k) mean(k == 4) ))

## 11) Percentual de pessoas em domicílios com existência de máquina de lavar
maquina <- with(domicilios, tapply(maquina, apond, function(j) mean(j == 1, na.rm = T)))

## 12) Percentual de pessoas em domicílios com existência de computador com acesso à Internet;
computador <- with(domicilios, tapply(computador, apond, function(j) mean(j == 1, na.rm = T)))

## 13) Percentual de pessoas em domicílios com alvenaria predominante nas paredes externas
alvenaria <- with(domicilios, tapply(alvenaria, apond, function(j) mean(j %in% c(1, 2), na.rm = T)))

## 14) Pegar o nome dos municípios

nome           <- domicilios %>% distinct(apond, Municipio)
nome$Municipio <- nome$Municipio%>%as.character()

# Banco final
dados.final <- cbind(nome[,2],esgoto, agua, lixo, densi_dorm, rdpc, rdep, escolaridade1, escolaridade2,
                     escolaridade3, escolaridade4, maquina, computador, alvenaria)%>%as.data.frame()
dados.final$apond <- rownames(dados.final)  

aux <- substring(dados.final$apond, 13, 13)
aux <- ifelse(aux==1,"" , aux)
aux2 <- paste0(nome$Municipio," " , aux)

nome1 <- gsub(pattern = "[^[:alnum:][:blank:]+?&/\\-]", replacement = "", x = aux2)
rownames(dados.final) <- nome1  

dados.final <- 


# Exportando
write.table(dados.final, "dados_cluster_RML.csv", sep = ";", quote = F, row.names = F)

