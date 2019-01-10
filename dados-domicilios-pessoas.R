rm(list = ls())

#setwd("/home/andrefbm/Dropbox/Observatório 2018/Dados")
setwd("/home/furriel/Dropbox/Observatório 2018/Dados")


library(data.table)
library(readxl)

files      <- list.files(pattern = ".csv")
domicilios <- fread(input = "domicilios.csv", sep = "auto", sep2 = "auto", integer64 = "double")
pessoas    <- fread(input = "pessoas.csv",    sep = "auto", sep2 = "auto", integer64 = "double")
cuts       <- read_xlsx(path = "Nomes APONDs_2010.xlsx", sheet = "Apond_RMM_2010")[, c(4, 5)]

domicilios$V0011 <- as.character(domicilios$V0011)
pessoas$V0011    <- as.character(pessoas$V0011)

# Áreas de ponderação
aponds <- cuts$Aponds

# Variáveis de interesse
vars_domi <- c("V0011", "V0002", "V1006",    "V0215",   "V0220",      "V0202",     "V0401",   "V0210", "V0204",      "V0207",  "V0209", "V6531")
cods_domi <- c("apond", "munic", "situacao", "maquina", "computador", "alvenaria", "pessoas", "lixo",  "dormitorio", "esgoto", "agua",  "rendimento")
vars_pess <- c("V0011", "V0002", "V6036", "V6400")
cods_pess <- c("apond", "munic", "idade", "instrucao")

# Recortando os bancos
domicilios1 <- subset(domicilios, subset = domicilios$V0011 %in% aponds, select = vars_domi)
pessoas1    <- subset(pessoas,    subset = pessoas$V0011 %in% aponds, select = vars_pess)

# Visando geral dos bancos
dim(domicilios1)
dim(pessoas1)
head(domicilios1)
head(pessoas1)

# Exportando os bancos
write.table(domicilios1, "domicilios1.csv", sep = ";", quote = F, row.names = F)
write.table(pessoas1,    "pessoas1.csv",    sep = ";", quote = F, row.names = F)

