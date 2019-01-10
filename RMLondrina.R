rm(list = ls())

library(rvest); library(data.table); library(readxl)
#--------------------------------------------------------------------------------
# Região metropolitana de Londrina
# ----------------------------------------------------------------------------

# Scraping dos municipios ---------------------------------------------------------
url <- "https://pt.wikipedia.org/wiki/Regi%C3%A3o_Metropolitana_de_Londrina"
tabela <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[3]/div[3]/div[4]/div/table[2]') %>%
  html_table()
tabela <- tabela[[1]]
aux <- tolower(tabela[,1])
aux <- gsub("`|\\'", "", iconv(aux, to="ASCII//TRANSLIT"))
aux[aux =="jaguapita[8]"]     <- "jaguapita"
aux[aux =="porecatu[7]"]      <- "porecatu"
aux[aux =="florestopolis[7]"] <- "florestopolis"
aux <- gsub(" ", "-", aux)
aux[4] <- "" 

results <- c()
for (i in 1:(length(aux)-1)){
webpage    <- read_html(paste0("https://cidades.ibge.gov.br/brasil/pr/",aux[i] ,"/panorama"))
results[i] <- webpage %>% html_nodes(".topo__valor") %>% html_text(trim = TRUE)
webpage <- NULL
}

results

results[1] <- "4100806"
results[4] <- "4102802"
aux[4]     <- "bela-vista-do-paraiso"
cut        <- cbind(tabela[1:25,1],aux[-26],results) %>% as.data.frame()
names(cut) <- c("Municipio","Municipio2", "muni")

# Realizando o cut ------------------------------------------------------------------
setwd("/home/furriel/Dropbox/Observatório 2018/Dados")
files      <- list.files(pattern = ".csv")
domicilios <- fread(input = "domicilios.csv", sep = "auto", sep2 = "auto", integer64 = "double")
pessoas    <- fread(input = "pessoas.csv",    sep = "auto", sep2 = "auto", integer64 = "double")

domicilios$V0011 <- as.character(domicilios$V0011)
domicilios$muni <- substr(domicilios$V0011,1,7)

pessoas$V0011    <- as.character(pessoas$V0011)
pessoas$muni     <- substr(pessoas$V0011,1,7)

domicilio1 <- merge(domicilios, cut, "muni")
pessoa1 <- merge(pessoas, cut, "muni")

# Selecionando as variábveis de interesse
vars_domi <- c("V0011", "V0002", "V1006",    "V0215",   "V0220",      "V0202",     "V0401",   "V0210", "V0204",      "V0207",  "V0209", "V6531", "Municipio", "Municipio2")
cods_domi <- c("apond", "munic", "situacao", "maquina", "computador", "alvenaria", "pessoas", "lixo",  "dormitorio", "esgoto", "agua",  "rendimento", "Municipio", "Municipio2")
vars_pess <- c("V0011", "V0002", "V6036", "V6400", "Municipio", "Municipio2")
cods_pess <- c("apond", "munic", "idade", "instrucao", "Municipio", "Municipio2")

domicilios1. <- subset(domicilio1, select = vars_domi)
pessoas1.    <- subset(pessoa1, select = vars_pess)

names(domicilios1.) <- cods_domi
names(pessoas1.)    <- cods_pess

# Exportando
#write.table(domicilios1., "domiciliosRML.csv", sep = ";", quote = F, row.names = F)
#write.table(pessoas1., "pessoasRML.csv",    sep = ";", quote = F, row.names = F)
