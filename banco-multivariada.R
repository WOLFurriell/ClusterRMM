rm(list = ls())

library(plyr)
library(factoextra)

setwd("C:/Users/T.I/Dropbox/Observat�rio 2018/Dados/Dados")

# Variaveis e codificação
vars_domi <- c("V0011", "V0002", "V1006",    "V0215",   "V0220",      "V0202",     "V0401",   "V0210", "V0204",      "V0207",  "V0209", "V6531")
cods_domi <- c("apond", "munic", "situacao", "maquina", "computador", "alvenaria", "pessoas", "lixo",  "dormitorio", "esgoto", "agua",  "rendimento")
vars_pess <- c("V0011", "V0002", "V6036", "V6400")
cods_pess <- c("apond", "munic", "idade", "instrucao")

# Dados domicilios
dados_dom        <- read.table("domicilios1.csv", sep = ";", header = T)
dados_dom$V0011  <- as.character(dados_dom$V0011)
names(dados_dom) <- cods_domi

dados_dom$densidade <- dados_dom$pessoas / dados_dom$dormitorio
head(dados_dom)


dados <- ldply(lapply(1:nrow(dados_dom), function(j) 
  { 
    x <- dados_dom[j, ]
    do.call("rbind", replicate(n = x$pessoas, expr = x, simplify = F))
  }
))

head(dados)

# Dados pessoas
dados_pess        <- read.table("pessoas1.csv", sep = ";", header = T)
dados_pess$V0011  <- as.character(dados_pess$V0011)
names(dados_pess) <- cods_pess

# Áreas de ponderação
apond <- unique(dados$apond)

# Cálculo das variáveis de acordo com apond
## 1) Percentual de pessoas cujo domicílio possui rede geral de esgoto ou pluvial, ou fossa séptica;
esgoto <- with(dados, tapply(esgoto, apond, function(j) mean(j %in% c(1, 2), na.rm = T)))

## 2) Percentual de pessoas cujo domicílio possui água distribuída por rede geral de abastecimento;
agua <- with(dados, tapply(agua, apond, function(j) mean(j %in% c(1, 2), na.rm = T)))

## 3) Percentual de pessoas cujo domicílio possui coleta de lixo diretamente por serviço de limpeza ou em caçamba de serviço de limpeza;
lixo <- with(dados, tapply(lixo, apond, function(j) mean(j %in% c(1, 2), na.rm = T)))
  
## 4) Percentual de pessoas em domicílios com densidade de até dois moradores por dormitório;
densi_dorm <- with(dados, tapply(densidade, apond, function(j) mean(j <= 2, na.rm = T)))

## 5) Mediana do rendimento domiciliar per capita;
rdpc <- with(dados_dom, tapply(rendimento, apond, median, na.rm = T))

## 6) Razão de dependência de menores de 15 anos (Pessoas de 0 a 14 anos / Pessoas de 15 a 64 anos)
rdep <- with(dados_pess, tapply(idade, apond, function(u) length(u[u >= 0 & u <= 14]) / length(u[u >= 15 & u <= 64])))

## 7) Percentual de pessoas sem instrução ou com fundamental incompleto e 18 anos ou mais de idade;
escolaridade1 <- with(dados_pess[dados_pess$idade >= 18,], tapply(instrucao, apond, function(k) mean(k == 1) ))

## 8) Percentual de pessoas com fundamental completo ou médio incompleto e 18 anos ou mais de idade;
escolaridade2 <- with(dados_pess[dados_pess$idade >= 18,], tapply(instrucao, apond, function(k) mean(k == 2) ))

## 9) Percentual de pessoas com médio completo ou superior incompleto e 18 anos ou mais de idade; 
escolaridade3 <- with(dados_pess[dados_pess$idade >= 18,], tapply(instrucao, apond, function(k) mean(k == 3) ))

## 10)  Percentual de pessoas com superior completo e 18 anos ou mais de idade;
escolaridade4 <- with(dados_pess[dados_pess$idade >= 18,], tapply(instrucao, apond, function(k) mean(k == 4) ))

## 11) Percentual de pessoas em domicílios com existência de máquina de lavar
maquina <- with(dados, tapply(maquina, apond, function(j) mean(j == 1, na.rm = T)))

## 12) Percentual de pessoas em domicílios com existência de computador com acesso à Internet;
computador <- with(dados, tapply(computador, apond, function(j) mean(j == 1, na.rm = T)))

## 13) Percentual de pessoas em domicílios com alvenaria predominante nas paredes externas
alvenaria <- with(dados, tapply(alvenaria, apond, function(j) mean(j %in% c(1, 2), na.rm = T)))

# Banco final
dados.final <- cbind(esgoto, agua, lixo, densi_dorm, rdpc, rdep, escolaridade1, escolaridade2, escolaridade3, escolaridade4, maquina, computador, alvenaria)
head(dados.final)

write.table(x = dados.final, file = "dados-cluster.csv", row.names = T, sep = ",", quote = F)


# Análise de cluster
df       <- scale(dados.final)
res.dist <- dist(df, method = "euclidean")

## Cluster Hierarquico
res.hc   <- hclust(d = res.dist, method = "ward.D2")
res.coph <- cophenetic(res.hc)
cor(res.dist, res.coph)
fviz_dend(res.hc, cex = 0.6)

# Agrupando os clusters em 3 grupos
grp <- cutree(res.hc, k = 3)

fviz_dend(res.hc, k = 3, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
          )
fviz_cluster(list(data = df, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Determinando o numero ideal de clusters
nb <- NbClust(df, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)
