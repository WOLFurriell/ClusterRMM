#-------------------------------------------------------------------------
# Aplicação Observatório RMM ------------------------------------------------
#-------------------------------------------------------------------------
rm(list = ls())
library(cluster)
library(factoextra)
library(readxl)
#------------------------------------------------------------------------------
# O BANCO DE DADOS
setwd("/home/furriel/Dropbox/Observatório 2018/Dados")
dados        <- read.table("dados-cluster.csv", sep = ",", header = T)
cuts         <- data.frame(read_xlsx(path = "Nomes APONDs_2010.xlsx", sheet = "Apond_RMM_2010")[, c(4, 6)])
dados$aponds <- rownames(dados)
names(cuts)[1]  <- "aponds"
dados        <- merge(dados, cuts, by = c("aponds"))
rownames(dados) <- dados$cidade
aponds          <- dados$aponds
nomes           <- dados$cidade

dados$cidade   <- NULL
dados$aponds    <- NULL

# -------------------------------------------------------------------------------------------------
# Hierarchical Clustering --------------------------------------------------------------------

df  <- scale(dados)
res.dist <- dist(df, method = "euclidean")

res.hc   <- hclust(d = res.dist, method = "ward.D2"); res.hc
# Dendograma
fviz_dend(res.hc, cex = 0.5)

# Verify the cluster tree
res.coph <- cophenetic(res.hc); res.coph

# Correlation between cophenetic distance and the original distance
cor(res.dist, res.coph)

# Pelo metodo average
res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, cophenetic(res.hc2))

# Cut tree into 4 groups
grp <- cutree(res.hc, k = 5)
head(grp, n = 5)
table(grp)

# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 5, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#ff0000", "#cc6600", "#3366ff", "#336600","#5900b3"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

fviz_cluster(list(data = df, cluster = grp),
             palette = c("#ff0000", "#cc6600", "#3366ff", "#336600","#5900b3"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

dados$cluster <- grp
dados$aponds  <- aponds
dados$nomes   <- nomes

write.csv(dados,file = "/home/furriel/Dropbox/Observatório 2018/Dados/grupos.csv")

# -------------------------------------------------------------------
# Medidas descritivas ---------------------------------
# ------------------------------------------------------------------

aggregate(dados[1:12], by=list(cluster=dados$grp), mean)

palette(1:5)

c("#ff0000", "#cc6600", "#3366ff", "#336600","#5900b3")
