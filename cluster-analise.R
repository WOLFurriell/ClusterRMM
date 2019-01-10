rm(list = ls())

# Bibliotecas -------------------------------------------------------------

library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(readxl)
library(NbClust)
library(clustertend)
library(dendextend)

paleta <- c( "#FF7F00", "#984EA3", "#377EB8", "#E41A1C",  "#4DAF4A")


# Diretório ---------------------------------------------------------------

wd1 <-"/home/andrefbm/Dropbox/Observatório 2018/Dados"
wd2 <-"/home/andrefbm/Dropbox/Observatório 2018/Apresentação/Figuras"
wd1 <-"C:/Users/User/Dropbox/Observatório 2018/Dados"
wd2 <-"C:/Users/User/Dropbox/Observatório 2018/Apresentação/Figuras"
setwd(wd2)


# Leitura e organização dos dados -----------------------------------------
dados           <- read.table(paste0(wd1, "/dados-cluster.csv"), sep = ",", header = T)
cuts            <- data.frame(read_xlsx(path = paste0(wd1,"/Nomes APONDs_2010.xlsx"), sheet = "Apond_RMM_2010")[, c(4, 5, 6)])
names(cuts)     <- tolower(names(cuts))
dados$aponds    <- rownames(dados)
dados           <- merge(dados, cuts, by = c("aponds"))
rownames(dados) <- dados$cidade


# Padronizando as variáveis -----------------------------------------------
df.scaled <- scale(dados[, -c(1, 15, 16)])
colMeans(df.scaled)
apply(df.scaled, 2, sd)


# Calculando a matriz de distancias ---------------------------------------
dist.eucl <- dist(df.scaled, method = "euclidean")


# Avaliando se existem possíveis cluster ----------------------------------
fviz_pca_ind(prcomp(df.scaled), title = "PCA", palette = "jco",
             geom = "point", ggtheme = theme_classic(), legend = "bottom")

hopkins(df.scaled, n = nrow(df.scaled) - 1) 

pdist <- fviz_dist(dist.eucl)
pdist <- pdist + theme(text = element_text(size = 14), legend.position = "top", 
                       legend.key.width = unit(1.5, "cm")) + labs(fill = "Dissimilaridade")
ggsave(filename = "distance.pdf", plot = pdist, width = 16, height = 10)


# Criando cluster pelo método hierarquico ---------------------------------

# The height of the fusion, provided on the vertical axis, indicates the (dis)similarity/distance
# between two objects/clusters. The higher the height of the fusion, the less similar the
# objects are. This height is known as the cophenetic distance between the two objects.

hc.ward   <- hclust(d = dist.eucl, method = "ward.D2")
hc.avg    <- hclust(d = dist.eucl, method = "average")
coph.ward <- cophenetic(hc.ward)
coph.avg  <- cophenetic(hc.avg)

cor(dist.eucl, coph.ward)
cor(dist.eucl, coph.avg)

x11(); fviz_dend(hc.ward,  cex = 0.6)
x11(); fviz_dend(hc.avg, cex = 0.6)

# Determinando o numero ideal de clusters ---------------------------------
nbWard  <- NbClust(df.scaled, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2")
nbAvg   <- NbClust(df.scaled, distance = "euclidean", min.nc = 2, max.nc = 10, method = "average")
fviz_nbclust(nbWard) # k = 3 ou 5
fviz_nbclust(nbAvg)  # k = 3 ou 5

fviz_nbclust(df.scaled, kmeans, method = "wss") 
fviz_nbclust(df.scaled, kmeans, method = "silhouette") 
fviz_nbclust(df.scaled, kmeans, nstart = 25, method = "gap_stat", nboot = 500)

# Cut o dendograma em k grupos --------------------------------------------
grp.ward.3 <- cutree(hc.ward, k = 3)
grp.ward.5 <- cutree(hc.ward, k = 5)
grp.avg.3  <- cutree(hc.avg,  k = 3)
grp.avg.5  <- cutree(hc.avg,  k = 5)

# k = 3
fviz_dend(hc.ward, k = 3, cex = 0.9, palette = "jco", color_labels_by_k = TRUE, rect = TRUE, rect_fill = TRUE, 
          lwd = 1.2, type = "rectangle", rect_border = "jco", horiz = T)
fviz_dend(hc.avg, k = 3, cex = 0.9, palette = "jco", color_labels_by_k = TRUE, rect = TRUE, rect_fill = TRUE, 
          lwd = 1.2, type = "rectangle", rect_border = "jco", horiz = T)

# k = 5
pward.5 <- fviz_dend(hc.ward, k = 5, cex = 0.9, k_colors = paleta, color_labels_by_k = TRUE, rect = TRUE, 
                     rect_fill = TRUE, lwd = 1.2, type = "rectangle", rect_border = "Set1", horiz = T, 
                     main = "", ylab = "Altura")
ggsave(filename = "dendograma-ward.pdf", plot = pward.5, width = 16, height = 10)

fviz_dend(hc.avg, k = 5, cex = 0.9, palette = "jco", color_labels_by_k = TRUE, rect = TRUE, rect_fill = TRUE, 
          lwd = 1.2, type = "rectangle", rect_border = "jco", horiz = T)

p2 <- fviz_cluster(list(data = df.scaled, cluster = grp.ward.5), palette = paleta, ellipse.type = "convex", 
                   repel = TRUE, show.clust.cent = FALSE, ggtheme = theme_minimal(), main = "")
p2 <- p2 + theme(text = element_text(size = 14)) + labs(col = "Cluster", shape = "Cluster", fill = "Cluster")
ggsave(filename = "princomp-ward.pdf", plot = p2, width = 12, height = 7)



# Comparando dendogramas --------------------------------------------------
dend.ward <- as.dendrogram(hc.ward)
dend.avg  <- as.dendrogram(hc.avg)
dend.list <- dendlist(dend.ward, dend.avg)
entanglement(dend.list)
cor.dendlist(dend.list, method = "cophenetic")
cor.dendlist(dend.list, method = "baker")


# Incluindo os cluster nos dados e resumindo informação ------------------
dados.fim <- cbind(dados, cluster = grp.ward.5)
rownames(dados.fim) <- NULL


df <- dados.fim %>% gather(key = "var", value = "valor", -c(aponds, nomes, cidade, cluster))
df %>% ggplot(aes(x = cluster, y = valor)) + 
  geom_point(aes(col = factor(cluster)), size = 2) + 
  stat_summary(aes(group = 1), fun.y = median, geom="point", size = 2.2, shape = 1, stroke= 1.2) + 
  facet_wrap( ~ factor(var), scales = "free") +
  labs(x = "", y = "", col = "Cluster") +
  scale_color_manual(values = paleta) +
  theme(text = element_text(size = 14), panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(), axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), legend.position = "top") -> p3.vars
ggsave(filename = "vars-cluster.pdf", plot = p3.vars, width = 12, height = 7)

resumos <- aggregate(dados[, -c(1, 15, 16)], by = list(cluster = grp.ward.5), median)
resumos <- resumos %>% gather("var", "valor", - cluster)

resumos %>% ggplot(aes(x = cluster, y = valor)) +
  geom_point(aes(col = factor(cluster)), size = 2) + 
  facet_wrap( ~ factor(var), scales = "free") +
  labs(x = "Mediana", y = "", col = "Cluster") +
  scale_color_brewer(palette = "Set1") +
  theme(text = element_text(size = 14), panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(), axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), legend.position = "top") -> p3.medianas
ggsave(filename = "medianas-cluster.pdf", plot = p3.medianas, width = 12, height = 7)


# Exportando dados com os clusters ----------------------------------------
write.table(dados.fim, file = "rmm-cluster.csv", sep = ",", row.names = F, quote = F)

