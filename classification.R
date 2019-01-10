rm(list = ls())

# Bibliotecas -------------------------------------------------------------

pkgs <- c("dplyr", "tidyr", "class", "caret", "plyr", "ggfortify", "factoextra")
sapply(pkgs, require, character.only = T)

# Diretório ---------------------------------------------------------------

wd1 <-"/home/andrefbm/Dropbox/Observatório 2018/Dados"
wd2 <- "/home/andrefbm/Dropbox/Observatório 2018/Apresentação/Figuras"
# wd1 <-"C:/Users/User/Dropbox/Observatório 2018/Dados"
# wd2 <-"C:/Users/User/Dropbox/Observatório 2018/Apresentação/Figuras"
setwd(wd2)

FF     <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}

# Leitura e organização dos dados -----------------------------------------
dados.rmm <- read.table(paste0(wd1, "/rmm-cluster.csv"), sep = ",", header = T)
dados.rml <- read.table(paste0(wd1, "/dados_cluster_RML.csv"), sep = ';', header = T)
dados.rmm$cluster <- factor(dados.rmm$cluster)
dados.rml$apond   <- factor(dados.rml$apond)

# Classficação via KNN ----------------------------------------------------

ks <- 2:12

deterk <- ldply(lapply(1:length(ks), function(i)
{
  set.seed(1212)
  amostras     <- sample(1:nrow(dados.rmm), size = ceiling(0.7 * nrow(dados.rmm)), replace = F)
  dados.treino <- dados.rmm[amostras, ]
  dados.teste  <- dados.rmm[-amostras, ]
  pred.knn     <- knn(train = dados.treino[, -c(1, 15, 16, 17)], test = dados.teste[, -c(1, 15, 16, 17)], 
                      cl = dados.treino$cluster, 
                      k = ks[i])
  df  <- data.frame(obs = dados.teste$cluster, pred = pred.knn)
  med <- defaultSummary(data = df, lev = levels(df$obs))
  return(med)
}))


medidas <- c('Acurácia', 'Kappa')

par(mar = c(3.2, 3.2, 0.8, 0.8), mfrow = c(1, 2), cex = 1.6)  
for(j in 1:ncol(deterk))
{
  y      <- deterk[, j]
  maximo <- ks[which.max(y)]
  # pdf(file = paste0(medidas[j], '.pdf'), width = 9)
  plot(ks, y, type = 'o', pch = 19, xaxt = 'n', xlab = '', ylab = '', yaxt = 'n', cex = 0.9)
  points(x = maximo, y = max(y), pch = 19, cex = 0.9, col = 'red')
  R <- range(y)
  axis(side = 1, ks)
  axis(side = 2, seq(R[1], R[2], l = 5), FF(seq(R[1], R[2], l = 5), 2))
  mtext(medidas[j], side = 2, line = 2.1, cex = 1.6)
  mtext('k', side = 1, line = 2.1, cex = 1.6)
  abline(h = seq(R[1], R[2], l = 5), v = ks, col = "lightgray", lty = "dotted")
  # graphics.off()
}

mod.k4            <- knn(train = dados.rmm[, -c(1, 15, 16, 17)], test = dados.rml[, -14],
                         cl = dados.rmm$cluster, k = 4)
dados.rml$cluster <- mod.k4


# Gráfico dos componentes principais --------------------------------------
# res.pca <- princomp(dados.rml[, - c(14, 15)])
# autoplot(princomp(dados.rml[, - c(14, 15)]), data = dados.rml, colour = 'cluster', size = 3)
fviz_cluster(list(data = dados.rml[, - c(14, 15)], cluster = mod.k4), palette = paleta, ellipse.type = "convex", 
             repel = TRUE, show.clust.cent = FALSE, ggtheme = theme_minimal(), main = "") +
  theme(text = element_text(size = 14)) + labs(col = "Cluster", shape = "Cluster", fill = "Cluster")
ggsave(filename = "princomp-rml.pdf", width = 12, height = 7)

# Gráfico das medidas descritivas -----------------------------------------
paleta <- c("#FF7F00", "#377EB8", "#E41A1C",  "#4DAF4A")

df <- dados.rml %>% gather(key = "var", value = "valor", -c(apond, cluster))
df %>% ggplot(aes(x = cluster, y = valor)) + 
  geom_point(aes(col = factor(cluster)), size = 2) + 
  stat_summary(aes(group = 1), fun.y = median, geom="point", size = 2.2, shape = 1, stroke= 1.2) + 
  facet_wrap( ~ factor(var), scales = "free") +
  labs(x = "", y = "", col = "Cluster") +
  scale_color_manual(values = paleta) +
  theme(text = element_text(size = 14), panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(), axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), legend.position = "top")
ggsave(filename = "descr-rml.pdf", width = 12, height = 7)


