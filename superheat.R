library("RColorBrewer")
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(df.scaled, scale = "none", col = col)


# install devtools if you don't have it already
install.packages("devtools")
# install the development version of superheat
devtools::install_github("rlbarter/superheat")
library(superheat)

df.minmax <- apply(dados2[, -c(1:3)], 2, function(x)(x-min(x))/(max(x)-min(x)))

png("superheat.png", height = 1100, width = 950, res = 150)
superheat(df.minmax, 
          # place dendrograms on columns and rows 
          row.dendrogram = T, 
          # make gridlines white for enhanced prettiness
          grid.hline.col = "white",
          grid.vline.col = "white",
          # rotate bottom label text
          bottom.label.text.angle = 45, 
          left.label.text.size = 2,
          bottom.label.text.size = 3,
          left.label.size = 0.3,
          bottom.label.size = 0.15,
          grid.hline.size = 2,
          grid.vline.size = 1)
dev.off()
