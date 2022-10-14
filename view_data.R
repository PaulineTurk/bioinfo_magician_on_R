# script to visualize gene expression data (GSE183947)
# setwd("~/Documents/Bioinfo/bioinformagician/R_scripts/bioinfo_magician_on_R")


# load libraries
library(tidyverse)
library(ggplot2)

data.long%>%
  head()


# 1.bar plot
#data.long$metastasis = as.factor(data.long$metastasis)
data.long$tissue = as.factor(data.long$tissue)

bar = data.long %>%
  filter(gene == 'BRCA1') %>%
  ggplot(., aes(x = samples, y = FPKM, fill = tissue)) +   # fill according to a column
  geom_col()

ggsave(bar, filename = "view_graph/bar.png", width = 10, height = 8)


# 2. density
density = data.long %>%
  filter(gene == 'BRCA1') %>%
  ggplot(., aes(x = FPKM, fill = tissue)) +  
  geom_density(alpha = 0.7)  # alpha for transparency

ggsave(density, filename = "view_graph/density.png", width = 10, height = 8)


# 3. boxplot
boxplot = data.long %>%
  filter(gene == 'BRCA1') %>%
  ggplot(., aes(x = metastasis, y = FPKM)) +
  # geom_boxplot()
  geom_violin()

ggsave(boxplot, filename = "view_graph/boxplot.png", width = 10, height = 8)


# 4. scatter plot
scatter_plot = data.long %>%
  filter(gene == "BRCA1" | gene == "BRCA2") %>%
  spread(key = gene, value = FPKM) %>%
  ggplot(., aes(x = BRCA1, y = BRCA2, color = tissue)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) # se: standard error (confidence interval)

ggsave(scatter_plot, filename = "view_graph/scatter_plot.png", width = 10, height = 8)


# 5. heatmap
genes.of.interest = c("BRCA1", "BRCA1", "TP53", "ALK", "MYCN")

heatmap.graph = data.long %>%
  filter(gene %in% genes.of.interest ) %>%
  ggplot(., aes(x = samples, y = gene, fill = FPKM)) +  # fill for the values in heatmap
  geom_tile()  + # for heatmap
  scale_fill_gradient(low = 'white', high = 'red')  # scale the color from white to red

ggsave(heatmap.graph, filename = 'view_graph/heatmap.png', width = 10, height = 8) # by default in inches













