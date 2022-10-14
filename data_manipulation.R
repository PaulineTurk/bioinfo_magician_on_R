# script to manipulate gene data
# setwd("~/Documents/Bioinfo/bioinformagician/R_scripts/bioinfo_magician_on_R")


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GEOquery")

# load libraries
library(dplyr)
library(tidyverse)
library(GEOquery)


# read in the data
data = read.csv(file = "~/Documents/Bioinfo/bioinformagician/bioinfo_101/GSE183947_fpkm.csv")
dim(data)

# get metadata using GEOquery
GSE = getGEO(GEO = "GSE183947", GSEMatrix = TRUE)
metadata = pData(phenoData(GSE [[1]]))


matadata.modified = metadata %>%
  select(1, 10, 11, 17) %>% # number of columns wanted to select
  rename(tissue = characteristics_ch1) %>%   # rename "characteristics_ch1" by "tissue"
  rename(metastasis = characteristics_ch1.1) %>%  # rename "characteristics_ch1.1" by "metastasis"
  mutate(tissue = gsub("tissue: ", "", tissue)) %>%  # globally substitute in the column tissue the 1st str into the second in the same column tissue
  mutate(metastasis = gsub("metastasis: ", "", metastasis))

# reshaping data
## it is easier to work with the long format (for exemple, to add the metadata)
data.long = data %>%
  rename(gene = X) %>%   # rename column X as gene
  gather(key = 'samples', value = 'FPKM', -gene)  # like pivot the table to create on row per info (per sample)


# join dataframes = data.long + metadata.modified
data.long = data.long%>%
  left_join(., matadata.modified, c("samples" = "description"))  # . == data.long  | matching values in columns samples and description


# explore data
data.long %>%
  filter(gene == 'BRCA1' | gene == 'BRCA2') %>%   # get info only from 'BRCA1' and 'BRCA2' genes
  group_by(gene, tissue) %>%   # create two groups
  summarise(mean_FPKM = mean(FPKM),           # create a column for the mean on FPKM info for each group
            median_FPKM = median(FPKM)) %>%   # create a column for the median on FPKM info for each group
  arrange(mean_FPKM)                         # arrange by increasing order the column selected, by default
# arrange(-mean_FPKM)                         # arrange by decreasing order the column selected




