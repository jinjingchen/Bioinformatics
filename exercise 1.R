library(dplyr)
library(tidyverse)
library(GEOquery)

data <- read.csv(file = "C:/Users/Charlotte/Downloads/GSE183947_fpkm.csv")

# get meta data
gse <- getGEO(GEO = 'GSE183947', GSEMatrix = TRUE)

gse

metadata <- pData(phenoData(gse[[1]]))
#head(metadata)


# metadata.subset <- select(metadata, c(1, 10, 11, 17))
metadata.modified <- metadata %>%
  select(1,10,11,17) %>%
  rename(tissue = characteristics_ch1)%>%       # rename columns
  rename(metastasis = characteristics_ch1.1)%>%
  mutate(tissue = gsub("tissue: ","", tissue))%>% #remove prefix "tissue"
  mutate(tissue = gsub("metastasis: ","", metastasis))

# reshaping data
data.long <- data %>%
  rename(gene = X)%>%
  gather(key = 'samples', value = 'FPKM', -gene)

#join dataframes = dat.long + metadata.modified
data.long <- data.long %>%
  left_join(.,metadata.modified, by = c("samples" = "description"))

#explore data
data.long %>%
  filter(gene == 'BRCA1'|gene == 'BRCA2') %>%
  group_by(gene, tissue) %>%
  summarize(mean_FPKM = mean(FPKM)) %>%
  head()

  
