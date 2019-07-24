data_tl = read.csv("Strains_phenotype_TEST.csv",header = F)
character_transfer_demo = as.factor(data_tl$V2)
names(character_transfer_demo) = data_tl$V1
str(character_transfer_demo)
character_transfer_demo