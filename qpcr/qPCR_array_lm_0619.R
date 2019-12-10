# pre analysis
setwd("/Users/liulihe95/Desktop/DataAsist/qpcr/")
library(openxlsx)
library(lme4)
library(tidyverse)
library(faraway)
data.raw = read.xlsx("2018_copy_2.xlsx",sep = ",")
data.raw$Sample = as.factor(data.raw$Sample)
data.raw$Treatment = as.factor(data.raw$Treatment)
data.raw$Treatment  <- relevel(data.raw$Treatment , ref = names(table(data.raw$Treatment)[3]))

# funcion ready
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# get gene index 
all_gene = names(table(data.raw$Gene))

# loop
all_p = numeric()
sig_out = list()
count = 0
for (i in seq_along(all_gene)){
  subset = dplyr::filter(data.raw, Gene == all_gene[i]) # extract gene 
  sub_model = lm(Cq_mean ~ Treatment,subset) # run individual model
  all_p[i] = lmp(sub_model) # put the p value to the container
  if (all_p[i] <= 0.05) { # suppose we find one sig model!
    count = count + 1 # stepper plus 1
    sig_out[[count]] = TukeyHSD(aov(Cq_mean ~ Treatment,subset), conf.level=.99) # run the contrast
    names(sig_out)[count] = all_gene[i] # assign names to the output for easier reading
    message("found one ", all_gene[i]) #promt
  }
}

# get all sig genes name
all_gene[which(all_p < 0.05)]




