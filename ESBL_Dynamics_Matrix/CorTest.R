library(Hmisc)
dat <- read.csv('0-12M_gene-Month_correlation.csv')
corr_list <- list()
for (i in 2:nrow(dat)){
  r <- cor.test(unlist(dat[1,],use.names = F),
                unlist(dat[i,],use.names = F),
                method='pearson')
  corr_list[[paste("Genes 1 &", i)]] <- c(r$estimate, p.val=r$p.value)
}
length(corr_list)
write.table(corr_list, "cor_out.txt", sep="\t")

