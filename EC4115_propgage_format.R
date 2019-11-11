test = read.csv("EC4115_prophage.txt",skip = 6,header = F,sep = "\t")
library(tidyverse)
final = data.frame(CDS_POSITION = character(),
                   BLAST_HIT = character(),
                   EVALUE = character(),
                   prophage_PRO_SEQ = character())
for (i in c(1:nrow(test))){
  teststr = unlist(strsplit(as.character(test[i,1]),"    "))
  teststr = teststr[which(teststr != "")] %>% 
    trimws(which = c("both"), whitespace = "[ \t\r\n]")
  if (length(teststr) == 4){
    teststr = data.frame(t(teststr));names(teststr) = names(final)
    rownames(teststr) = i
    final = rbind(final,teststr)
    message("good")
  } else {
    message("not good at ", i, "th row")
  }
}
write.csv(final,file = "EC4115_prophage_formatted.csv")
