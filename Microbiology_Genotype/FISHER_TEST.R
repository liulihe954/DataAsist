############################################################################################################################
###                                            Chi square and fisher test for independence                    ###############
############################################################################################################################
###           group 1 and group 2, n and m samples, respectively
###           Chi-square and Yates correction and simulate p.value
###          to check if the distributions are independent or not

###   pkg prep  # if necessary 
install.packages("MASS")  
install.packages("gdata")
library(MASS)
library(gdata)
############################################################################################################################
###                                            function structure                                            ###############
############################################################################################################################
### we write a function to achieve
### we use for loop for testing multiple times
### structure demo
#     for (i in c(1 : nrow(df))){  # df is the data set you have (snaps in rows and sample/names in column)
      #  1.extract every single row and format the data
      #  2.run the test and get the p-value
      #  3. put values in a containter
      #  4. put threshold and screen for significant snaps
#     }
############################################################################################################################
###                                            example dataset                                               ###############
############################################################################################################################
setwd("/Users/liulihe95/Desktop/Personal/LinTeng_counts") # you should set to your directory where your dataset lives!
dataset = read.xls("test_dataset.xlsx",sheet = 1, method=c("tab"), header= TRUE) # this is for .xlsx; method for use depends on what type of dataset you have
dataset1 = dataset[c(1:50),c(10:ncol(dataset))] # get a sub-dataset to run; here we use 50 rows (expect to get 50 pairwise testing and 50 pvalues)

Chitest4indepd = function(dataset1,n1,n2,thres){
  all_pvalue = as.numeric() # we set a blank container for all the pvalues --- "box1"
  all_contingency = list() # we set a blank list for all the contigency table --- "box2"
  all_snaps = rownames(dataset1) # get all snp info (sequence same as raw dataset)
  signiciant = numeric(nrow(dataset1))
  for (i in c(1:nrow(dataset1))){ 
    #  1.extract every single row and format the data
    col1 = as.vector(t(dataset1[i,c(1:(ncol(dataset1)))])) # extract one row each time and one by one
    col2 = c(rep("g1",n1),rep("g2",n2)) # put lables one for further contigency table making
    ctg_table = xtabs(~ group + type, data.frame(group = col2,type = col1)) # make contigency table for further chi test
    #  2.chi test - run the test and get the p-value
    chi_test = chisq.test(ctg_table,correct = TRUE, simulate.p.value = TRUE) # yates correction and simulate
    #  3.put values in a containter
    all_pvalue[i] = chi_test$p.value # put every single p value in the "box1"
    if (chi_test$p.value <= thres) {signiciant[i] = 1}
    all_contingency[[i]] = ctg_table # put every single contigency table in the "box2"
    names(all_contingency)[i] = all_snaps[i] # give names to the contigency table
    # 4. put threshold and screen for significant snaps
    sig_index = which(all_pvalue <= thres) # use threshold to check
  }
  print(paste("nice!",length(all_pvalue),"p value generated from",nrow(dataset1),"snps tests"))
  # compile some stuff for output
  all_pvalue = data.frame(all_pvalue);rownames(all_pvalue) = all_snaps
  sig_pvalue = data.frame(pvalue = all_pvalue[sig_index,]);rownames(sig_pvalue) = all_snaps[sig_index]
  dataset_out = cbind(dataset1, pvalue = all_pvalue, significant = signiciant)
  write.csv(dataset_out,"chi_pvalue.txt",quote = F)
  print("output file is ready: two column for significance")
  Results = list(Contigency_table = all_contingency, P_value = all_pvalue, signif_snp_index = sig_index, signif_snp_and_p = sig_pvalue)
  # output
  return(Results)
  
}
Fisher4indepd = function(dataset1,n1,n2,thres){
  all_pvalue = as.numeric() # we set a blank container for all the pvalues --- "box1"
  all_contingency = list() # we set a blank list for all the contigency table --- "box2"
  all_snaps = rownames(dataset1) # get all snp info (sequence same as raw dataset)
  signiciant = numeric(nrow(dataset1))
  for (i in c(1:nrow(dataset1))){ 
    #  1.extract every single row and format the data
    col1 = as.vector(t(dataset1[i,c(1:(ncol(dataset1)))])) # extract one row each time and one by one
    col2 = c(rep("g1",n1),rep("g2",n2)) # put lables one for further contigency table making
    ctg_table = xtabs(~ group + type, data.frame(group = col2,type = col1)) # make contigency table for further chi test
    #  2.chi test - run the test and get the p-value
    fisher_test = fisher.test(ctg_table) # fisher exact test (no argument)
    #  3.put values in a containter
    all_pvalue[i] = fisher_test$p.value # put every single p value in the "box1"
    if (fisher_test$p.value <= thres) {signiciant[i] = 1}
    all_contingency[[i]] = ctg_table # put every single contigency table in the "box2"
    names(all_contingency)[i] = all_snaps[i] # give names to the contigency table
    # 4. put threshold and screen for significant snaps
    sig_index = which(all_pvalue <= thres) # use threshold to check
  }
  print(paste("nice!",length(all_pvalue),"p value generated from",nrow(dataset1),"snps tests"))
  # compile some stuff for output
  all_pvalue = data.frame(all_pvalue);rownames(all_pvalue) = all_snaps
  sig_pvalue = data.frame(pvalue = all_pvalue[sig_index,]);rownames(sig_pvalue) = all_snaps[sig_index]
  dataset_out = cbind(dataset1, pvalue = all_pvalue, significant = signiciant)
  write.csv(dataset_out,"fisher_pvalue.txt",quote = F)
  print("output file is ready: two column for significance")
  Results = list(Contigency_table = all_contingency, P_value = all_pvalue, signif_snp_index = sig_index, signif_snp_and_p = sig_pvalue)
  # output
  return(Results)
}

test_results_chi = Chitest4indepd(dataset1,77,78,0.5)
test_results_fisher = Fisher4indepd(dataset1,77,78,0.5)

# write the output to files (.txt)
# dput(test_results$Contigency_table, file = "all_contigency_table.txt") 
# write.csv(test_results$P_value,"all_pvalues.txt",quote = F)
# write.csv(test_results$signif_snp_and_p ,"all_significant_snp_and_pvalue.txt",quote = F)

