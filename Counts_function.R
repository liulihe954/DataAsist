#########################################################################
############################      Mini instructions #####################
# A. structure of the dataset --- columns sample names and rows are every loci, column names are the sample names, row names just natural numbers (no more else!!!)
# B. pairwise smaples with same nucleotide any loci will be counted, others are considered as "different".
# C. The function complosed can do selection of any pair you wanna look at

#######################################################################
############################      prep    #############################
###    grab file
library(gdata)
setwd("/Users/liulihe95/Desktop/LinTeng_counts") # you should set to your directory where your dataset lives!
###    get dataset - here we just take 200 rows from the begining
dataset = read.xls("test_dataset.xlsx",sheet = 1, method=c("tab"), header= TRUE) # this is for excel; method depens on what type of dataset you have
dataset1 = dataset[c(1:100),c(10:ncol(dataset))] # get a sub-dataset to run
###   know the structures of dataset
dim(dataset1) ## get dimensions; first number is the number of "loci", second number is the number of "sample"
sample_names = names(dataset1) # store - every single sample name in the dataset

### just a test for data structure
# test_index = dataset[,3] - dataset[,5]
# test_table = data.frame(table(dataset[,3] - dataset[,5]))
######################################################################
############################      function    ########################
DoComparison = function(dataset, sample1, sample2){ ## here we need three inputs - the whole dataset and two sample index for comparison. e.g. compre number 100 and 101
  if (sample1 <= ncol(dataset) & sample2 <= ncol(dataset)){ ## just make sure the two numbers do not exceed the boundary (number of all samples)
    ## set up name index and "container matrix" -- the dimensions are the number of sample (n*n), the entries are the pairwise comparisons
    name_index = names(dataset);length(name_index)
    diff_matrix = matrix(0,length(name_index),length(name_index)); rownames(diff_matrix) = name_index; colnames(diff_matrix) = name_index
    same_matrix = matrix(0,length(name_index),length(name_index)); rownames(same_matrix) = name_index; colnames(same_matrix) = name_index
    loci_sum = nrow(dataset); loci_sum ## all loci - for subtraction calculation
    ## use loop to do one by one check - need two pedometers i and j, note that, j depends on i, because just need the upper triangle (matrices are symmetrical)
    for (i in c(1:(ncol(dataset)-1))){
      for (j in c((i+1):ncol(dataset))){
        diff_index = data.frame(table(dataset[,i] - dataset[,j])) # substruction and formating
        same_sum = diff_index[which(diff_index[,1]=="0"),2] # select the "same"
        diff_sum = loci_sum - same_sum # subtract to get the "diff"
        diff_matrix[i,j] = diff_matrix[j,i] = diff_sum # put in the entries
        same_matrix[i,j] = same_matrix[j,i] = same_sum # put again, nice!
      }
  }
} else{ cat("Shit! At least one sample size dimension if out of the boundary (sample size):", ncol(dataset))} ## alert, just in case 
  ## return the results we need -- the difference matrix and the same matrix, and the selection of specific one
  diff_matrix = data.frame(diff_matrix)
  same_matrix = data.frame(same_matrix)
  Comparison = list(same = same_matrix[sample1,sample2],
                    difference = diff_matrix[sample1,sample2])
  compare_promt = paste(name_index[sample1],"--and--",name_index[sample2])
  return(list(Comparison = list(compare_promt ,Comparison), differential_matrix = diff_matrix, indential_matrix = same_matrix)) ## store the counts results
  
  ### Bing!!! ###
  }

### testing using a 100 rows subdataset
# Results1 = DoComparison(dataset1,19,124) 
# Results1$Comparison
