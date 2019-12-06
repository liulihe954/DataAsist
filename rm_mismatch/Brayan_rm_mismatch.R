mismatch_rm = function(data1,data2,target1,target2){
  rm_count = 0
  final_index = rep(F,nrow(data2))
  for (i in seq_along(data1[,names(data1)== target1])){
    index1 = data1[,names(data1)== target1]
    index2 = data2[,names(data2)== target2]
    if (index2[i+rm_count] == index1[i]){final_index[i+rm_count] = T}
    if (!index2[i+rm_count] == index1[i]){
      final_index[i+rm_count+1] = T
      rm_count = rm_count + 1
    }
  }
  #results = 
  return(list(count = rm_count,
              final_index = final_index))
}

data1 = data.frame(col1 = c(1:50))
data2 = data.frame(col2 = c(1:5,1,6:12,1,13:19,1,20:25,1,26:30,1,31:40,1,41:47,1,48:50))
test = mismatch_rm(data1,data2,"col1","col2")
out = data2[test$final_index,]

table(data2$col2)

index1 = data1[,names(data1)== "col1"]
index2 = data2[,names(data2)== "col2"]
index2[i+rm_count] == index1[i]


try = mismatch_rm(l,d,"CCS","CCS")

