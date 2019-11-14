####====================================####
#==    Pivital rotation function        ===#
#==        ---using tidyverse         ===#
#####===============================######
Pivot_Rotate = function(test, # Input data, must have the same structure as you have
                        safe = "/", # to differentiate "mm/dd/yy"
                        # Next, provide the keyword of your output (e.g. "Farm_1_rotate")
                        # suffix will append by itself
                        # then you will get "Farm_1_rotate.txt" or "Farm_1_rotate.RData", 
                        #depends on argument "output_type"
                        keyword = "", 
                        output_type = "txt"){# can be ".txt" or ".RData"
  library(tidyverse)
  All_names = names(sort(table(test[,1]))) #Obtain all names to dertermin all possible header names
  detect_header_index = str_extract(All_names, "/")# Use "/" to detect: differentiate "mm/dd/yy" from c("Date","Day",..), etc
  seperate_name = All_names[is.na(detect_header_index)] # Obtain values for exclusion
  
  #Next step - get header_index using matching, the famous %in%, this much safer than count by HANDS!!!
  header_index = which(test[,1] %in% seperate_name)
  # create a container for output
  final = data.frame(Date = character(),
                     Minutes = character(),
                     Animal = character(),
                     Action = numeric())
  # loop along all the header_row and parse the section(from head to tail)
  for (i in seq_along(header_index)){
    # We are using one bin point (i) and the next bin point (i+1) at the same time
    # HOWEVER, this only works when (i < total bin points)
    # because when (i == total), (i+1) will be out of bound!
    #
    # Situation 1: when ( i < total )
    if ( i < length(header_index)){ 
      target = header_index[i] # aim at the sepcific header row
      header_row_tmp = as.vector(test[target,-c(1:2)]) # take the actually cow id in the headerrow as the column names of the following sub-dataset (time section)
      header_row = as.character(header_row_tmp[!is.na(header_row_tmp)]) # remove NAs
      section_data = test[c((target + 1):(header_index[i+1]-1)),c(1:(length(header_row)+2))] # select section data using two bins (like head and tail)
    } else { # while (i == total), we use the total number of rows as the "tail"
      section_data = test[c((target + 1):nrow(test)),c(1:(length(header_row)+2))]
    }
    colnames(section_data) = c("Date","Minutes",header_row) 
    # perform roate using external function
    rotate_tpm <- section_data %>% 
      pivot_longer(header_row, names_to = "Animal", values_to = "Action") %>% 
      drop_na() # remove na, just in case
    # potentially could customize column names when needed. uncomment these and renamme
    # %>% rename(New_name_1 =Date,
    #            New_name_1 = Minutes,
    #            New_name_1= animal,
    #            New_name_1 = action)
    final = rbind(final,rotate_tpm)
    message("Now working on day ", i,"/",length(header_index), "---",section_data[1,1])
  }
  if (output_typ == "txt"){
    write.table(final,file = paste(keyword,".txt",sep = ""),
                quote = F,row.names=FALSE)
  } else {save(final, file = paste(keyword,".RData",sep = ""))}
}
####==================####
#==     Farm1        ===#
#####===============####
Farm_1_raw = read.table("Victoria.txt",header = F, sep = "\t")
Pivot_Rotate(test = Farm_1_raw,
             safe = "/",
             keyword = "Farm_1_Rotated")

### For testing
#library(tidyverse)
#load("Farm_1_Rotated.RData")
#final %>% dplyr::filter(Date == "7/1/2018",
#                        Minutes == "0:06") %>%  print()
####==================####
#==     Farm2        ===#
###==================### 
Together_raw = read.table("Together_Jacob.txt",header = F, sep = "\t")
Pivot_Rotate(test = Together_raw,
             safe = "/",
             keyword = "Together_Jacob_Rotated")
###################

