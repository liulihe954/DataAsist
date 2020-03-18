setwd('/Users/liulihe95/Desktop/DataAsist/RFI_Mariana/')
library(readxl)
library(tidyverse)

# DATA PRE
RFIdata_35_raw = read_xlsx('Stats30.xlsx',1) %>%
  dplyr::select(-CalvDate,-DIM) %>%
  dplyr::filter(ECMDay != '.',
                DMI != '.',
                BW != '.',
                MBW != '.') %>%
  mutate_at(vars(ECMDay,DMI,BW,MBW),as.numeric) %>%
  #mutate_at(vars(TRT), as.character) %>% 
  arrange(Cow)
#
RFIdata_100_raw = read_xlsx('Stats30.xlsx',2) %>%
  dplyr::select(-CalvDate,-DIM) %>%
  dplyr::filter(ECM != '.',
                DMI != '.',
                BW != '.',
                MBW != '.') %>%
  mutate_at(vars(ECM,DMI,BW,MBW),as.numeric) %>%
  mutate_at(vars(TRT), as.character) %>% 
  arrange(Cow)

# prob wrong

# FIT MOD
animal_index = unique(RFIdata_35_raw$Cow)
residual_all = data.frame(Animal = c(),
                          Residual = c())

for (i in seq_along(animal_index)){
  tmp = RFIdata_35_raw %>% 
    dplyr::filter(Cow == animal_index[i])
  #lmod = lm(DMI ~  TRT + ECMDay + BW + MBW, data = tmp)
  mean_rid = mean(residuals(lmod))
  residual_all[i,1] = animal_index[i]
  residual_all[i,2] = mean_rid
}




summary(RFIdata_35_raw$TRT)
str(tmp)

str(RFIdata_35_raw)
