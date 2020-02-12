# get all dates
dates <- base::as.Date(c("2007-06-22", "2004-02-13"))
# massage all dates
dates_new <- mydates[1] - 21
# re-arrange and re-assign
dataset$dates = c(dates_new[2:length(dates_new)],"0")
# get index for 0
group_info = table(dataset$ID);attributes(group_info) = NULL
for (i in seq_along(group_info)){
  sub_index = sum(group_info[1:i])
  dataset$dates[sub_index] = "0"
}
