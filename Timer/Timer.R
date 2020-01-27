#' A function puts system to sleep for the pre-set time duration.
#'
#' @param set The amount of time you need to re-wake R, the unit is 'minutes'. default 5 min.
#' @param Infinite logical, default False; if Ture, a infinite timer will be trigered, with control + c (in mac) as a way to terminate.
#' @param verbose logical, default False; if Ture, a message will pop up every minute as a reminder of the amount of time left.

Timer = function(set = 5,Infinite = F,verbose = F){
  # run
  tStart = as.POSIXct(Sys.time())
  p1 <- proc.time()
  if (Infinite){
    message('You just set a infinite loop, please use control+c to exit.')
    repeat {Sys.sleep(1)}
  } else {
    message('A timer of ',set,' minutes was set at ',Sys.time(),', please use control+c to exit.')
    for (i in seq_len(set)){
      Sys.sleep(60)
      if (verbose) message(set-i,' mins left.')
    }
    message('time out')
  }
}
### test ###
test = 'Timer(3,Infinite = F,verbose = T)'

### to use ###
# library(RCurl)
# script <- getURL("https://raw.githubusercontent.com/liulihe954/DataAssist/master/Timer/Timer.R", ssl.verifypeer = FALSE)
# eval(parse(text = script))
