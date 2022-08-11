library(r4ss)

for (re in 80:100) {
  dir <- paste0("C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/Loop/test_25_cw/",re,"/")
  setwd(dir)
  
  Flag <- 0
  save(Flag,file="Flag.RData")
  
  skip_to_next <- FALSE
  
  tryCatch({
    
    print(paste0("******************************* Replicate: ",re," ******************************************"))
    
    print("run the model with comp weight = 1")
    File <- readLines(paste0(dir,"YFT_IO_raw.ctl"), warn = F) # read template control file
    writeLines(File, paste0(dir,"YFT_IO.ctl")) # use the template in the first run
    
    command <- paste("cd", dir, "& go_nohess.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    
    print("use Francis weight")
    Rep = SS_output(dir=dir,ncols=400,covar=F,verbose = FALSE, printstats = FALSE)
    
    Francis_length <- rep(1,16)
    for(i in c(1:13,16)) {
      weight <- SSMethod.TA1.8(fit = Rep, type = "len", fleet = i, plotit = FALSE)
      Francis_length[i] <- round(weight[1],2)
    }
    
    weight <- SSMethod.TA1.8(fit = Rep, type = "size", fleet = 17, plotit = FALSE)
    Francis_size <- round(weight[1],2)
    
    # print("update weight")
    File <- readLines(paste0(dir,"control.ss_new"), warn = F)
    
    Line <- match("1 #_Variance_adjustments_to_input_values", File)
    
    File[Line+5] = paste0(gsub(", "," ",toString(c(Francis_length,1)))," #_mult_by_lencomp_N") # Francis weight
    
    Line <- match(" 11 1 1 0 1", File)
    File[Line-4] <- "2"
    File[Line+1] <- paste0("6 17 1 ",toString(Francis_size), " 1")
    
    writeLines(File, paste0(dir,"YFT_IO.ctl"))
    
    File <- readLines(paste0(dir,"starter.ss"), warn = F)
    File[3] <- 1 # use par file
    writeLines(File, paste0(dir,"starter.ss"))
    
    command <- paste("cd", dir, "& go.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    
    print("recruitment bias adjustment")
    Rep = SS_output(dir=dir,ncols=400,covar=T,verbose = FALSE, printstats = FALSE)
    
    result <- SS_fitbiasramp(replist =  Rep)
    
    # fix a bug in r4ss
    File <- readLines(paste0(dir,"YFT_IO.ctl"), warn = F)
    
    Line <- match(" 1 #_lambda for Fcast_recr_like occurring before endyr+1", File)
    
    File[Line+1] = result$df[1,1]
    File[Line+2] = result$df[2,1]
    File[Line+3] = result$df[3,1]
    File[Line+4] = result$df[4,1]
    File[Line+5] = result$df[5,1]
    
    writeLines(File, paste0(dir,"YFT_IO.ctl"))
    
    command <- paste("cd", dir, "& go.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    
  }, error = function(e) {skip_to_next <<- TRUE})
  if(skip_to_next) {next}
  
  # plor results
  # Rep = SS_output(dir=dir,ncols=400,covar=T)
  # SS_plots(replist=Rep, forecastplot=F, uncertainty=T, datplot=T, btarg=0, minbthresh=0)
  
  Flag <- 1
  save(Flag,file="Flag.RData")
  print("Converged!!!")
}
