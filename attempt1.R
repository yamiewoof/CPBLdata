rm(list=ls())
setwd("c:\\baseball")
data2019 <- read.csv("log_process\\logdata\\logdata_2019.csv",na.strings=c("","NA"))

library(dplyr)

firstgame <- data2019 %>% select(4:12,15,17,18) %>% slice(1:162)
firstgame$base1N <- NA
firstgame$base2N <- NA
firstgame$base3N <- NA
firstgame$outN <- NA

################
dummy_list <- list(
  inning = "NA",
  rem_type = "NA", base1 = "NA", base2 = "NA", base3 = "NA", 
  player = "NA",outs = "NA"
)
dummy_list$inning
################

for ( i in 1:nrow(firstgame))  {
  if(i==1){
    inning_now <- "一上"
    base1_now <- NA
    base2_now <- NA
    base3_now <- NA
    out_now <- 0
  }
  
  if (firstgame$inning[i] != inning_now){
    inning_now <- firstgame$inning[i]
    out_now <- 0
  } 
  else{
    if (grepl("一出局", firstgame$log[i])){
    out_now <- 1
    }
    
    else if (grepl("二出局", firstgame$log[i])){
    out_now <- 2
    }
    
    else if (grepl("三出局", firstgame$log[i])){
    out_now <- 3
    }
    
    firstgame$outN[i] <- out_now
    
  }

}

print(dummy_list)
 


