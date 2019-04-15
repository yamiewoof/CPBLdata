rm(list=ls())
setwd("c:\\baseball")
data2019 <- read.csv("log_process\\logdata\\logdata_2019.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)

library(dplyr)

firstgame <- data2019 %>% select(4:12,15,17,18) %>% slice(1:162)
firstgame$base1N <- NA
firstgame$base2N <- NA
firstgame$base3N <- NA
firstgame$basesit <- NA
firstgame$outN <- NA

hit1_type <- c("一壘安打", "左外野安打", "中外野安打", "右外野安打", 
               "穿越安打", "平飛安打", "內野安打", "中間方向安打", "德州安打", "滾地安打", "不死三振")
base1 <- c("佔一壘", "一壘有人", "上一壘")
base2 <- c("佔二壘", "二壘有人", "上二壘")
base3 <- c("佔三壘", "三壘有人", "上三壘")
base12 <- c("佔一二壘", "一二壘有人", "佔一、二壘", "一、二壘有人")
base13 <- c("佔一三壘", "一三壘有人", "佔一、三壘", "一、三壘有人")
base23 <- c("佔二三壘", "二三壘有人", "佔二、三壘", "二、三壘有人")
base123 <- c("滿壘")

allbases <-  c("佔一壘", "一壘有人", "上一壘","佔二壘", "二壘有人", "上二壘","佔三壘", "三壘有人", "上三壘","佔一二壘", "一二壘有人", "佔一、二壘", "一、二壘有人","佔一三壘", "一三壘有人", "佔一、三壘", "一、三壘有人","佔二三壘", "二三壘有人", "佔二、三壘", "二、三壘有人","滿壘")

onfirstbase <- c("一壘安打", "左外野安打", "中外野安打", "右外野安打", 
                 "穿越安打", "平飛安打", "內野安打", "中間方向安打", "德州安打", "滾地安打", "不死三振","保送")
onsecondbase <- c("二壘安打")
onthirdbase  <- c("三壘安打")

for ( i in 1:nrow(firstgame))  {
  if(i==1){
    inning_now <- "一上"
    base1_now <- NA
    base2_now <- NA
    base3_now <- NA
    firstgame$basesit[i] <- 0
    out_now <- 0
  }
  if(i>1){
    firstgame$base1N[i] <- firstgame$base1N[i-1]
    firstgame$base2N[i] <- firstgame$base2N[i-1]
    firstgame$base3N[i] <- firstgame$base3N[i-1]
  }
  if (firstgame$inning[i] != inning_now){
    inning_now <- firstgame$inning[i]
    out_now <- 0
    firstgame$base1N[i] <- NA
    firstgame$base2N[i] <- NA
    firstgame$base3N[i] <- NA
    firstgame$basesit[i] <- 0
  } 
  
    if (grepl("一出局", firstgame$log[i])){
    out_now <- 1
    }
    
    if (grepl("二出局", firstgame$log[i])){
    out_now <- 2
    }
    
    if (grepl("三出局", firstgame$log[i])){
    out_now <- 3
    }
    
    if (grepl("四壞球保送", firstgame$log[i])){
    firstgame$base1N[i] <- firstgame$Player[i]
    }

    for (j in 1:length(hit1_type)) {
      if (grepl(hit1_type[j], firstgame$log[i])){
        firstgame$base1N[i] <- firstgame$Player[i]
      }}
 ########################################################## 
   
    for (j in 1:length(base1)) {
      if (grepl(base1[j], firstgame$log[i])){
        firstgame$basesit[i] <- 1
      }}
  
    for (j in 1:length(base2)) {
      if (grepl(base2[j], firstgame$log[i])){
        firstgame$basesit[i] <- 2
      }}
  
    for (j in 1:length(base3)) {
      if (grepl(base3[j], firstgame$log[i])){
        firstgame$basesit[i] <- 3
      }}
    
    for (j in 1:length(base12)) {
      if (grepl(base12[j], firstgame$log[i])){
        firstgame$basesit[i] <- 12
      }}
    
    for (j in 1:length(base13)) {
      if (grepl(base13[j], firstgame$log[i])){
        firstgame$basesit[i] <- 13
      }}
    
    for (j in 1:length(base23)) {
      if (grepl(base23[j], firstgame$log[i])){
        firstgame$basesit[i] <- 23
      }}
    
    for (j in 1:length(base123)) {
      if (grepl(base123[j], firstgame$log[i])){
        firstgame$basesit[i] <- 123
      }}
  
    count <- 0
    for (j in 1:length(allbases)) {
      if (grepl(allbases[j], firstgame$log[i]) == TRUE)            #沒出現幾人在壘關鍵詞
        {count <- count + 1
      }}
      if(count==0){
        for (k in 1:length(onfirstbase)){
          if (grepl(onfirstbase[k], firstgame$log[i])==TRUE){       #壘包沒人，上一壘
            firstgame$basesit[i] <- 1
          }}
        for (k in 1:length(onsecondbase)){
          if (grepl(onsecondbase[k], firstgame$log[i])==TRUE){      #壘包沒人，上二壘
            firstgame$basesit[i] <- 2
          }}
        for (k in 1:length(onthirdbase)){
          if (grepl(onthirdbase[k], firstgame$log[i])==TRUE){       #壘包沒人，上三壘
            firstgame$basesit[i] <- 3
          }}
      }
      
  
  
    firstgame$outN[i] <- out_now
    
    

}


#######################################################################################################

#base1sub <- subset(firstgame, grepl(base1 , firstgame$log))
# length(allbases)
# 
# count <- 0
# for (j in 1:length(allbases)) {
#   if (grepl(allbases[j], firstgame$log[19])){
#     count <- count + 1
# 
#   }
#   }
# print(count)
# 
# base1sub <- subset(firstgame, grepl("有人" , firstgame$log))
# 
# s <- 0
# for (i in 1:5){
#   s <-  s + i
# }
# print(s)

