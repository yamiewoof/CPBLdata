rm(list=ls())
setwd("c:\\baseball")

library(dplyr)
data2016 <- read.csv("log_process\\logdata\\logdata_2016.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
data2016 <- cbind(year = 2016,data2016)
data2017 <- read.csv("log_process\\logdata\\logdata_2017.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
data2017 <- cbind(year = 2017,data2017)
data2018 <- read.csv("log_process\\logdata\\logdata_2018.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
data2018 <- cbind(year = 2018,data2018)
data2019 <- read.csv("log_process\\logdata\\logdata_2019.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
data2019 <- cbind(year = 2019,data2019)
data2016to19 <- bind_rows(data2016,data2017,data2018,data2019)
#write.csv(data2016to19 ,file = sprintf("log_process\\logdata\\data2016to19.csv"), row.names=FALSE)



allgames <- data2016to19 %>% select(1,5:13, 15, 16,18,19)
# allgames <- data2019 %>% select(5:13, 15, 16,18,19) %>% slice(1:162) #做第一場
allgames$base1N <- NA
allgames$base2N <- NA
allgames$base3N <- NA
allgames$basesit <- NA
allgames$outN <- NA
allgames$rem_typeN <- NA

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
alldead <- c("雙殺")

for ( i in 1:nrow(allgames))  {
  if(i==1){
    inning_now <- "一上"
    base1_now <- NA
    base2_now <- NA
    base3_now <- NA
    allgames$basesit[i] <- 0
    out_now <- 0
  }

  
  if(i>1){
    allgames$base1N[i] <- allgames$base1N[i-1]
    allgames$base2N[i] <- allgames$base2N[i-1]
    allgames$base3N[i] <- allgames$base3N[i-1]
  }
  if (allgames$inning[i] != inning_now){
    inning_now <- allgames$inning[i]
    out_now <- 0
    allgames$base1N[i] <- NA
    allgames$base2N[i] <- NA
    allgames$base3N[i] <- NA
    allgames$basesit[i] <- 0
  } 
  
    if (grepl("一出局", allgames$log[i])){
    out_now <- 1
    }
    
    if (grepl("二出局", allgames$log[i])){
    out_now <- 2
    }
    
    # if (grepl("三出局", allgames$log[i])){
    # out_now <- 3
    # }
    
    # if (grepl("四壞球保送", allgames$log[i])){
    # allgames$base1N[i] <- allgames$Player[i]
    # }

    # for (j in 1:length(hit1_type)) {
    #   if (grepl(hit1_type[j], allgames$log[i])){
    #     allgames$base1N[i] <- allgames$Player[i]
    #   }}
 ########################################################### 
   
    for (j in 1:length(base1)) {
      if (grepl(base1[j], allgames$log[i])){
        allgames$basesit[i] <- 1
      }}
  
    for (j in 1:length(base2)) {
      if (grepl(base2[j], allgames$log[i])){
        allgames$basesit[i] <- 2
      }}
  
    for (j in 1:length(base3)) {
      if (grepl(base3[j], allgames$log[i])){
        allgames$basesit[i] <- 3
      }}
    
    for (j in 1:length(base12)) {
      if (grepl(base12[j], allgames$log[i])){
        allgames$basesit[i] <- 12
      }}
    
    for (j in 1:length(base13)) {
      if (grepl(base13[j], allgames$log[i])){
        allgames$basesit[i] <- 13
      }}
    
    for (j in 1:length(base23)) {
      if (grepl(base23[j], allgames$log[i])){
        allgames$basesit[i] <- 23
      }}
    
    for (j in 1:length(base123)) {
      if (grepl(base123[j], allgames$log[i])){
        allgames$basesit[i] <- 123
      }}
  

    if (grepl("全壘打", allgames$result[i])){                       
      allgames$basesit[i] <- 0
    }

  
    count <- 0
    for (j in 1:length(allbases)) {
      if (grepl(allbases[j], allgames$log[i]) == TRUE)            #沒出現幾人在壘關鍵詞
        {count <- count + 1
      }}
      if(count==0){
        for (k in 1:length(onfirstbase)){
          if (grepl(onfirstbase[k], allgames$log[i])==TRUE){       #壘包沒人，上一壘
            allgames$basesit[i] <- 1
          }}
        for (k in 1:length(onsecondbase)){
          if (grepl(onsecondbase[k], allgames$log[i])==TRUE){      #壘包沒人，上二壘
            allgames$basesit[i] <- 2
          }}
        for (k in 1:length(onthirdbase)){
          if (grepl(onthirdbase[k], allgames$log[i])==TRUE){       #壘包沒人，上三壘
            allgames$basesit[i] <- 3
          }}
        for (k in 1:length(alldead)){
          if (grepl(alldead[k], allgames$log[i])==TRUE){       #壘包沒人，上三壘
            allgames$basesit[i] <- 0
          }}
      }
  
      if (is.na(allgames$basesit[i])){
        allgames$basesit[i] <- allgames$basesit[i-1]
      }
  
  
    allgames$outN[i] <- out_now
    
    #決定rem_type
    
    #rem = 1
    if (    allgames$basesit[i] == 0 && allgames$outN[i] == 0   ){
      allgames$rem_typeN[i] <- 1
    }
    
    #rem = 2
    if (    allgames$basesit[i] == 0 && allgames$outN[i] == 1   ){
      allgames$rem_typeN[i] <- 2
    }

    #rem = 3
    if (    allgames$basesit[i] == 0 && allgames$outN[i] == 2   ){
      allgames$rem_typeN[i] <- 3
    }
    
    #rem = 4
    if (    allgames$basesit[i] == 1 && allgames$outN[i] == 0   ){
      allgames$rem_typeN[i] <- 4
    }
    
    #rem = 5
    if (    allgames$basesit[i] == 1 && allgames$outN[i] == 1   ){
      allgames$rem_typeN[i] <- 5
    }
    
    #rem = 6
    if (    allgames$basesit[i] == 1 && allgames$outN[i] == 2   ){
      allgames$rem_typeN[i] <- 6
    }
    
    #rem = 7
    if (    allgames$basesit[i] == 2 && allgames$outN[i] == 0   ){
      allgames$rem_typeN[i] <- 7
    }
    
    #rem = 8
    if (    allgames$basesit[i] == 2 && allgames$outN[i] == 1   ){
      allgames$rem_typeN[i] <- 8
    }
    
    #rem = 9
    if (    allgames$basesit[i] == 2 && allgames$outN[i] == 2   ){
      allgames$rem_typeN[i] <- 9
    }
    
    #rem = 10
    if (    allgames$basesit[i] == 3 && allgames$outN[i] == 0   ){
      allgames$rem_typeN[i] <- 10
    }
    
    #rem = 11
    if (    allgames$basesit[i] == 3 && allgames$outN[i] == 1   ){
      allgames$rem_typeN[i] <- 11
    }
    
    #rem = 12
    if (    allgames$basesit[i] == 3 && allgames$outN[i] == 2   ){
      allgames$rem_typeN[i] <- 12
    }
    
    #rem = 13
    if (    allgames$basesit[i] == 12 && allgames$outN[i] == 0   ){
      allgames$rem_typeN[i] <- 13
    }
    
    #rem = 14
    if (    allgames$basesit[i] == 12 && allgames$outN[i] == 1   ){
      allgames$rem_typeN[i] <- 14
    }
    
    #rem = 15
    if (    allgames$basesit[i] == 12 && allgames$outN[i] == 2   ){
      allgames$rem_typeN[i] <- 15
    }
    
    #rem = 16
    if (    allgames$basesit[i] == 13 && allgames$outN[i] == 0   ){
      allgames$rem_typeN[i] <- 16
    }
    
    #rem = 17
    if (    allgames$basesit[i] == 13 && allgames$outN[i] == 1   ){
      allgames$rem_typeN[i] <- 17
    }
    
    #rem = 18
    if (    allgames$basesit[i] == 13 && allgames$outN[i] == 2   ){
      allgames$rem_typeN[i] <- 18
    }
    
    #rem = 19
    if (    allgames$basesit[i] == 23 && allgames$outN[i] == 0   ){
      allgames$rem_typeN[i] <- 19
    }
    
    #rem = 20
    if (    allgames$basesit[i] == 23 && allgames$outN[i] == 1   ){
      allgames$rem_typeN[i] <- 20
    }
    
    #rem = 21
    if (    allgames$basesit[i] == 23 && allgames$outN[i] == 2   ){
      allgames$rem_typeN[i] <- 21
    }
    
    #rem = 22
    if (    allgames$basesit[i] == 123 && allgames$outN[i] == 0   ){
      allgames$rem_typeN[i] <- 22
    }
    
    #rem = 23
    if (    allgames$basesit[i] == 123 && allgames$outN[i] == 1   ){
      allgames$rem_typeN[i] <- 23
    }
    
    #rem = 24
    if (    allgames$basesit[i] == 123 && allgames$outN[i] == 2   ){
      allgames$rem_typeN[i] <- 24
    }
    
    
}

allgames$rem_typeN <- as.factor(allgames$rem_typeN)
str(allgames)




#write.csv(allgames,file = sprintf("log_process\\logdata\\allgames.csv"), row.names=FALSE)

#######################################################################################################

#base1sub <- subset(allgames, grepl(base1 , allgames$log))
# length(allbases)
# 
# count <- 0
# for (j in 1:length(allbases)) {
#   if (grepl(allbases[j], allgames$log[19])){
#     count <- count + 1
# 
#   }
#   }
# print(count)
# 
# base1sub <- subset(allgames, grepl("有人" , allgames$log))
# 
# s <- 0
# for (i in 1:5){
#   s <-  s + i
# }
# print(s)

 # DPsub <- subset(allgames, grepl("再傳" , allgames$log)&grepl("雙殺" , allgames$log)==FALSE)
