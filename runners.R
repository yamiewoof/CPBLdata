allgames <- read.csv("log_process\\logdata\\allgames.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
cpbldata <- allgames
head(cpbldata)

for (i in 1:(nrow(allgames)-1)){
  cpbldata$rem_typeNafter[i] <- cpbldata$rem_typeN[i+1] 
} 

cpbldata12 <- subset(cpbldata,rem_typeN==12)
tosee <- subset(cpbldata12,rem_typeN==12&rem_typeNafter==15)
table(cpbldata12$rem_typeNafter)

str(cpbldata)

#開始assign跑者
for (i in 1:(nrow(cpbldata)-1)){
  
  #rem=12(2出局3壘有人)
  if  (cpbldata$rem_typeN[i] == 12){
    
    #變rem=6(2出局1、3壘有人)
    if  (cpbldata$rem_typeNafter[i] == 6){
      cpbldata$base1N[i+1] <- cpbldata$Player[i+1]
    }
    
    #變rem=18(2出局1、3壘有人)
    if  (cpbldata$rem_typeNafter[i] == 18){
      cpbldata$base3N[i+1] <- cpbldata$base1N[i]
      cpbldata$base1N[i+1] <- cpbldata$Player[i+1]
    }
    
    
  }
}