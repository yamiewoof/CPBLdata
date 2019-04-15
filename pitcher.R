#抓取投手名字的function
pitcherfunction=function(a){
  colonloc <- unlist(gregexpr(pattern =':',a)) + 1
  endloc <- nchar(a)
  a <- strsplit(a,"")
  a[[1]][colonloc:endloc]
  b <- paste(a[[1]][colonloc:endloc],collapse="")
  a <- paste(a[[1]][1:endloc],collapse="")
  ifelse(a == b, return("warning") , return(b))
}

#定義先發投手
for (line in data2019$log){
  linenum <- which(grepl(line, data2019$log))
  start <- "先發投手"
  if( grepl(start,line) == TRUE){ 
    
    
    linenum <- which( data2019$log == line)
    data2019[linenum,"Pitcher"] <- pitcherfunction(line)}
  
}

#定義後援投手  
for (line in data2019$log){
  linenum <- which(grepl(line, data2019$log))
  pitchg <- "更換投手"
  if( grepl(pitchg,line) == TRUE){ 
    
    
    linenum <- which( data2019$log == line)
    data2019[linenum,"Pitcher"] <- pitcherfunction(line)}
  
}

#在換投手前，補上現任投手名字

#輸入一開始主隊投手
pitcherhome <- "李茲"
#輸入一開始客隊投手
pitcheraway <- "羅里奇"

for (i in 1:nrow(data2019)){
  ifelse(   grepl("上",data2019[i,"inning"])  ,      
            ifelse(is.na(data2019[i,"Pitcher"]),data2019[i,"Pitcher"] <- pitcherhome , pitcherhome <- data2019[i,"Pitcher"]) ,
            ifelse(is.na(data2019[i,"Pitcher"]),data2019[i,"Pitcher"] <- pitcheraway , pitcheraway <- data2019[i,"Pitcher"]) 
            
  )}

#去除掉更換投手後面有野手的紀錄(如"更換投手：陳韻文，1B高國慶，RF唐肇廷")
comma <- "，"
for (i in 1:nrow(trying)){
  if( grepl(comma,data2019[i,"Pitcher"]) == TRUE){ 
    commaloc <- unlist(gregexpr(pattern ='，',data2019[i,"Pitcher"]))
    a <- strsplit(data2019[i,"Pitcher"],"")
    word <- a[[1]][1:commaloc[1]-1]
    data2019[i,"Pitcher"] <- paste(word,collapse="")
  }
}