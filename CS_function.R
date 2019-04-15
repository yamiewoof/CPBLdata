# 抓盜壘失敗
cs_function = function(dummy_list, log_row) {
  
  # 展開 dummy_list 的值
  rem_type <- dummy_list$rem_type
  base1 <- dummy_list$base1
  base2 <- dummy_list$base2
  base3 <- dummy_list$base3
  player <- dummy_list$player
  
  
  cs_type <- c("盜壘失敗")
  
  # 將上面的類型都跑過
  for (i in 1:length(cs_type)) {
    
    if (grepl(cs_type[i], log_row)) {
      # cs_function start work
      # catch player name
      player <- substr (log_row, regexpr("：",log_row)-3, regexpr("：",log_row)-1 )
      
      if (rem_type %in% c(1,2,3)) {
        #如果壘包上沒跑者卻盜壘(壘包少紀錄)
        print("warning") 
      } else if (rem_type %in% c(4,5,6)) {
        base1 <- "NA"
        # 1B 死掉
        rem_type <- rem_type - 2
            }
        
        
        else if (rem_type %in% c(7,8,9)) {
        base2 <- "NA"

        rem_type <- rem_type - 5
        
      
      } else if (rem_type %in% c(10,11,12)) {
        # 得一分
        print("warning")
          
        
      } else if (rem_type %in% c(13,14,15)) {
        if (grepl("一壘有人",log_row)){
          rem_type <- rem_type - 8
          base2 <- "NA"
        }else{
          base3 <- "NA"
          base2 <- base1
          base1 <- "NA"
          rem_type <- rem_type - 5
        }
        
        
      } else if (rem_type %in% c(16,17,18)) {
        if (grepl("得分",log_row) == FALSE){
          base1 <- "NA"
          rem_type <- rem_type - 5
        } else{
          base1 <- "NA"
          base3 <- "NA"
          rem_type <- rem_type - 12
        }
        
        
      } else if (rem_type %in% c(19,20,21)) {
        print("warning")
        }
        
      } else if (rem_type %in% c(22,23,24)) {
        print("warning")
      }
  
  else {
    # dummy_list 不變
  }     
}
      
      
      

  # 將區域變數復原為 dummy_list 傳出
  dummy_list$rem_type <- rem_type
  dummy_list$base1 <- base1
  dummy_list$base2 <- base2
  dummy_list$base3 <- base3 
  dummy_list$player <- player
  return(dummy_list)
}
