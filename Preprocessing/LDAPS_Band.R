rm(list=ls())

setwd("D:/(2020)Redtide_Prediction")

library(sp)
library(rgdal)

# LDAPS 파일을 입력받아서 Dataset을 출력시켜주는 함수
LDAPS_DB = function(LDAPS){
  grib = readGDAL(LDAPS)
  D = grib@data
  
  xy = coordinates(grib)
  sp = SpatialPoints(xy, proj4string = grib@proj4string)
  sp_trans = spTransform(sp, CRS('+proj=longlat +datum=WGS84'))
  xy = coordinates(sp_trans)
  
  newD = cbind(xy, D)
  
  return(newD)
}

Time_S_All = Sys.time()
for (B in 1:136){
  ##################################################################
  Band = B
  Year = '2016'
  Month = 7
  FolderName = 'LDAPS_2016'
  nDay = 30
  ##################################################################
  
  # # nDay 계산 
  # # 홀수 달
  # if (Month %% 2 == 1){
  #   nDay = 31
  # }else if (Month %% 2 == 0){
  #   # 짝수 달
  #   if (Month != 2){
  #     nDay = 30
  #   }else if (Month == 2){
  #     nDay = 28
  #   }
  # }
  
  # Month
  if (Month < 10){
    MONTH = paste('0', Month, sep = '')
  }else if (Month > 9){
    MONTH = as.character(Month)
  }
  
  TT = c('00', '06', '12', '18')
  
  Time_S = Sys.time() # 시작시간 
  for (Day in 1:nDay){
  #for (Day in 1:3){
    
    if (Day < 10){
      DAY = paste('0', Day, sep = '')
    }else if (Day > 9){
      DAY = as.character(Day)
    } # end if
    
    if (Day == 1){
      # Day == 1 이면, LDAPS_Table 생성 
      for (TimeID in TT){
        # LDAPS 00
        if (TimeID == '00'){
          LDAPS_Folder = paste(Year, MONTH, DAY, sep = '')
          LDAPS_File = paste('l015v070erlounish000.', Year, MONTH, DAY, TimeID, '.gb2', sep = '')
          LDAPS_Path = paste(FolderName, LDAPS_Folder, LDAPS_File, sep = '/')
          
          LDAPS_Daily = LDAPS_DB(LDAPS_Path)[,c(1, 2, Band+2)]
          LDAPS_Daily = LDAPS_Daily[(LDAPS_Daily$x>127.4) & (LDAPS_Daily$x<129.5) & (LDAPS_Daily$y>34.4) & (LDAPS_Daily$y<35.5),]
          LDAPS_Table = LDAPS_Daily
          
          names(LDAPS_Table)[ncol(LDAPS_Table)] = paste(Year, MONTH, DAY, TimeID, sep = '')
          
          print(LDAPS_File)
        }else{
          LDAPS_Folder = paste(Year, MONTH, DAY, sep = '')
          LDAPS_File = paste('l015v070erlounish000.', Year, MONTH, DAY, TimeID, '.gb2', sep = '')
          LDAPS_Path = paste(FolderName, LDAPS_Folder, LDAPS_File, sep = '/')
          
          LDAPS_Daily = LDAPS_DB(LDAPS_Path)[, c(1, 2, Band+2)]
          LDAPS_Daily = LDAPS_Daily[(LDAPS_Daily$x>127.4) & (LDAPS_Daily$x<129.5) & (LDAPS_Daily$y>34.4) & (LDAPS_Daily$y<35.5),]
          LDAPS_Table = cbind(LDAPS_Table, LDAPS_Daily[,3])
          
          names(LDAPS_Table)[ncol(LDAPS_Table)] = paste(Year, MONTH, DAY, TimeID, sep = '')
          
          print(LDAPS_File)
        } # end if
      } # end for(TIME in TT)
    }else{
      # Day != 1 이면, LDAPS_Table 누적
      for (TimeID in TT){
        # LDAPS 00
        if (TimeID == '00'){
          LDAPS_Folder = paste(Year, MONTH, DAY, sep = '')
          LDAPS_File = paste('l015v070erlounish000.', Year, MONTH, DAY, TimeID, '.gb2', sep = '')
          LDAPS_Path = paste(FolderName, LDAPS_Folder, LDAPS_File, sep = '/')
          
          LDAPS_Daily = LDAPS_DB(LDAPS_Path)[,c(1, 2, Band+2)]
          LDAPS_Daily = LDAPS_Daily[(LDAPS_Daily$x>127.4) & (LDAPS_Daily$x<129.5) & (LDAPS_Daily$y>34.4) & (LDAPS_Daily$y<35.5),]
          LDAPS_Table = cbind(LDAPS_Table, LDAPS_Daily[,3])
          
          names(LDAPS_Table)[ncol(LDAPS_Table)] = paste(Year, MONTH, DAY, TimeID, sep = '')
          
          print(LDAPS_File)
        }else{
          LDAPS_Folder = paste(Year, MONTH, DAY, sep = '')
          LDAPS_File = paste('l015v070erlounish000.', Year, MONTH, DAY, TimeID, '.gb2', sep = '')
          LDAPS_Path = paste(FolderName, LDAPS_Folder, LDAPS_File, sep = '/')
          
          LDAPS_Daily = LDAPS_DB(LDAPS_Path)[, c(1, 2, Band+2)]
          LDAPS_Daily = LDAPS_Daily[(LDAPS_Daily$x>127.4) & (LDAPS_Daily$x<129.5) & (LDAPS_Daily$y>34.4) & (LDAPS_Daily$y<35.5),]
          LDAPS_Table = cbind(LDAPS_Table, LDAPS_Daily[,3])
          
          names(LDAPS_Table)[ncol(LDAPS_Table)] = paste(Year, MONTH, DAY, TimeID, sep = '')
          
          print(LDAPS_File)
        } # end if(TIME == '00')
      } # end for(TIME in TT)
    } # end if-else
  
  } # end for(Day in 1:nDay)
  
  Time_E = Sys.time()
  
  print(Time_E - Time_S)
  
  # Export
  EfileName = paste(Year, MONTH, '_', Band, '.csv', sep = '')
  write.table(LDAPS_Table,
              file = EfileName,
              sep = ',',
              col.names = TRUE,
              row.names = FALSE)
}

Time_E_All = Sys.time()

print(Time_E_All - Time_S_All)
