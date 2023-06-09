# Purpose: Bay Scallop ParentalXOffsrping OA Exposure -- LoLin script to calc non bias raw rates
# measure respiration rate from raw Loligo output data 
# using Lolin.R (Olito etal. 201?) for reproducible and non-bias calculation of respiration rates

# Written by: Sam J Gurr (last edit 3/16/2023)

# LOAD PACKAGES :::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(devtools) # devtools::install_github # use devtools to instlal github link
library(LoLinR) # install_github('colin-olito/LoLinR') # install LoLinR from github
library(dplyr)
library(lubridate)
library(rMR) 
library(stringr)

# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::

setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_parentXoffspring_OA/RAnalysis")
#("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer

# CHANGE THE FOLLOWING ..THEN CONTROL A + ENTER ::::::::::::::::::::::

path.p    <- "Data/Physiology/Respiration" #the location of all your respirometry files 
a         <- 0.4
ouputNAME <- "Output/Respiration/RR_LoLin_raw.csv" 

# ANALYSIS  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Objective: use LoLinr to run all respiration rates in a non-bias and autonomous fashion
# Ouputs: there will be two main resources from this script: 
#   (1) cumulative spreasheet of all respiration rate value for each Channel on each day
#   (2) a folder of plots from the LoLinR script to a plots folder - this will allow troubleshooting and sanity checks 

# I. call subfolders as dataframe and create a dataframe for the output

# call the subfolder names for the outside loop 'i' (i.e. 20210914)
folder.names           <- basename(list.files(path = path.p, pattern = "202", recursive = FALSE)) #list all csv file names in the folder and subfolders
folder.names.table     <- data.frame(folder.names)

# folder.names (only one)
# 1      20230316

# Call the cumulative dataframe that we will write to in the for loop below
df_total             <- data.frame() # start dataframe 
resp.table           <- data.frame(matrix(nrow = 1, ncol = 7)) # create dataframe to save cumunalitively during for loop
colnames(resp.table) <- c('Date', 'Channel', 'Lpc', 'Leq' , 'Lz', 'alpha','Filename') # names for comuns in the for loop

# II. A bunch o' fors and if/elses - commented throughout!


# outside 'i' loop - call each subfolder one at a time for analysis
for(i in 1:nrow(folder.names.table)) { # for every subfolder 'i' ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "csv$", recursive = TRUE)))) #%>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
  
  # inside 'm' loop - call each  raw .txt or raw .csv file file witin the subfolder 'i'
  for(m in 1:nrow(file.names.table)) { # for every raw .txt or csv file 'm' in the subfolder 'i' :::::::::::::::::::::::::::::::::::::
    
    Resp.Data           <- read.table(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), sep = ',', header = TRUE,skip = 47) #reads in the data files
    
    Resp.Data$date      <- paste((sub("2023.*", "", Resp.Data$Date..DD.MM.YYYY.)), '2023', sep='') #  date - use 'sub' to call everything before 2023, add back 2023 using paste
    Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2023/", "", Resp.Data$Time..HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
    Resp.Data$seconds   <- (Resp.Data$time_Sec - Resp.Data$time_Sec[1])    # secs - calc the sec time series
    Resp.Data$minutes   <- (Resp.Data$time_Sec - Resp.Data$time_Sec[1])/60 # mins - calc the minute time series
    temperature_C       <- as.numeric(Resp.Data$CH1.temp...C.[1])
    barromP_kPa         <- as.numeric(Resp.Data$Barometric.pressure..hPa.[1]) / 10
    salinity.pp.thou    <- as.numeric(Resp.Data$Salinity....[1])
    Resp.Data           <- Resp.Data %>% # use 'dplyr' 
                              dplyr::select(c(date, 
                                              seconds, 
                                              minutes, 
                                              contains("..Oxygen."))) # call unique column names for the 8 Channels

    colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] <- substr( ( colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] ), 1,2) 

    Resp.Data_15sec <- Resp.Data[seq(1, nrow(Resp.Data), 4), ] %>% # call data every minute (truncated from every 15 seconds!)
                        dplyr::filter(minutes > 60 & minutes < 160)# truncated dataset - review the raw data plots for th reasoning behind this truncation

              # inside 'j' loop - for each 'raw' txt file 'm', call each O2 sensor/resp chamber 'j' for analysis
              for(j in 4:(ncol(Resp.Data_15sec))){ # for each sensor column 'j' (..starting at column 4) :::::::::::::::::::::::::::::::
              
                Resp_loop    <- (Resp.Data_15sec[,c(3,j)]) %>% 
                                              dplyr::filter(!str_detect(((Resp.Data_15sec[,c(3,j)])[,2]),"NaN")) %>%  # noticed some random rows have 'NaN' - so I will loop the min and Channels to omit Nas before proceeding
                                              dplyr::mutate(minutes = as.numeric(minutes)) #  %>% # convert minutes to numeric

                Resp_loop$mgL     <- na.omit(Resp.Data_15sec[j])
                Resp_loop$mgL     <- as.numeric(unlist(Resp_loop$mgL)) # need to unlist and call as numeric to run LoLinR
                Resp_loop$minutes <- as.numeric(unlist(Resp_loop$minutes)) # need to unlist and call as numeric to run LoLinR
                Resp_loop %>% dplyr::filter(mgL > (0.8 * max(na.omit(Resp.Data %>% select(colnames(Resp.Data_15sec[j]))))) ) # grab all data over 80% air saturation for the particular column (pre-filtered by time as 'Resp.Data')

                    # now run data!
                     if (nrow(Resp_loop) < 1) { # if column 'j' is NA write NA in the cumulative sheet...
                        resp.table$Date                <- Resp.Data_15sec[1,1]
                        resp.table$Channel             <- colnames(Resp_loop)[2] 
                        resp.table[3:ncol(resp.table)] <- 'NA'
                        df       <- data.frame(resp.table) # name dataframe for this single row
                        df_total <- rbind(df_total,df) #bind to a cumulative list dataframe
                        print(df_total) # print to monitor progress
                        
                        } else { # else run LoLinR for x=mins and y=mg/l O2
                          
                            model <- rankLocReg(
                            xall    = as.numeric(Resp_loop[, 1]), 
                            yall    = as.numeric(Resp_loop[, 3]), # call x as the minute timeseries and y as the mg L-1 O2 
                            alpha   = a,  # alpha was assigned earlier as 0.4 by the authors default suggestions - review Olito et al. and their github page for details
                            method  = "pc", 
                            verbose = TRUE) 
                        
                            sum.table <- summary(model)
                            
                            resp.table$Date       <- Resp.Data_15sec[1,1]
                            resp.table$Channel    <- colnames(Resp_loop)[2] 
                            resp.table$Lpc        <- sum.table$Lcompare[3,6] # Lpc slope - call the column 'b1'
                            resp.table$Leq        <- sum.table$Lcompare[2,6] # Leq slope - call the column 'b1'
                            resp.table$Lz         <- sum.table$Lcompare[1,6] # Lz slope  - call the column 'b1'
                            resp.table$alpha      <- a
                            resp.table$Filename   <- file.names.table[m,1]
                            
                            df       <- data.frame(resp.table) # name dataframe for this single row
                            df_total <- rbind(df_total,df) #bind to a cumulative list dataframe
                            print(df_total) # print to monitor progress
                            }  # end of  else statement (if column 'j' is NA write NA in the cumulative sheet, else run LoLinR for x=mins and y = mg/l O2)
                            
                
                # save plots every inside loop and name by date_run_vialposition
 
                pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_parentXoffspring_OA/RAnalysis/Output/Respiration/plots_raw_LoLinR/",folder.names.table[i,1],"_", substr((sub(".*resp_","",file.names.table[m,1])), 1, 6),"_",colnames(Resp_loop)[2],"_regression.pdf")) # 20211026_resp_unfed.csv ONLY
                plot(model)
                dev.off()
                
         } # end of inside for loop 'j' (for each sensor column 'j' [a] isolate mins and CH_ for analysis [b] convert CH_ data to mg/L using 'DO.unit.convert' [c] calc respi rates with LoLin R)
    
     } # end of inside  for loop 'm' (for every 'raw' .txt file 'm' in the subfolder 'i')
  
} # end of outside for loop 'i' (for every subfolder 'i')

# merge with the preexisiting table
# NOTE: raw values here are...
# (1) in units of mg/L min-1 (slope of line mg/l / mins in LoLinR - check the plot outputs to see)
# (2) not normalized for volume chamber 
# (3) not normalized for blank resp rate 
# (4) not normalized for a size/individual metric (i.e. Tissue Dry weight, shell length, etc.)
cumulative_resp_table <- read.csv(file=ouputNAME, header=TRUE) #call the pre existing cumulative table
new_table             <- rbind(cumulative_resp_table, df_total) # bind the new table from the for loop to the pre exisiting table
write.table(new_table,ouputNAME,sep=",", row.names=FALSE)  # write out to the path names outputNAME
# View(new_table) # view if you like!
