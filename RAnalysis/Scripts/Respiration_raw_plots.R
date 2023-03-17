# Purpose: Bay Scallop ParentalXOffsrping OA Exposure -- raw PresEns output data 
# using Lolin.R (Olito etal. 201?) for reproducible and non-bias calculation of respiration rates

# Written by: Sam J Gurr (last edit 3/16/2023)

# LOAD PACKAGES :::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(devtools) # devtools::install_github # use devtools to instlal github link
library(LoLinR) # install_github('colin-olito/LoLinR') # install LoLinR from github
library(dplyr)
library(lubridate)
library(rMR) 
library(ggplot2)
library(stringr)
# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::

setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_parentXoffspring_OA/RAnalysis")

# CHANGE THE FOLLOWING ..THEN CONTROL A + ENTER ::::::::::::::::::::::
path.p    <- "Data/Physiology/Respiration" #the location of all your respirometry files 
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
                colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] <- substr( ( colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] ), 1,2)   # clean these column names to make things easier - first 3 characters
                
                Resp.Data_15sec <- Resp.Data # .csvs are from the SDR (Presens) data - these data are already taken every 15 seconds - rename for clarity in the next statements

                date.plot  <- folder.names.table[i,1]
                run.plot   <- substr( (sub(".*M_","",file.names.table[m,1])), 1,13)
                plot_title <- paste(date.plot, run.plot, sep = '_')
                PLOT <- Resp.Data_15sec %>% 
                  dplyr::select(-c('date', 'seconds')) %>%  
                  reshape2::melt(id.vars = "minutes",variable.name = "channel", value.name = "mg.L.min") %>%
                  ggplot(aes(x = minutes , y = mg.L.min)) +
                  geom_smooth(method = "loess", se=FALSE, color="black", formula = mg.L.min ~ minutes) +
                  theme_classic() +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", plot.title = element_text(size=10))+ 
                  labs(y = expression(RAW_mg~L^{-1}~O[2]%.%min^{-1})) +
                  xlab("minutes") +
                  geom_point() + 
                  ggtitle(plot_title) +
                  facet_wrap(~channel)
                            
                #pdf(paste0("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", substr( (sub(".*M_","",file.names.table[m,1])), 1,13),"_regression.pdf"), width=10, height=12)
                pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_parentXoffspring_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", substr( (sub(".*M_","",file.names.table[m,1])), 1,13),"_regression.pdf"), width=10, height=12)
                print(PLOT)
                dev.off()

         
      }
}

        
    