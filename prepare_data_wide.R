## ------------------------------------------------------------------------------------------------------------- ##  
####################### Practical Project Statistical Analysis Script ########################
## ------------------------------------------------------------------------------------------------------------- ##  


# clear workspace variables and arrays
  rm(list=ls())     

# Sys.setenv(LANG = "en")            # set console output language to english 

# ------------------------ Loop to install and load the required packages if they are not installed before ------------------------ #

packages <- c("ARTool","mvnormtest", "tidyverse", "rstatix", "lsr", "FSA", "ggpubr", "ggplot2", "dplyr", "textshape", "reshape", "psych", "car", "janitor", "Amelia", "MASS", "mlbench", "rgl", "corrplot", "readtext", "ggstatsplot", "testthat", "broom")

# Install the packages if they are not available checking from the list 
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages]) 
}

# Load packages
library(dplyr) 
lapply(packages, library, character.only = TRUE) %>%
invisible()

library(ggplot2)
theme_set(
  theme_classic() + 
    theme(legend.position = "top") ) 

rm(packages)    # remove var packages from workspace 






# ---------------- Combine all the subject's stats files into one file ---------- # 

# 1. Set path where csv file "results_full" is located
MAIN_PATH <-"C:/Users/japneet/Desktop/PP_Final/stats_analysis/Me/stats_group_analysis"       # all files for group stats analysis 
PATHIN <- file.path(MAIN_PATH, "stats_group_final")            # data files 
PATHOUT <- file.path(MAIN_PATH, "results")                     # combined data in wide format  
script_path <- "C:/Users/japneet/Desktop/PP_Final/stats_analysis/Me/scripts"                 # scripts 

# 2. set working directory to where all the stats files are saved
setwd(PATHIN)

# 3. Define all the vars needed in the for loop 
data.file <- ""                                     # output var for final file combining stats and demog data                                
sub <- ""                                           # output var for reshaped subject stats file for ROI pairs 

# 4. Get the list of all the stats text files from the current directory 
stats_filelist <- list.files(pattern = "\\.txt$")                    


# 5. Loop through all the subject specfic stats files 
for(i in 1:length(stats_filelist)){                                 

  # 6. read the file and save in the var file i 
  file <- read.table(stats_filelist[i],header=FALSE, sep=",")    
  
  # 7. Second loop runs from 1 to 6 
  for (i in 1:6){ 
    
    # 8. store the data from row 1 column 2 to 7 from file i
    FFA_OFA <- file[1,2:7]                                            
    FFA_V1 <- file[2,2:7]
    FFA_PSTS <- file[3,2:7]
    OFA_V1 <- file[4,2:7]
    OFA_PSTS <- file[5,2:7]
    V1_PSTS <- file[6,2:7]
    
    # 9. Combine all the data for subject in one row, total 36 columns, contains stats for all the 
    sub <- data.frame(cbind(FFA_OFA, FFA_V1, FFA_PSTS, OFA_V1, OFA_PSTS, V1_PSTS))                
    
  }
  
  # combine stats of all subjects from subject list 
  data.file <- data.frame(rbind(data.file,sub))
  
  # replicate column names 
  sub1 <- sub                
  
  # match the column names of sub1 and data.file
  colnames(sub1) <- colnames(data.file)        
}

# ----- Remove unnecessary variables 
rm(FFA_OFA)
rm(FFA_PSTS)
rm(FFA_V1)
rm(V1_PSTS)
rm(OFA_PSTS)
rm(OFA_V1)


# ---- Clean the data.file 

# clear the empty first row from the data.file 
data.file = data.file[-c(1),]

# write the headings of different ROI pair's stats in one file 
headings <- data.frame("FFA_OFA_Lmean", "FFA_OFA_LStd", "FFA_OFA_Rmean", "FFA_OFA_RStd", "FFA_OFA_LRmean", "FFA_OFA_LRStd", 
                       "FFA_V1_Lmean", "FFA_V1_LStd", "FFA_V1_Rmean", "FFA_V1_RStd", "FFA_V1_LRmean", "FFA_V1_LRStd", 
                       "FFA_PSTS_Lmean", "FFA_PSTS_LStd", "FFA_PSTS_Rmean", "FFA_PSTS_RStd", "FFA_PSTS_LRmean", "FFA_PSTS_LRStd", 
                       "OFA_V1_Lmean", "OFA_V1_LStd", "OFA_V1_Rmean", "OFA_V1_RStd", "OFA_V1_LRmean", "OFA_V1_LRStd", 
                       "OFA_PSTS_Lmean", "OFA_PSTS_LStd", "OFA_PSTS_Rmean", "OFA_PSTS_RStd", "OFA_PSTS_LRmean", "OFA_PSTS_LRStd",
                       "V1_PSTS_Lmean", "V1_PSTS_LStd", "V1_PSTS_Rmean", "V1_PSTS_RStd", "V1_PSTS_LRmean", "V1_PSTS_LRStd")


# replicate the var headings 
data1 <- headings       

# match the column names of the variable headings with sub1
colnames(data1) <- colnames(data.file)                       

# Add headings row to the the stats.file data
final_data <- data.frame(rbind(data1,data.file))


# write a text file for all subjects stats
setwd(script_path) 
write.csv(final_data, file = "final_data.csv")




# --------- Combine the data file with demog details folder ------------- # 

# read the subject stats file and subject details file 
sub_list <- read.csv("SUB_PP_group.csv", header=TRUE, sep=",")
stats <- read.csv("final_data.csv", header=TRUE, sep="," )

names(stats) <- stats[1,]      # set the first row to header
stats <- stats[-1,]            # remove the first row  
stats <- stats[,-1]            # remove first column 

# combine the stats and subject file together 
combined_data <- data.frame(cbind(sub_list,stats)) 
sub_ids <- as.data.frame(combined_data[1])

rownames(combined_data) <- combined_data[,1]

combined_data <- subset (combined_data, select = -1)

# Convert the FA mean and std values to numeric 

for (i in 9:ncol(combined_data)){
  
  combined_data[,i] <- sapply(combined_data[,i],as.numeric)
  
}

  
  
  
  
  
# Create the combined text file in the current directory 
setwd(PATHOUT)
write.table(combined_data, file = "combined_data_wide.csv", sep=",", row.names = FALSE, col.names = TRUE)  # save all results as a file


# --------- Clear unnecessary variables ------------- # 
    
     rm(file)
     rm(headings)
     rm(data.file)
     rm(sub)
     rm(sub_list)
     rm(sub1)
     rm(stats)
     rm(data1)
     rm(final_data)
     rm(stats_filelist)
     rm(i)
     rm(installed_packages)

     
