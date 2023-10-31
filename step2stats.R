# ________________ STEP 2. STATISTICAL ANALYSIS ______________________________________ # 

  rm(list=ls()) 
  dev.off()       # clear plots 

#   PATHIN <- "C:/Users/Tammaso/japneetPP/stats_analysis/Me/scripts"
  PATHIN <- "C:/Users/japneet/Desktop/PP_Final/tract/results"
    setwd(PATHIN)

    ## Get the data in wide format ### 
   
 #   source("prepare_data_wide.R")


    combined_data <- read.csv("combined_group_wide.csv", header=TRUE, sep=",")
    


# - - HYPOTHESIS 1: To compare the FA values of the ROI pairs of preterm with term born neonates in both hemispheres. - - - - - - # 

# Variables needed: FA LR mean values, Sub_id, group  

# STEP 1: Data needed: sub_id, ROI_pairs_LR mean, group
  LR_mean <- select_at(combined_data, vars(sess_id, group, gender, ends_with("LRmean"))) 
#   LR_mean <- cbind(sub_ids,LR_mean)

# STEP 2: transform to long format
  LRmean_long <- LR_mean %>%
  pivot_longer(cols = ends_with("LRmean"), names_to = "variables", values_to = "value") 

  LRmean_long$value <- as.numeric(as.character(LRmean_long$value))        # convert the value column from char to numeric. 

# STEP 3: View the outliers by plotting the data using box plots before removing them  
  temp<-LR_mean[2:6]         # extract the FA_LRmean values 
  boxplot(temp$FFA_OFA_LRmean, temp$FFA_V1_LRmean, temp$FFA_PSTS_LRmean, temp$OFA_V1_LRmean, temp$OFA_PSTS_LRmean, temp$V1_PSTS_LRmean,  names = c("FFA_OFA", "FFA_V1", "FFA_PSTS", "OFA_V1", "OFA_PSTS", "V1_PSTS"))
  
  title("Outliers") 

# STEP 4: Remove outliers

  setwd(script_path) 
  source("replace_outliers.R")                                            # get the function to replace outliers 


  # Create a loop that goes through all the rows and saves the cleaned data in a new matrix after removing outliers

    cleaned_data = matrix(nrow = 39, ncol = 6)        # empty matrix to store cleaned data 

    for(i in 1:ncol(cleaned_data)){
      
      for (n in 1:ncol(temp)){
      cleaned_data$i <- replace_outliers(temp[n])
      
      }
      
      #print(i)
         
            }
  
clean_FFA_OFA<-replace_outliers(temp$FFA_OFA_LRmean)
clean_FFA_V1<-replace_outliers(temp$FFA_V1_LRmean)
clean_FFA_PSTS<-replace_outliers(temp$FFA_PSTS_LRmean)
clean_OFA_V1<-replace_outliers(temp$OFA_V1_LRmean)
clean_OFA_PSTS<-replace_outliers(temp$OFA_PSTS_LRmean)
clean_V1_PSTS<-replace_outliers(temp$V1_PSTS_LRmean)


# LRmean.clean <- data.frame(replace_outliers(LRmean_long$value))       # remove outliers 




clean_FFA_PSTS<-replace_outliers(FFA_OFA)
boxplot(clean_FFA_PSTS)











