##################################################################################################################
#                                                                                                                #
#                                              STATISTICAL ANALYSIS                                              # 
#                                                                                                                #
##################################################################################################################

# ------------------------------------------------------------------------------------------------------------ # 
# ----------- PREPARE DATA 
# ------------------------------------------------------------------------------------------------------------ # 
# Clean workspace and set path 
rm(list=ls()) 
# path <- "C:/Users/japneet/Desktop/PP_final/stats_analysis/Me/scripts"
path <- "C:/Users/japneet/Desktop/PP_final/tract/results"  
setwd(path)

# Get the data in wide format 
combined_data <- read.csv("combined_group_wide.csv", header=TRUE, sep=",")

# Clean data 
combined_data <- subset (combined_data, select = -2:-4)
combined_data <- subset (combined_data, select = -3:-4)


# Specify details in the data for simplicity 
combined_data[,3][combined_data[,3] == 0] <- "Term-born"  # replace 0s and 1s with strings
combined_data[,3][combined_data[,3] == 1] <- "Preterm"




######### YOU GET A CLEAN DATA FILE IN WIDE FORMAT WITHOUT ANY UNNECESSARY VARS 

# ------------------------------------------------------------------------------------------------------------ # 
# Hypothesis 1 :  To test the difference in term/preterm and males/females in their FA values (WM strength) in 
# the ROI pairs associated with face processing.  
# ------------------------------------------------------------------------------------------------------------ # 

# 2 independent vars: group, gender 
# 1 dependent variable: FFA-OFA LR mean 

# FFA_OFA_LRmean <- select_at(combined_data, vars(sess_id, gender, group, ends_with("FFA_OFA_LRmean")))
# 
# ggplot(data = FFA_OFA_LRmean, 
#        aes(x = FFA_OFA_LRmean, y = gender)) +
#   geom_boxplot() +
#   xlab("FFA_OFA_LRmean") +
#   ylab("Life satisfation") 
# 





# Create a new table to get the LR mean data of all the ROI pairs   
  both_hem_stats <- select_at(combined_data, vars(sess_id, gender, group, ends_with("LRmean")))
 
  
# ----------- Convert the data to long format 
  # vars needed: 
  #  LR_roipairwise_mean: sess_id, group, gender,mean
  
  
  # transform to long format
  LR_mean_long <- both_hem_stats %>%
    pivot_longer(cols = ends_with("LRmean"), names_to = "variables", values_to = "value") 
  
  
  
  
##################################################################################################################
# Statistical Analysis ------------------------------------------------------
# Run a Two-Way ANOVA because we have multiple independent variables. 
##################################################################################################################

#-------- Define the factors 

  combined_data$group <- as.factor(LR_mean_long$group)  # preterm/term
  combined_data$gender <- as.factor(LR_mean_long$gender) # male/female
  combined_data$sess_id <- as.factor(LR_mean_long$sess_id) # subject list 
  str(LR_mean_long)
  
#-------- Assumtions to test for 2 way ANOVA

# 1. Normal distribution  
 # ggplot(combined_data, aes(x=FFA_OFA_Lmean)) + geom_density()


  plot<- ggline(combined_data, x="FFA_OFA_LRmean", y="group")
 

# 2. Homogenity of variance
# 3. Observations are independent ?      
# 4. Groups have same sample size           




  anovalist <- "" # create empty arrays for looping
  filelist <- ""

 

