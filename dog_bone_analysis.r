# Data cleaning/Exp data analysis "Frogs" pilot 2
#
#
# loading libraries
library(tidyverse)
library(cowplot)

# setting working directory
setwd("Pilot 2/Pilot 2 data/EF")

#reading csv files to a dataframe and putting all those dataframes in a list
all_filenames <- dir(pattern = "csv$"  ,recursive = F)

filenames_list <-list()

for(i in c(all_filenames)){
  x <- read.csv(file = i,sep = ",",header = T)
  dfname <- gsub(pattern = ".csv",replacement = "",x = i) 
  filenames_list[[dfname]] <- x
}

rm(x,dfname,i)

# now, binding each task in one dataframe per task
dog_bone <- filenames_list[grep(pattern = "child_trails",
                                x = names(filenames_list))] 


dog_bone<- bind_rows(dog_bone)

index <- map_lgl(dog_bone, ~ all(is.na(.)))
dog_bone <- dog_bone[, !index]

dog_bone <- dog_bone %>% 
  mutate(condition = 
           case_when( set>0 & (trial_id<=12| trial_id>36&trial_id<=48 )~ "dog",
                      set>0 & (trial_id>12&trial_id<=24 | trial_id>48&trial_id<=60 )~ "bone",
                      set>0 & (trial_id>24&trial_id<=36 | trial_id>60&trial_id<=72 )~ "dog-bone")
  )
# We're filtering just trials and the first trial in 0-back and 1-back, and first and second in 2-back
# trials_db <- dog_bone %>% filter(trialN>0)