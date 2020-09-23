#set current working directory to the one this script is in (when in RStudio)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)

library(stringi)
library(dplyr)

#load the data
mydata = read.csv("../data/parkrun_survey_data_8Sep2018.csv")

#remove participant name and running club, survey geocoordinates, and parkrun event id
remove = c("Name", "club", "LocationLatitude", "LocationLongitude", "parkrunid")
anon_dat = mydata[ , -which(names(mydata) %in% remove)]

#randomise participants' parkrun id numbers
ids = as.character(mydata$Athlete_ID)
random_ids = stri_rand_strings(n_distinct(ids), nchar(ids[1]), '[A-Z0-9]')[match(ids, unique(ids))]

#check
length(unique(ids)) == length(unique(random_ids)) 

#drop the original ID variable, and add the randomised ID to the dataset
anon_dat = anon_dat[ , -which(names(anon_dat) %in% c("Athlete_ID"))]
anon_dat$Athlete_ID = random_ids

#save the randomised data as a .csv
write.csv(anon_dat, "../data/parkrun_survey_data.csv", row.names=FALSE)


