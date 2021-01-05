"
Author: Arran J. Davis
Email: arran.davis@anthro.ox.ac.uk | davis.arran@gmail.com
Affiliation: Social Body Lab, Institute of Cognitive and Evolutionary Anthropology, University of Oxford
Date: 24/09/2020
"

#clean environment
rm(list= ls())

#set current working directory to the one this script is in (when in RStudio)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)

library(lubridate)
library(coda)
library(languageR)
library(lme4)
library(lmerTest)
library(e1071)
library(dplyr)
library(ggplot2)

#load the data
mydata = read.csv("../data/parkrun_survey_data.csv")

################################################################################################################################################

### VARIABLE CREATION AND TRANSFORMATIONS ###

#create age categories by taking the midpoint of the categories provided by parkrun: (18-19)(20-24)(25-29)(30-34)(35-39)(40-44)(45-49)(50-54)(55-59)(60-64)(65-69)(70-74)(75-79)
mydata$age_integer <- ifelse(mydata$age == "1", 18.5,
                             ifelse(mydata$age == "2", 22,
                                    ifelse(mydata$age == "3", 27,
                                           ifelse(mydata$age == "4", 32,
                                                  ifelse(mydata$age == "5", 37,
                                                         ifelse(mydata$age == "6", 42,
                                                                ifelse(mydata$age == "7", 47,
                                                                       ifelse(mydata$age == "8", 52,
                                                                              ifelse(mydata$age == "9", 57,
                                                                                     ifelse(mydata$age == "10", 62,
                                                                                            ifelse(mydata$age == "11", 67,
                                                                                                   ifelse(mydata$age == "12", 72,
                                                                                                          ifelse(mydata$age == "13", 77, NA)))))))))))))

#summarise mean age category (category could change over time) for each participant
age_data = mydata %>%
    group_by(Athlete_ID) %>% 
    summarise(age_in_years = mean(age_integer))

#mean,median, range, and SD in age (by the age category)
mean(age_data$age_in_years)
median(age_data$age_in_years)
sd(age_data$age_in_years)
range(age_data$age_in_years)

#table of age categories
table(mydata$age)

### ### ### ### ### ### ### ### ###

#recode the just_before variable so that: 0 = not being social before the run and 1 = equals being social before the run
mydata$just_before2 <- ifelse(mydata$just_before == "1", 0, ifelse(mydata$just_before == "3", 0, ifelse(mydata$just_before == "2", 1, NA)))

#recode the came_with variable so that 1 = came alone, 2 = came with acquaintance, and 3 = a combined category of coming with friends/family (3) or friends/family AND acquaintances (4)
mydata$came_with <- ifelse(mydata$came_with == "1", 1, ifelse(mydata$came_with == "2", 2, ifelse(mydata$came_with == "3", 3, ifelse(mydata$came_with == "4", 3, NA))))

#recode the came_with variable so that 0 = came alone (1) or with acquaintances (2) and 1 = a combined category of coming with friends/family (3) or a friends/family/acquaintances mix (4 from above)
mydata$came_with2 <- ifelse(mydata$came_with == "1", 0, ifelse(mydata$came_with == "2", 0, ifelse(mydata$came_with == "3", 1, NA)))

#create variable that indicates whether or not participants slowed down, sped up, or ran at a natural pace
mydata$slowed_spedup_natural_3 = ifelse(mydata$slowed_spedup_natural == "1", -1, ifelse(mydata$slowed_spedup_natural == "2", 1, ifelse(mydata$slowed_spedup_natural == "3", 0, ifelse(mydata$slowed_spedup_natural == "4", 0, NA))))

#log run times
mydata$time.lg = log(mydata$time)

################################################################################################################################################

### CREATE PARKRUN COMMUNITY COMPONENT ###

library("corpcor")
library("GPArotation")
library("psych")
library("coefficientalpha")

#get descriptives for the two questions that make up the factor
mean(mydata$support_felt)
sd(mydata$support_felt)
min(mydata$support_felt)
max(mydata$support_felt)

mean(mydata$community_inclusion)
sd(mydata$community_inclusion)
min(mydata$community_inclusion)
max(mydata$community_inclusion)

#create a data frame of the variables of interest
cor.dat <- mydata[,c("support_felt", "community_inclusion")]

#create a matrix
community.matrix <- cor(cor.dat)

#Bartlett's test to see if PCA is appropriate (see Field et al., 2014; p. 775)
cortest.bartlett(cor.dat)

#do a KMO (see Field et al., 2014; p. 776)
KMO(cor.dat)

#determinant of correlation matrix (see Field et al., 2014; p. 777)
det(community.matrix)

#the PCA
PCA.mod <-principal(cor.dat, scores = TRUE)
PCA.mod #h2 is communalities, SS loadings are eigenvalues
PCA.mod.final <-principal(cor.dat, nfactors = 1, rotate = "oblimin", scores = TRUE)
PCA.mod.final

#Cronbach's alpha
psych::alpha(cor.dat, warnings = FALSE)

#add scores to the main dataset
parkrun_community_factor <- cbind(mydata, PCA.mod.final$scores)
mydata$parkrun_community_factor = parkrun_community_factor$PC1

################################################################################################################################################

### GET SURVEY RESPONSE TIMES ###

#combine columns for date and time survey was sent
mydata <- transform(mydata, newcol=paste(date_ymd, survey_send, sep="  "))

#rename column
names(mydata)[names(mydata) == 'newcol'] <- 'survey_received'

#convert character string to time
Sys.setenv(TZ='Europe/London') #updates system time zone (perhaps unnecessary)
mydata$survey_received <- as.POSIXct(strptime(mydata$survey_received, "%Y-%m-%d %H:%M:%S"))
mydata$survey_end <- as.POSIXct(strptime(mydata$survey_end, "%Y-%m-%d %H:%M:%S"))
mydata$survey_begin <- as.POSIXct(strptime(mydata$survey_begin, "%Y-%m-%d %H:%M:%S"))

### TIME TO COMPLETE SURVEY (SURVEY RECIEVED TO SURVEY SUBMITTED) ###

#in minutes
mydata$time_to_respond = as.numeric(mydata$survey_end - mydata$survey_received)
mean(mydata$time_to_respond)
median(mydata$time_to_respond)
sd(mydata$time_to_respond)
range(mydata$time_to_respond)
skewness(mydata$time_to_respond)

#in hours
mydata$time_to_respond_hr = mydata$time_to_respond / 60
mean(mydata$time_to_respond_hr)
median(mydata$time_to_respond_hr)
sd(mydata$time_to_respond_hr)
range(mydata$time_to_respond_hr)
skewness(mydata$time_to_respond_hr)

#create a histogram of the time taken to submit the survey
time_to_submit <- ggplot(mydata, aes(x=time_to_respond_hr)) + 
  geom_histogram(bins = 167, alpha=0.5, position="identity", colour = "black") + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        legend.key = element_blank(), text = element_text(size = 20, family = "sans"), axis.text.y = element_blank()) +
  xlab("Time taken to submit survey (hr)") + 
  ylab("Count") +
  theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1))) +
  theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(1))) + 
  theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(1))) +
  scale_x_continuous(breaks = c(seq(0,168,24))) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())

ggsave("../outputs/time_to_submit_hist.jpg", time_to_submit, width = 10, height = 10)

#give percentage of participants that took less than one hour to respond
(1 - (table(mydata$time_to_respond < 60)[1] / sum(table(mydata$time_to_respond < 60)))) * 100

#total number of participants that took less than one hour to respond
nrow(mydata) - table(mydata$time_to_respond < 60)[1]

#percentage
(nrow(mydata) - table(mydata$time_to_respond < 60)[1]) / nrow(mydata)

#give how many people responded within two hours
nrow(subset(mydata, time_to_respond < (2*60)))
#percentage
(nrow(mydata) - table(mydata$time_to_respond < (2*60))[1]) / nrow(mydata)

#give how many people responded within one day
nrow(subset(mydata, time_to_respond < (24*60)))
#percentage
(nrow(mydata) - table(mydata$time_to_respond < (24*60))[1]) / nrow(mydata)

#give how many people responded within two days
nrow(subset(mydata, time_to_respond < (48*60)))
#percentage
(nrow(mydata) - table(mydata$time_to_respond < (48*60))[1]) / nrow(mydata)

#give how many people responded within four days
nrow(subset(mydata, time_to_respond < (96*60)))
#percentage
(nrow(mydata) - table(mydata$time_to_respond < (96*60))[1]) / nrow(mydata)

### TIME TAKEN TO FILL OUT SURVEY (SURVEY BEGIN TO SURVEY END) ###

mydata$time_to_fill_out = as.numeric((mydata$survey_end- mydata$survey_begin) / 60)

#in minutes
mean(mydata$time_to_fill_out)
sd(mydata$time_to_fill_out)
range(mydata$time_to_fill_out)
median(mydata$time_to_fill_out)

#get percent of responses that took less than 5 minutes (survey was designed to take 5 minutes)
(1 - (table(mydata$time_to_fill_out < 5)[1] / sum(table(mydata$time_to_fill_out < 10)))) * 100

#less than 10 minutes
(1 - (table(mydata$time_to_fill_out < 10)[1] / sum(table(mydata$time_to_fill_out < 5)))) * 100

#get the percentage that took more than half an hour
(table(mydata$time_to_fill_out < 30)[1] / sum(table(mydata$time_to_fill_out < 30))) * 100

#remove big outliers
table(mydata$time_to_fill_out)
q = subset(mydata, time_to_fill_out < 30)

#create a histogram
time_to_complete <- ggplot(q, aes(x=time_to_fill_out)) + 
  geom_histogram(bins = 30, alpha=0.5, position="identity", colour = "black") + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        legend.key = element_blank(), text = element_text(size = 20, family = "sans"), axis.text.y = element_blank()) +
  xlab("Time taken to complete survey (min)") + 
  ylab("Count") +
  theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1))) +
  theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(1))) + 
  theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(1))) +
  scale_x_continuous(breaks = c(seq(0,35,5))) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())

ggsave("../outputs/time_to_complete_hist.jpg", time_to_complete, width = 10, height = 10)

################################################################################################################################################

### GET THE NUMBER OF RESPONSES PER PARTICIPANT ###

total_survey_responses = mydata %>%

  #sumarise unique locations attended by each parkrunner
  group_by(Athlete_ID) %>% 
  summarize(n = n_distinct(survey_end))

table(total_survey_responses$n)

#make a histogram of this
responses_per_participant <- ggplot(total_survey_responses, aes(x=n)) + 
  geom_histogram(bins = 17, alpha=0.5, position="identity", colour = "black") + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        legend.key = element_blank(), text = element_text(size = 20, family = "sans"), axis.text.y = element_blank()) +
  xlab("Total number of survey responses") + 
  ylab("Count") +
  theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1))) +
  theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(1))) + 
  theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(1))) +
  scale_x_continuous(breaks = c(seq(0,17,1))) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())

ggsave("../outputs/responses_per_participant_hist.jpg", responses_per_participant, width = 10, height = 10)

#descriptives for number of responses per participant
mean(total_survey_responses$n)
sd(total_survey_responses$n)
range(total_survey_responses$n)
median(total_survey_responses$n)
skewness(total_survey_responses$n)

################################################################################################################################################

### GENDER DESCRIPTIVES ###

#subset one row per participant
one_run_per <- mydata %>% group_by(Athlete_ID) %>% sample_n(1)

#percent of female participants
table(one_run_per$gender)[2] / (sum(table(one_run_per$gender)))

#percentage of total responses filled out by females
table(mydata$gender)[2] / (sum(table(mydata$gender)))

### 5 KM RUN TIME DESCRIPTIVES ###

#get run time skew, mean, SD, and range
skewness(mydata$time)
mean(mydata$time)
sd(mydata$time)
range(mydata$time)

#make a histogram
run_time_hist = ggplot(mydata, aes(x=time)) + 
  geom_histogram(bins = 35, alpha=0.5, position="identity", colour = "black") + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        legend.key = element_blank(), text = element_text(size = 20, family = "sans"), axis.text.y = element_blank()) +
  xlab("5 km run time (min)") + 
  ylab("Count") +
  theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1))) +
  theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(1))) + 
  theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(1))) +
  scale_x_continuous(breaks = c(seq(15,60,5))) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())

ggsave("../outputs/run_time_hist.jpg", run_time_hist, width = 10, height = 10)

################################################################################################################################################

### RPE AND FATIGUE RELATIONSHIP ###

#correlation between RPE and fatigue
cor(mydata$exertion, mydata$fatigued)

#mean, sd, and range ofg RPE variable
mean(mydata$exertion + 5)
sd(mydata$exertion + 5)
range(mydata$exertion + 5)

#make a histogram of this
rpe_responses <- ggplot(mydata, aes(x=exertion + 5)) + 
  geom_histogram(bins = 15, alpha=0.5, position="identity", colour = "black") + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        legend.key = element_blank(), text = element_text(size = 20, family = "sans"), axis.text.y = element_blank()) +
  xlab("Rate of perceived exertion (RPE)") + 
  ylab("Count") +
  theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1))) +
  theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(1))) + 
  theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(1))) +
  scale_x_continuous(breaks = c(seq(6,20,1))) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())

ggsave("../outputs/rpe_responses_hist.jpg", rpe_responses, width = 10, height = 10)

################################################################################################################################################

### DESCRIPTIVE STATISTICS TABLE FOR NON-SOCIAL VARIABLES ###

#create the table
library(officer)
library(flextable)
library(magrittr)

#create a data frame of the variables of interest
non_social_dat = as.data.frame(mydata[, c("fatigued",
                                          "pain_of_fatigue",
                                          "enjoyment",
                                          "how_energising")])

#get the total number of responses for the 'pain_of_fatigue' variable
sum(table(mydata$pain_of_fatigue, useNA = "no"))

#get the percentage of times that participants answered the 'fatigued' question with a 5 or more (leading them to be asked the 'pain_of_fatigue' question)
(sum(table(mydata$pain_of_fatigue, useNA = "no")) / nrow(mydata)) * 100

#summarise the main variables
non_social_dat_mean_summary = as.data.frame(t(non_social_dat %>% summarise_all(funs(mean), na.rm = TRUE)))
non_social_dat_sd_summary = as.data.frame(t(non_social_dat %>% summarise_all(funs(sd), na.rm = TRUE)))
non_social_dat_min_summary = as.data.frame(t(non_social_dat %>% summarise_all(funs(min), na.rm = TRUE)))
non_social_dat_max_summary = as.data.frame(t(non_social_dat %>% summarise_all(funs(max), na.rm = TRUE)))

#create an empty data frame with each question type
non_social_dat_summary = data.frame(matrix(NA, nrow = 4))
non_social_dat_summary$Question =  c("How physically fatigued did you feel during your run today?",
                                   "How physically painful did this fatigue feel?",
                                   "How much did you enjoy your run today?",
                                   "How energising did it feel to be with the other parkrunners today?")

#add each summary to the dataset, and drop the original NA column
non_social_dat_summary$Mean = round(non_social_dat_mean_summary$V1, 3)
non_social_dat_summary$SD = round(non_social_dat_sd_summary$V1, 3)
non_social_dat_summary$Minimum = non_social_dat_min_summary$V1
non_social_dat_summary$Maximum = non_social_dat_max_summary$V1

#create a range variable
non_social_dat_summary$Range = paste(non_social_dat_summary$Minimum, non_social_dat_summary$Maximum, sep = " - ")

#drop unnecessary variables
non_social_dat_summary = non_social_dat_summary[ , c("Question","Mean", "SD", "Range")]

#create flextable object
ft <- flextable(data = non_social_dat_summary) %>% theme_zebra %>% autofit

#see flextable in RStudio viewer
ft

#create a temporary file
tmp <- tempfile(fileext = "wellbeing_t4_summary.docx")

#create a 'docx' file
read_docx() %>% 
  body_add_flextable(ft) %>% 
  print(target = tmp)

#open Word document
browseURL(tmp)

#get the count of runs where participants reported speeding up or slowing down to run with their running partners
table(mydata$slowed_spedup_natural)

#percentage slowed down to run with their running partner
(table(mydata$slowed_spedup_natural)[1] / sum(table(mydata$slowed_spedup_natural))) * 100

#percentage sped up to run with their running partner
(table(mydata$slowed_spedup_natural)[2] / sum(table(mydata$slowed_spedup_natural))) * 100

#percentage who ran at natural pace with their running partner
(table(mydata$slowed_spedup_natural)[3] / sum(table(mydata$slowed_spedup_natural))) * 100

#percentage who ran on their own 
(table(mydata$slowed_spedup_natural)[4] / sum(table(mydata$slowed_spedup_natural))) * 100

################################################################################################################################################

### DESCRIPTIVE STATISTICS OF SURVEY QUESTIONS ###

#motivations for attending parkrun (1 = "to improve my ranking"; 2 = "to improve my time; 3 = "to run together with other people")
table(mydata$ranking_time_people)

#in percentages
(table(mydata$ranking_time_people)[1] / sum(table(mydata$ranking_time_people))) * 100
(table(mydata$ranking_time_people)[2] / sum(table(mydata$ranking_time_people))) * 100
(table(mydata$ranking_time_people)[3] / sum(table(mydata$ranking_time_people))) * 100

#who people ran with (1 = "on my own", 2 = "alongside one or more acquaintances", 3 = "alongside one or more friends/family members", 4 = "alongside a mix of acquaintances and friends/family members")
table(mydata$run_description)

#in percentages
(table(mydata$run_description)[1] / sum(table(mydata$run_description))) * 100
(table(mydata$run_description)[2] / sum(table(mydata$run_description))) * 100
(table(mydata$run_description)[3] / sum(table(mydata$run_description))) * 100
(table(mydata$run_description)[4] / sum(table(mydata$run_description))) * 100

#who participants came or met up with (1 = "nobody else"; 2 = "one or more acquaintances"; 3 = "one or more friends/family members or a mix of acquaintances and friends/family members")
table(mydata$came_with)
table(mydata$came_with2)

#in percentages
(table(mydata$came_with)[1] / sum(table(mydata$came_with))) * 100
(table(mydata$came_with)[2] / sum(table(mydata$came_with))) * 100
(table(mydata$came_with)[3] / sum(table(mydata$came_with))) * 100

#percentages for the recoded, binary variable
(table(mydata$came_with2)[1] / sum(table(mydata$came_with2))) * 100
(table(mydata$came_with2)[2] / sum(table(mydata$came_with2))) * 100

#pre-run sociality
table(mydata$just_before)
table(mydata$just_before2)

#in percentages
(table(mydata$just_before)[1] / sum(table(mydata$just_before))) * 100
(table(mydata$just_before)[2] / sum(table(mydata$just_before))) * 100
(table(mydata$just_before)[3] / sum(table(mydata$just_before))) * 100

#percentages for the recoded, binary variable
(table(mydata$just_before2)[1] / sum(table(mydata$just_before2))) * 100
(table(mydata$just_before2)[2] / sum(table(mydata$just_before2))) * 100

################################################################################################################################################

### EFFECT OF RESPONSE LAG ON OUTCOME-RELEVANT VARIABLES ###

library(lmerTest)
library(MuMIn)

#test whether response lag times predict responses to any of the survey questions
time.fatigue = lmer(fatigued ~ 1 + log(time_to_respond) + (1 + log(time_to_respond) | Athlete_ID), data = mydata)
summary(time.fatigue)
r.squaredGLMM(time.fatigue)

time.energy = lmer(how_energising ~ 1 + log(time_to_respond) + (1 + log(time_to_respond) | Athlete_ID), data = mydata)
summary(time.energy)
r.squaredGLMM(time.energy)

time.enjoyment = lmer(enjoyment ~ 1 + log(time_to_respond) + (1 + log(time_to_respond) | Athlete_ID), data = mydata)
summary(time.enjoyment)
r.squaredGLMM(time.enjoyment)

time.pcc = lmer(parkrun_community_factor ~ 1 + log(time_to_respond) + (1 + log(time_to_respond) | Athlete_ID), data = mydata)
summary(time.pcc)
r.squaredGLMM(time.pcc)

################################################################################################################################################

### DESCRIPTIVES FOR HIGH AND LOW RESPONDERS ###

#create a variable that is the total number of survey responses for each participant
mydata$total_responses = ave(mydata$Survey_number, mydata[,c("Athlete_ID")], FUN=length)

#get the mean and standard deviation
mean(mydata$total_responses)

#get the number of responses by participant needed to make up at least 50% of the dataset
for (i in seq(min(mydata$total_responses), max(mydata$total_responses))){
  sub_dat = subset(mydata, total_responses >= i)
  percent_total = (nrow(sub_dat)/ nrow(mydata)) * 100
  print(paste("Minimum survey responses per participant:", i, ", Total responses in resulting dataset:", nrow(sub_dat),
              ", Number of participants in resulting dataset", length(unique(sub_dat$Athlete_ID)), ", Percent of total responses:", percent_total))
}

### ### ###

#participants who responded nine or more times made up over half of the dataset
high_responders = subset(mydata, total_responses >= 9)
low_responders = subset(mydata, total_responses < 9)

#count and percentage of participants and responses in each category
nrow(high_responders)
(nrow(high_responders)/nrow(mydata)) *100
length(unique(high_responders$Athlete_ID))
(length(unique(high_responders$Athlete_ID)) / length(unique(mydata$Athlete_ID))) * 100

nrow(low_responders)
(nrow(low_responders)/nrow(mydata)) *100
length(unique(low_responders$Athlete_ID))
(length(unique(low_responders$Athlete_ID)) / length(unique(mydata$Athlete_ID))) * 100

### ### ###

#get the age make-up of high responders
age_data_h <- high_responders %>%
  group_by(Athlete_ID) %>% 
  summarise(n = mean(age_integer))

#mean age for high responders
mean(age_data_h$n)
sd(age_data_h$n)

#get the age make-up of low responders
age_data_l <- low_responders %>%
  group_by(Athlete_ID) %>% 
  summarise(n = mean(age_integer))

#mean age for high responders
mean(age_data_l$n)
sd(age_data_l$n)

#compare the distributions
t.test(age_data_h$n, age_data_l$n)

### ### ###

#drop duplicates from high and low responder datasets
unique_high = high_responders[!rev(duplicated(rev(high_responders$Athlete_ID))),]
unique_low = low_responders[!rev(duplicated(rev(low_responders$Athlete_ID))),]

#get the percentage female in each dataset
(table(unique_high$gender)['W'] / sum(table(unique_high$gender))) * 100
(table(unique_low$gender)['W'] / sum(table(unique_low$gender))) * 100

#test for differences
prop.test(x = c(table(unique_high$gender)['W'], table(unique_low$gender)['W']), n = c(sum(table(unique_high$gender)), sum(table(unique_low$gender))))

### ### ###

#test for differences in the parkrun community component
mean(high_responders$parkrun_community_factor)
sd(high_responders$parkrun_community_factor)

mean(low_responders$parkrun_community_factor)
sd(low_responders$parkrun_community_factor)

t.test(high_responders$parkrun_community_factor, low_responders$parkrun_community_factor)

#test for differences in who participants came with
(table(high_responders$came_with2)['0'] / sum(table(high_responders$came_with2))) * 100
(table(low_responders$came_with2)['0'] / sum(table(high_responders$came_with2))) * 100

prop.test(x = c(table(high_responders$came_with2)['0'], table(low_responders$came_with2)['0']), n = c(sum(table(high_responders$came_with2)), sum(table(low_responders$came_with2))))

#test for differences in pre-run sociality
(table(high_responders$just_before2)['1'] / sum(table(high_responders$just_before2))) * 100
(table(low_responders$just_before2)['1'] / sum(table(low_responders$just_before2))) * 100

prop.test(x = c(table(high_responders$just_before2)['1'], table(low_responders$just_before2)['1']), n = c(sum(table(high_responders$just_before2)), sum(table(low_responders$just_before2))))

#test for differences in felt energy
mean(high_responders$how_energising)
sd(high_responders$how_energising)

mean(low_responders$how_energising, na.rm = TRUE)
sd(low_responders$how_energising, na.rm = TRUE)

t.test(high_responders$how_energising, low_responders$how_energising)

#test for differences in fatigue
mean(high_responders$fatigued)
sd(high_responders$fatigued)

mean(low_responders$fatigued)
sd(low_responders$fatigued)

t.test(high_responders$fatigued, low_responders$fatigued)

#test for differences in 5 km run times
mean(high_responders$time)
sd(high_responders$time)

mean(low_responders$time)
sd(low_responders$time)

t.test(high_responders$time.lg, low_responders$time.lg)

################################################################################################################################################

### MULTILEVEL MODELS ON RPE ###

#test the effects of each of the social predictor variables on RPE
RPE_1 = lmer(exertion ~ 1 + parkrun_community_factor + (1 | Athlete_ID), data = mydata)
summary(RPE_1)
r.squaredGLMM(RPE_1)

RPE_2 = lmer(exertion ~ 1 + came_with2 + (1 + came_with2 | Athlete_ID), data = mydata)
summary(RPE_2)
r.squaredGLMM(RPE_2)

RPE_3 = lmer(exertion ~ 1 + just_before2 + (1 | Athlete_ID), data = mydata)
summary(RPE_3)
r.squaredGLMM(RPE_3)

################################################################################################################################################

### MULTILEVEL MODELS ON FATIGUE LEVELS ###

library(lmerTest)
library(MuMIn)

#make binary predictors factors
mydata$came_with2 = as.factor(mydata$came_with2)
mydata$just_before2 = as.factor(mydata$just_before2)

### ### ###

#model for Hypothesis 1.1
hypothesis_1.1 = lmer(fatigued ~ 1 + parkrun_community_factor + (1 + parkrun_community_factor | Athlete_ID), data = mydata)
summary(hypothesis_1.1)
r.squaredGLMM(hypothesis_1.1)

#model for Hypothesis 1.2
hypothesis_1.2 = lmer(fatigued ~ 1 + came_with2 + (1 + came_with2 | Athlete_ID), data = mydata)
summary(hypothesis_1.2)
--r.squaredGLMM(hypothesis_1.2)

#model for Hypothesis 1.3
hypothesis_1.3 = lmer(fatigued ~ 1 + just_before2 + (1 | Athlete_ID), data = mydata)
summary(hypothesis_1.3)
r.squaredGLMM(hypothesis_1.3)

################################################################################################################################################

### MULTILEVEL MODELS ON ENERGY LEVELS ###

#model for Hypothesis 2.1
hypothesis_2.1 = lmer(how_energising ~ 1 + parkrun_community_factor + (1| Athlete_ID), data = mydata)
summary(hypothesis_2.1)
r.squaredGLMM(hypothesis_2.1)

#model for Hypothesis 2.2
hypothesis_2.2 = lmer(how_energising ~ 1 + came_with2 +  (1 | Athlete_ID), data = mydata)
summary(hypothesis_2.2)
r.squaredGLMM(hypothesis_2.2)

#model for Hypothesis 2.3
hypothesis_2.3 = lmer(how_energising ~ 1 + just_before2 + (1 + just_before2 | Athlete_ID), data = mydata)
summary(hypothesis_2.3)
r.squaredGLMM(hypothesis_2.3)

################################################################################################################################################

### MAIN MULTILEVEL MODELS ON ENJOYMENT LEVELS ###

#model for Hypothesis 3.1
hypothesis_3.1 = lmer(enjoyment ~ 1 + parkrun_community_factor + (1| Athlete_ID), data = mydata)
summary(hypothesis_3.1)
r.squaredGLMM(hypothesis_3.1)

#model for Hypothesis 3.2
hypothesis_3.2 = lmer(enjoyment ~ 1 + came_with2 + (1 | Athlete_ID), data = mydata)
summary(hypothesis_3.2)
r.squaredGLMM(hypothesis_3.2)

#model for Hypothesis 3.3
hypothesis_3.3 = lmer(enjoyment ~ 1 + just_before2 + (1 + just_before2 | Athlete_ID), data = mydata)
summary(hypothesis_3.3)
r.squaredGLMM(hypothesis_3.3)

################################################################################################################################################

### MAIN MULTILEVEL MODELS ON 5KM RUN TIMES ###

#model for Hypothesis 4.1
hypothesis_4.1 = lmer(time.lg ~ 1 + parkrun_community_factor + slowed_spedup_natural_3 + (1 + parkrun_community_factor | Athlete_ID), data = mydata)
summary(hypothesis_4.1)
r.squaredGLMM(hypothesis_4.1)

#model for Hypothesis 4.2
hypothesis_4.2 = lmer(time.lg ~ 1 + came_with2 + slowed_spedup_natural_3 + (1 + came_with2 | Athlete_ID), data = mydata)
summary(hypothesis_4.2)
r.squaredGLMM(hypothesis_4.2)

#model for Hypothesis 4.3
hypothesis_4.3 = lmer(time.lg ~ 1 + just_before2 + slowed_spedup_natural_3 + (1 + just_before2 | Athlete_ID), data = mydata)
summary(hypothesis_4.3)
r.squaredGLMM(hypothesis_4.3)

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS ###

library(mediation)
set.seed(2014)
detach("package:lmerTest", unload=TRUE)

#make binary predictors integers (a requirement of the mediate package)
mydata$came_with2_int = as.integer(mydata$came_with2)
mydata$just_before2_int = as.integer(mydata$just_before2)

################################################################################################################################################

### HYPOTHESIS 5.1 ###

#mediation relationship: parkrun_community_factor -> how_energising -> time

med_fit_5.1 = lmer(how_energising ~ parkrun_community_factor + slowed_spedup_natural_3 + (1 + parkrun_community_factor | Athlete_ID), data = mydata)
summary(med_fit_5.1)
r.squaredGLMM(med_fit_5.1)

out_fit_5.1 = lmer(time.lg ~ how_energising + parkrun_community_factor + slowed_spedup_natural_3 + (1 + parkrun_community_factor | Athlete_ID), data = mydata)
summary(out_fit_5.1)
r.squaredGLMM(out_fit_5.1)

med_out_5.1 = mediate(med_fit_5.1, out_fit_5.1, treat = "parkrun_community_factor", mediator = "how_energising", covariates = "slowed_spedup_natural_3", sims = 1000, boot.ci.type = "bca")
summary(med_out_5.1)

################################################################################################################################################

### HYPOTHESIS 5.2 ###

#mediation relationship: came_with2 -> how_energising -> time

#more complex random effects structures failed to converge
med_fit_5.2 = lmer(how_energising ~ came_with2_int + slowed_spedup_natural_3 + (1 | Athlete_ID), data = mydata)
summary(med_fit_5.2)
r.squaredGLMM(med_fit_5.2)

out_fit_5.2 = lmer(time.lg ~ how_energising + came_with2_int + slowed_spedup_natural_3 + (1 + came_with2_int| Athlete_ID), data = mydata) # more complex random effects structures don't converge
summary(out_fit_5.2)
r.squaredGLMM(out_fit_5.2)

med_out_5.2 = mediate(med_fit_5.2, out_fit_5.2, treat = "came_with2_int", mediator = "how_energising", covariates = "slowed_spedup_natural_3", sims = 1000, boot.ci.type = "bca")
summary(med_out_5.2)

################################################################################################################################################

### HYPOTHESIS 5.3 ###

#mediation relationship: just_before2 -> how_energising -> time

med_fit_5.3 = lmer(how_energising ~ just_before2_int + slowed_spedup_natural_3 +  (1 + just_before2_int | Athlete_ID), data = mydata)
summary(med_fit_5.3)
r.squaredGLMM(med_fit_5.3)

out_fit_5.3 = lmer(time.lg ~ how_energising + just_before2_int + slowed_spedup_natural_3 + (1 + just_before2_int | Athlete_ID), data = mydata)
summary(out_fit_5.3)
r.squaredGLMM(out_fit_5.3)

med_out_5.3 <- mediate(med_fit_5.3, out_fit_5.3, treat = "just_before2_int", mediator = "how_energising", covariates = "slowed_spedup_natural_3", sims = 1000, boot.ci.type = "bca")
summary(med_out_5.3)

################################################################################################################################################

### HYPOTHESIS 6.1 ###

#mediation relationship: parkrun_community_factor -> fatigued -> time

med_fit_6.1 = lmer(fatigued ~ parkrun_community_factor + slowed_spedup_natural_3 + (1 + parkrun_community_factor | Athlete_ID), data = mydata)
summary(med_fit_6.1)
r.squaredGLMM(med_fit_6.1)

out_fit_6.1 = lmer(time.lg ~ fatigued + parkrun_community_factor + slowed_spedup_natural_3 + (1 + parkrun_community_factor | Athlete_ID), data = mydata)
summary(out_fit_6.1)
r.squaredGLMM(out_fit_6.1)

med_out_6.1 <- mediate(med_fit_6.1, out_fit_6.1, treat = "parkrun_community_factor", mediator = "fatigued", covariates = "slowed_spedup_natural_3", sims = 1000, boot.ci.type = "bca")
summary(med_out_6.1)

################################################################################################################################################

### HYPOTHESIS 6.2 ###

#mediation relationship: came_with2 -> fatigue -> time

med_fit_6.2 = lmer(fatigued ~ came_with2_int + slowed_spedup_natural_3 + (1 + came_with2_int | Athlete_ID), data = mydata)
summary(med_fit_6.2)
r.squaredGLMM(med_fit_6.2)

out_fit_6.2 = lmer(time.lg ~ fatigued + came_with2_int + slowed_spedup_natural_3 + (1 + came_with2_int| Athlete_ID), data = mydata) # more complex random effects structures don't converge
summary(out_fit_6.2)
r.squaredGLMM(out_fit_6.2)

med_out_6.2 <- mediate(med_fit_6.2, out_fit_6.2, treat = "came_with2_int", mediator = "fatigued", covariates = "slowed_spedup_natural_3", sims = 1000, boot.ci.type = "bca")
summary(med_out_6.2)

################################################################################################################################################

### HYPOTHESIS 6.3 ###

#mediation relationship: just_before2 -> fatigued -> time

#more complex random effects structures failed to converge
med_fit_6.3 = lmer(fatigued ~ just_before2_int + slowed_spedup_natural_3 + (1 | Athlete_ID), data = mydata)
summary(med_fit_6.3)
r.squaredGLMM(med_fit_6.3)

#more complex random effects structures failed to converge
out_fit_6.3 = lmer(time.lg ~ fatigued + just_before2_int + slowed_spedup_natural_3 + (1 | Athlete_ID), data = mydata)
summary(out_fit_6.3)
r.squaredGLMM(out_fit_6.3)

med_out_6.3 = mediate(med_fit_6.3, out_fit_6.3, treat = "just_before2_int", mediator = "fatigued", covariates = "slowed_spedup_natural_3", sims = 1000, boot.ci.type = "bca")
summary(med_out_6.3)

################################################################################################################################################

### IMPROVEMENTS IN RUN TIMES ###

#indirect effect of (-0.002)
(mean(mydata$time) * 60 ) * .002

#indirect effect of (-0.007)
(mean(mydata$time) * 60 ) * .007

################################################################################################################################################

### GRAPHS ###

#violin plot of came_with2 and how_energised
energy_by_came_with = ggplot(mydata, aes(x = came_with2, y = how_energising, grouping(came_with2))) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  xlab("Came or met up with family and/or friends") +
  ylab("Perceived energy") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) + 
  scale_y_continuous(breaks = pretty(mydata$how_energising, n = 7)) +
  theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1.75), margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1.75), margin = margin(t = 0, r = 5, b = 0, l = 0))) +
  theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(1.75))) + 
  theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(1.75))) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))
  
ggsave("../outputs/energy_by_came_with.jpg", energy_by_came_with, width = 10, height = 10)

### ### ### ### ### ### ### ### ###

#violin plot of just_before2 and how_energised
energy_by_just_before = ggplot(mydata, aes(x = just_before2, y = how_energising, grouping(just_before2))) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  xlab("Pre-run sociality") +
  ylab("Perceived energy") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) + 
  scale_y_continuous(breaks = pretty(mydata$how_energising, n = 7)) +
  theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1.75), margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1.75), margin = margin(t = 0, r = 5, b = 0, l = 0))) +
  theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(1.75))) + 
  theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(1.75))) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))

ggsave("../outputs/energy_by_just_before.jpg", energy_by_just_before, width = 10, height = 10)

### ### ### ### ### ### ### ### ###

#regression line
mod1 = lm(how_energising ~ 1 +  parkrun_community_factor, data = mydata)

#intercept
int = mod1$coefficients[1]
slope = mod1$coefficients[2]

#scatter plot of parkrun_community_factor and how_energising
energy_by_community_factor = ggplot(mydata, aes(x = parkrun_community_factor, y = how_energising)) +
  #geom_point(alpha = 0.3) +    #this makes dots density-specific
  geom_count() +
  geom_abline(intercept = int, slope = slope) +
  xlab("parkrun community component") +
  ylab("Perceived energy") +
  scale_y_continuous(breaks = pretty(mydata$how_energising, n = 7)) +
  theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1.75), margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1.75), margin = margin(t = 0, r = 5, b = 0, l = 0))) +
  theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(1.75))) + 
  theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(1.75))) +
  theme(legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2), legend.title = element_text(face = "bold"))

energy_by_community_factor$labels$size = "Count"
energy_by_community_factor
  
ggsave("../outputs/energy_by_community_component.jpg", energy_by_community_factor, width = 10, height = 10)

### ### ### ### ### ### ### ### ###

#regression line
mod2 = lm(time ~ 1 +  how_energising, data = mydata)

#intercept
int = mod2$coefficients[1]
slope = mod2$coefficients[2]

#scatter plot of how_energising and run times
run_times_by_energy = ggplot(mydata, aes(x = how_energising, y = time)) +
  geom_point(alpha = 0.3) +    #this makes dots density-specific
  geom_abline(intercept = int, slope = slope) + #this is the lmer regression line of time on how_energising (with no x covariates and random slopes)
    xlab("Perceived energy") +
    ylab("5 km run time (min)") +
    scale_x_continuous(breaks = pretty(mydata$how_energising, n = 7)) +
    scale_y_continuous(limits = c(15,40), breaks = c(15,20,25,30,35,40)) +
    theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1.75), margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1.75), margin = margin(t = 0, r = 5, b = 0, l = 0))) +
    theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(1.75))) + 
    theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(1.75))) +
    theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))

ggsave("../outputs/run_times_by_energy.jpg", run_times_by_energy, width = 10, height = 10)

################################################################################################################################################

### ASSUMPTION CHECKS ###

#load local functions adapted from the R script provided by Snijders & Bosker (2012); https://www.stats.ox.ac.uk/~snijders/ch10.r
source("parkrun_survey_analysis_assumption_checks.R")

################################################################################################################################################

### MULTILEVEL MODELS ON FATIGUE LEVELS (HYPOTHESIS 1.1 - HYPOTHESIS 1.3) ###

#multilevel models on fatigue levels - variable names
fatigue_mlms = c(hypothesis_1.1, hypothesis_1.2, hypothesis_1.3)
fatigue_mlm_dir_names = c("hypothesis_1.1", "hypothesis_1.2", "hypothesis_1.3")
predictors = c("parkrun_community_factor", "came_with2", "just_before2")
predictors_integers = c("parkrun_community_factor", "came_with2_int", "just_before2_int")
predictor_names = c("parkrun community component", "Who participants came or met up with", "Pre-run sociality")
outcome = "fatigued"
level_two_variable = "Athlete_ID"
level_one_variable = "time.lg"

#do assumption checks for each model
check_assumptions(code_dir, fatigue_mlms, fatigue_mlm_dir_names, predictors, predictors_integers, predictor_names, outcome, level_two_variable, level_one_variable, mydata)

################################################################################################################################################

### MULTILEVEL MODELS ON ENERGY LEVELS (HYPOTHESIS 2.1 - HYPOTHESIS 2.3) ###

#first drop the lone survey with an 'NA' value for the energy variable
subdat = mydata[complete.cases(mydata[ , "how_energising"]),]

#multilevel models on energy levels - variable names
energy_mlms = c(hypothesis_2.1, hypothesis_2.2, hypothesis_2.3)
energy_mlm_dir_names = c("hypothesis_2.1", "hypothesis_2.2", "hypothesis_2.3")
predictors = c("parkrun_community_factor", "came_with2", "just_before2")
predictors_integers = c("parkrun_community_factor", "came_with2_int", "just_before2_int")
predictor_names = c("parkrun community component", "Who participants came or met up with", "Pre-run sociality")
outcome = "how_energising"
level_two_variable = "Athlete_ID"
level_one_variable = "time.lg"

#do assumption checks for each model
check_assumptions(code_dir, energy_mlms, energy_mlm_dir_names, predictors, predictors_integers, predictor_names, outcome, level_two_variable, level_one_variable, subdat)

################################################################################################################################################

### MAIN MULTILEVEL MODELS ON ENJOYMENT LEVELS (HYPOTHESIS 3.1 - HYPOTHESIS 3.3) ###

#multilevel models on energy levels - variable names
enjoyment_mlms = c(hypothesis_3.1, hypothesis_3.2, hypothesis_3.3)
enjoyment_mlm_dir_names = c("hypothesis_3.1", "hypothesis_3.2", "hypothesis_3.3")
predictors = c("parkrun_community_factor", "came_with2", "just_before2")
predictors_integers = c("parkrun_community_factor", "came_with2_int", "just_before2_int")
predictor_names = c("parkrun community component", "Who participants came or met up with", "Pre-run sociality")
outcome = "enjoyment"
level_two_variable = "Athlete_ID"
level_one_variable = "time.lg"

#do assumption checks for each model
check_assumptions(code_dir, enjoyment_mlms, enjoyment_mlm_dir_names, predictors, predictors_integers, predictor_names, outcome, level_two_variable, level_one_variable, mydata)

################################################################################################################################################

### MAIN MULTILEVEL MODELS ON 5KM RUN TIMES (HYPOTHESIS 4.1 - HYPOTHESIS 4.3) ###

#multilevel models on 5 km run times - variable names
runtime_mlms = c(hypothesis_4.1, hypothesis_4.2, hypothesis_4.3)
runtime_mlm_dir_names = c("hypothesis_4.1", "hypothesis_4.2", "hypothesis_4.3")
predictors = c("parkrun_community_factor", "came_with2", "just_before2")
predictors_integers = c("parkrun_community_factor", "came_with2_int", "just_before2_int")
predictor_names = c("parkrun community component", "Who participants came or met up with", "Pre-run sociality")
outcome = "time.lg"
level_two_variable = "Athlete_ID"
level_one_variable = "slowed_spedup_natural_3"

#do assumption checks for each model
check_assumptions(code_dir, runtime_mlms, runtime_mlm_dir_names, predictors, predictors_integers, predictor_names, outcome, level_two_variable, level_one_variable, mydata)

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 5.1) ###

#create a directory for all the results from the multilevel mediation analysis, set the directory to that
setwd(code_dir)
mediation_dir_name = 'hypothesis_5.1'
all_dirs = unlist(strsplit(code_dir, "/"))
all_dirs = all_dirs[1:(length(all_dirs) - 1)]
mlmm_dir = paste0(paste(all_dirs[1:length(all_dirs)], collapse = "/"), "/outputs/", mediation_dir_name)
dir.create(mlmm_dir)
setwd(mlmm_dir)

#first drop the lone survey with an 'NA' value for the energy variable
subdat = mydata[complete.cases(mydata[ , "how_energising"]),]

#mediator as outcome - variable names
med_mlms_5.1 = c(med_fit_5.1)
med_mlm_dir_names_5.1 = c("med_fit_5.1")
predictor = c("parkrun_community_factor")
predictor_integer = c("parkrun_community_factor")
predictor_name = c("parkrun community component")
outcome = "how_energising"
level_two_variable = "Athlete_ID"
level_one_variable = "slowed_spedup_natural_3"

#do assumption checks for the model
check_mediation_assumptions(mlmm_dir, med_mlms_5.1, med_mlm_dir_names_5.1, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, subdat)

### ### ###

#find extreme outlier for level-one heteroscedasticity
comp.models = level_one_heteroscedasticity(predictor, predictor_name, outcome, level_two_variable, subdat)

#intercept coefficients from olselist
frame1_int_olselist = as.data.frame(comp.models[, 1, 1])
colnames(frame1_int_olselist)[1] = "Estimate"
frame1_int_olselist$coefficient = "Intercept"
frame1_int_olselist$Model = "OLS regression"
frame1_int_olselist$level_two_unit = rownames(frame1_int_olselist)

#intercept coefficients from lmer
frame1_int_lme = as.data.frame(comp.models[, 2, 1])
colnames(frame1_int_lme)[1] = "Estimate" 
frame1_int_lme$coefficient = "Intercept"
frame1_int_lme$Model = "Multilevel model"
frame1_int_lme$level_two_unit = rownames(frame1_int_lme)

#predictor coefficients from olselist
frame2_pred_olselist = as.data.frame(comp.models[, 1, 2])
colnames(frame2_pred_olselist)[1] = "Estimate"
frame2_pred_olselist$coefficient = "Predictor"
frame2_pred_olselist$Model = "OLS regression"
frame2_pred_olselist$level_two_unit = rownames(frame2_pred_olselist)

#intercept coefficients from lmer
frame2_pred_lme = as.data.frame(comp.models[, 2, 2])
colnames(frame2_pred_lme)[1] = "Estimate" 
frame2_pred_lme$coefficient = "Predictor"
frame2_pred_lme$Model = "Multilevel model"
frame2_pred_lme$level_two_unit = rownames(frame2_pred_lme)

#combine and create row name variable
new_frame = do.call("rbind", list(frame1_int_olselist, frame1_int_lme, frame2_pred_olselist, frame2_pred_lme)) 

#extract the outlying participant, observe their data
ord_frame = new_frame[ order(-new_frame$Estimate), ]
outlier = ord_frame$level_two_unit[1]
outlier_data = subset(subdat, subdat$Athlete_ID == outlier) #there is nothing out of the ordinary

#remove the participant from the graph (for visualisation purposes)
no_outlier = subset(new_frame, new_frame$level_two_unit != outlier)

#create list and function from renaming predictor variable
variable_names <- list(
  'Intercept'="Intercept",
  'Predictor'= predictor_name
) 

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

#make the graph
graph = ggplot(no_outlier, aes(x=Estimate, y=level_two_unit, group=Model)) + 
  geom_point(aes(colour=Model), na.rm = TRUE) + 
  suppressWarnings(facet_grid(. ~ coefficient, scales = 'free', labeller=variable_labeller)) +
  ylab("Level-two unit") +
  theme(legend.key = element_rect(fill = "transparent", colour = "transparent"),
        axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family="Arial", colour="black", size=rel(1)),
        legend.title = element_text(family="Arial", face="bold", colour="black", size=rel(1)),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(family="Arial", face="bold", colour="black", size=rel(1)))

#make the plots
jpeg('visualisation_for_level_one_heteroscedasticity_no_outlier.jpg', width = 15, height = 10, units = "in",res = 300, pointsize = 12, quality = 100)
plot(graph)
dev.off()

### ### ###

#level-one homoscedasticity outliers (those not close to the mean coefficient estimate)
pred_cutoff = sd(no_outlier[no_outlier$coefficient == "Predictor","Estimate"], na.rm = TRUE) * 3
pred_mean = mean(no_outlier[no_outlier$coefficient == "Predictor","Estimate"], na.rm = TRUE)
get_outliers(comp.models, predictor, pred_mean, pred_cutoff, level_two_variable, subdat)

### ### ###

#mediator as covariate predicting outcome - variable names
out_mlms_5.1 = c(out_fit_5.1)
out_mlm_dir_names_5.1 = c("out_fit_5.1")
predictor = c("parkrun_community_factor")
predictor_integer = c("parkrun_community_factor")
predictor_name = c("parkrun community component")
outcome = "time.lg"
level_two_variable = "Athlete_ID"
level_one_variable = "how_energising"

#do assumption checks for the model
check_mediation_assumptions(mlmm_dir, out_mlms_5.1, out_mlm_dir_names_5.1, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, subdat)

#check the assumptions of the multilevel mediation mediation analysis (first create and set an output directory for the mediation assumption test)
med_dir = file.path(mlmm_dir, 'mediation')
dir.create(med_dir)
setwd(med_dir)

#variable names
main_predictor = "parkrun_community_factor"
covariate = "slowed_spedup_natural_3"
mediator = "how_energising"
level_two_variable = "Athlete_ID"
outcome = "time.lg"

sequential_ignorability(main_predictor, covariate, mediator, outcome, level_two_variable, subdat)

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 5.2) ###

#create a directory for all the results from the multilevel mediation analysis, set the directory to that
setwd(code_dir)
mediation_dir_name = 'hypothesis_5.2'
all_dirs = unlist(strsplit(code_dir, "/"))
all_dirs = all_dirs[1:(length(all_dirs) - 1)]
mlmm_dir = paste0(paste(all_dirs[1:length(all_dirs)], collapse = "/"), "/outputs/", mediation_dir_name)
dir.create(mlmm_dir)
setwd(mlmm_dir)

#first drop the lone survey with an 'NA' value for the energy variable
subdat = mydata[complete.cases(mydata[ , "how_energising"]),]

#mediator as outcome - variable names
med_mlms_5.2 = c(med_fit_5.2)
med_mlm_dir_names_5.2 = c("med_fit_5.2")
predictor = c("came_with2")
predictor_integer = c("came_with2_int")
predictor_name = c("Who participants came or met up with")
outcome = "how_energising"
level_two_variable = "Athlete_ID"
level_one_variable = "slowed_spedup_natural_3"

#do assumption checks for the model
check_mediation_assumptions(mlmm_dir, med_mlms_5.2, med_mlm_dir_names_5.2, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, subdat)

#mediator as covariate predicting outcome - variable names
out_mlms_5.2 = c(out_fit_5.2)
out_mlm_dir_names_5.2 = c("out_fit_5.2")
predictor = c("came_with2")
predictor_integer = c("came_with2_int")
predictor_name = c("Who participants came or met up with")
outcome = "time.lg"
level_two_variable = "Athlete_ID"
level_one_variable = "how_energising"

#do assumption checks for the model
check_mediation_assumptions(mlmm_dir, out_mlms_5.2, out_mlm_dir_names_5.2, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, subdat)

#check the assumptions of the multilevel mediation mediation analysis (first create and set an output directory for the mediation assumption test)
med_dir = file.path(mlmm_dir, 'mediation')
dir.create(med_dir)
setwd(med_dir)

#variable names
main_predictor = "came_with2_int"
covariate = "slowed_spedup_natural_3"
mediator = "how_energising"
level_two_variable = "Athlete_ID"
outcome = "time.lg"

sequential_ignorability(main_predictor, covariate, mediator, outcome, level_two_variable, subdat)

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 5.3) ###

#create a directory for all the results from the multilevel mediation analysis, set the directory to that
setwd(code_dir)
mediation_dir_name = 'hypothesis_5.3'
all_dirs = unlist(strsplit(code_dir, "/"))
all_dirs = all_dirs[1:(length(all_dirs) - 1)]
mlmm_dir = paste0(paste(all_dirs[1:length(all_dirs)], collapse = "/"), "/outputs/", mediation_dir_name)
dir.create(mlmm_dir)
setwd(mlmm_dir)

#first drop the lone survey with an 'NA' value for the energy variable
subdat = mydata[complete.cases(mydata[ , "how_energising"]),]

#mediator as outcome - variable names
med_mlms_5.3 = c(med_fit_5.3)
med_mlm_dir_names_5.3 = c("med_fit_5.3")
predictor = c("just_before2")
predictor_integer = c("just_before2_int")
predictor_name = c("Pre-run sociality")
outcome = "how_energising"
level_two_variable = "Athlete_ID"
level_one_variable = "slowed_spedup_natural_3"

#do assumption checks for the model
check_mediation_assumptions(mlmm_dir, med_mlms_5.3, med_mlm_dir_names_5.3, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, subdat)

#mediator as covariate predicting outcome - variable names
out_mlms_5.3 = c(out_fit_5.3)
out_mlm_dir_names_5.3 = c("out_fit_5.3")
predictor = c("just_before2")
predictor_integer = c("just_before2_int")
predictor_name = c("Pre-run sociality")
outcome = "time.lg"
level_two_variable = "Athlete_ID"
level_one_variable = "how_energising"

#do assumption checks for the model
check_mediation_assumptions(mlmm_dir, out_mlms_5.3, out_mlm_dir_names_5.3, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, subdat)

#check the assumptions of the multilevel mediation mediation analysis (first create and set an output directory for the mediation assumption test)
med_dir = file.path(mlmm_dir, 'mediation')
dir.create(med_dir)
setwd(med_dir)

#variable names
main_predictor = "just_before2_int"
covariate = "slowed_spedup_natural_3"
mediator = "how_energising"
level_two_variable = "Athlete_ID"
outcome = "time.lg"

sequential_ignorability(main_predictor, covariate, mediator, outcome, level_two_variable, subdat)

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 6.1) ###

#create a directory for all the results from the multilevel mediation analysis, set the directory to that
setwd(code_dir)
mediation_dir_name = 'hypothesis_6.1'
all_dirs = unlist(strsplit(code_dir, "/"))
all_dirs = all_dirs[1:(length(all_dirs) - 1)]
mlmm_dir = paste0(paste(all_dirs[1:length(all_dirs)], collapse = "/"), "/outputs/", mediation_dir_name)
dir.create(mlmm_dir)
setwd(mlmm_dir)

#mediator as outcome - variable names
med_mlms_6.1 = c(med_fit_6.1)
med_mlm_dir_names_6.1 = c("med_fit_6.1")
predictor = c("parkrun_community_factor")
predictor_integer = c("parkrun_community_factor")
predictor_name = c("parkrun community component")
outcome = "fatigued"
level_two_variable = "Athlete_ID"
level_one_variable = "slowed_spedup_natural_3"

#do assumption checks for the model
check_mediation_assumptions(mlmm_dir, med_mlms_6.1, med_mlm_dir_names_6.1, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, mydata)

#mediator as covariate predicting outcome - variable names
out_mlms_6.1 = c(out_fit_6.1)
out_mlm_dir_names_6.1 = c("out_fit_6.1")
predictor = c("parkrun_community_factor")
predictor_integer = c("parkrun_community_factor")
predictor_name = c("parkrun community component")
outcome = "time.lg"
level_two_variable = "Athlete_ID"
level_one_variable = "fatigued"

#do assumption checks for the model
check_mediation_assumptions(mlmm_dir, out_mlms_6.1, out_mlm_dir_names_6.1, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, mydata)

#check the assumptions of the multilevel mediation mediation analysis (first create and set an output directory for the mediation assumption test)
med_dir = file.path(mlmm_dir, 'mediation')
dir.create(med_dir)
setwd(med_dir)

#variable names
main_predictor = "parkrun_community_factor"
covariate = "slowed_spedup_natural_3"
mediator = "how_energising"
level_two_variable = "Athlete_ID"
outcome = "time.lg"

sequential_ignorability(main_predictor, covariate, mediator, outcome, level_two_variable, mydata)

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 6.2) ###

#create a directory for all the results from the multilevel mediation analysis, set the directory to that
setwd(code_dir)
mediation_dir_name = 'hypothesis_6.2'
all_dirs = unlist(strsplit(code_dir, "/"))
all_dirs = all_dirs[1:(length(all_dirs) - 1)]
mlmm_dir = paste0(paste(all_dirs[1:length(all_dirs)], collapse = "/"), "/outputs/", mediation_dir_name)
dir.create(mlmm_dir)
setwd(mlmm_dir)

#mediator as outcome - variable names
med_mlms_6.2 = c(med_fit_6.2)
med_mlm_dir_names_6.2 = c("med_fit_6.2")
predictor = c("came_with2")
predictor_integer = c("came_with2_int")
predictor_name = c("Who participants came or met up with")
outcome = "fatigued"
level_two_variable = "Athlete_ID"
level_one_variable = "slowed_spedup_natural_3"

#do assumption checks for the model (ignore singular fit warning - result of the models mode complex random effects structure)
check_mediation_assumptions(mlmm_dir, med_mlms_6.2, med_mlm_dir_names_6.2, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, mydata)

#mediator as covariate predicting outcome - variable names
out_mlms_6.2 = c(out_fit_6.2)
out_mlm_dir_names_6.2 = c("out_fit_6.2")
predictor = c("came_with2")
predictor_integer = c("came_with2_int")
predictor_name = c("Who participants came or met up with")
outcome = "time.lg"
level_two_variable = "Athlete_ID"
level_one_variable = "fatigued"

#do assumption checks for the model
check_mediation_assumptions(mlmm_dir, out_mlms_6.2, out_mlm_dir_names_6.2, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, mydata)

#check the assumptions of the multilevel mediation mediation analysis (first create and set an output directory for the mediation assumption test)
med_dir = file.path(mlmm_dir, 'mediation')
dir.create(med_dir)
setwd(med_dir)

#variable names
predictor_integer = c("came_with2_int")
covariate = "slowed_spedup_natural_3"
mediator = "fatigued"
level_two_variable = "Athlete_ID"
outcome = "time.lg"

sequential_ignorability(main_predictor, covariate, mediator, outcome, level_two_variable, mydata)

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 6.3) ###

#create a directory for all the results from the multilevel mediation analysis, set the directory to that
setwd(code_dir)
mediation_dir_name = 'hypothesis_6.3'
all_dirs = unlist(strsplit(code_dir, "/"))
all_dirs = all_dirs[1:(length(all_dirs) - 1)]
mlmm_dir = paste0(paste(all_dirs[1:length(all_dirs)], collapse = "/"), "/outputs/", mediation_dir_name)
dir.create(mlmm_dir)
setwd(mlmm_dir)

#mediator as outcome - variable names
med_mlms_6.3 = c(med_fit_6.3)
med_mlm_dir_names_6.3 = c("med_fit_6.3")
predictor = c("just_before2")
predictor_integer = c("just_before2_int")
predictor_name = c("Pre-run sociality")
outcome = "fatigued"
level_two_variable = "Athlete_ID"
level_one_variable = "slowed_spedup_natural_3"

#do assumption checks for the model (ignore singular fit warning - result of the models mode complex random effects structure)
check_mediation_assumptions(mlmm_dir, med_mlms_6.3, med_mlm_dir_names_6.3, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, mydata)

#mediator as covariate predicting outcome - variable names
out_mlms_6.3 = c(out_fit_6.3)
out_mlm_dir_names_6.3 = c("out_fit_6.3")
predictor = c("just_before2")
predictor_integer = c("just_before2_int")
predictor_name = c("Pre-run sociality")
outcome = "time.lg"
level_two_variable = "Athlete_ID"
level_one_variable = "fatigued"

#do assumption checks for the model
check_mediation_assumptions(mlmm_dir, out_mlms_6.3, out_mlm_dir_names_6.3, predictor, predictor_integer, predictor_name, outcome, level_two_variable, level_one_variable, mydata)

#check the assumptions of the multilevel mediation mediation analysis (first create and set an output directory for the mediation assumption test)
med_dir = file.path(mlmm_dir, 'mediation')
dir.create(med_dir)
setwd(med_dir)

#variable names
predictor_integer = c("just_before2_int")
covariate = "slowed_spedup_natural_3"
mediator = "fatigued"
level_two_variable = "Athlete_ID"
outcome = "time.lg"

sequential_ignorability(main_predictor, covariate, mediator, outcome, level_two_variable, mydata)
