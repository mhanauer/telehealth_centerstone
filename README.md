title: "telehealth_noms"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Stack variables

Post stacking transformations

All other grants
0600 = Baseline
0601 = 6 month resasessment


CCBHC
For assessment see below 3-months is actually vitals and so on
3-month vitals
6 month reassessment
9-month vitals
12 month reassessment

Assessment: 
0600 = Baseline Assessment
0301 = 3 Month Reassessment (vitals)
0302 = 6 Month Reassessment
0303 = 9 Month Reassessment (vitals)
0304 = 12 Month Reassessment
0699 = clincial discharge

Assessment_new
0 = Baseline
1 = 3 month reassessment (vitals)
2 = 6 month reassessment (302 from CCBHC and 601 from all other grants)
3 = 9 month reassessment (vitals)
4 = 12 month reassessment
5 = clinical discharge

Now review the missing data
Only include Baseline and 6-month
telehealth.y means they were in telehealth at 6 months which is what we want

# Data mergeing
For CCBHC IN, IL all the same

telehealth: Telehealth = 1; Pre-telehealth = 0 telehealth defined as those with any assessment date on or after 4-2-2020

### Run this prior to any analysis to load data ####
```{r}
library(prettyR)
library(see)
library(performance)
###
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")

## Run this for machine learning
#IN =  read.csv("CCBHC_IN_8.10.20.csv", header = TRUE, na.strings =  c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IN =  read.csv("CCBHC_IN_5.28.20.csv", header = TRUE, na.strings =  c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))


# Run this FHHC for machine learning data
#FHHC = read.csv("fhhc_noms_8_10_20.csv", header= TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
FHHC = read.csv("fhhc_noms_5_27_20.csv", header= TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
ICP = read.csv("SPARS Data Download 5.23.2020_ICP.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
SOCAT = read.csv("SOCAT NOMs download 5.27.20.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
#Run this for machine learning
#IL_adult = read.csv("IL_adult_8_10_20.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IL_adult = read.csv("data down 5.26.20 adult CCBHC IL.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))

### Run this for machine learning
#IL_youth = read.csv("IL_youth_8_10_20.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IL_youth = read.csv("data down 5.26.20 child CCBHC IL.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
FL_ACT = read.csv("FL-ACT SPARS data download  5.28.2020.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))

## Now stack them
### Create an empty data and then fill it with NAs.  Keep the first 44 those are correct and match
SOCAT$RespondentType = NULL
SOCAT_matrix = matrix(NA, ncol = 185-43, nrow = dim(SOCAT)[1])
SOCAT_matrix = data.frame(SOCAT_matrix)
colnames(SOCAT_matrix) = colnames(ICP[,44:185])
SOCAT_full = data.frame(SOCAT[,1:43], SOCAT_matrix)
dim(SOCAT_full)
### Change variables that match
SOCAT_full$Nervous = SOCAT$Nervous
SOCAT_full$Hopeless = SOCAT$Hopeless
SOCAT_full$Restless = SOCAT$Restless
SOCAT_full$Depressed = SOCAT$Depressed
SOCAT_full$EverythingEffort = SOCAT$EverythingEffort
SOCAT_full$Worthless = SOCAT$Worthless
SOCAT_full$Tobacco_Use = SOCAT$Tobacco_Use
SOCAT_full$Alcohol_Use = SOCAT$Alcohol_Use
SOCAT_full$StreetOpioids_Use = SOCAT$StreetOpioids_Use
SOCAT_full$RxOpioids_Use = SOCAT$RxOpioids_Use
SOCAT_full$NightsHomeless = SOCAT$NightsHomeless
SOCAT_full$NightsHospitalMHC = SOCAT$NightsHospitalMHC
SOCAT_full$NightsDetox = SOCAT$NightsDetox
SOCAT_full$NightsJail = SOCAT$NightsJail
SOCAT_full$TimesER = SOCAT$TimesER
SOCAT_full$Housing = SOCAT$Housing
SOCAT = SOCAT_full

IL_youth$RespondentType = NULL
IL_youth_matrix = matrix(NA, ncol = 185-43, nrow = dim(IL_youth)[1])
IL_youth_matrix = data.frame(IL_youth_matrix)
colnames(IL_youth_matrix) = colnames(ICP[,44:185])
IL_youth_full = data.frame(IL_youth[,1:43], IL_youth_matrix)
dim(IL_youth_full)
### Change variables that match
IL_youth_full$Nervous = IL_youth$Nervous
IL_youth_full$Hopeless = IL_youth$Hopeless
IL_youth_full$Restless = IL_youth$Restless
IL_youth_full$Depressed = IL_youth$Depressed
IL_youth_full$EverythingEffort = IL_youth$EverythingEffort
IL_youth_full$Worthless = IL_youth$Worthless
IL_youth_full$Tobacco_Use = IL_youth$Tobacco_Use
IL_youth_full$Alcohol_Use = IL_youth$Alcohol_Use
IL_youth_full$StreetOpioids_Use = IL_youth$StreetOpioids_Use
IL_youth_full$RxOpioids_Use = IL_youth$RxOpioids_Use
IL_youth_full$NightsHomeless = IL_youth$NightsHomeless
IL_youth_full$NightsHospitalMHC = IL_youth$NightsHospitalMHC
IL_youth_full$NightsDetox = IL_youth$NightsDetox
IL_youth_full$NightsJail = IL_youth$NightsJail
IL_youth_full$TimesER = IL_youth$TimesER
IL_youth_full$Housing = IL_youth$Housing
IL_youth = IL_youth_full

IN_IL_KY_CCBHC = rbind(IN[,1:185], IL_youth[,1:185], IL_adult[,1:185])
dim(IN_IL_KY_CCBHC)
FHHC = FHHC[,1:185]
ICP = ICP[,1:185]
FL_ACT = FL_ACT[,1:185]
dim(ICP)
dim(SOCAT)
### Add grant ID
IN_IL_KY_CCBHC$grant = rep("IN_IL_KY_CCBHC", dim(IN_IL_KY_CCBHC)[1])
FHHC$grant = rep("FHHC", dim(FHHC)[1])
ICP$grant = rep("ICP", dim(ICP)[1])
SOCAT$grant = rep("SOCAT", dim(SOCAT)[1])
FL_ACT$grant = rep("FL_ACT", dim(FL_ACT)[1])
dim(SOCAT)
telehealth_noms = rbind(IN_IL_KY_CCBHC, FHHC, ICP, SOCAT, FL_ACT)
dim(telehealth_noms)
### Create a new ConsumerID that is a mix of grant and ConsumerID
telehealth_noms$ConsumerID_grant = paste0(telehealth_noms$ConsumerID, telehealth_noms$GrantID)

### Figure out how you can stack FHHC data
dim(telehealth_noms)

## Rename to the above

## No one has multiple reassessments


## Create recoded assessment variable
telehealth_noms$Assessment_new = ifelse(telehealth_noms$Assessment == 600, 0, ifelse(telehealth_noms$Assessment == 301, 1, ifelse(telehealth_noms$Assessment == 302, 2, ifelse(telehealth_noms$Assessment == 303, 3, ifelse(telehealth_noms$Assessment == 601,2, NA)))))
telehealth_noms$Assessment_new = as.numeric(telehealth_noms$Assessment_new)
describe.factor(telehealth_noms$Assessment_new, decr.order= FALSE)
### Create full date variable
telehealth_noms$date = paste0(telehealth_noms$FFY, "-", telehealth_noms$Month, "-", "01")
library(lubridate)
telehealth_noms$date = ymd(telehealth_noms$date)
head(telehealth_noms$date)

telehealth_noms$telehealth = ifelse(telehealth_noms$date >= "2020-04-01", 1, 0)
telehealth_noms[c("date","telehealth")]
### Cannot be greater than 2020-09-30 last day of grant
#telehealth_noms = subset(telehealth_noms, date < "2020-09-30")
## Check that all dates post 2014 most grants are for at most five years
#telehealth_noms = subset(telehealth_noms, date > "2014-01-01")
telehealth_noms[c("date","telehealth")]
dim(telehealth_noms)
describe.factor(telehealth_noms$grant)
range(telehealth_noms$date)
### Create a NOMS data set  
telehealth_noms_wide = subset(telehealth_noms, Assessment_new == 0 | Assessment_new == 2)
dim(telehealth_noms)[1]
describe.factor(telehealth_noms$Assessment_new)


####################
library(naniar)
miss_var_summary(telehealth_noms)

head(telehealth_noms)
miss_var_summary(subset(telehealth_noms, Assessment_new == 2))
miss_var_summary(subset(telehealth_noms, Assessment_new == 0))

### These people have two baselines delete them 'A00276''A00295''A00298'

telehealth_noms_wide = telehealth_noms_wide[order(telehealth_noms_wide$ConsumerID),]
telehealth_noms_wide_test = subset(telehealth_noms_wide, ConsumerID == "'A00276'" | ConsumerID == "'A00295'" | ConsumerID == "'A00298'")
## If there is no interview then delete the second, if there is only one interview delete the none interview, if there are two interviews for baseline delete the second see conductedinterview variable
telehealth_noms_wide[c(1942, 1960, 1965),]
telehealth_noms_wide = telehealth_noms_wide[-c(1942, 1960, 1965),] 


#telehealth_noms_wide = telehealth_noms_wide[-c(3754, 3779),] 
telehealth_noms_base_noms = subset(telehealth_noms_wide,Assessment_new == 0)
telehealth_noms_month6_noms = subset(telehealth_noms_wide,Assessment_new == 2)
describe.factor(telehealth_noms_base_noms$grant)
describe.factor(telehealth_noms_month6_noms$grant)

head(telehealth_noms_base_noms)
dim(telehealth_noms_month6_noms)
telehealth_noms_wide_noms = merge(telehealth_noms_base_noms, telehealth_noms_month6_noms, by = "ConsumerID_grant", all.y = TRUE)
dim(telehealth_noms_wide_noms)
telehealth_noms_wide_noms = telehealth_noms_wide_noms[order(telehealth_noms_wide_noms$ConsumerID_grant),]
telehealth_noms_month6_noms = telehealth_noms_month6_noms[order(telehealth_noms_month6_noms$ConsumerID_grant),]
telehealth_noms_month6_noms$ConsumerID_grant == telehealth_noms_wide_noms$ConsumerID_grant

head(telehealth_noms_month6_noms)
describe.factor(telehealth_noms_month6_noms$telehealth)
describe.factor(telehealth_noms_month6_noms$grant)
describe.factor(telehealth_noms_wide_noms$telehealth.y)

### Gender is female
telehealth_noms_wide_noms$Gender.y = ifelse(telehealth_noms_wide_noms$Gender.y == 2, 1, 0)

#### Create diagnosis variables
describe.factor(telehealth_noms_wide_noms$DiagnosisOne.y)
test_dat = subset(telehealth_noms_wide_noms, DiagnosisOne.y == "59")
describe.factor(test_dat$telehealth.y)

### Enough 62 which is 62 = F40-F48 – Anxiety, dissociative, stress-related, somatoform and other nonpsychotic mental disorders
#59 = F33 – Major depressive disorder, recurrent
#57 = F31 – Bipolar disorder
telehealth_noms_wide_noms$dep = ifelse(telehealth_noms_wide_noms$DiagnosisOne.y == 59, 1, 0)
telehealth_noms_wide_noms$bipolar = ifelse(telehealth_noms_wide_noms$DiagnosisOne.y == 57, 1, 0)
describe.factor(telehealth_noms_wide_noms$dep)
telehealth_noms_wide_noms$InterviewDate.y
dim(telehealth_noms_wide_noms)
```
############
Clean data for machine learning
Quarter.x
DiagnosisOne.x (get dummary vars for this variable)
Gender.x
SiteID.x (get dummary vars)
```{r}
#write.csv(telehealth_noms_wide_noms, "telehealth_noms_wide_noms.csv", row.names = FALSE)
telehealth_noms_wide_noms[c("Quarter.x", "DiagnosisOne.x", "Gender.x", "HispanicLatino.x", "RaceWhite.x", "RaceBlack.x", "Agegroup.x", "SexualIdentity.x", "OverallHealth.x", "CapableManagingHealthCareNeeds.x", "HandlingDailyLife.x", "ControlLife.x", "DealWithCrisis.x", "GetsAlongWithFamily.x", "SocialSituations.x", "SchoolOrWork.x", "FunctioningHousing.x", "Symptoms.x", "Nervous.x", "Hopeless.x", "Restless.x", "Depressed.x", "EverythingEffort.x", "Worthless.x", "PsychologicalEmotionalProblems.x", "LifeQuality.x", "EnoughEnergyForEverydayLife.x", "PerformDailyActivitiesSatisfaction.x", "HealthSatisfaction.x", "SelfSatisfaction.x", "RelationshipSatisfaction.x", "SelfSatisfaction.x", "RelationshipSatisfaction.x", "Tobacco_Use.x", "Alcohol_Use.x", "Cannabis_Use.x", "Cocaine_Use.x", "Meth_Use.x", "RxOpioids_Use.x", "StreetOpioids_Use.x", "ViolenceTrauma.x", "VT_NightmaresThoughts.x", "VT_NotThinkAboutIt.x", "VT_OnGuard.x", "VT_NumbDetached.x", "PhysicallyHurt.x", "NightsHomeless.x", "NightsHospitalMHC.x", "NightsDetox.x", "NightsJail.x", "TimesER.x", "Housing.x", "LivingConditionsSatisfaction.x", "Education.x", "Employment.x", "noughMoneyForNeeds.x", "NumTimesArrested.x", "Friendships.x", "EnjoyPeople.x", "BelongInCommunity.x", "SupportFromFamily.x", "SupportiveFamilyFriends.x")]
```


################################################
Full data set created can start data analysis
################################################




Let's try satisfaction 
c.	how satisfied are you with your ability to perform your daily living activities? PerformDailyActivitiesSatisfaction
d.	how satisfied are you with your health? HealthSatisfaction
how satisfied are you with yourself? SelfSatisfaction
how satisfied are you with your personal relationships? RelationshipSatisfaction

### General instructions ####
1. Grab the variable you want and put into data frame
2. Check the descriptives to make sure everything is in range
3. Conduct psychometrics 

SMI
Major depressive, bioplar and schizoprenia
https://www.psychiatry.org/patients-families/what-is-mental-illness#:~:text=Examples%20of%20serious%20mental%20illness,disorder%2C%20schizophrenia%20and%20bipolar%20disorder.
https://mentalillnesspolicy.org/serious-mental-illness-not/
http://www.bhevolution.org/public/severe_mental_illness.page


```{r}
### All items should be 1 to 5
telehealth_noms_wide_noms_sat = telehealth_noms_wide_noms[c("telehealth.y", "PerformDailyActivitiesSatisfaction.x", "PerformDailyActivitiesSatisfaction.y", "HealthSatisfaction.x", "HealthSatisfaction.y", "SelfSatisfaction.x", "SelfSatisfaction.y", "RelationshipSatisfaction.x","RelationshipSatisfaction.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_noms_wide_noms_sat,2, function(x){describe.factor(x)})
library(psych)


### Plug in all the .x variables 
omega_sat_base =  omega(telehealth_noms_wide_noms_sat[c(2,4,6,8)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
omega_sat_6month =  omega(telehealth_noms_wide_noms_sat[c(3,5,7,9)], poly = TRUE)
omega_sat_6month

### Plug in all the .x variables 
vss(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly", correct = 0)
```
Create a total score for each 

IN_IL_dat_wide_noms_sat_month6_complete  = satisfaction total scores only with complete data for 6-month
```{r}
### Plug in all .x variables
telehealth_noms_wide_noms_sat$total_base = apply(telehealth_noms_wide_noms_sat[c(2,4,6,8)], 1, mean, na.rm = TRUE)
### Plug in all .y expect for telehealth.y
telehealth_noms_wide_noms_sat$total_month6 = apply(telehealth_noms_wide_noms_sat[c(3,5,7,9)], 1, mean, na.rm = TRUE)
hist(telehealth_noms_wide_noms_sat$total_base)
hist(telehealth_noms_wide_noms_sat$total_month6)

## No data for difference scores so try just 6months
range(telehealth_noms_wide_noms_sat$total_month6, na.rm = TRUE)
dim(telehealth_noms_wide_noms_sat)
head(telehealth_noms_wide_noms_sat)

telehealth_noms_wide_noms_sat_month6_complete = na.omit(telehealth_noms_wide_noms_sat[c("total_month6", "telehealth.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])
dim(telehealth_noms_wide_noms_sat_month6_complete)
telehealth_noms_wide_noms_sat_month6_complete

### creating face to face
telehealth_noms_wide_noms_sat_month6_complete$face_to_face = ifelse(telehealth_noms_wide_noms_sat_month6_complete$telehealth.y == 1,0,1)

apply(telehealth_noms_wide_noms_sat_month6_complete, 2, function(x){describe.factor(x)})

```
Analysis sat
Percentage change: https://stats.idre.ucla.edu/sas/faq/how-can-i-interpret-log-transformed-variables-in-terms-of-percent-change-in-linear-regression/
  
You need to expondiate the parameter estimate
Doesn't work to well when you get above 20% differences: https://people.duke.edu/~rnau/411log.htm

P-change is bad for regression because of: 
(3-2)/2 
(2-3)/3

Change the telehealth.y to be face to face 1 and telehealth.y  = 0

```{r}
library(rstanarm)
library(descr)
### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
n_total = dim(telehealth_noms_wide_noms_sat_month6_complete)[1]

bayes_p_change_sat = stan_glm(log(total_month6)~ face_to_face + Agegroup.y+ Gender.y+ RaceWhite.y + dep +bipolar , prior = my_prior, data = telehealth_noms_wide_noms_sat_month6_complete, seed = 123)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_sat_sum = round(bayes_p_change_sat$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_sat_sum = round(exp(bayes_p_change_sat_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_sat_sum= bayes_p_change_sat_sum - 1
bayes_p_change_sat_sum
car::vif(bayes_p_change_sat)
lmtest::bptest(bayes_p_change_sat)
#launch_shinystan(bayes_p_change_sat)
### Grabing the means, sds, and n's for each group
mean_sd_sat= round(compmeans(telehealth_noms_wide_noms_sat_month6_complete$total_month6, telehealth_noms_wide_noms_sat_month6_complete$telehealth.y),2)
mean_sd_sat
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_sat_d =  psych::cohen.d(telehealth_noms_wide_noms_sat_month6_complete$total_month6, group = telehealth_noms_wide_noms_sat_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)
results_sat = data.frame(par_estimate = bayes_p_change_sat_sum[2,1], sd_p_change =  bayes_p_change_sat_sum[2,2], ci_95 = paste0(bayes_p_change_sat_sum[2,3], ",", bayes_p_change_sat_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_sat[1,2], n_post_telehealth = mean_sd_sat[2,2], raw_p_change = round((mean_sd_sat[2,1] -  mean_sd_sat[1,1]) /  mean_sd_sat[1,1],3), telehealth_mean =  mean_sd_sat[2,1], face_to_face_mean =  mean_sd_sat[1,1], telehealth_sd= mean_sd_sat[2,3], face_to_face_sd= mean_sd_sat[1,3],freq_cohen_d = round(month_6_sat_d$cohen.d[2],3))

write.csv(results_sat, "results_sat.csv", row.names = FALSE)
results_sat


```
Sat with diagnoses 

```{r}

### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
n_total = dim(telehealth_noms_wide_noms_sat_month6_complete)[1]

bayes_p_change_sat_dep = stan_glm(log(total_month6)~ face_to_face*dep +face_to_face*bipolar + Agegroup.y+ Gender.y+ RaceWhite.y, prior = my_prior, data = telehealth_noms_wide_noms_sat_month6_complete, seed = 123)
##
#launch_shinystan(bayes_p_change_sat_dep)
check_collinearity(bayes_p_change_sat_dep)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat_dep$stan_summary if you are unsure
bayes_p_change_sat_dep_sum = round(bayes_p_change_sat_dep$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_sat_dep_sum = round(exp(bayes_p_change_sat_dep_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_sat_dep_sum= bayes_p_change_sat_dep_sum - 1
bayes_p_change_sat_dep_sum

### Grabing the means, sds, and n's for each group
mean_sd_sat_dep= round(compmeans(telehealth_noms_wide_noms_sat_month6_complete$total_month6, telehealth_noms_wide_noms_sat_month6_complete$telehealth.y),2)
mean_sd_sat_dep
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_sat_dep_d =  psych::cohen.d(telehealth_noms_wide_noms_sat_month6_complete$total_month6, group = telehealth_noms_wide_noms_sat_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_dep_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat_dep to whatever you are measuring results_(fill in name)
results_sat_dep = data.frame(par_estimate = bayes_p_change_sat_dep_sum[2,1], sd_p_change =  bayes_p_change_sat_dep_sum[2,2], ci_95 = paste0(bayes_p_change_sat_dep_sum[2,3], ",", bayes_p_change_sat_dep_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_sat_dep[1,2], n_post_telehealth = mean_sd_sat_dep[2,2], raw_p_change = round((mean_sd_sat_dep[2,1] -  mean_sd_sat_dep[1,1]) /  mean_sd_sat_dep[1,1],3), telehealth_mean =  mean_sd_sat_dep[2,1], face_to_face_mean =  mean_sd_sat_dep[1,1], telehealth_sd= mean_sd_sat_dep[2,3], face_to_face_sd= mean_sd_sat_dep[1,3],freq_cohen_d = round(month_6_sat_dep_d$cohen.d[2],3))

write.csv(results_sat_dep, "results_sat_dep.csv", row.names = FALSE)
results_sat_dep


```
Sat with diagnosis with t.test for just telehealth (Run diagnosis regression to get telehealth_noms_wide_noms_sat_month6_complete)
```{r}
sat_diag_tele_only_dat = subset(telehealth_noms_wide_noms_sat_month6_complete, telehealth.y == 1)
hist(sat_diag_tele_only_dat$total_month6)
qqnorm(sat_diag_tele_only_dat$total_month6)
compmeans(sat_diag_tele_only_dat$total_month6, sat_diag_tele_only_dat$dep)

### recode dep so it compares dep first relative to not depressed
sat_diag_tele_only_dat$dep_recode = ifelse(sat_diag_tele_only_dat$dep == 1, 0,1)
dep_sat = t.test(sat_diag_tele_only_dat$total_month6 ~ sat_diag_tele_only_dat$dep_recode)
dep_sat = data.frame(t_value = dep_sat$statistic, p_value = dep_sat$p.value, lower = dep_sat$conf.int[1], upper = dep_sat$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(sat_diag_tele_only_dat$total_month6 ~ sat_diag_tele_only_dat$dep_recode)

n_dep_sat =describe.factor(sat_diag_tele_only_dat$dep)
n_dep_sat = data.frame(n_dep_sat)
n_dep_sat = n_dep_sat[1,]
n_dep_sat = data.frame(n_dep_sat)
colnames(n_dep_sat) = n_dep_sat
colnames(n_dep_sat) = c("No major depression count", "Major depression count")
n_sat_dat = data.frame(dim(sat_diag_tele_only_dat)[1])
colnames(n_sat_dat) = "Satisfaction total n"

sat_dep_means = compmeans(sat_diag_tele_only_dat$total_month6, sat_diag_tele_only_dat$dep)

sat_dep_d = psych::cohen.d(sat_diag_tele_only_dat$total_month6, group = sat_diag_tele_only_dat$dep)


results_sat_dep = data.frame(dep_sat, n_total = n_sat_dat, n_dep_sat[2], n_dep_sat[1], raw_p_change = round((sat_dep_means[2,1] -  sat_dep_means[1,1]) /  sat_dep_means[1,1],3), dep_mean =  sat_dep_means[2,1], no_dep_mean =  sat_dep_means[1,1], dep_sd= sat_dep_means[2,3], no_dep_sd= sat_dep_means[1,3],freq_cohen_d = round(sat_dep_d$cohen.d[2],3))
results_sat_dep = round(results_sat_dep, 2)
library(gt)
title_results_sat_dep = "T-test results for satisfaction with outcomes at 6-months for telehealth clients with major depression"
table_results_sat_dep = 
  gt(results_sat_dep) %>%
  tab_header(title = title_results_sat_dep)%>%
  cols_label(t_value = md("T-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Satisfaction.total.n = md("Satisfaction total n") , Major.depression.count = md("Major depression count"), No.major.depression.count = md("No major depression count"), raw_p_change = md("raw p change"), dep_mean = md("Depression mean"), no_dep_mean = md("No depression mean"), dep_sd = md("Depression sd"), no_dep_sd = md("No depression sd"), freq_cohen_d = md("Cohen D"))
table_results_sat_dep

gtsave(table_results_sat_dep, "table_results_sat_dep.png")
#############################################################################################################################
### Bipolar now
sat_diag_tele_only_dat = subset(telehealth_noms_wide_noms_sat_month6_complete, telehealth.y == 1)
hist(sat_diag_tele_only_dat$total_month6)
qqnorm(sat_diag_tele_only_dat$total_month6)
compmeans(sat_diag_tele_only_dat$total_month6, sat_diag_tele_only_dat$bipolar)

### recode bipolar so it compares bipolar first relative to not bipolarressed
sat_diag_tele_only_dat$bipolar_recode = ifelse(sat_diag_tele_only_dat$bipolar == 1, 0,1)
bipolar_sat = t.test(sat_diag_tele_only_dat$total_month6 ~ sat_diag_tele_only_dat$bipolar_recode)
bipolar_sat = data.frame(t_value = bipolar_sat$statistic, p_value = bipolar_sat$p.value, lower = bipolar_sat$conf.int[1], upper = bipolar_sat$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(sat_diag_tele_only_dat$total_month6 ~ sat_diag_tele_only_dat$bipolar_recode)

n_bipolar_sat =describe.factor(sat_diag_tele_only_dat$bipolar)
n_bipolar_sat = data.frame(n_bipolar_sat)
n_bipolar_sat = n_bipolar_sat[1,]
n_bipolar_sat = data.frame(n_bipolar_sat)
colnames(n_bipolar_sat) = n_bipolar_sat
colnames(n_bipolar_sat) = c("No bipolar count", "Bipolar count")
n_sat_dat = data.frame(dim(sat_diag_tele_only_dat)[1])
colnames(n_sat_dat) = "Satisfaction total n"

sat_bipolar_means = compmeans(sat_diag_tele_only_dat$total_month6, sat_diag_tele_only_dat$bipolar)

sat_bipolar_d = psych::cohen.d(sat_diag_tele_only_dat$total_month6, group = sat_diag_tele_only_dat$bipolar)


results_sat_bipolar = data.frame(bipolar_sat, n_total = n_sat_dat, n_bipolar_sat[2], n_bipolar_sat[1], raw_p_change = round((sat_bipolar_means[2,1] -  sat_bipolar_means[1,1]) /  sat_bipolar_means[1,1],3), bipolar_mean =  sat_bipolar_means[2,1], no_bipolar_mean =  sat_bipolar_means[1,1], bipolar_sd= sat_bipolar_means[2,3], no_bipolar_sd= sat_bipolar_means[1,3],freq_cohen_d = round(sat_bipolar_d$cohen.d[2],3))
results_sat_bipolar = round(results_sat_bipolar, 2)
library(gt)
title_results_sat_bipolar = "T-test results for satisfaction with outcomes at 6-months for telehealth clients with primary diagnosis as bipolar"
table_results_sat_bipolar = 
  gt(results_sat_bipolar) %>%
  tab_header(title = title_results_sat_bipolar)%>%
  cols_label(t_value = md("T-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Satisfaction.total.n = md("Satisfaction total n") , Bipolar.count = md("Bipolar count"), No.bipolar.count = md("No bipolar count"), raw_p_change = md("raw p change"), bipolar_mean = md("Bipolar mean"), no_bipolar_mean = md("No bipolar mean"), bipolar_sd = md("Bipolar sd"), no_bipolar_sd = md("No bipolar sd"), freq_cohen_d = md("Cohen D"))
table_results_sat_bipolar

gtsave(table_results_sat_bipolar, "table_results_sat_bipolar.png")

```

#################
Code from Github
1. Grab code from master branch


##################### 
Follow the code above and plug in the variables below instead of the variable above
Post to github as your name patch-1,2,3...n (however, many patchs you have).  Let Matt know when you are finished.
Or just save code on telehealth folder on T drive

######################################

######Cat

Next section
Dealing with everyday life with mental health illness
Sat
a.	I deal effectively with daily problems.
b.	I am able to control my life. T\CRI_Research\telehealth_evaluation
c.	I am able to deal with crisis.
d.	I am getting along with my family.
e.	I do well in social situations.
f.	I do well in school and/or work.
g.	My housing situation is satisfactory.
h.	My symptoms are not bothering me.

```{r}
telehealth_noms_wide_noms_deal = telehealth_noms_wide_noms[c("telehealth.y", "HandlingDailyLife.x", "HandlingDailyLife.y", "ControlLife.x", "ControlLife.y", "DealWithCrisis.x", "DealWithCrisis.y", "GetsAlongWithFamily.x","GetsAlongWithFamily.y", "SocialSituations.x", "SocialSituations.y", "SchoolOrWork.x", "SchoolOrWork.y", "FunctioningHousing.x", "FunctioningHousing.y", "Symptoms.x","Symptoms.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_noms_wide_noms_deal,2, function(x){describe.factor(x)})
library(psych)

### Plug in all the .x variables 
omega_deal_base =  omega(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
omega_deal_6month =  omega(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], poly = TRUE)
omega_deal_6month

### Plug in all the .x variables 
vss(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], cor = "poly")
fa(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], cor = "poly", correct = 0)



### Plug in all .x variables
telehealth_noms_wide_noms_deal$total_base = apply(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], 1, mean, na.rm = TRUE)
### Plug in all .y expect for telehealth.y
telehealth_noms_wide_noms_deal$total_month6 = apply(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], 1, mean, na.rm = TRUE)
hist(telehealth_noms_wide_noms_deal$total_base)
hist(telehealth_noms_wide_noms_deal$total_month6)

## No data for difference scores so try just 6months
range(telehealth_noms_wide_noms_deal$total_month6, na.rm = TRUE)
dim(telehealth_noms_wide_noms_deal)
head(telehealth_noms_wide_noms_deal)
telehealth_noms_wide_noms_deal
telehealth_noms_wide_noms_deal_month6_complete = na.omit(telehealth_noms_wide_noms_deal[c("total_month6", "telehealth.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])
dim(telehealth_noms_wide_noms_deal_month6_complete)
telehealth_noms_wide_noms_deal_month6_complete
telehealth_noms_wide_noms_deal_month6_complete$face_to_face = ifelse(telehealth_noms_wide_noms_deal_month6_complete$telehealth.y == 1,0,1)



library(rstanarm)
library(descr)
### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
describe.factor(telehealth_noms_wide_noms_deal_month6_complete$telehealth.y)
n_total = dim(telehealth_noms_wide_noms_deal_month6_complete)[1]

bayes_p_change_deal = stan_glm(log(total_month6)~ face_to_face + Agegroup.y+ Gender.y+ RaceWhite.y+ dep+ bipolar, prior = my_prior, data = telehealth_noms_wide_noms_deal_month6_complete, seed = 123)
car::vif(bayes_p_change_deal)
lmtest::bptest(bayes_p_change_deal)
#launch_shinystan(bayes_p_change_deal)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_deal_sum = round(bayes_p_change_deal$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
options(scipen = 999)
### Log postioer is werid, because you exp'ed it.
bayes_p_change_deal_sum = round(exp(bayes_p_change_deal_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_deal_sum= bayes_p_change_deal_sum - 1
bayes_p_change_deal_sum
### Grabing the means, sds, and n's for each group
mean_sd_deal= round(compmeans(telehealth_noms_wide_noms_deal_month6_complete$total_month6, telehealth_noms_wide_noms_deal_month6_complete$telehealth.y),2)
mean_sd_deal
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_deal_d =  psych::cohen.d(telehealth_noms_wide_noms_deal_month6_complete$total_month6, group = telehealth_noms_wide_noms_deal_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)
results_deal = data.frame(par_estimate = bayes_p_change_deal_sum[2,1], sd_p_change =  bayes_p_change_deal_sum[2,2], ci_95 = paste0(bayes_p_change_deal_sum[2,3], ",", bayes_p_change_deal_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_deal[1,2], n_post_telehealth = mean_sd_deal[2,2], raw_p_change = round((mean_sd_deal[2,1] -  mean_sd_deal[1,1]) /  mean_sd_deal[1,1],3), telehealth_mean =  mean_sd_deal[2,1], face_to_face_mean =  mean_sd_deal[1,1], telehealth_sd= mean_sd_deal[2,3], face_to_face_sd= mean_sd_deal[1,3],freq_cohen_d = round(month_6_deal_d$cohen.d[2],3))

write.csv(results_deal, "results_deal.csv", row.names = FALSE)
results_deal

```
Deal with diagnosis
```{r}
telehealth_noms_wide_noms_deal = telehealth_noms_wide_noms[c("telehealth.y", "HandlingDailyLife.x", "HandlingDailyLife.y", "ControlLife.x", "ControlLife.y", "DealWithCrisis.x", "DealWithCrisis.y", "GetsAlongWithFamily.x","GetsAlongWithFamily.y", "SocialSituations.x", "SocialSituations.y", "SchoolOrWork.x", "SchoolOrWork.y", "FunctioningHousing.x", "FunctioningHousing.y", "Symptoms.x","Symptoms.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_noms_wide_noms_deal,2, function(x){describe.factor(x)})
library(psych)

### Plug in all the .x variables 
omega_deal_base =  omega(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
omega_deal_6month =  omega(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], poly = TRUE)
omega_deal_6month

### Plug in all the .x variables 
vss(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], cor = "poly")
fa(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], cor = "poly", correct = 0)



### Plug in all .x variables
telehealth_noms_wide_noms_deal$total_base = apply(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], 1, mean, na.rm = TRUE)
### Plug in all .y expect for telehealth.y
telehealth_noms_wide_noms_deal$total_month6 = apply(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], 1, mean, na.rm = TRUE)
hist(telehealth_noms_wide_noms_deal$total_base)
hist(telehealth_noms_wide_noms_deal$total_month6)

## No data for difference scores so try just 6months
range(telehealth_noms_wide_noms_deal$total_month6, na.rm = TRUE)
dim(telehealth_noms_wide_noms_deal)
head(telehealth_noms_wide_noms_deal)
telehealth_noms_wide_noms_deal
telehealth_noms_wide_noms_deal_month6_complete = na.omit(telehealth_noms_wide_noms_deal[c("total_month6", "telehealth.y", "Agegroup.y", "Gender.y", "RaceWhite.y","dep", "bipolar")])
dim(telehealth_noms_wide_noms_deal_month6_complete)
telehealth_noms_wide_noms_deal_month6_complete
telehealth_noms_wide_noms_deal_month6_complete$face_to_face = ifelse(telehealth_noms_wide_noms_deal_month6_complete$telehealth.y == 1,0,1)



library(rstanarm)
library(descr)
### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
describe.factor(telehealth_noms_wide_noms_deal_month6_complete$telehealth.y)
n_total = dim(telehealth_noms_wide_noms_deal_month6_complete)[1]

bayes_p_change_deal = stan_glm(log(total_month6)~ face_to_face*dep +  face_to_face*bipolar+ Agegroup.y+ Gender.y+ RaceWhite.y, prior = my_prior, data = telehealth_noms_wide_noms_deal_month6_complete, seed = 123)
check_collinearity(bayes_p_change_deal)
#launch_shinystan(bayes_p_change_deal)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_deal_sum = round(bayes_p_change_deal$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_deal_sum = round(exp(bayes_p_change_deal_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_deal_sum= bayes_p_change_deal_sum - 1
bayes_p_change_deal_sum
### Grabing the means, sds, and n's for each group
mean_sd_deal= round(compmeans(telehealth_noms_wide_noms_deal_month6_complete$total_month6, telehealth_noms_wide_noms_deal_month6_complete$telehealth.y),2)
mean_sd_deal
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_deal_d =  psych::cohen.d(telehealth_noms_wide_noms_deal_month6_complete$total_month6, group = telehealth_noms_wide_noms_deal_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)
results_deal_diag = data.frame(par_estimate = bayes_p_change_deal_sum[2,1], sd_p_change =  bayes_p_change_deal_sum[2,2], ci_95 = paste0(bayes_p_change_deal_sum[2,3], ",", bayes_p_change_deal_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_deal[1,2], n_post_telehealth = mean_sd_deal[2,2], raw_p_change = round((mean_sd_deal[2,1] -  mean_sd_deal[1,1]) /  mean_sd_deal[1,1],3), telehealth_mean =  mean_sd_deal[2,1], face_to_face_mean =  mean_sd_deal[1,1], telehealth_sd= mean_sd_deal[2,3], face_to_face_sd= mean_sd_deal[1,3],freq_cohen_d = round(month_6_deal_d$cohen.d[2],3))

write.csv(results_deal_diag, "results_deal_diag.csv", row.names = FALSE)
results_deal_diag
```


Manage mental health with diagnosis with t.test for just telehealth (Run diagnosis regression to get telehealth_noms_wide_noms_deal_month6_complete)
```{r}
deal_diag_tele_only_dat = subset(telehealth_noms_wide_noms_deal_month6_complete, telehealth.y == 1)
hist(deal_diag_tele_only_dat$total_month6)
qqnorm(deal_diag_tele_only_dat$total_month6)
compmeans(deal_diag_tele_only_dat$total_month6, deal_diag_tele_only_dat$dep)

### recode dep so it compares dep first relative to not depressed
deal_diag_tele_only_dat$dep_recode = ifelse(deal_diag_tele_only_dat$dep == 1, 0,1)
dep_deal = t.test(deal_diag_tele_only_dat$total_month6 ~ deal_diag_tele_only_dat$dep_recode)
dep_deal = data.frame(t_value = dep_deal$statistic, p_value = dep_deal$p.value, lower = dep_deal$conf.int[1], upper = dep_deal$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(deal_diag_tele_only_dat$total_month6 ~ deal_diag_tele_only_dat$dep_recode)

n_dep_deal =describe.factor(deal_diag_tele_only_dat$dep)
n_dep_deal = data.frame(n_dep_deal)
n_dep_deal = n_dep_deal[1,]
n_dep_deal = data.frame(n_dep_deal)
colnames(n_dep_deal) = n_dep_deal
colnames(n_dep_deal) = c("No major depression count", "Major depression count")
n_deal_dat = data.frame(dim(deal_diag_tele_only_dat)[1])
colnames(n_deal_dat) = "Manage mental health total n"

deal_dep_means = compmeans(deal_diag_tele_only_dat$total_month6, deal_diag_tele_only_dat$dep)

deal_dep_d = psych::cohen.d(deal_diag_tele_only_dat$total_month6, group = deal_diag_tele_only_dat$dep)


results_deal_dep = data.frame(dep_deal, n_total = n_deal_dat, n_dep_deal[2], n_dep_deal[1], raw_p_change = round((deal_dep_means[2,1] -  deal_dep_means[1,1]) /  deal_dep_means[1,1],3), dep_mean =  deal_dep_means[2,1], no_dep_mean =  deal_dep_means[1,1], dep_sd= deal_dep_means[2,3], no_dep_sd= deal_dep_means[1,3],freq_cohen_d = round(deal_dep_d$cohen.d[2],3))
results_deal_dep = round(results_deal_dep, 2)
library(gt)
title_results_deal_dep = "T-test results for managing mental health at 6-months for telehealth clients with major depression"
table_results_deal_dep = 
  gt(results_deal_dep) %>%
  tab_header(title = title_results_deal_dep)%>%
  cols_label(t_value = md("T-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Satisfaction.total.n = md("Satisfaction total n") , Major.depression.count = md("Major depression count"), No.major.depression.count = md("No major depression count"), raw_p_change = md("raw p change"), dep_mean = md("Depression mean"), no_dep_mean = md("No depression mean"), dep_sd = md("Depression sd"), no_dep_sd = md("No depression sd"), freq_cohen_d = md("Cohen D"))
table_results_deal_dep

gtsave(table_results_deal_dep, "table_results_deal_dep.png")
#############################################################################################################################
### Bipolar now
deal_diag_tele_only_dat = subset(telehealth_noms_wide_noms_deal_month6_complete, telehealth.y == 1)
hist(deal_diag_tele_only_dat$total_month6)
qqnorm(deal_diag_tele_only_dat$total_month6)
compmeans(deal_diag_tele_only_dat$total_month6, deal_diag_tele_only_dat$bipolar)

### recode bipolar so it compares bipolar first relative to not bipolarressed
deal_diag_tele_only_dat$bipolar_recode = ifelse(deal_diag_tele_only_dat$bipolar == 1, 0,1)
bipolar_deal = t.test(deal_diag_tele_only_dat$total_month6 ~ deal_diag_tele_only_dat$bipolar_recode)
bipolar_deal = data.frame(t_value = bipolar_deal$statistic, p_value = bipolar_deal$p.value, lower = bipolar_deal$conf.int[1], upper = bipolar_deal$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(deal_diag_tele_only_dat$total_month6 ~ deal_diag_tele_only_dat$bipolar_recode)

n_bipolar_deal =describe.factor(deal_diag_tele_only_dat$bipolar)
n_bipolar_deal = data.frame(n_bipolar_deal)
n_bipolar_deal = n_bipolar_deal[1,]
n_bipolar_deal = data.frame(n_bipolar_deal)
colnames(n_bipolar_deal) = n_bipolar_deal
colnames(n_bipolar_deal) = c("No bipolar count", "Bipolar count")
n_deal_dat = data.frame(dim(deal_diag_tele_only_dat)[1])
colnames(n_deal_dat) = "Manage mental health total n"

deal_bipolar_means = compmeans(deal_diag_tele_only_dat$total_month6, deal_diag_tele_only_dat$bipolar)

deal_bipolar_d = psych::cohen.d(deal_diag_tele_only_dat$total_month6, group = deal_diag_tele_only_dat$bipolar)


results_deal_bipolar = data.frame(bipolar_deal, n_total = n_deal_dat, n_bipolar_deal[2], n_bipolar_deal[1], raw_p_change = round((deal_bipolar_means[2,1] -  deal_bipolar_means[1,1]) /  deal_bipolar_means[1,1],3), bipolar_mean =  deal_bipolar_means[2,1], no_bipolar_mean =  deal_bipolar_means[1,1], bipolar_sd= deal_bipolar_means[2,3], no_bipolar_sd= deal_bipolar_means[1,3],freq_cohen_d = round(deal_bipolar_d$cohen.d[2],3))
results_deal_bipolar = round(results_deal_bipolar, 2)
library(gt)
title_results_deal_bipolar = "T-test results for managing mental health at 6-months for telehealth clients with primary diagnosis as bipolar"
table_results_deal_bipolar = 
  gt(results_deal_bipolar) %>%
  tab_header(title = title_results_deal_bipolar)%>%
  cols_label(t_value = md("T-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Satisfaction.total.n = md("Satisfaction total n") , Bipolar.count = md("Bipolar count"), No.bipolar.count = md("No bipolar count"), raw_p_change = md("raw p change"), bipolar_mean = md("Bipolar mean"), no_bipolar_mean = md("No bipolar mean"), bipolar_sd = md("Bipolar sd"), no_bipolar_sd = md("No bipolar sd"), freq_cohen_d = md("Cohen D"))
table_results_deal_bipolar

gtsave(table_results_deal_bipolar, "table_results_deal_bipolar.png")



```



####Jess
####################################################
Feeling in last 30 days
4 = All of the Time
3 = Most of the Time
2 = Some of the Time
1 = A Little of the Time
0 = None of the Time

Scale is changed to add one so I can take the log!!!!!!!!!
During the past 30 days, about how often did you feel …
a.	nervous?
b.	hopeless?
c.	restless or fidgety?
d.	so depressed that nothing could cheer you up?
e.	that everything was an effort?
f.	worthless?
```{r}

telehealth_noms_wide_noms_feel = telehealth_noms_wide_noms[c("telehealth.y", "Nervous.y", "Hopeless.y", "Restless.y", "Depressed.y", "EverythingEffort.y", "Worthless.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_noms_wide_noms_feel,2, function(x){describe.factor(x)})
library(psych)

telehealth_noms_wide_noms_feel[c(2:7)] = telehealth_noms_wide_noms_feel[c(2:7)]+1
### Plug in all the .y variables except telehealth.y 
omega_feel_6month =  omega(telehealth_noms_wide_noms_feel[c(2:7)], poly = TRUE)
omega_feel_6month

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_feel[c(2:7)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_feel[c(2:7)], cor = "poly", correct = 0)



### Plug in all .y expect for telehealth.y
telehealth_noms_wide_noms_feel$total_month6 = apply(telehealth_noms_wide_noms_feel[c(2:7)], 1, mean, na.rm = TRUE)
hist(log(telehealth_noms_wide_noms_feel$total_month6))

## No data for difference scores so try just 6months
range(telehealth_noms_wide_noms_feel$total_month6, na.rm = TRUE)
dim(telehealth_noms_wide_noms_feel)
head(telehealth_noms_wide_noms_feel)
telehealth_noms_wide_noms_feel
telehealth_noms_wide_noms_feel_month6_complete = na.omit(telehealth_noms_wide_noms_feel[c("total_month6", "telehealth.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])

#Scale is changed to add one so I can take the log!!!!!!!!! Need to change after NA


dim(telehealth_noms_wide_noms_feel_month6_complete)
telehealth_noms_wide_noms_feel_month6_complete
telehealth_noms_wide_noms_feel_month6_complete$face_to_face = ifelse(telehealth_noms_wide_noms_feel_month6_complete$telehealth.y == 1,0,1)




library(rstanarm)
library(descr)
### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
describe.factor(telehealth_noms_wide_noms_feel_month6_complete$telehealth.y)
n_total = dim(telehealth_noms_wide_noms_feel_month6_complete)[1]
telehealth_noms_wide_noms_feel_month6_complete
apply(telehealth_noms_wide_noms_feel_month6_complete, 2, function(x){describe.factor(x)})

bayes_p_change_feel = stan_glm(log(total_month6)~ face_to_face + Agegroup.y+ Gender.y+ RaceWhite.y + dep + bipolar, prior = my_prior, data = telehealth_noms_wide_noms_feel_month6_complete, seed = 123)
car::vif(bayes_p_change_feel)
lmtest::bptest(bayes_p_change_feel)
#launch_shinystan(bayes_p_change_feel)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_feel_sum = round(bayes_p_change_feel$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_feel_sum = round(exp(bayes_p_change_feel_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_feel_sum= bayes_p_change_feel_sum - 1
bayes_p_change_feel_sum
### Grabing the means, sds, and n's for each group
mean_sd_feel= round(compmeans(telehealth_noms_wide_noms_feel_month6_complete$total_month6, telehealth_noms_wide_noms_feel_month6_complete$telehealth.y),2)
mean_sd_feel
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_feel_d =  psych::cohen.d(telehealth_noms_wide_noms_feel_month6_complete$total_month6, group = telehealth_noms_wide_noms_feel_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)
results_feel = data.frame(par_estimate = bayes_p_change_feel_sum[2,1], sd_p_change =  bayes_p_change_feel_sum[2,2], ci_95 = paste0(bayes_p_change_feel_sum[2,3], ",", bayes_p_change_feel_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_feel[1,2], n_post_telehealth = mean_sd_feel[2,2], raw_p_change = round((mean_sd_feel[2,1] -  mean_sd_feel[1,1]) /  mean_sd_feel[1,1],3), telehealth_mean =  mean_sd_feel[2,1], face_to_face_mean =  mean_sd_feel[1,1], telehealth_sd= mean_sd_feel[2,3], face_to_face_sd= mean_sd_feel[1,3],freq_cohen_d = round(month_6_feel_d$cohen.d[2],3))

write.csv(results_feel, "results_feel.csv", row.names = FALSE)
results_feel
```
Feeling last 30 days with diagnosis
```{r}
telehealth_noms_wide_noms_feel = telehealth_noms_wide_noms[c("telehealth.y", "Nervous.y", "Hopeless.y", "Restless.y", "Depressed.y", "EverythingEffort.y", "Worthless.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_noms_wide_noms_feel,2, function(x){describe.factor(x)})
library(psych)

telehealth_noms_wide_noms_feel[c(2:7)] = telehealth_noms_wide_noms_feel[c(2:7)]+1
apply(telehealth_noms_wide_noms_feel,2, function(x){describe.factor(x)})
### Plug in all the .y variables except telehealth.y 
omega_feel_6month =  omega(telehealth_noms_wide_noms_feel[c(2:7)], poly = TRUE)
omega_feel_6month

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_feel[c(2:7)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_feel[c(2:7)], cor = "poly", correct = 0)



### Plug in all .y expect for telehealth.y
telehealth_noms_wide_noms_feel$total_month6 = apply(telehealth_noms_wide_noms_feel[c(2:7)], 1, mean, na.rm = TRUE)
hist(log(telehealth_noms_wide_noms_feel$total_month6))

## No data for difference scores so try just 6months
range(telehealth_noms_wide_noms_feel$total_month6, na.rm = TRUE)
dim(telehealth_noms_wide_noms_feel)
head(telehealth_noms_wide_noms_feel)
telehealth_noms_wide_noms_feel
telehealth_noms_wide_noms_feel_month6_complete = na.omit(telehealth_noms_wide_noms_feel[c("total_month6", "telehealth.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])

#Scale is changed to add one so I can take the log!!!!!!!!! Need to change after NA


dim(telehealth_noms_wide_noms_feel_month6_complete)
telehealth_noms_wide_noms_feel_month6_complete
telehealth_noms_wide_noms_feel_month6_complete$face_to_face = ifelse(telehealth_noms_wide_noms_feel_month6_complete$telehealth.y == 1,0,1)

library(rstanarm)
library(descr)
### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
describe.factor(telehealth_noms_wide_noms_feel_month6_complete$telehealth.y)
n_total = dim(telehealth_noms_wide_noms_feel_month6_complete)[1]
telehealth_noms_wide_noms_feel_month6_complete
apply(telehealth_noms_wide_noms_feel_month6_complete, 2, function(x){describe.factor(x)})

bayes_p_change_feel = stan_glm(log(total_month6)~ face_to_face*dep + face_to_face*bipolar + Agegroup.y+ Gender.y+ RaceWhite.y, prior = my_prior, data = telehealth_noms_wide_noms_feel_month6_complete, seed = 123)
check_collinearity(bayes_p_change_feel)
#launch_shinystan(bayes_p_change_feel)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_feel_sum = round(bayes_p_change_feel$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_feel_sum = round(exp(bayes_p_change_feel_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_feel_sum= bayes_p_change_feel_sum - 1
bayes_p_change_feel_sum
### Grabing the means, sds, and n's for each group
mean_sd_feel= round(compmeans(telehealth_noms_wide_noms_feel_month6_complete$total_month6, telehealth_noms_wide_noms_feel_month6_complete$telehealth.y),2)
mean_sd_feel
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_feel_d =  psych::cohen.d(telehealth_noms_wide_noms_feel_month6_complete$total_month6, group = telehealth_noms_wide_noms_feel_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)
results_feel_diag = data.frame(par_estimate = bayes_p_change_feel_sum[2,1], sd_p_change =  bayes_p_change_feel_sum[2,2], ci_95 = paste0(bayes_p_change_feel_sum[2,3], ",", bayes_p_change_feel_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_feel[1,2], n_post_telehealth = mean_sd_feel[2,2], raw_p_change = round((mean_sd_feel[2,1] -  mean_sd_feel[1,1]) /  mean_sd_feel[1,1],3), telehealth_mean =  mean_sd_feel[2,1], face_to_face_mean =  mean_sd_feel[1,1], telehealth_sd= mean_sd_feel[2,3], face_to_face_sd= mean_sd_feel[1,3],freq_cohen_d = round(month_6_feel_d$cohen.d[2],3))

write.csv(results_feel_diag, "results_feel_diag.csv", row.names = FALSE)
results_feel_diag
```
Manage last 30 days with diagnosis with t.test for just telehealth Run diagnosis regression to get telehealth_noms_wide_noms_feel_month6_complete)
### Redo with wilcox test
```{r}
feel_diag_tele_only_dat = subset(telehealth_noms_wide_noms_feel_month6_complete, telehealth.y == 1)
hist(feel_diag_tele_only_dat$total_month6)
qqnorm(feel_diag_tele_only_dat$total_month6)
compmeans(feel_diag_tele_only_dat$total_month6, feel_diag_tele_only_dat$dep)

### recode dep so it compares dep first relative to not depressed
feel_diag_tele_only_dat$dep_recode = ifelse(feel_diag_tele_only_dat$dep == 1, 0,1)
dep_feel = wilcox.test(feel_diag_tele_only_dat$total_month6 ~ feel_diag_tele_only_dat$dep_recode, conf.int = TRUE)

dep_feel = data.frame(t_value = dep_feel$statistic, p_value = dep_feel$p.value, lower = dep_feel$conf.int[1], upper = dep_feel$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(feel_diag_tele_only_dat$total_month6 ~ feel_diag_tele_only_dat$dep_recode)

n_dep_feel =describe.factor(feel_diag_tele_only_dat$dep)
n_dep_feel = data.frame(n_dep_feel)
n_dep_feel = n_dep_feel[1,]
n_dep_feel = data.frame(n_dep_feel)
colnames(n_dep_feel) = n_dep_feel
colnames(n_dep_feel) = c("No major depression count", "Major depression count")
n_feel_dat = data.frame(dim(feel_diag_tele_only_dat)[1])
colnames(n_feel_dat) = "Mental health 30 days total n"

feel_dep_means = compmeans(feel_diag_tele_only_dat$total_month6, feel_diag_tele_only_dat$dep)

feel_dep_d = psych::cohen.d(feel_diag_tele_only_dat$total_month6, group = feel_diag_tele_only_dat$dep)


results_feel_dep = data.frame(dep_feel, n_total = n_feel_dat, n_dep_feel[2], n_dep_feel[1], raw_p_change = round((feel_dep_means[2,1] -  feel_dep_means[1,1]) /  feel_dep_means[1,1],3), dep_mean =  feel_dep_means[2,1], no_dep_mean =  feel_dep_means[1,1], dep_sd= feel_dep_means[2,3], no_dep_sd= feel_dep_means[1,3])
results_feel_dep = round(results_feel_dep, 2)
library(gt)
title_results_feel_dep = "Wilcox test results for mental health in last 30 days at 6-months for telehealth clients with major depression"
table_results_feel_dep = 
  gt(results_feel_dep) %>%
  tab_header(title = title_results_feel_dep)%>%
  cols_label(t_value = md("W-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Mental.health.30.days.total.n = md("Mental health 30 days total n") , Major.depression.count = md("Major depression count"), No.major.depression.count = md("No major depression count"), raw_p_change = md("raw p change"), dep_mean = md("Depression mean"), no_dep_mean = md("No depression mean"), dep_sd = md("Depression sd"), no_dep_sd = md("No depression sd"))
table_results_feel_dep

gtsave(table_results_feel_dep, "table_results_feel_dep.png")
#############################################################################################################################
### Bipolar now
feel_diag_tele_only_dat = subset(telehealth_noms_wide_noms_feel_month6_complete, telehealth.y == 1)
hist(feel_diag_tele_only_dat$total_month6)
qqnorm(feel_diag_tele_only_dat$total_month6)
compmeans(feel_diag_tele_only_dat$total_month6, feel_diag_tele_only_dat$bipolar)

### recode bipolar so it compares bipolar first relative to not bipolarressed
feel_diag_tele_only_dat$bipolar_recode = ifelse(feel_diag_tele_only_dat$bipolar == 1, 0,1)
bipolar_feel = wilcox.test(feel_diag_tele_only_dat$total_month6 ~ feel_diag_tele_only_dat$bipolar_recode, conf.int = TRUE)
bipolar_feel = data.frame(t_value = bipolar_feel$statistic, p_value = bipolar_feel$p.value, lower = bipolar_feel$conf.int[1], upper = bipolar_feel$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(feel_diag_tele_only_dat$total_month6 ~ feel_diag_tele_only_dat$bipolar_recode)

n_bipolar_feel =describe.factor(feel_diag_tele_only_dat$bipolar)
n_bipolar_feel = data.frame(n_bipolar_feel)
n_bipolar_feel = n_bipolar_feel[1,]
n_bipolar_feel = data.frame(n_bipolar_feel)
colnames(n_bipolar_feel) = n_bipolar_feel
colnames(n_bipolar_feel) = c("No bipolar count", "Bipolar count")
n_feel_dat = data.frame(dim(feel_diag_tele_only_dat)[1])
colnames(n_feel_dat) = "Mental health 30 days total n"

feel_bipolar_means = compmeans(feel_diag_tele_only_dat$total_month6, feel_diag_tele_only_dat$bipolar)

feel_bipolar_d = psych::cohen.d(feel_diag_tele_only_dat$total_month6, group = feel_diag_tele_only_dat$bipolar)


results_feel_bipolar = data.frame(bipolar_feel, n_total = n_feel_dat, n_bipolar_feel[2], n_bipolar_feel[1], raw_p_change = round((feel_bipolar_means[2,1] -  feel_bipolar_means[1,1]) /  feel_bipolar_means[1,1],3), bipolar_mean =  feel_bipolar_means[2,1], no_bipolar_mean =  feel_bipolar_means[1,1], bipolar_sd= feel_bipolar_means[2,3], no_bipolar_sd= feel_bipolar_means[1,3],freq_cohen_d = round(feel_bipolar_d$cohen.d[2],3))
results_feel_bipolar = round(results_feel_bipolar, 2)
library(gt)
title_results_feel_bipolar = "Wilcox test results for mental health in last 30 days at 6-months for telehealth clients with primary diagnosis as bipolar"
table_results_feel_bipolar = 
  gt(results_feel_bipolar) %>%
  tab_header(title = title_results_feel_bipolar)%>%
  cols_label(t_value = md("W-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Mental.health.30.days.total.n = md("Mental health 30 days total n") , Bipolar.count = md("Bipolar count"), No.bipolar.count = md("No bipolar count"), raw_p_change = md("raw p change"), bipolar_mean = md("Bipolar mean"), no_bipolar_mean = md("No bipolar mean"), bipolar_sd = md("Bipolar sd"), no_bipolar_sd = md("No bipolar sd"), freq_cohen_d = md("Cohen D"))
table_results_feel_bipolar

gtsave(table_results_feel_bipolar, "table_results_feel_bipolar.png")



```

#######################

6.	The following questions relate to your experience with alcohol, cigarettes, and other drugs. Some of the substances we’ll talk about are prescribed by a doctor (like pain medications). But I will only record those if you have taken them for reasons or in doses other than prescribed.

In the past 30 days, how often have you used …

a.	tobacco products (cigarettes, chewing tobacco, cigars, etc.)?
Tobacco_Use

b.	alcoholic beverages (beer, wine, liquor, etc.)?
Alcohol_Use

k.	prescription opioids (fentanyl, oxycodone [OxyContin, Percocet], hydrocodone [Vicodin], methadone, buprenorphine, etc.)?
RxOpioids_Use

j.	street opioids (heroin, opium, etc.)?
StreetOpioids_Use

```{r}

### All items should be 1 to 5
### Not enough for drugs
telehealth_alc_tob = telehealth_noms_wide_noms[c("telehealth.y", "Tobacco_Use.x", "Tobacco_Use.y", "Alcohol_Use.x", "Alcohol_Use.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_alc_tob,2, function(x){describe.factor(x)})
library(psych)
### Only two items not needed.
### Plug in all the .x variables 
#omega_sat_base =  omega(telehealth_noms_wide_noms_sat[c(2,4,6,8)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
#omega_sat_6month =  omega(telehealth_noms_wide_noms_sat[c(3,5,7,9)], poly = TRUE)
#omega_sat_6month

### Plug in all the .x variables 
#vss(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly")
#fa(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
#vss(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly")
#fa(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly", correct = 0)

########### 
# Create total scores and use sum so you can get counts 
### Plug in all .x variables
telehealth_alc_tob$total_base = apply(telehealth_alc_tob[c(2,4)], 1, mean, na.rm = TRUE)
### Plug in all .y expect for telehealth.y
telehealth_alc_tob$total_month6 = apply(telehealth_alc_tob[c(3,5)], 1, mean, na.rm = TRUE)
hist(log(telehealth_alc_tob$total_month6))

### Make binary anything greater than 1 means you used alcohol or tobacco 
describe.factor(telehealth_alc_tob$total_month6)
hist(telehealth_alc_tob$total_month6)
compmeans(telehealth_alc_tob$total_month6, telehealth_alc_tob$telehealth.y)
describeBy(telehealth_alc_tob$total_month6, telehealth_alc_tob$telehealth.y)


telehealth_alc_tob$total_month6 = ifelse(telehealth_alc_tob$total_month6 == 1, 1,0)

## No data for difference scores so try just 6months
range(telehealth_alc_tob$total_month6, na.rm = TRUE)
dim(telehealth_alc_tob)
head(telehealth_alc_tob)
telehealth_alc_tob
telehealth_alc_tob_complete = na.omit(telehealth_alc_tob[c("total_month6", "telehealth.y","Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])
dim(telehealth_alc_tob_complete)
telehealth_alc_tob_complete
describe.factor(telehealth_alc_tob_complete$telehealth.y)

############ Run posisson regression
library(rstanarm)
library(descr)
### Scale is exp(.2) which means 20% difference in each direction


my_prior = normal(location = 0, scale = exp(.2), autoscale = FALSE)
my_prior = student_t(4,0,.2)
describe.factor(telehealth_alc_tob_complete$telehealth.y)
n_total = dim(telehealth_alc_tob_complete)[1]
## Take log of outcome to get percentage change interpretation

telehealth_alc_tob_complete$face_to_face = ifelse(telehealth_alc_tob_complete$telehealth.y == 1, 0,1)
bayes_p_change_al_tob = stan_glm(total_month6~ face_to_face + Agegroup.y + Gender.y + RaceWhite.y + dep + bipolar, prior = my_prior, family = binomial(link = "logit"), data = telehealth_alc_tob_complete, seed = 123)
bayes_p_change_al_tob
car::vif(bayes_p_change_al_tob)
#launch_shinystan(bayes_p_change_al_tob)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_al_tob_sum = round(bayes_p_change_al_tob$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_al_tob_sum = round(exp(bayes_p_change_al_tob_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_al_tob_sum= bayes_p_change_al_tob_sum - 1
bayes_p_change_al_tob_sum

### Need mean sd change here
telehealth_alc_tob_complete

mean_sd_alc_tob= round(compmeans(telehealth_alc_tob_complete$total_month6, telehealth_alc_tob_complete$telehealth.y),2)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)

results_alc_tob = data.frame(odds_change_alc_tob = bayes_p_change_al_tob_sum[2,1], sd_odds_change =  bayes_p_change_al_tob_sum[2,2], ci_95 = paste0(bayes_p_change_al_tob_sum[2,3], ",", bayes_p_change_al_tob_sum[2,4]), n_total = mean_sd_sat[3,2], n_pre_telehealth = mean_sd_sat[1,2], n_post_telehealth = mean_sd_sat[2,2], raw_p_change = round((mean_sd_alc_tob[2,1]-mean_sd_alc_tob[1,1])/mean_sd_alc_tob[2,1],2), p_post = mean_sd_alc_tob[2,1], p_pre = mean_sd_alc_tob[1,1])

write.csv(results_alc_tob, "results_alc_tob.csv", row.names = FALSE)
results_alc_tob
prior_summary(bayes_p_change_al_tob)
exp(.2)


```
Alcohol and tobacoo diagnosis
```{r}
### All items should be 1 to 5
### Not enough for drugs
telehealth_alc_tob = telehealth_noms_wide_noms[c("telehealth.y", "Tobacco_Use.x", "Tobacco_Use.y", "Alcohol_Use.x", "Alcohol_Use.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_alc_tob,2, function(x){describe.factor(x)})
library(psych)
### Only two items not needed.
### Plug in all the .x variables 
#omega_sat_base =  omega(telehealth_noms_wide_noms_sat[c(2,4,6,8)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
#omega_sat_6month =  omega(telehealth_noms_wide_noms_sat[c(3,5,7,9)], poly = TRUE)
#omega_sat_6month

### Plug in all the .x variables 
#vss(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly")
#fa(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
#vss(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly")
#fa(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly", correct = 0)

########### 
# Create total scores and use sum so you can get counts 
### Plug in all .x variables
telehealth_alc_tob$total_base = apply(telehealth_alc_tob[c(2,4)], 1, mean, na.rm = TRUE)
### Plug in all .y expect for telehealth.y
telehealth_alc_tob$total_month6 = apply(telehealth_alc_tob[c(3,5)], 1, mean, na.rm = TRUE)
hist(log(telehealth_alc_tob$total_month6))

### Make binary anything greater than 1 means you used alcohol or tobacco 
describe.factor(telehealth_alc_tob$total_month6)
hist(telehealth_alc_tob$total_month6)
compmeans(telehealth_alc_tob$total_month6, telehealth_alc_tob$telehealth.y)
describeBy (telehealth_alc_tob$total_month6, telehealth_alc_tob$telehealth.y)


telehealth_alc_tob$total_month6 = ifelse(telehealth_alc_tob$total_month6 == 1, 1,0)

## No data for difference scores so try just 6months
range(telehealth_alc_tob$total_month6, na.rm = TRUE)
dim(telehealth_alc_tob)
head(telehealth_alc_tob)
telehealth_alc_tob
telehealth_alc_tob_complete = na.omit(telehealth_alc_tob[c("total_month6", "telehealth.y","Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])
dim(telehealth_alc_tob_complete)
telehealth_alc_tob_complete
describe.factor(telehealth_alc_tob_complete$telehealth.y)

############ Run posisson regression
library(rstanarm)
library(descr)
### Scale is exp(.2) which means 20% difference in each direction


my_prior = normal(location = 0, scale = exp(.2), autoscale = FALSE)
my_prior = student_t(4,0,.2)
describe.factor(telehealth_alc_tob_complete$telehealth.y)
n_total = dim(telehealth_alc_tob_complete)[1]
## Take log of outcome to get percentage change interpretation

telehealth_alc_tob_complete$face_to_face = ifelse(telehealth_alc_tob_complete$telehealth.y == 1, 0,1)
bayes_p_change_al_tob = stan_glm(total_month6~ face_to_face*dep + face_to_face*bipolar + Agegroup.y + Gender.y + RaceWhite.y, prior = my_prior, family = binomial(link = "logit"), data = telehealth_alc_tob_complete, seed = 123)
bayes_p_change_al_tob
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_al_tob_sum = round(bayes_p_change_al_tob$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_al_tob_sum = round(exp(bayes_p_change_al_tob_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_al_tob_sum= bayes_p_change_al_tob_sum - 1
bayes_p_change_al_tob_sum

### Need mean sd change here
telehealth_alc_tob_complete

mean_sd_alc_tob= round(compmeans(telehealth_alc_tob_complete$total_month6, telehealth_alc_tob_complete$telehealth.y),2)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)

results_alc_tob_diag = data.frame(odds_change_alc_tob = bayes_p_change_al_tob_sum[2,1], sd_odds_change =  bayes_p_change_al_tob_sum[2,2], ci_95 = paste0(bayes_p_change_al_tob_sum[2,3], ",", bayes_p_change_al_tob_sum[2,4]), n_total = mean_sd_sat[3,2], n_pre_telehealth = mean_sd_sat[1,2], n_post_telehealth = mean_sd_sat[2,2], raw_p_change = round((mean_sd_alc_tob[2,1]-mean_sd_alc_tob[1,1])/mean_sd_alc_tob[2,1],2), p_post = mean_sd_alc_tob[2,1], p_pre = mean_sd_alc_tob[1,1])

write.csv(results_alc_tob_diag, "results_alc_tob_diag.csv", row.names = FALSE)
results_alc_tob
prior_summary(bayes_p_change_al_tob)
exp(.2)
```
Alcohol and tobacco with diagnosis with t.test for just telehealth 
### Redo with wilcox test
```{r}
#### Get full outcome don't need to dicotmize
telehealth_alc_tob = telehealth_noms_wide_noms[c("telehealth.y", "Tobacco_Use.x", "Tobacco_Use.y", "Alcohol_Use.x", "Alcohol_Use.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_alc_tob,2, function(x){describe.factor(x)})
library(psych)

telehealth_alc_tob$total_month6 = apply(telehealth_alc_tob[c(3,5)], 1, mean, na.rm = TRUE)
hist(telehealth_alc_tob$total_month6)
telehealth_alc_tob_complete = na.omit(telehealth_alc_tob[c("total_month6", "telehealth.y","Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])

alc_tob_diag_tele_only_dat = subset(telehealth_alc_tob_complete, telehealth.y == 1)
hist(alc_tob_diag_tele_only_dat$total_month6)
qqnorm(alc_tob_diag_tele_only_dat$total_month6)
compmeans(alc_tob_diag_tele_only_dat$total_month6, alc_tob_diag_tele_only_dat$dep)

### recode dep so it compares dep first relative to not depressed
alc_tob_diag_tele_only_dat$dep_recode = ifelse(alc_tob_diag_tele_only_dat$dep == 1, 0,1)
dep_alc_tob = wilcox.test(alc_tob_diag_tele_only_dat$total_month6 ~ alc_tob_diag_tele_only_dat$dep_recode, conf.int = TRUE)

dep_alc_tob = data.frame(t_value = dep_alc_tob$statistic, p_value = dep_alc_tob$p.value, lower = dep_alc_tob$conf.int[1], upper = dep_alc_tob$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(alc_tob_diag_tele_only_dat$total_month6 ~ alc_tob_diag_tele_only_dat$dep_recode)

n_dep_alc_tob =describe.factor(alc_tob_diag_tele_only_dat$dep)
n_dep_alc_tob = data.frame(n_dep_alc_tob)
n_dep_alc_tob = n_dep_alc_tob[1,]
n_dep_alc_tob = data.frame(n_dep_alc_tob)
colnames(n_dep_alc_tob) = n_dep_alc_tob
colnames(n_dep_alc_tob) = c("No major depression count", "Major depression count")
n_alc_tob_dat = data.frame(dim(alc_tob_diag_tele_only_dat)[1])
colnames(n_alc_tob_dat) = "Alcohol or tobacco use total n"

alc_tob_dep_means = compmeans(alc_tob_diag_tele_only_dat$total_month6, alc_tob_diag_tele_only_dat$dep)

alc_tob_dep_d = psych::cohen.d(alc_tob_diag_tele_only_dat$total_month6, group = alc_tob_diag_tele_only_dat$dep)


results_alc_tob_dep = data.frame(dep_alc_tob, n_total = n_alc_tob_dat, n_dep_alc_tob[2], n_dep_alc_tob[1], raw_p_change = round((alc_tob_dep_means[2,1] -  alc_tob_dep_means[1,1]) /  alc_tob_dep_means[1,1],3), dep_mean =  alc_tob_dep_means[2,1], no_dep_mean =  alc_tob_dep_means[1,1], dep_sd= alc_tob_dep_means[2,3], no_dep_sd= alc_tob_dep_means[1,3])
results_alc_tob_dep = round(results_alc_tob_dep, 2)
library(gt)
title_results_alc_tob_dep = "Wilcox test results for alcohol or tobacco last 30 days at 6-months for telehealth clients with major depression"
table_results_alc_tob_dep = 
  gt(results_alc_tob_dep) %>%
  tab_header(title = title_results_alc_tob_dep)%>%
  cols_label(t_value = md("W-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Alcohol.or.tobacco.use.total.n
 = md("Alcohol or tobacco total n") , Major.depression.count = md("Major depression count"), No.major.depression.count = md("No major depression count"), raw_p_change = md("raw p change"), dep_mean = md("Depression mean"), no_dep_mean = md("No depression mean"), dep_sd = md("Depression sd"), no_dep_sd = md("No depression sd"))
table_results_alc_tob_dep

gtsave(table_results_alc_tob_dep, "table_results_alc_tob_dep.png")
#############################################################################################################################
### Bipolar now
alc_tob_diag_tele_only_dat = subset(telehealth_alc_tob, telehealth.y == 1)
hist(alc_tob_diag_tele_only_dat$total_month6)
qqnorm(alc_tob_diag_tele_only_dat$total_month6)
compmeans(alc_tob_diag_tele_only_dat$total_month6, alc_tob_diag_tele_only_dat$bipolar)

### recode bipolar so it compares bipolar first relative to not bipolarressed
alc_tob_diag_tele_only_dat$bipolar_recode = ifelse(alc_tob_diag_tele_only_dat$bipolar == 1, 0,1)
bipolar_alc_tob = wilcox.test(alc_tob_diag_tele_only_dat$total_month6 ~ alc_tob_diag_tele_only_dat$bipolar_recode, conf.int = TRUE)
bipolar_alc_tob = data.frame(t_value = bipolar_alc_tob$statistic, p_value = bipolar_alc_tob$p.value, lower = bipolar_alc_tob$conf.int[1], upper = bipolar_alc_tob$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(alc_tob_diag_tele_only_dat$total_month6 ~ alc_tob_diag_tele_only_dat$bipolar_recode)

n_bipolar_alc_tob =describe.factor(alc_tob_diag_tele_only_dat$bipolar)
n_bipolar_alc_tob = data.frame(n_bipolar_alc_tob)
n_bipolar_alc_tob = n_bipolar_alc_tob[1,]
n_bipolar_alc_tob = data.frame(n_bipolar_alc_tob)
colnames(n_bipolar_alc_tob) = n_bipolar_alc_tob
colnames(n_bipolar_alc_tob) = c("No bipolar count", "Bipolar count")
n_alc_tob_dat = data.frame(dim(alc_tob_diag_tele_only_dat)[1])
colnames(n_alc_tob_dat) = "Alcohol or tobacco use total n"

alc_tob_bipolar_means = compmeans(alc_tob_diag_tele_only_dat$total_month6, alc_tob_diag_tele_only_dat$bipolar)

alc_tob_bipolar_d = psych::cohen.d(alc_tob_diag_tele_only_dat$total_month6, group = alc_tob_diag_tele_only_dat$bipolar)


results_alc_tob_bipolar = data.frame(bipolar_alc_tob, n_total = n_alc_tob_dat, n_bipolar_alc_tob[2], n_bipolar_alc_tob[1], raw_p_change = round((alc_tob_bipolar_means[2,1] -  alc_tob_bipolar_means[1,1]) /  alc_tob_bipolar_means[1,1],3), bipolar_mean =  alc_tob_bipolar_means[2,1], no_bipolar_mean =  alc_tob_bipolar_means[1,1], bipolar_sd= alc_tob_bipolar_means[2,3], no_bipolar_sd= alc_tob_bipolar_means[1,3],freq_cohen_d = round(alc_tob_bipolar_d$cohen.d[2],3))
results_alc_tob_bipolar = round(results_alc_tob_bipolar, 2)
library(gt)
title_results_alc_tob_bipolar = "Wilcox test results for alcohol or tobacco last 30 days at 6-months for telehealth clients with primary diagnosis as bipolar"
table_results_alc_tob_bipolar = 
  gt(results_alc_tob_bipolar) %>%
  tab_header(title = title_results_alc_tob_bipolar)%>%
  cols_label(t_value = md("W-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Alcohol.or.tobacco.use.total.n
 = md("Alcohol or tobacco total n") , Bipolar.count = md("Bipolar count"), No.bipolar.count = md("No bipolar count"), raw_p_change = md("raw p change"), bipolar_mean = md("Bipolar mean"), no_bipolar_mean = md("No bipolar mean"), bipolar_sd = md("Bipolar sd"), no_bipolar_sd = md("No bipolar sd"), freq_cohen_d = md("Cohen D"))
table_results_alc_tob_bipolar

gtsave(table_results_alc_tob_bipolar, "table_results_alc_tob_bipolar.png")



```

#################
PERCEPTION OF CARE
1.	In order to provide the best possible mental health and related services, we need to know what you think about the services you received during the past 30 days, the people who provided it, and the results. Please indicate your disagreement/agreement with each of the following statements.
a.	Staff here believe that I can grow, change, and recover.
Recover

b.	I felt free to complain.
Complain

c.	I was given information about my rights.
Rights

d.	Staff encouraged me to take responsibility for how I live my life.
Responsibility

e.	Staff told me what side effects to watch out for.
SideEffects

f.	Staff respected my wishes about who is and who is not to be given information about my treatment.
SharingTreatmentInformation

g.	Staff were sensitive to my cultural background (race, religion, language, etc.).
SensitiveToCulture

h.	Staff helped me obtain the information I needed so that I could take charge of managing my illness.
InformationNeeded

i.	I was encouraged to use consumer-run programs (support groups, drop-in centers, crisis phone line, etc.).
ConsumerRunPrograms

j.	I felt comfortable asking questions about my treatment and medication.
ComfortableAskingQuestions

a.	I, not staff, decided my treatment goals.
TreatmentGoals

b.	I like the services I received here.
LikeServices

c.	If I had other choices, I would still get services from this agency.
Choices

d.	I would recommend this agency to a friend or family member.
RecommendAgency

```{r}
telehealth_noms_wide_pc = telehealth_noms_wide_noms[c("telehealth.y", "Recover.x", "Recover.y", "Rights.x", "Rights.y", "Responsibility.x", "Responsibility.y", "SideEffects.x", "SideEffects.y", "SharingTreatmentInformation.x", "SharingTreatmentInformation.y", "SensitiveToCulture.x", "SensitiveToCulture.y", "InformationNeeded.x", "InformationNeeded.y", "ConsumerRunPrograms.x", "ConsumerRunPrograms.y", "ComfortableAskingQuestions.x", "ComfortableAskingQuestions.y", "TreatmentGoals.x", "TreatmentGoals.y", "LikeServices.x", "LikeServices.y", "Choices.x", "Choices.y", "RecommendAgency.x", "RecommendAgency.y")]

apply(telehealth_noms_wide_pc, 2, function(x){describe.factor(x)})

telehealth_noms_wide_pc$total_month6 = apply(telehealth_noms_wide_pc[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], 1, mean, na.rm = TRUE)


### Psychometrics
### Plug in all the .x variables 
omega_pc_6month =  omega(telehealth_noms_wide_pc[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], poly = TRUE)
omega_pc_6month

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_pc[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], cor = "poly")
fa(telehealth_noms_wide_pc[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], cor = "poly", correct = 0)

telehealth_noms_wide_pc_complete = na.omit(telehealth_noms_wide_pc[c("telehealth.y", "total_month6")])
n_total =  dim(telehealth_noms_wide_pc_complete)[1]
### Just do a wilcox test for now.
mean_sd_pc = compmeans(telehealth_noms_wide_pc_complete$total_month6, telehealth_noms_wide_pc_complete$telehealth.y)
mean_sd_pc = round(mean_sd_pc,3)

### This is comparing face to face with telehealth
results_pc = wilcox.test(telehealth_noms_wide_pc_complete$total_month6 ~telehealth_noms_wide_pc_complete$telehealth.y, conf.int = TRUE)
round(results_pc$estimate,3)


results_pc = data.frame(diff_location = round(results_pc$estimate,3), ci_95 = paste0(round(results_pc$conf.int[1],3), ",", round(results_pc$conf.int[2],3)), n_total = n_total, n_pre_telehealth = mean_sd_pc[1,2], n_post_telehealth = mean_sd_pc[2,2], mean_telehealth = mean_sd_pc[2,1], mean_pre_telehealth = mean_sd_pc[1,1])

results_pc
write.csv(results_pc, "results_pc.csv", row.names = FALSE)

#### What percent of staff have a positive perception of care with centertone 
telehealth_only_positive = subset(telehealth_noms_wide_pc_complete, telehealth.y == 1)
telehealth_only_positive$positive = ifelse(telehealth_only_positive$total_month6 > 3, 1, 0)

agree_p_n =  describe.factor(telehealth_only_positive$positive)
agree_p_n = data.frame(agree_p_n)
agree_p_n$total = apply(agree_p_n, 1, sum)
colnames(agree_p_n) = c("agree_or_greater", "less_than_agree", "total")
agree_p_n = round(agree_p_n,2)
write.csv(agree_p_n, "agree_p_n.csv")

#### Recomend
recommend_agree = subset(telehealth_noms_wide_pc, telehealth.y == 1)
recommend_agree = recommend_agree[c("telehealth.y", "RecommendAgency.y")]
recommend_agree_complete = na.omit(recommend_agree)
recommend_agree_complete$recommend_agree = ifelse(recommend_agree_complete$RecommendAgency.y > 3, 1, 0)
recommend_agree_results =  data.frame(describe.factor(recommend_agree_complete$recommend_agree))
recommend_agree_results$total = apply(recommend_agree_results, 1, sum)
colnames(recommend_agree_results) = c("agree_or_greater", "less_than_agree", "total")
recommend_agree_results = round(recommend_agree_results,2)
recommend_agree_results
write.csv(recommend_agree_results, "recommend_agree_results.csv")
```
Perception of care for dep
```{r}
telehealth_noms_wide_pc_dep = telehealth_noms_wide_noms[c("telehealth.y", "Recover.x", "Recover.y", "Rights.x", "Rights.y", "Responsibility.x", "Responsibility.y", "SideEffects.x", "SideEffects.y", "SharingTreatmentInformation.x", "SharingTreatmentInformation.y", "SensitiveToCulture.x", "SensitiveToCulture.y", "InformationNeeded.x", "InformationNeeded.y", "ConsumerRunPrograms.x", "ConsumerRunPrograms.y", "ComfortableAskingQuestions.x", "ComfortableAskingQuestions.y", "TreatmentGoals.x", "TreatmentGoals.y", "LikeServices.x", "LikeServices.y", "Choices.x", "Choices.y", "RecommendAgency.x", "RecommendAgency.y", "dep", "bipolar")]

apply(telehealth_noms_wide_pc_dep, 2, function(x){describe.factor(x)})

telehealth_noms_wide_pc_dep$total_month6 = apply(telehealth_noms_wide_pc_dep[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], 1, mean, na.rm = TRUE)


telehealth_noms_wide_pc_dep_complete = na.omit(telehealth_noms_wide_pc_dep[c("telehealth.y", "total_month6", "dep", "bipolar")])
telehealth_noms_wide_pc_dep_complete = subset(telehealth_noms_wide_pc_dep_complete, dep == 1)
n_total =  dim(telehealth_noms_wide_pc_dep_complete)[1]


#### What percent of staff with dep  have a positive perception of care with centertone 
telehealth_only_positive = subset(telehealth_noms_wide_pc_dep_complete, telehealth.y == 1)
telehealth_only_positive$positive = ifelse(telehealth_only_positive$total_month6 > 3, 1, 0)

agree_p_n =  describe.factor(telehealth_only_positive$positive)
agree_p_n = data.frame(agree_p_n, less_than_agree = c(0,0))
agree_p_n$total = apply(agree_p_n, 1, sum)
colnames(agree_p_n) = c("agree_or_greater", "less_than_agree", "total")
agree_p_n_dep = round(agree_p_n,2)
write.csv(agree_p_n_dep, "agree_p_n_dep.csv")

#### Recomend for dep
recommend_agree = subset(telehealth_noms_wide_pc_dep, telehealth.y == 1)
recommend_agree = subset(recommend_agree, dep == 1)
recommend_agree = recommend_agree[c("telehealth.y", "RecommendAgency.y")]
recommend_agree_complete = na.omit(recommend_agree)
recommend_agree_complete$recommend_agree = ifelse(recommend_agree_complete$RecommendAgency.y > 3, 1, 0)
recommend_agree_results =  data.frame(describe.factor(recommend_agree_complete$recommend_agree))
recommend_agree_results =data.frame(recommend_agree_results, less_than_agree = c(0,0)) 
recommend_agree_results$total = apply(recommend_agree_results, 1, sum)
colnames(recommend_agree_results) = c("agree_or_greater", "less_than_agree", "total")
recommend_agree_results = round(recommend_agree_results,2)
recommend_agree_results
write.csv(recommend_agree_results, "recommend_agree_results.csv")
```
Perception of care for bipolar
```{r}
telehealth_noms_wide_pc_bipolar = telehealth_noms_wide_noms[c("telehealth.y", "Recover.x", "Recover.y", "Rights.x", "Rights.y", "Responsibility.x", "Responsibility.y", "SideEffects.x", "SideEffects.y", "SharingTreatmentInformation.x", "SharingTreatmentInformation.y", "SensitiveToCulture.x", "SensitiveToCulture.y", "InformationNeeded.x", "InformationNeeded.y", "ConsumerRunPrograms.x", "ConsumerRunPrograms.y", "ComfortableAskingQuestions.x", "ComfortableAskingQuestions.y", "TreatmentGoals.x", "TreatmentGoals.y", "LikeServices.x", "LikeServices.y", "Choices.x", "Choices.y", "RecommendAgency.x", "RecommendAgency.y", "bipolar", "bipolar")]

apply(telehealth_noms_wide_pc_bipolar, 2, function(x){describe.factor(x)})

telehealth_noms_wide_pc_bipolar$total_month6 = apply(telehealth_noms_wide_pc_bipolar[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], 1, mean, na.rm = TRUE)


telehealth_noms_wide_pc_bipolar_complete = na.omit(telehealth_noms_wide_pc_bipolar[c("telehealth.y", "total_month6", "bipolar", "bipolar")])
telehealth_noms_wide_pc_bipolar_complete = subset(telehealth_noms_wide_pc_bipolar_complete, bipolar == 1)
n_total =  dim(telehealth_noms_wide_pc_bipolar_complete)[1]


#### What percent of staff with bipolar  have a positive perception of care with centertone 
telehealth_only_positive = subset(telehealth_noms_wide_pc_bipolar_complete, telehealth.y == 1)
telehealth_only_positive$positive = ifelse(telehealth_only_positive$total_month6 > 3, 1, 0)

agree_p_n =  describe.factor(telehealth_only_positive$positive)
agree_p_n = data.frame(agree_p_n, less_than_agree = c(0,0))
agree_p_n$total = apply(agree_p_n, 1, sum)
colnames(agree_p_n) = c("agree_or_greater", "less_than_agree", "total")
agree_p_n_bipolar = round(agree_p_n,2)
write.csv(agree_p_n_bipolar, "agree_p_n_bipolar.csv")

#### Recomend for bipolar
recommend_agree = subset(telehealth_noms_wide_pc_bipolar, telehealth.y == 1)
recommend_agree = subset(recommend_agree, bipolar == 1)
recommend_agree = recommend_agree[c("telehealth.y", "RecommendAgency.y")]
recommend_agree_complete = na.omit(recommend_agree)
recommend_agree_complete$recommend_agree = ifelse(recommend_agree_complete$RecommendAgency.y > 3, 1, 0)
recommend_agree_results =  data.frame(describe.factor(recommend_agree_complete$recommend_agree))
recommend_agree_results =data.frame(recommend_agree_results) 
recommend_agree_results$total = apply(recommend_agree_results, 1, sum)
colnames(recommend_agree_results) = c("agree_or_greater", "less_than_agree", "total")
recommend_agree_results = round(recommend_agree_results,2)
recommend_agree_results
write.csv(recommend_agree_results, "recommend_agree_results.csv")

```
Health care and job analysis

All nights vairable are 1 to 30.
1 = Full time, 2 = part-time
```{r}
healthcare_dat = data.frame(NightsDetox.x = telehealth_noms_wide_noms$NightsDetox.x, NightsDetox.y = telehealth_noms_wide_noms$NightsDetox.y, NightsHospitalMHC.x = telehealth_noms_wide_noms$NightsHospitalMHC.x, NightsHospitalMHC.y = telehealth_noms_wide_noms$NightsHospitalMHC.y, TimesER.x = telehealth_noms_wide_noms$TimesER.x, TimesER.y = telehealth_noms_wide_noms$TimesER.y, NightsJail.x = telehealth_noms_wide_noms$NightsJail.x, NightsJail.y = telehealth_noms_wide_noms$NightsJail.y, Employment.x = telehealth_noms_wide_noms$Employment.x, Employment.y = telehealth_noms_wide_noms$Employment.y)
```
Check missingness and descriptives
```{r}
healthcare_dat
miss_var_summary(healthcare_dat)
apply(healthcare_dat, 2, function(x){describe.factor(x)})
```
Create employment variables and combine detox and MHC variables
For employment assume 20 for part-time and 30 for full time.  Create variables for both and look at the difference between the two and take that amount times the rate
```{r}
healthcare_dat$part_time.x = ifelse(healthcare_dat$Employment.x == 2, 1, 0)
healthcare_dat$part_time.y = ifelse(healthcare_dat$Employment.y == 2, 1, 0)

healthcare_dat$full_time.x = ifelse(healthcare_dat$Employment.x == 1, 1, 0)
healthcare_dat$full_time.y = ifelse(healthcare_dat$Employment.y == 1, 1, 0)

healthcare_dat$MHC_detox.x = healthcare_dat$NightsHospitalMHC.x+ healthcare_dat$NightsDetox.x
healthcare_dat$MHC_detox.y = healthcare_dat$NightsHospitalMHC.y+ healthcare_dat$NightsDetox.y
```
Look at ER visits.  Get the average number of visits at post minus intake then times by with complete data
```{r}
er_visit_dat = healthcare_dat[c("TimesER.x","TimesER.y")]
er_visit_dat_complete = na.omit(er_visit_dat)
n_er_visit_dat_complete =  dim(er_visit_dat)[1]
er_visit_dat_complete$diff_er =er_visit_dat_complete$TimesER.y  -er_visit_dat_complete$TimesER.x
sum_er_diff = round(sum(er_visit_dat_complete$diff_er), 2)
er_cost= round(sum_er_diff*5680.56,2)
er_cost
```
Hosptial reduction costs
```{r}
hos_visit_dat = healthcare_dat[c("MHC_detox.y","MHC_detox.x")]
hos_visit_dat_complete = na.omit(hos_visit_dat)
n_hos_visit_dat_complete =  dim(hos_visit_dat)[1]
hos_visit_dat_complete$diff_hos =hos_visit_dat_complete$MHC_detox.y -hos_visit_dat_complete$MHC_detox.x
range(hos_visit_dat_complete$diff_hos)
sum_hos_diff = round(sum(hos_visit_dat_complete$diff_hos),2)
hos_cost= round(sum_hos_diff*2534.62,2)
hos_cost
```
Jail time
```{r}
jail_visit_dat = healthcare_dat[c("NightsJail.y","NightsJail.x")]
jail_visit_dat_complete = na.omit(jail_visit_dat)
n_jail_visit_dat_complete =  dim(jail_visit_dat)[1]
jail_visit_dat_complete$diff_jail =jail_visit_dat_complete$NightsJail.y -jail_visit_dat_complete$NightsJail.x
sum_jail_diff = round(sum(jail_visit_dat_complete$diff_jail),2)
jail_cost= round(sum_jail_diff*103.53,2)
jail_cost
```
Employment
Look at the average difference between part time and full jobs.  This is the percentage change which is the percentage of the population that changed that is affected. For example, if the mean of the difference is .15, then 15% of people of increased job so take the amount for that measure times .15 * the number of people in the complete data set.

```{r}
part_dat = healthcare_dat[c("part_time.x","part_time.y")]
part_dat_complete = na.omit(part_dat)
n_part_dat_complete =  dim(part_dat)[1]
part_dat_complete$diff_part =part_dat_complete$part_time.y -part_dat_complete$part_time.x
sum_part_diff = round(sum(part_dat_complete$diff_part),2)
part_cost= round(sum_part_diff*1740,2)
part_cost

full_dat = healthcare_dat[c("full_time.x","full_time.y")]
full_dat_complete = na.omit(full_dat)
n_full_dat_complete =  dim(full_dat)[1]
full_dat_complete$diff_full =full_dat_complete$full_time.y -full_dat_complete$full_time.x
range(full_dat_complete$diff_full)
sum_full_diff = round(sum(full_dat_complete$diff_full),2)
full_cost= round(mean_full_diff*3480, 2)
full_cost


### Get total cost
total_cost = sum(er_cost, hos_cost, jail_cost, -part_cost, -full_cost)
total_cost
```
Make Table with each amount per unit for measure, number of measurses, and the units, then amount.
```{r}
library(dplyr)
tab_dat = matrix(c("ER", "hospital", "jail", "part_time", "full_time", "Total", 5680.56, 2534.62, 103.53, 1740, 3480, "", sum_er_diff, sum_hos_diff, sum_jail_diff, sum_part_diff, sum_full_diff, "", er_cost, hos_cost, jail_cost, part_cost, full_cost, total_cost), nrow = 6)
tab_dat = data.frame(tab_dat)
colnames(tab_dat) = c("measure", "savings_per_unit", "unit_difference", "cost_savings")
tab_dat
library(gt)
title_tab_dat = c(paste0("Cost saving and job benefits from NOMS data 6-1-20", " ", "n","=", dim(healthcare_dat)[1]))
### Add title, change names of measure, make $, add footnote for part and full time cost savings, add footnote for average saving percentage to make it clear what the units are.
tab_dat$measure = recode(tab_dat$measure, "part_time" = "Part time", "jail"= "Jail", "hospital" = "Hospital", "full_time" = "Full time")
write.csv(tab_dat, "tab_dat.csv", row.names = FALSE)
tab_dat = read.csv("tab_dat.csv", header = TRUE)
tab_dat_table =  gt(tab_dat) %>%
  tab_header(title = title_tab_dat) %>%
  tab_footnote(footnote = "Unit is change in number of ER visits",  locations = cells_body(columns = vars(unit_difference), rows = 1)) %>%
  tab_footnote(footnote = "Unit is change in number of days",  locations = cells_body(columns = vars(unit_difference), rows = c(2,3))) %>%
  tab_footnote(footnote = "Unit is change in number for part / full time jobs",  locations = cells_body(columns = vars(unit_difference), rows = c(4,5)))%>%
  fmt_currency(columns = vars(savings_per_unit, cost_savings))%>%
  tab_footnote(footnote = "Full time and part-time amounts have a negative sign added to them before creating the total to put them in the same direction as cost savings",  locations = cells_body(columns = vars(cost_savings), rows = c(4,5)))%>%
  cols_label(measure = md("Measure"), savings_per_unit = md("Savings per unit"), unit_difference = md("Unit"), cost_savings = md("Cost savings"))
tab_dat_table
gtsave(tab_dat_table, "tab_dat_table.png")   
10*5680.56  
```
Response rates
Want telehealth_noms, because that is not merged yet.  
Then keep Assessment_new 0 for baseline and 2 for 6-month

```{r}
library(lubridate)
library(tidyr)
response_dat =  data.frame(ConsumerID = telehealth_noms$ConsumerID, ConductedInterview = telehealth_noms$ConductedInterview, InterviewDate = telehealth_noms$InterviewDate, Assessment_new = telehealth_noms$Assessment_new)
response_dat = subset(response_dat, Assessment_new == 0 | Assessment_new == 2)
response_dat$InterviewDate = mdy(response_dat$InterviewDate)
### Assume everyone in the system counts towards the reassessment rate
## Let's limit the data set to only those who are eligible then get the number of 6-months relative to the total baselines.  So describe.factor for the number of 2's divided by the number of 0's. 
## If you have interview date for 0 that is less than  "2019-11-01" create a variable
### If you have interview date for 2 that is greater than or equal to "2019-11-01" and not "1869-01-01" then a one 
## Then keep if either rule 1 or rule 2 is true
response_dat$rule_1 = ifelse(response_dat$Assessment_new == 0 & response_dat$InterviewDate < "2019-11-01",1, 0)
response_dat$rule_2 = ifelse(response_dat$Assessment_new == 2 & response_dat$InterviewDate >= "2019-11-01" & response_dat$InterviewDate != "1869-01-01", 1, 0)
describe.factor(response_dat$rule_2)
subset(response_dat, rule_2==1)
response_dat_eligible = subset(response_dat, rule_1 == 1 | rule_2 == 1)
tail(response_dat_eligible,100)
response_dat_eligible
### Let's try and see if you limit to only those ID's who 

all_response_rate_dat =  describe.factor(response_dat_eligible$Assessment_new)
elig_n_all = all_response_rate_dat[1,1]
elig_n_6_month =all_response_rate_dat[1,2]
all_response_rate = round(elig_n_6_month /elig_n_all ,2)
all_response_rate
## What is the rate before telehealth?  Exclude anyone with an intake or reassessment before 2020-04-01
response_dat_eligible_face = subset(response_dat, InterviewDate < "2020-04-01")
### Get rid of client 7 months prior to 2020-4-1, because they would not be eligible
response_dat_eligible_face$rule_1 = ifelse(response_dat_eligible_face$Assessment_new == 0 & response_dat_eligible_face$InterviewDate < "2019-09-01",1, 0)
response_dat_eligible_face$rule_2 = ifelse(response_dat_eligible_face$Assessment_new == 2 & response_dat_eligible_face$InterviewDate >= "2019-09-01" & response_dat_eligible_face$InterviewDate != "1869-01-01", 1, 0)
subset(response_dat_eligible_face, rule_2==1)

response_dat_eligible_face = subset(response_dat_eligible_face, rule_1 == 1 | rule_2 == 1)

response_dat_eligible_face

face_response_rate_dat =  describe.factor(response_dat_eligible_face$Assessment_new)
elig_n_all_face = face_response_rate_dat[1,1]
elig_n_6_month_face =face_response_rate_dat[1,2]
response_rate_face = round(elig_n_6_month_face /elig_n_all_face ,2)
response_rate_face

```
Response rate table
Total N is used for N is proportion tests: https://sixsigmastudyguide.com/one-and-two-sample-proportion-hypothesis-tests/
https://stattrek.com/hypothesis-test/difference-in-proportions.aspx
```{r}
tab_response_rate = matrix(c(elig_n_all, elig_n_6_month, all_response_rate, elig_n_all_face, elig_n_6_month_face, response_rate_face), nrow = 2, byrow = TRUE)
tab_response_rate = data.frame(tab_response_rate) 
colnames(tab_response_rate) = c("n_all", "n_6", "rr")
all_telehealth = c("Telehealth", "Face to face")
tab_response_rate = data.frame(all_telehealth, tab_response_rate)

title_tab_response = c("Comparing response rates for face to face and telehealth from NOMS data as of 6-1-20")
tab_response = gt(tab_response_rate)%>%
     tab_header(title = title_tab_response)%>%
    tab_footnote(footnote = "N all for telehealth  means all clients with intake data prior to 11-1-19, which is 7 months prior 6-1-20.  Only including clients who have intake data 7 months prior to 6-1-20 ensures we only include clients who should have received a 6-month reassessment.  However, we also included a client if they had a 6-month reassessment and the intake was after 11-1-19, because that means their data was collected early.",  locations = cells_body(columns = vars(n_all), rows = c(1)))%>%
    tab_footnote(footnote = "N all for face to face first subsetting all data prior to 4-1-2019.  Then all clients with intake data prior to 09-1-19, which is 7 months prior 4-1-20.  Only including clients who have intake data 7 months prior to 4-1-20 ensures we only include clients who should have received a 6-month reassessment.  However, we also included a client if they had a 6-month reassessment and the intake was after 9-1-19, because that means their data was collected early.",  locations = cells_body(columns = vars(n_all), rows = c(2)))%>%
    tab_footnote(footnote = "No statistically significant difference between response rates p = .34.",  locations = cells_body(columns = vars(rr), rows = c(1,2)))%>%
    cols_label(all_telehealth = md("Time label"), n_all = md("Total N eligible"), n_6 = md("Total 6-months eligible"), rr = md("Response rate"))
tab_response

```
Response rate statistical test
Statistical test for client clincian sub p = (p1 * n1 + p2 * n2) / (n1 + n2) 
SE = sqrt[p * ( 1 - p ) * [ (1/n1) + (1/n2) ] 
z = (p1 - p2) / SE 
https://www.cyclismo.org/tutorial/R/pValues.html
---
title: "telehealth_noms"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Stack variables

Post stacking transformations

All other grants
0600 = Baseline
0601 = 6 month resasessment


CCBHC
For assessment see below 3-months is actually vitals and so on
3-month vitals
6 month reassessment
9-month vitals
12 month reassessment

Assessment: 
0600 = Baseline Assessment
0301 = 3 Month Reassessment (vitals)
0302 = 6 Month Reassessment
0303 = 9 Month Reassessment (vitals)
0304 = 12 Month Reassessment
0699 = clincial discharge

Assessment_new
0 = Baseline
1 = 3 month reassessment (vitals)
2 = 6 month reassessment (302 from CCBHC and 601 from all other grants)
3 = 9 month reassessment (vitals)
4 = 12 month reassessment
5 = clinical discharge

Now review the missing data
Only include Baseline and 6-month
telehealth.y means they were in telehealth at 6 months which is what we want

# Data mergeing
For CCBHC IN, IL all the same

telehealth: Telehealth = 1; Pre-telehealth = 0 telehealth defined as those with any assessment date on or after 4-2-2020

### Run this prior to any analysis to load data ####
```{r}
library(prettyR)
library(see)
library(performance)
###
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
IN =  read.csv("CCBHC_IN_5.28.20.csv", header = TRUE, na.strings =  c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
FHHC = read.csv("fhhc_noms_5_27_20.csv", header= TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
ICP = read.csv("SPARS Data Download 5.23.2020_ICP.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
SOCAT = read.csv("SOCAT NOMs download 5.27.20.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IL_adult = read.csv("data down 5.26.20 adult CCBHC IL.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IL_youth = read.csv("data down 5.26.20 child CCBHC IL.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
FL_ACT = read.csv("FL-ACT SPARS data download  5.28.2020.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))

## Now stack them
### Create an empty data and then fill it with NAs.  Keep the first 44 those are correct and match
SOCAT$RespondentType = NULL
SOCAT_matrix = matrix(NA, ncol = 185-43, nrow = dim(SOCAT)[1])
SOCAT_matrix = data.frame(SOCAT_matrix)
colnames(SOCAT_matrix) = colnames(ICP[,44:185])
SOCAT_full = data.frame(SOCAT[,1:43], SOCAT_matrix)
dim(SOCAT_full)
### Change variables that match
SOCAT_full$Nervous = SOCAT$Nervous
SOCAT_full$Hopeless = SOCAT$Hopeless
SOCAT_full$Restless = SOCAT$Restless
SOCAT_full$Depressed = SOCAT$Depressed
SOCAT_full$EverythingEffort = SOCAT$EverythingEffort
SOCAT_full$Worthless = SOCAT$Worthless
SOCAT_full$Tobacco_Use = SOCAT$Tobacco_Use
SOCAT_full$Alcohol_Use = SOCAT$Alcohol_Use
SOCAT_full$StreetOpioids_Use = SOCAT$StreetOpioids_Use
SOCAT_full$RxOpioids_Use = SOCAT$RxOpioids_Use
SOCAT_full$NightsHomeless = SOCAT$NightsHomeless
SOCAT_full$NightsHospitalMHC = SOCAT$NightsHospitalMHC
SOCAT_full$NightsDetox = SOCAT$NightsDetox
SOCAT_full$NightsJail = SOCAT$NightsJail
SOCAT_full$TimesER = SOCAT$TimesER
SOCAT_full$Housing = SOCAT$Housing
SOCAT = SOCAT_full

IL_youth$RespondentType = NULL
IL_youth_matrix = matrix(NA, ncol = 185-43, nrow = dim(IL_youth)[1])
IL_youth_matrix = data.frame(IL_youth_matrix)
colnames(IL_youth_matrix) = colnames(ICP[,44:185])
IL_youth_full = data.frame(IL_youth[,1:43], IL_youth_matrix)
dim(IL_youth_full)
### Change variables that match
IL_youth_full$Nervous = IL_youth$Nervous
IL_youth_full$Hopeless = IL_youth$Hopeless
IL_youth_full$Restless = IL_youth$Restless
IL_youth_full$Depressed = IL_youth$Depressed
IL_youth_full$EverythingEffort = IL_youth$EverythingEffort
IL_youth_full$Worthless = IL_youth$Worthless
IL_youth_full$Tobacco_Use = IL_youth$Tobacco_Use
IL_youth_full$Alcohol_Use = IL_youth$Alcohol_Use
IL_youth_full$StreetOpioids_Use = IL_youth$StreetOpioids_Use
IL_youth_full$RxOpioids_Use = IL_youth$RxOpioids_Use
IL_youth_full$NightsHomeless = IL_youth$NightsHomeless
IL_youth_full$NightsHospitalMHC = IL_youth$NightsHospitalMHC
IL_youth_full$NightsDetox = IL_youth$NightsDetox
IL_youth_full$NightsJail = IL_youth$NightsJail
IL_youth_full$TimesER = IL_youth$TimesER
IL_youth_full$Housing = IL_youth$Housing
IL_youth = IL_youth_full

IN_IL_KY_CCBHC = rbind(IN[,1:185], IL_youth[,1:185], IL_adult[,1:185])
dim(IN_IL_KY_CCBHC)
FHHC = FHHC[,1:185]
ICP = ICP[,1:185]
FL_ACT = FL_ACT[,1:185]
dim(ICP)
dim(SOCAT)
### Add grant ID
IN_IL_KY_CCBHC$grant = rep("IN_IL_KY_CCBHC", dim(IN_IL_KY_CCBHC)[1])
FHHC$grant = rep("FHHC", dim(FHHC)[1])
ICP$grant = rep("ICP", dim(ICP)[1])
SOCAT$grant = rep("SOCAT", dim(SOCAT)[1])
FL_ACT$grant = rep("FL_ACT", dim(FL_ACT)[1])
dim(SOCAT)
telehealth_noms = rbind(IN_IL_KY_CCBHC, FHHC, ICP, SOCAT, FL_ACT)
dim(telehealth_noms)
### Create a new ConsumerID that is a mix of grant and ConsumerID
telehealth_noms$ConsumerID_grant = paste0(telehealth_noms$ConsumerID, telehealth_noms$GrantID)

### Figure out how you can stack FHHC data
dim(telehealth_noms)

## Rename to the above

## No one has multiple reassessments


## Create recoded assessment variable
telehealth_noms$Assessment_new = ifelse(telehealth_noms$Assessment == 600, 0, ifelse(telehealth_noms$Assessment == 301, 1, ifelse(telehealth_noms$Assessment == 302, 2, ifelse(telehealth_noms$Assessment == 303, 3, ifelse(telehealth_noms$Assessment == 601,2, NA)))))
telehealth_noms$Assessment_new = as.numeric(telehealth_noms$Assessment_new)
describe.factor(telehealth_noms$Assessment_new, decr.order= FALSE)
### Create full date variable
telehealth_noms$date = paste0(telehealth_noms$FFY, "-", telehealth_noms$Month, "-", "01")
library(lubridate)
telehealth_noms$date = ymd(telehealth_noms$date)
head(telehealth_noms$date)

telehealth_noms$telehealth = ifelse(telehealth_noms$date >= "2020-04-01", 1, 0)
telehealth_noms[c("date","telehealth")]
### Cannot be greater than 2020-09-30 last day of grant
telehealth_noms = subset(telehealth_noms, date < "2020-09-30")
## Check that all dates post 2014 most grants are for at most five years
telehealth_noms = subset(telehealth_noms, date > "2014-01-01")
telehealth_noms[c("date","telehealth")]
dim(telehealth_noms)
describe.factor(telehealth_noms$grant)
range(telehealth_noms$date)
### Create a NOMS data set  
telehealth_noms_wide = subset(telehealth_noms, Assessment_new == 0 | Assessment_new == 2)
dim(telehealth_noms)[1]
describe.factor(telehealth_noms$Assessment_new)


####################
library(naniar)
miss_var_summary(telehealth_noms)

head(telehealth_noms)
miss_var_summary(subset(telehealth_noms, Assessment_new == 2))
miss_var_summary(subset(telehealth_noms, Assessment_new == 0))

### These people have two baselines delete them 'A00276''A00295''A00298'

telehealth_noms_wide = telehealth_noms_wide[order(telehealth_noms_wide$ConsumerID),]
telehealth_noms_wide_test = subset(telehealth_noms_wide, ConsumerID == "'A00276'" | ConsumerID == "'A00295'" | ConsumerID == "'A00298'")
## If there is no interview then delete the second, if there is only one interview delete the none interview, if there are two interviews for baseline delete the second see conductedinterview variable
telehealth_noms_wide[c(1942, 1960, 1965),]
telehealth_noms_wide = telehealth_noms_wide[-c(1942, 1960, 1965),] 


#telehealth_noms_wide = telehealth_noms_wide[-c(3754, 3779),] 
telehealth_noms_base_noms = subset(telehealth_noms_wide,Assessment_new == 0)
telehealth_noms_month6_noms = subset(telehealth_noms_wide,Assessment_new == 2)
describe.factor(telehealth_noms_base_noms$grant)
describe.factor(telehealth_noms_month6_noms$grant)

head(telehealth_noms_base_noms)
dim(telehealth_noms_month6_noms)
telehealth_noms_wide_noms = merge(telehealth_noms_base_noms, telehealth_noms_month6_noms, by = "ConsumerID_grant", all.y = TRUE)
dim(telehealth_noms_wide_noms)
telehealth_noms_wide_noms = telehealth_noms_wide_noms[order(telehealth_noms_wide_noms$ConsumerID_grant),]
telehealth_noms_month6_noms = telehealth_noms_month6_noms[order(telehealth_noms_month6_noms$ConsumerID_grant),]
telehealth_noms_month6_noms$ConsumerID_grant == telehealth_noms_wide_noms$ConsumerID_grant

head(telehealth_noms_month6_noms)
describe.factor(telehealth_noms_month6_noms$telehealth)
describe.factor(telehealth_noms_month6_noms$grant)
describe.factor(telehealth_noms_wide_noms$telehealth.y)

### Gender is female
telehealth_noms_wide_noms$Gender.y = ifelse(telehealth_noms_wide_noms$Gender.y == 2, 1, 0)

#### Create diagnosis variables
describe.factor(telehealth_noms_wide_noms$DiagnosisOne.y)
test_dat = subset(telehealth_noms_wide_noms, DiagnosisOne.y == "59")
describe.factor(test_dat$telehealth.y)

### Enough 62 which is 62 = F40-F48 – Anxiety, dissociative, stress-related, somatoform and other nonpsychotic mental disorders
#59 = F33 – Major depressive disorder, recurrent
#57 = F31 – Bipolar disorder
telehealth_noms_wide_noms$dep = ifelse(telehealth_noms_wide_noms$DiagnosisOne.y == 59, 1, 0)
telehealth_noms_wide_noms$bipolar = ifelse(telehealth_noms_wide_noms$DiagnosisOne.y == 57, 1, 0)
describe.factor(telehealth_noms_wide_noms$dep)
telehealth_noms_wide_noms$InterviewDate.y
```

################################################
Full data set created can start data analysis
################################################




Let's try satisfaction 
c.	how satisfied are you with your ability to perform your daily living activities? PerformDailyActivitiesSatisfaction
d.	how satisfied are you with your health? HealthSatisfaction
how satisfied are you with yourself? SelfSatisfaction
how satisfied are you with your personal relationships? RelationshipSatisfaction

### General instructions ####
1. Grab the variable you want and put into data frame
2. Check the descriptives to make sure everything is in range
3. Conduct psychometrics 

SMI
Major depressive, bioplor and schizoprenia
https://www.psychiatry.org/patients-families/what-is-mental-illness#:~:text=Examples%20of%20serious%20mental%20illness,disorder%2C%20schizophrenia%20and%20bipolar%20disorder.
https://mentalillnesspolicy.org/serious-mental-illness-not/
http://www.bhevolution.org/public/severe_mental_illness.page


```{r}
### All items should be 1 to 5
telehealth_noms_wide_noms_sat = telehealth_noms_wide_noms[c("telehealth.y", "PerformDailyActivitiesSatisfaction.x", "PerformDailyActivitiesSatisfaction.y", "HealthSatisfaction.x", "HealthSatisfaction.y", "SelfSatisfaction.x", "SelfSatisfaction.y", "RelationshipSatisfaction.x","RelationshipSatisfaction.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_noms_wide_noms_sat,2, function(x){describe.factor(x)})
library(psych)


### Plug in all the .x variables 
omega_sat_base =  omega(telehealth_noms_wide_noms_sat[c(2,4,6,8)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
omega_sat_6month =  omega(telehealth_noms_wide_noms_sat[c(3,5,7,9)], poly = TRUE)
omega_sat_6month

### Plug in all the .x variables 
vss(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly", correct = 0)
```
Create a total score for each 

IN_IL_dat_wide_noms_sat_month6_complete  = satisfaction total scores only with complete data for 6-month
```{r}
### Plug in all .x variables
telehealth_noms_wide_noms_sat$total_base = apply(telehealth_noms_wide_noms_sat[c(2,4,6,8)], 1, mean, na.rm = TRUE)
### Plug in all .y expect for telehealth.y
telehealth_noms_wide_noms_sat$total_month6 = apply(telehealth_noms_wide_noms_sat[c(3,5,7,9)], 1, mean, na.rm = TRUE)
hist(telehealth_noms_wide_noms_sat$total_base)
hist(telehealth_noms_wide_noms_sat$total_month6)

## No data for difference scores so try just 6months
range(telehealth_noms_wide_noms_sat$total_month6, na.rm = TRUE)
dim(telehealth_noms_wide_noms_sat)
head(telehealth_noms_wide_noms_sat)

telehealth_noms_wide_noms_sat_month6_complete = na.omit(telehealth_noms_wide_noms_sat[c("total_month6", "telehealth.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])
dim(telehealth_noms_wide_noms_sat_month6_complete)
telehealth_noms_wide_noms_sat_month6_complete

### creating face to face
telehealth_noms_wide_noms_sat_month6_complete$face_to_face = ifelse(telehealth_noms_wide_noms_sat_month6_complete$telehealth.y == 1,0,1)

apply(telehealth_noms_wide_noms_sat_month6_complete, 2, function(x){describe.factor(x)})

```
Analysis sat
Percentage change: https://stats.idre.ucla.edu/sas/faq/how-can-i-interpret-log-transformed-variables-in-terms-of-percent-change-in-linear-regression/
  
You need to expondiate the parameter estimate
Doesn't work to well when you get above 20% differences: https://people.duke.edu/~rnau/411log.htm

P-change is bad for regression because of: 
(3-2)/2 
(2-3)/3

Change the telehealth.y to be face to face 1 and telehealth.y  = 0

```{r}
library(rstanarm)
library(descr)
### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
n_total = dim(telehealth_noms_wide_noms_sat_month6_complete)[1]

bayes_p_change_sat = stan_glm(log(total_month6)~ face_to_face + Agegroup.y+ Gender.y+ RaceWhite.y, prior = my_prior, data = telehealth_noms_wide_noms_sat_month6_complete, seed = 123)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_sat_sum = round(bayes_p_change_sat$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_sat_sum = round(exp(bayes_p_change_sat_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_sat_sum= bayes_p_change_sat_sum - 1
bayes_p_change_sat_sum

### Grabing the means, sds, and n's for each group
mean_sd_sat= round(compmeans(telehealth_noms_wide_noms_sat_month6_complete$total_month6, telehealth_noms_wide_noms_sat_month6_complete$telehealth.y),2)
mean_sd_sat
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_sat_d =  psych::cohen.d(telehealth_noms_wide_noms_sat_month6_complete$total_month6, group = telehealth_noms_wide_noms_sat_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)
results_sat = data.frame(par_estimate = bayes_p_change_sat_sum[2,1], sd_p_change =  bayes_p_change_sat_sum[2,2], ci_95 = paste0(bayes_p_change_sat_sum[2,3], ",", bayes_p_change_sat_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_sat[1,2], n_post_telehealth = mean_sd_sat[2,2], raw_p_change = round((mean_sd_sat[2,1] -  mean_sd_sat[1,1]) /  mean_sd_sat[1,1],3), telehealth_mean =  mean_sd_sat[2,1], face_to_face_mean =  mean_sd_sat[1,1], telehealth_sd= mean_sd_sat[2,3], face_to_face_sd= mean_sd_sat[1,3],freq_cohen_d = round(month_6_sat_d$cohen.d[2],3))

write.csv(results_sat, "results_sat.csv", row.names = FALSE)
results_sat


```
Sat with diagnoses 

```{r}

### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
n_total = dim(telehealth_noms_wide_noms_sat_month6_complete)[1]

bayes_p_change_sat_dep = stan_glm(log(total_month6)~ face_to_face*dep +face_to_face*bipolar + Agegroup.y+ Gender.y+ RaceWhite.y, prior = my_prior, data = telehealth_noms_wide_noms_sat_month6_complete, seed = 123)
##
#launch_shinystan(bayes_p_change_sat_dep)
check_collinearity(bayes_p_change_sat_dep)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat_dep$stan_summary if you are unsure
bayes_p_change_sat_dep_sum = round(bayes_p_change_sat_dep$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_sat_dep_sum = round(exp(bayes_p_change_sat_dep_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_sat_dep_sum= bayes_p_change_sat_dep_sum - 1
bayes_p_change_sat_dep_sum

### Grabing the means, sds, and n's for each group
mean_sd_sat_dep= round(compmeans(telehealth_noms_wide_noms_sat_month6_complete$total_month6, telehealth_noms_wide_noms_sat_month6_complete$telehealth.y),2)
mean_sd_sat_dep
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_sat_dep_d =  psych::cohen.d(telehealth_noms_wide_noms_sat_month6_complete$total_month6, group = telehealth_noms_wide_noms_sat_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_dep_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat_dep to whatever you are measuring results_(fill in name)
results_sat_dep = data.frame(par_estimate = bayes_p_change_sat_dep_sum[2,1], sd_p_change =  bayes_p_change_sat_dep_sum[2,2], ci_95 = paste0(bayes_p_change_sat_dep_sum[2,3], ",", bayes_p_change_sat_dep_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_sat_dep[1,2], n_post_telehealth = mean_sd_sat_dep[2,2], raw_p_change = round((mean_sd_sat_dep[2,1] -  mean_sd_sat_dep[1,1]) /  mean_sd_sat_dep[1,1],3), telehealth_mean =  mean_sd_sat_dep[2,1], face_to_face_mean =  mean_sd_sat_dep[1,1], telehealth_sd= mean_sd_sat_dep[2,3], face_to_face_sd= mean_sd_sat_dep[1,3],freq_cohen_d = round(month_6_sat_dep_d$cohen.d[2],3))

write.csv(results_sat_dep, "results_sat_dep.csv", row.names = FALSE)
results_sat_dep


```
Sat with diagnosis with t.test for just telehealth (Run diagnosis regression to get telehealth_noms_wide_noms_sat_month6_complete)
```{r}
sat_diag_tele_only_dat = subset(telehealth_noms_wide_noms_sat_month6_complete, telehealth.y == 1)
hist(sat_diag_tele_only_dat$total_month6)
qqnorm(sat_diag_tele_only_dat$total_month6)
compmeans(sat_diag_tele_only_dat$total_month6, sat_diag_tele_only_dat$dep)

### recode dep so it compares dep first relative to not depressed
sat_diag_tele_only_dat$dep_recode = ifelse(sat_diag_tele_only_dat$dep == 1, 0,1)
dep_sat = t.test(sat_diag_tele_only_dat$total_month6 ~ sat_diag_tele_only_dat$dep_recode)
dep_sat = data.frame(t_value = dep_sat$statistic, p_value = dep_sat$p.value, lower = dep_sat$conf.int[1], upper = dep_sat$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(sat_diag_tele_only_dat$total_month6 ~ sat_diag_tele_only_dat$dep_recode)

n_dep_sat =describe.factor(sat_diag_tele_only_dat$dep)
n_dep_sat = data.frame(n_dep_sat)
n_dep_sat = n_dep_sat[1,]
n_dep_sat = data.frame(n_dep_sat)
colnames(n_dep_sat) = n_dep_sat
colnames(n_dep_sat) = c("No major depression count", "Major depression count")
n_sat_dat = data.frame(dim(sat_diag_tele_only_dat)[1])
colnames(n_sat_dat) = "Satisfaction total n"

sat_dep_means = compmeans(sat_diag_tele_only_dat$total_month6, sat_diag_tele_only_dat$dep)

sat_dep_d = psych::cohen.d(sat_diag_tele_only_dat$total_month6, group = sat_diag_tele_only_dat$dep)


results_sat_dep = data.frame(dep_sat, n_total = n_sat_dat, n_dep_sat[2], n_dep_sat[1], raw_p_change = round((sat_dep_means[2,1] -  sat_dep_means[1,1]) /  sat_dep_means[1,1],3), dep_mean =  sat_dep_means[2,1], no_dep_mean =  sat_dep_means[1,1], dep_sd= sat_dep_means[2,3], no_dep_sd= sat_dep_means[1,3],freq_cohen_d = round(sat_dep_d$cohen.d[2],3))
results_sat_dep = round(results_sat_dep, 2)
library(gt)
title_results_sat_dep = "T-test results for satisfaction with outcomes at 6-months for telehealth clients with major depression"
table_results_sat_dep = 
  gt(results_sat_dep) %>%
  tab_header(title = title_results_sat_dep)%>%
  cols_label(t_value = md("T-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Satisfaction.total.n = md("Satisfaction total n") , Major.depression.count = md("Major depression count"), No.major.depression.count = md("No major depression count"), raw_p_change = md("raw p change"), dep_mean = md("Depression mean"), no_dep_mean = md("No depression mean"), dep_sd = md("Depression sd"), no_dep_sd = md("No depression sd"), freq_cohen_d = md("Cohen D"))
table_results_sat_dep

gtsave(table_results_sat_dep, "table_results_sat_dep.png")
#############################################################################################################################
### Bipolar now
sat_diag_tele_only_dat = subset(telehealth_noms_wide_noms_sat_month6_complete, telehealth.y == 1)
hist(sat_diag_tele_only_dat$total_month6)
qqnorm(sat_diag_tele_only_dat$total_month6)
compmeans(sat_diag_tele_only_dat$total_month6, sat_diag_tele_only_dat$bipolar)

### recode bipolar so it compares bipolar first relative to not bipolarressed
sat_diag_tele_only_dat$bipolar_recode = ifelse(sat_diag_tele_only_dat$bipolar == 1, 0,1)
bipolar_sat = t.test(sat_diag_tele_only_dat$total_month6 ~ sat_diag_tele_only_dat$bipolar_recode)
bipolar_sat = data.frame(t_value = bipolar_sat$statistic, p_value = bipolar_sat$p.value, lower = bipolar_sat$conf.int[1], upper = bipolar_sat$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(sat_diag_tele_only_dat$total_month6 ~ sat_diag_tele_only_dat$bipolar_recode)

n_bipolar_sat =describe.factor(sat_diag_tele_only_dat$bipolar)
n_bipolar_sat = data.frame(n_bipolar_sat)
n_bipolar_sat = n_bipolar_sat[1,]
n_bipolar_sat = data.frame(n_bipolar_sat)
colnames(n_bipolar_sat) = n_bipolar_sat
colnames(n_bipolar_sat) = c("No bipolar count", "Bipolar count")
n_sat_dat = data.frame(dim(sat_diag_tele_only_dat)[1])
colnames(n_sat_dat) = "Satisfaction total n"

sat_bipolar_means = compmeans(sat_diag_tele_only_dat$total_month6, sat_diag_tele_only_dat$bipolar)

sat_bipolar_d = psych::cohen.d(sat_diag_tele_only_dat$total_month6, group = sat_diag_tele_only_dat$bipolar)


results_sat_bipolar = data.frame(bipolar_sat, n_total = n_sat_dat, n_bipolar_sat[2], n_bipolar_sat[1], raw_p_change = round((sat_bipolar_means[2,1] -  sat_bipolar_means[1,1]) /  sat_bipolar_means[1,1],3), bipolar_mean =  sat_bipolar_means[2,1], no_bipolar_mean =  sat_bipolar_means[1,1], bipolar_sd= sat_bipolar_means[2,3], no_bipolar_sd= sat_bipolar_means[1,3],freq_cohen_d = round(sat_bipolar_d$cohen.d[2],3))
results_sat_bipolar = round(results_sat_bipolar, 2)
library(gt)
title_results_sat_bipolar = "T-test results for satisfaction with outcomes at 6-months for telehealth clients with primary diagnosis as bipolar"
table_results_sat_bipolar = 
  gt(results_sat_bipolar) %>%
  tab_header(title = title_results_sat_bipolar)%>%
  cols_label(t_value = md("T-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Satisfaction.total.n = md("Satisfaction total n") , Bipolar.count = md("Bipolar count"), No.bipolar.count = md("No bipolar count"), raw_p_change = md("raw p change"), bipolar_mean = md("Bipolar mean"), no_bipolar_mean = md("No bipolar mean"), bipolar_sd = md("Bipolar sd"), no_bipolar_sd = md("No bipolar sd"), freq_cohen_d = md("Cohen D"))
table_results_sat_bipolar

gtsave(table_results_sat_bipolar, "table_results_sat_bipolar.png")

```

#################
Code from Github
1. Grab code from master branch


##################### 
Follow the code above and plug in the variables below instead of the variable above
Post to github as your name patch-1,2,3...n (however, many patchs you have).  Let Matt know when you are finished.
Or just save code on telehealth folder on T drive

######################################

######Cat

Next section
Dealing with everyday life with mental health illness
Sat
a.	I deal effectively with daily problems.
b.	I am able to control my life. T\CRI_Research\telehealth_evaluation
c.	I am able to deal with crisis.
d.	I am getting along with my family.
e.	I do well in social situations.
f.	I do well in school and/or work.
g.	My housing situation is satisfactory.
h.	My symptoms are not bothering me.

```{r}
telehealth_noms_wide_noms_deal = telehealth_noms_wide_noms[c("telehealth.y", "HandlingDailyLife.x", "HandlingDailyLife.y", "ControlLife.x", "ControlLife.y", "DealWithCrisis.x", "DealWithCrisis.y", "GetsAlongWithFamily.x","GetsAlongWithFamily.y", "SocialSituations.x", "SocialSituations.y", "SchoolOrWork.x", "SchoolOrWork.y", "FunctioningHousing.x", "FunctioningHousing.y", "Symptoms.x","Symptoms.y", "Agegroup.y", "Gender.y", "RaceWhite.y")]
apply(telehealth_noms_wide_noms_deal,2, function(x){describe.factor(x)})
library(psych)

### Plug in all the .x variables 
omega_deal_base =  omega(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
omega_deal_6month =  omega(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], poly = TRUE)
omega_deal_6month

### Plug in all the .x variables 
vss(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], cor = "poly")
fa(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], cor = "poly", correct = 0)



### Plug in all .x variables
telehealth_noms_wide_noms_deal$total_base = apply(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], 1, mean, na.rm = TRUE)
### Plug in all .y expect for telehealth.y
telehealth_noms_wide_noms_deal$total_month6 = apply(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], 1, mean, na.rm = TRUE)
hist(telehealth_noms_wide_noms_deal$total_base)
hist(telehealth_noms_wide_noms_deal$total_month6)

## No data for difference scores so try just 6months
range(telehealth_noms_wide_noms_deal$total_month6, na.rm = TRUE)
dim(telehealth_noms_wide_noms_deal)
head(telehealth_noms_wide_noms_deal)
telehealth_noms_wide_noms_deal
telehealth_noms_wide_noms_deal_month6_complete = na.omit(telehealth_noms_wide_noms_deal[c("total_month6", "telehealth.y", "Agegroup.y", "Gender.y", "RaceWhite.y")])
dim(telehealth_noms_wide_noms_deal_month6_complete)
telehealth_noms_wide_noms_deal_month6_complete
telehealth_noms_wide_noms_deal_month6_complete$face_to_face = ifelse(telehealth_noms_wide_noms_deal_month6_complete$telehealth.y == 1,0,1)



library(rstanarm)
library(descr)
### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
describe.factor(telehealth_noms_wide_noms_deal_month6_complete$telehealth.y)
n_total = dim(telehealth_noms_wide_noms_deal_month6_complete)[1]

bayes_p_change_deal = stan_glm(log(total_month6)~ face_to_face + Agegroup.y+ Gender.y+ RaceWhite.y, prior = my_prior, data = telehealth_noms_wide_noms_deal_month6_complete, seed = 123)
#launch_shinystan(bayes_p_change_deal)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_deal_sum = round(bayes_p_change_deal$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
options(scipen = 999)
### Log postioer is werid, because you exp'ed it.
bayes_p_change_deal_sum = round(exp(bayes_p_change_deal_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_deal_sum= bayes_p_change_deal_sum - 1
bayes_p_change_deal_sum
### Grabing the means, sds, and n's for each group
mean_sd_deal= round(compmeans(telehealth_noms_wide_noms_deal_month6_complete$total_month6, telehealth_noms_wide_noms_deal_month6_complete$telehealth.y),2)
mean_sd_deal
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_deal_d =  psych::cohen.d(telehealth_noms_wide_noms_deal_month6_complete$total_month6, group = telehealth_noms_wide_noms_deal_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)
results_deal = data.frame(par_estimate = bayes_p_change_deal_sum[2,1], sd_p_change =  bayes_p_change_deal_sum[2,2], ci_95 = paste0(bayes_p_change_deal_sum[2,3], ",", bayes_p_change_deal_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_deal[1,2], n_post_telehealth = mean_sd_deal[2,2], raw_p_change = round((mean_sd_deal[2,1] -  mean_sd_deal[1,1]) /  mean_sd_deal[1,1],3), telehealth_mean =  mean_sd_deal[2,1], face_to_face_mean =  mean_sd_deal[1,1], telehealth_sd= mean_sd_deal[2,3], face_to_face_sd= mean_sd_deal[1,3],freq_cohen_d = round(month_6_deal_d$cohen.d[2],3))

write.csv(results_deal, "results_deal.csv", row.names = FALSE)
results_deal

```
Deal with diagnosis
```{r}
telehealth_noms_wide_noms_deal = telehealth_noms_wide_noms[c("telehealth.y", "HandlingDailyLife.x", "HandlingDailyLife.y", "ControlLife.x", "ControlLife.y", "DealWithCrisis.x", "DealWithCrisis.y", "GetsAlongWithFamily.x","GetsAlongWithFamily.y", "SocialSituations.x", "SocialSituations.y", "SchoolOrWork.x", "SchoolOrWork.y", "FunctioningHousing.x", "FunctioningHousing.y", "Symptoms.x","Symptoms.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_noms_wide_noms_deal,2, function(x){describe.factor(x)})
library(psych)

### Plug in all the .x variables 
omega_deal_base =  omega(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
omega_deal_6month =  omega(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], poly = TRUE)
omega_deal_6month

### Plug in all the .x variables 
vss(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], cor = "poly")
fa(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], cor = "poly", correct = 0)



### Plug in all .x variables
telehealth_noms_wide_noms_deal$total_base = apply(telehealth_noms_wide_noms_deal[c(2,4,6,8,10,12,14,16)], 1, mean, na.rm = TRUE)
### Plug in all .y expect for telehealth.y
telehealth_noms_wide_noms_deal$total_month6 = apply(telehealth_noms_wide_noms_deal[c(3,5,7,9,11,13,15,17)], 1, mean, na.rm = TRUE)
hist(telehealth_noms_wide_noms_deal$total_base)
hist(telehealth_noms_wide_noms_deal$total_month6)

## No data for difference scores so try just 6months
range(telehealth_noms_wide_noms_deal$total_month6, na.rm = TRUE)
dim(telehealth_noms_wide_noms_deal)
head(telehealth_noms_wide_noms_deal)
telehealth_noms_wide_noms_deal
telehealth_noms_wide_noms_deal_month6_complete = na.omit(telehealth_noms_wide_noms_deal[c("total_month6", "telehealth.y", "Agegroup.y", "Gender.y", "RaceWhite.y","dep", "bipolar")])
dim(telehealth_noms_wide_noms_deal_month6_complete)
telehealth_noms_wide_noms_deal_month6_complete
telehealth_noms_wide_noms_deal_month6_complete$face_to_face = ifelse(telehealth_noms_wide_noms_deal_month6_complete$telehealth.y == 1,0,1)



library(rstanarm)
library(descr)
### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
describe.factor(telehealth_noms_wide_noms_deal_month6_complete$telehealth.y)
n_total = dim(telehealth_noms_wide_noms_deal_month6_complete)[1]

bayes_p_change_deal = stan_glm(log(total_month6)~ face_to_face*dep +  face_to_face*bipolar+ Agegroup.y+ Gender.y+ RaceWhite.y, prior = my_prior, data = telehealth_noms_wide_noms_deal_month6_complete, seed = 123)
check_collinearity(bayes_p_change_deal)
#launch_shinystan(bayes_p_change_deal)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_deal_sum = round(bayes_p_change_deal$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_deal_sum = round(exp(bayes_p_change_deal_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_deal_sum= bayes_p_change_deal_sum - 1
bayes_p_change_deal_sum
### Grabing the means, sds, and n's for each group
mean_sd_deal= round(compmeans(telehealth_noms_wide_noms_deal_month6_complete$total_month6, telehealth_noms_wide_noms_deal_month6_complete$telehealth.y),2)
mean_sd_deal
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_deal_d =  psych::cohen.d(telehealth_noms_wide_noms_deal_month6_complete$total_month6, group = telehealth_noms_wide_noms_deal_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)
results_deal_diag = data.frame(par_estimate = bayes_p_change_deal_sum[2,1], sd_p_change =  bayes_p_change_deal_sum[2,2], ci_95 = paste0(bayes_p_change_deal_sum[2,3], ",", bayes_p_change_deal_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_deal[1,2], n_post_telehealth = mean_sd_deal[2,2], raw_p_change = round((mean_sd_deal[2,1] -  mean_sd_deal[1,1]) /  mean_sd_deal[1,1],3), telehealth_mean =  mean_sd_deal[2,1], face_to_face_mean =  mean_sd_deal[1,1], telehealth_sd= mean_sd_deal[2,3], face_to_face_sd= mean_sd_deal[1,3],freq_cohen_d = round(month_6_deal_d$cohen.d[2],3))

write.csv(results_deal_diag, "results_deal_diag.csv", row.names = FALSE)
results_deal_diag
```


Manage mental health with diagnosis with t.test for just telehealth (Run diagnosis regression to get telehealth_noms_wide_noms_deal_month6_complete)
```{r}
deal_diag_tele_only_dat = subset(telehealth_noms_wide_noms_deal_month6_complete, telehealth.y == 1)
hist(deal_diag_tele_only_dat$total_month6)
qqnorm(deal_diag_tele_only_dat$total_month6)
compmeans(deal_diag_tele_only_dat$total_month6, deal_diag_tele_only_dat$dep)

### recode dep so it compares dep first relative to not depressed
deal_diag_tele_only_dat$dep_recode = ifelse(deal_diag_tele_only_dat$dep == 1, 0,1)
dep_deal = t.test(deal_diag_tele_only_dat$total_month6 ~ deal_diag_tele_only_dat$dep_recode)
dep_deal = data.frame(t_value = dep_deal$statistic, p_value = dep_deal$p.value, lower = dep_deal$conf.int[1], upper = dep_deal$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(deal_diag_tele_only_dat$total_month6 ~ deal_diag_tele_only_dat$dep_recode)

n_dep_deal =describe.factor(deal_diag_tele_only_dat$dep)
n_dep_deal = data.frame(n_dep_deal)
n_dep_deal = n_dep_deal[1,]
n_dep_deal = data.frame(n_dep_deal)
colnames(n_dep_deal) = n_dep_deal
colnames(n_dep_deal) = c("No major depression count", "Major depression count")
n_deal_dat = data.frame(dim(deal_diag_tele_only_dat)[1])
colnames(n_deal_dat) = "Manage mental health total n"

deal_dep_means = compmeans(deal_diag_tele_only_dat$total_month6, deal_diag_tele_only_dat$dep)

deal_dep_d = psych::cohen.d(deal_diag_tele_only_dat$total_month6, group = deal_diag_tele_only_dat$dep)


results_deal_dep = data.frame(dep_deal, n_total = n_deal_dat, n_dep_deal[2], n_dep_deal[1], raw_p_change = round((deal_dep_means[2,1] -  deal_dep_means[1,1]) /  deal_dep_means[1,1],3), dep_mean =  deal_dep_means[2,1], no_dep_mean =  deal_dep_means[1,1], dep_sd= deal_dep_means[2,3], no_dep_sd= deal_dep_means[1,3],freq_cohen_d = round(deal_dep_d$cohen.d[2],3))
results_deal_dep = round(results_deal_dep, 2)
library(gt)
title_results_deal_dep = "T-test results for managing mental health at 6-months for telehealth clients with major depression"
table_results_deal_dep = 
  gt(results_deal_dep) %>%
  tab_header(title = title_results_deal_dep)%>%
  cols_label(t_value = md("T-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Satisfaction.total.n = md("Satisfaction total n") , Major.depression.count = md("Major depression count"), No.major.depression.count = md("No major depression count"), raw_p_change = md("raw p change"), dep_mean = md("Depression mean"), no_dep_mean = md("No depression mean"), dep_sd = md("Depression sd"), no_dep_sd = md("No depression sd"), freq_cohen_d = md("Cohen D"))
table_results_deal_dep

gtsave(table_results_deal_dep, "table_results_deal_dep.png")
#############################################################################################################################
### Bipolar now
deal_diag_tele_only_dat = subset(telehealth_noms_wide_noms_deal_month6_complete, telehealth.y == 1)
hist(deal_diag_tele_only_dat$total_month6)
qqnorm(deal_diag_tele_only_dat$total_month6)
compmeans(deal_diag_tele_only_dat$total_month6, deal_diag_tele_only_dat$bipolar)

### recode bipolar so it compares bipolar first relative to not bipolarressed
deal_diag_tele_only_dat$bipolar_recode = ifelse(deal_diag_tele_only_dat$bipolar == 1, 0,1)
bipolar_deal = t.test(deal_diag_tele_only_dat$total_month6 ~ deal_diag_tele_only_dat$bipolar_recode)
bipolar_deal = data.frame(t_value = bipolar_deal$statistic, p_value = bipolar_deal$p.value, lower = bipolar_deal$conf.int[1], upper = bipolar_deal$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(deal_diag_tele_only_dat$total_month6 ~ deal_diag_tele_only_dat$bipolar_recode)

n_bipolar_deal =describe.factor(deal_diag_tele_only_dat$bipolar)
n_bipolar_deal = data.frame(n_bipolar_deal)
n_bipolar_deal = n_bipolar_deal[1,]
n_bipolar_deal = data.frame(n_bipolar_deal)
colnames(n_bipolar_deal) = n_bipolar_deal
colnames(n_bipolar_deal) = c("No bipolar count", "Bipolar count")
n_deal_dat = data.frame(dim(deal_diag_tele_only_dat)[1])
colnames(n_deal_dat) = "Manage mental health total n"

deal_bipolar_means = compmeans(deal_diag_tele_only_dat$total_month6, deal_diag_tele_only_dat$bipolar)

deal_bipolar_d = psych::cohen.d(deal_diag_tele_only_dat$total_month6, group = deal_diag_tele_only_dat$bipolar)


results_deal_bipolar = data.frame(bipolar_deal, n_total = n_deal_dat, n_bipolar_deal[2], n_bipolar_deal[1], raw_p_change = round((deal_bipolar_means[2,1] -  deal_bipolar_means[1,1]) /  deal_bipolar_means[1,1],3), bipolar_mean =  deal_bipolar_means[2,1], no_bipolar_mean =  deal_bipolar_means[1,1], bipolar_sd= deal_bipolar_means[2,3], no_bipolar_sd= deal_bipolar_means[1,3],freq_cohen_d = round(deal_bipolar_d$cohen.d[2],3))
results_deal_bipolar = round(results_deal_bipolar, 2)
library(gt)
title_results_deal_bipolar = "T-test results for managing mental health at 6-months for telehealth clients with primary diagnosis as bipolar"
table_results_deal_bipolar = 
  gt(results_deal_bipolar) %>%
  tab_header(title = title_results_deal_bipolar)%>%
  cols_label(t_value = md("T-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Satisfaction.total.n = md("Satisfaction total n") , Bipolar.count = md("Bipolar count"), No.bipolar.count = md("No bipolar count"), raw_p_change = md("raw p change"), bipolar_mean = md("Bipolar mean"), no_bipolar_mean = md("No bipolar mean"), bipolar_sd = md("Bipolar sd"), no_bipolar_sd = md("No bipolar sd"), freq_cohen_d = md("Cohen D"))
table_results_deal_bipolar

gtsave(table_results_deal_bipolar, "table_results_deal_bipolar.png")



```



####Jess
####################################################
Feeling in last 30 days
4 = All of the Time
3 = Most of the Time
2 = Some of the Time
1 = A Little of the Time
0 = None of the Time

Scale is changed to add one so I can take the log!!!!!!!!!
During the past 30 days, about how often did you feel …
a.	nervous?
b.	hopeless?
c.	restless or fidgety?
d.	so depressed that nothing could cheer you up?
e.	that everything was an effort?
f.	worthless?
```{r}

telehealth_noms_wide_noms_feel = telehealth_noms_wide_noms[c("telehealth.y", "Nervous.y", "Hopeless.y", "Restless.y", "Depressed.y", "EverythingEffort.y", "Worthless.y", "Agegroup.y", "Gender.y", "RaceWhite.y")]
apply(telehealth_noms_wide_noms_feel,2, function(x){describe.factor(x)})
library(psych)

telehealth_noms_wide_noms_feel[c(2:7)] = telehealth_noms_wide_noms_feel[c(2:7)]+1
### Plug in all the .y variables except telehealth.y 
omega_feel_6month =  omega(telehealth_noms_wide_noms_feel[c(2:7)], poly = TRUE)
omega_feel_6month

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_feel[c(2:7)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_feel[c(2:7)], cor = "poly", correct = 0)



### Plug in all .y expect for telehealth.y
telehealth_noms_wide_noms_feel$total_month6 = apply(telehealth_noms_wide_noms_feel[c(2:7)], 1, mean, na.rm = TRUE)
hist(log(telehealth_noms_wide_noms_feel$total_month6))

## No data for difference scores so try just 6months
range(telehealth_noms_wide_noms_feel$total_month6, na.rm = TRUE)
dim(telehealth_noms_wide_noms_feel)
head(telehealth_noms_wide_noms_feel)
telehealth_noms_wide_noms_feel
telehealth_noms_wide_noms_feel_month6_complete = na.omit(telehealth_noms_wide_noms_feel[c("total_month6", "telehealth.y", "Agegroup.y", "Gender.y", "RaceWhite.y")])

#Scale is changed to add one so I can take the log!!!!!!!!! Need to change after NA


dim(telehealth_noms_wide_noms_feel_month6_complete)
telehealth_noms_wide_noms_feel_month6_complete
telehealth_noms_wide_noms_feel_month6_complete$face_to_face = ifelse(telehealth_noms_wide_noms_feel_month6_complete$telehealth.y == 1,0,1)




library(rstanarm)
library(descr)
### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
describe.factor(telehealth_noms_wide_noms_feel_month6_complete$telehealth.y)
n_total = dim(telehealth_noms_wide_noms_feel_month6_complete)[1]
telehealth_noms_wide_noms_feel_month6_complete
apply(telehealth_noms_wide_noms_feel_month6_complete, 2, function(x){describe.factor(x)})

bayes_p_change_feel = stan_glm(log(total_month6)~ face_to_face + Agegroup.y+ Gender.y+ RaceWhite.y, prior = my_prior, data = telehealth_noms_wide_noms_feel_month6_complete, seed = 123)
#launch_shinystan(bayes_p_change_feel)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_feel_sum = round(bayes_p_change_feel$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_feel_sum = round(exp(bayes_p_change_feel_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_feel_sum= bayes_p_change_feel_sum - 1
bayes_p_change_feel_sum
### Grabing the means, sds, and n's for each group
mean_sd_feel= round(compmeans(telehealth_noms_wide_noms_feel_month6_complete$total_month6, telehealth_noms_wide_noms_feel_month6_complete$telehealth.y),2)
mean_sd_feel
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_feel_d =  psych::cohen.d(telehealth_noms_wide_noms_feel_month6_complete$total_month6, group = telehealth_noms_wide_noms_feel_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)
results_feel = data.frame(par_estimate = bayes_p_change_feel_sum[2,1], sd_p_change =  bayes_p_change_feel_sum[2,2], ci_95 = paste0(bayes_p_change_feel_sum[2,3], ",", bayes_p_change_feel_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_feel[1,2], n_post_telehealth = mean_sd_feel[2,2], raw_p_change = round((mean_sd_feel[2,1] -  mean_sd_feel[1,1]) /  mean_sd_feel[1,1],3), telehealth_mean =  mean_sd_feel[2,1], face_to_face_mean =  mean_sd_feel[1,1], telehealth_sd= mean_sd_feel[2,3], face_to_face_sd= mean_sd_feel[1,3],freq_cohen_d = round(month_6_feel_d$cohen.d[2],3))

write.csv(results_feel, "results_feel.csv", row.names = FALSE)
results_feel
```
Feeling last 30 days with diagnosis
```{r}
telehealth_noms_wide_noms_feel = telehealth_noms_wide_noms[c("telehealth.y", "Nervous.y", "Hopeless.y", "Restless.y", "Depressed.y", "EverythingEffort.y", "Worthless.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_noms_wide_noms_feel,2, function(x){describe.factor(x)})
library(psych)

telehealth_noms_wide_noms_feel[c(2:7)] = telehealth_noms_wide_noms_feel[c(2:7)]+1
apply(telehealth_noms_wide_noms_feel,2, function(x){describe.factor(x)})
### Plug in all the .y variables except telehealth.y 
omega_feel_6month =  omega(telehealth_noms_wide_noms_feel[c(2:7)], poly = TRUE)
omega_feel_6month

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_feel[c(2:7)], cor = "poly", n = 3)
fa(telehealth_noms_wide_noms_feel[c(2:7)], cor = "poly", correct = 0)



### Plug in all .y expect for telehealth.y
telehealth_noms_wide_noms_feel$total_month6 = apply(telehealth_noms_wide_noms_feel[c(2:7)], 1, mean, na.rm = TRUE)
hist(log(telehealth_noms_wide_noms_feel$total_month6))

## No data for difference scores so try just 6months
range(telehealth_noms_wide_noms_feel$total_month6, na.rm = TRUE)
dim(telehealth_noms_wide_noms_feel)
head(telehealth_noms_wide_noms_feel)
telehealth_noms_wide_noms_feel
telehealth_noms_wide_noms_feel_month6_complete = na.omit(telehealth_noms_wide_noms_feel[c("total_month6", "telehealth.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])

#Scale is changed to add one so I can take the log!!!!!!!!! Need to change after NA


dim(telehealth_noms_wide_noms_feel_month6_complete)
telehealth_noms_wide_noms_feel_month6_complete
telehealth_noms_wide_noms_feel_month6_complete$face_to_face = ifelse(telehealth_noms_wide_noms_feel_month6_complete$telehealth.y == 1,0,1)

library(rstanarm)
library(descr)
### Scale is .2 which means 20% difference in each direction
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
describe.factor(telehealth_noms_wide_noms_feel_month6_complete$telehealth.y)
n_total = dim(telehealth_noms_wide_noms_feel_month6_complete)[1]
telehealth_noms_wide_noms_feel_month6_complete
apply(telehealth_noms_wide_noms_feel_month6_complete, 2, function(x){describe.factor(x)})

bayes_p_change_feel = stan_glm(log(total_month6)~ face_to_face*dep + face_to_face*bipolar + Agegroup.y+ Gender.y+ RaceWhite.y, prior = my_prior, data = telehealth_noms_wide_noms_feel_month6_complete, seed = 123)
check_collinearity(bayes_p_change_feel)
#launch_shinystan(bayes_p_change_feel)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_feel_sum = round(bayes_p_change_feel$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_feel_sum = round(exp(bayes_p_change_feel_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_feel_sum= bayes_p_change_feel_sum - 1
bayes_p_change_feel_sum
### Grabing the means, sds, and n's for each group
mean_sd_feel= round(compmeans(telehealth_noms_wide_noms_feel_month6_complete$total_month6, telehealth_noms_wide_noms_feel_month6_complete$telehealth.y),2)
mean_sd_feel
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_feel_d =  psych::cohen.d(telehealth_noms_wide_noms_feel_month6_complete$total_month6, group = telehealth_noms_wide_noms_feel_month6_complete$face_to_face)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)
results_feel_diag = data.frame(par_estimate = bayes_p_change_feel_sum[2,1], sd_p_change =  bayes_p_change_feel_sum[2,2], ci_95 = paste0(bayes_p_change_feel_sum[2,3], ",", bayes_p_change_feel_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_feel[1,2], n_post_telehealth = mean_sd_feel[2,2], raw_p_change = round((mean_sd_feel[2,1] -  mean_sd_feel[1,1]) /  mean_sd_feel[1,1],3), telehealth_mean =  mean_sd_feel[2,1], face_to_face_mean =  mean_sd_feel[1,1], telehealth_sd= mean_sd_feel[2,3], face_to_face_sd= mean_sd_feel[1,3],freq_cohen_d = round(month_6_feel_d$cohen.d[2],3))

write.csv(results_feel_diag, "results_feel_diag.csv", row.names = FALSE)
results_feel_diag
```
Manage last 30 days with diagnosis with t.test for just telehealth Run diagnosis regression to get telehealth_noms_wide_noms_feel_month6_complete)
### Redo with wilcox test
```{r}
feel_diag_tele_only_dat = subset(telehealth_noms_wide_noms_feel_month6_complete, telehealth.y == 1)
hist(feel_diag_tele_only_dat$total_month6)
qqnorm(feel_diag_tele_only_dat$total_month6)
compmeans(feel_diag_tele_only_dat$total_month6, feel_diag_tele_only_dat$dep)

### recode dep so it compares dep first relative to not depressed
feel_diag_tele_only_dat$dep_recode = ifelse(feel_diag_tele_only_dat$dep == 1, 0,1)
dep_feel = wilcox.test(feel_diag_tele_only_dat$total_month6 ~ feel_diag_tele_only_dat$dep_recode, conf.int = TRUE)

dep_feel = data.frame(t_value = dep_feel$statistic, p_value = dep_feel$p.value, lower = dep_feel$conf.int[1], upper = dep_feel$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(feel_diag_tele_only_dat$total_month6 ~ feel_diag_tele_only_dat$dep_recode)

n_dep_feel =describe.factor(feel_diag_tele_only_dat$dep)
n_dep_feel = data.frame(n_dep_feel)
n_dep_feel = n_dep_feel[1,]
n_dep_feel = data.frame(n_dep_feel)
colnames(n_dep_feel) = n_dep_feel
colnames(n_dep_feel) = c("No major depression count", "Major depression count")
n_feel_dat = data.frame(dim(feel_diag_tele_only_dat)[1])
colnames(n_feel_dat) = "Mental health 30 days total n"

feel_dep_means = compmeans(feel_diag_tele_only_dat$total_month6, feel_diag_tele_only_dat$dep)

feel_dep_d = psych::cohen.d(feel_diag_tele_only_dat$total_month6, group = feel_diag_tele_only_dat$dep)


results_feel_dep = data.frame(dep_feel, n_total = n_feel_dat, n_dep_feel[2], n_dep_feel[1], raw_p_change = round((feel_dep_means[2,1] -  feel_dep_means[1,1]) /  feel_dep_means[1,1],3), dep_mean =  feel_dep_means[2,1], no_dep_mean =  feel_dep_means[1,1], dep_sd= feel_dep_means[2,3], no_dep_sd= feel_dep_means[1,3])
results_feel_dep = round(results_feel_dep, 2)
library(gt)
title_results_feel_dep = "Wilcox test results for mental health in last 30 days at 6-months for telehealth clients with major depression"
table_results_feel_dep = 
  gt(results_feel_dep) %>%
  tab_header(title = title_results_feel_dep)%>%
  cols_label(t_value = md("W-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Mental.health.30.days.total.n = md("Mental health 30 days total n") , Major.depression.count = md("Major depression count"), No.major.depression.count = md("No major depression count"), raw_p_change = md("raw p change"), dep_mean = md("Depression mean"), no_dep_mean = md("No depression mean"), dep_sd = md("Depression sd"), no_dep_sd = md("No depression sd"))
table_results_feel_dep

gtsave(table_results_feel_dep, "table_results_feel_dep.png")
#############################################################################################################################
### Bipolar now
feel_diag_tele_only_dat = subset(telehealth_noms_wide_noms_feel_month6_complete, telehealth.y == 1)
hist(feel_diag_tele_only_dat$total_month6)
qqnorm(feel_diag_tele_only_dat$total_month6)
compmeans(feel_diag_tele_only_dat$total_month6, feel_diag_tele_only_dat$bipolar)

### recode bipolar so it compares bipolar first relative to not bipolarressed
feel_diag_tele_only_dat$bipolar_recode = ifelse(feel_diag_tele_only_dat$bipolar == 1, 0,1)
bipolar_feel = wilcox.test(feel_diag_tele_only_dat$total_month6 ~ feel_diag_tele_only_dat$bipolar_recode, conf.int = TRUE)
bipolar_feel = data.frame(t_value = bipolar_feel$statistic, p_value = bipolar_feel$p.value, lower = bipolar_feel$conf.int[1], upper = bipolar_feel$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(feel_diag_tele_only_dat$total_month6 ~ feel_diag_tele_only_dat$bipolar_recode)

n_bipolar_feel =describe.factor(feel_diag_tele_only_dat$bipolar)
n_bipolar_feel = data.frame(n_bipolar_feel)
n_bipolar_feel = n_bipolar_feel[1,]
n_bipolar_feel = data.frame(n_bipolar_feel)
colnames(n_bipolar_feel) = n_bipolar_feel
colnames(n_bipolar_feel) = c("No bipolar count", "Bipolar count")
n_feel_dat = data.frame(dim(feel_diag_tele_only_dat)[1])
colnames(n_feel_dat) = "Mental health 30 days total n"

feel_bipolar_means = compmeans(feel_diag_tele_only_dat$total_month6, feel_diag_tele_only_dat$bipolar)

feel_bipolar_d = psych::cohen.d(feel_diag_tele_only_dat$total_month6, group = feel_diag_tele_only_dat$bipolar)


results_feel_bipolar = data.frame(bipolar_feel, n_total = n_feel_dat, n_bipolar_feel[2], n_bipolar_feel[1], raw_p_change = round((feel_bipolar_means[2,1] -  feel_bipolar_means[1,1]) /  feel_bipolar_means[1,1],3), bipolar_mean =  feel_bipolar_means[2,1], no_bipolar_mean =  feel_bipolar_means[1,1], bipolar_sd= feel_bipolar_means[2,3], no_bipolar_sd= feel_bipolar_means[1,3],freq_cohen_d = round(feel_bipolar_d$cohen.d[2],3))
results_feel_bipolar = round(results_feel_bipolar, 2)
library(gt)
title_results_feel_bipolar = "Wilcox test results for mental health in last 30 days at 6-months for telehealth clients with primary diagnosis as bipolar"
table_results_feel_bipolar = 
  gt(results_feel_bipolar) %>%
  tab_header(title = title_results_feel_bipolar)%>%
  cols_label(t_value = md("W-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Mental.health.30.days.total.n = md("Mental health 30 days total n") , Bipolar.count = md("Bipolar count"), No.bipolar.count = md("No bipolar count"), raw_p_change = md("raw p change"), bipolar_mean = md("Bipolar mean"), no_bipolar_mean = md("No bipolar mean"), bipolar_sd = md("Bipolar sd"), no_bipolar_sd = md("No bipolar sd"), freq_cohen_d = md("Cohen D"))
table_results_feel_bipolar

gtsave(table_results_feel_bipolar, "table_results_feel_bipolar.png")



```

#######################

6.	The following questions relate to your experience with alcohol, cigarettes, and other drugs. Some of the substances we’ll talk about are prescribed by a doctor (like pain medications). But I will only record those if you have taken them for reasons or in doses other than prescribed.

In the past 30 days, how often have you used …

a.	tobacco products (cigarettes, chewing tobacco, cigars, etc.)?
Tobacco_Use

b.	alcoholic beverages (beer, wine, liquor, etc.)?
Alcohol_Use

k.	prescription opioids (fentanyl, oxycodone [OxyContin, Percocet], hydrocodone [Vicodin], methadone, buprenorphine, etc.)?
RxOpioids_Use

j.	street opioids (heroin, opium, etc.)?
StreetOpioids_Use

```{r}

### All items should be 1 to 5
### Not enough for drugs
telehealth_alc_tob = telehealth_noms_wide_noms[c("telehealth.y", "Tobacco_Use.x", "Tobacco_Use.y", "Alcohol_Use.x", "Alcohol_Use.y", "Agegroup.y", "Gender.y", "RaceWhite.y")]
apply(telehealth_alc_tob,2, function(x){describe.factor(x)})
library(psych)
### Only two items not needed.
### Plug in all the .x variables 
#omega_sat_base =  omega(telehealth_noms_wide_noms_sat[c(2,4,6,8)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
#omega_sat_6month =  omega(telehealth_noms_wide_noms_sat[c(3,5,7,9)], poly = TRUE)
#omega_sat_6month

### Plug in all the .x variables 
#vss(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly")
#fa(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
#vss(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly")
#fa(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly", correct = 0)

########### 
# Create total scores and use sum so you can get counts 
### Plug in all .x variables
telehealth_alc_tob$total_base = apply(telehealth_alc_tob[c(2,4)], 1, mean, na.rm = TRUE)
### Plug in all .y expect for telehealth.y
telehealth_alc_tob$total_month6 = apply(telehealth_alc_tob[c(3,5)], 1, mean, na.rm = TRUE)
hist(log(telehealth_alc_tob$total_month6))

### Make binary anything greater than 1 means you used alcohol or tobacco 
describe.factor(telehealth_alc_tob$total_month6)
hist(telehealth_alc_tob$total_month6)
compmeans(telehealth_alc_tob$total_month6, telehealth_alc_tob$telehealth.y)
describeBy(telehealth_alc_tob$total_month6, telehealth_alc_tob$telehealth.y)


telehealth_alc_tob$total_month6 = ifelse(telehealth_alc_tob$total_month6 == 1, 1,0)

## No data for difference scores so try just 6months
range(telehealth_alc_tob$total_month6, na.rm = TRUE)
dim(telehealth_alc_tob)
head(telehealth_alc_tob)
telehealth_alc_tob
telehealth_alc_tob_complete = na.omit(telehealth_alc_tob[c("total_month6", "telehealth.y","Agegroup.y", "Gender.y", "RaceWhite.y")])
dim(telehealth_alc_tob_complete)
telehealth_alc_tob_complete
describe.factor(telehealth_alc_tob_complete$telehealth.y)

############ Run posisson regression
library(rstanarm)
library(descr)
### Scale is exp(.2) which means 20% difference in each direction


my_prior = normal(location = 0, scale = exp(.2), autoscale = FALSE)
my_prior = student_t(4,0,.2)
describe.factor(telehealth_alc_tob_complete$telehealth.y)
n_total = dim(telehealth_alc_tob_complete)[1]
## Take log of outcome to get percentage change interpretation

telehealth_alc_tob_complete$face_to_face = ifelse(telehealth_alc_tob_complete$telehealth.y == 1, 0,1)
bayes_p_change_al_tob = stan_glm(total_month6~ face_to_face + Agegroup.y + Gender.y + RaceWhite.y, prior = my_prior, family = binomial(link = "logit"), data = telehealth_alc_tob_complete, seed = 123)
bayes_p_change_al_tob
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_al_tob_sum = round(bayes_p_change_al_tob$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_al_tob_sum = round(exp(bayes_p_change_al_tob_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_al_tob_sum= bayes_p_change_al_tob_sum - 1
bayes_p_change_al_tob_sum

### Need mean sd change here
telehealth_alc_tob_complete

mean_sd_alc_tob= round(compmeans(telehealth_alc_tob_complete$total_month6, telehealth_alc_tob_complete$telehealth.y),2)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)

results_alc_tob = data.frame(odds_change_alc_tob = bayes_p_change_al_tob_sum[2,1], sd_odds_change =  bayes_p_change_al_tob_sum[2,2], ci_95 = paste0(bayes_p_change_al_tob_sum[2,3], ",", bayes_p_change_al_tob_sum[2,4]), n_total = mean_sd_sat[3,2], n_pre_telehealth = mean_sd_sat[1,2], n_post_telehealth = mean_sd_sat[2,2], raw_p_change = round((mean_sd_alc_tob[2,1]-mean_sd_alc_tob[1,1])/mean_sd_alc_tob[2,1],2), p_post = mean_sd_alc_tob[2,1], p_pre = mean_sd_alc_tob[1,1])

write.csv(results_alc_tob, "results_alc_tob.csv", row.names = FALSE)
results_alc_tob
prior_summary(bayes_p_change_al_tob)
exp(.2)


```
Alcohol and tobacoo diagnosis
```{r}
### All items should be 1 to 5
### Not enough for drugs
telehealth_alc_tob = telehealth_noms_wide_noms[c("telehealth.y", "Tobacco_Use.x", "Tobacco_Use.y", "Alcohol_Use.x", "Alcohol_Use.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_alc_tob,2, function(x){describe.factor(x)})
library(psych)
### Only two items not needed.
### Plug in all the .x variables 
#omega_sat_base =  omega(telehealth_noms_wide_noms_sat[c(2,4,6,8)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
#omega_sat_6month =  omega(telehealth_noms_wide_noms_sat[c(3,5,7,9)], poly = TRUE)
#omega_sat_6month

### Plug in all the .x variables 
#vss(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly")
#fa(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
#vss(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly")
#fa(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly", correct = 0)

########### 
# Create total scores and use sum so you can get counts 
### Plug in all .x variables
telehealth_alc_tob$total_base = apply(telehealth_alc_tob[c(2,4)], 1, mean, na.rm = TRUE)
### Plug in all .y expect for telehealth.y
telehealth_alc_tob$total_month6 = apply(telehealth_alc_tob[c(3,5)], 1, mean, na.rm = TRUE)
hist(log(telehealth_alc_tob$total_month6))

### Make binary anything greater than 1 means you used alcohol or tobacco 
describe.factor(telehealth_alc_tob$total_month6)
hist(telehealth_alc_tob$total_month6)
compmeans(telehealth_alc_tob$total_month6, telehealth_alc_tob$telehealth.y)
describeBy (telehealth_alc_tob$total_month6, telehealth_alc_tob$telehealth.y)


telehealth_alc_tob$total_month6 = ifelse(telehealth_alc_tob$total_month6 == 1, 1,0)

## No data for difference scores so try just 6months
range(telehealth_alc_tob$total_month6, na.rm = TRUE)
dim(telehealth_alc_tob)
head(telehealth_alc_tob)
telehealth_alc_tob
telehealth_alc_tob_complete = na.omit(telehealth_alc_tob[c("total_month6", "telehealth.y","Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])
dim(telehealth_alc_tob_complete)
telehealth_alc_tob_complete
describe.factor(telehealth_alc_tob_complete$telehealth.y)

############ Run posisson regression
library(rstanarm)
library(descr)
### Scale is exp(.2) which means 20% difference in each direction


my_prior = normal(location = 0, scale = exp(.2), autoscale = FALSE)
my_prior = student_t(4,0,.2)
describe.factor(telehealth_alc_tob_complete$telehealth.y)
n_total = dim(telehealth_alc_tob_complete)[1]
## Take log of outcome to get percentage change interpretation

telehealth_alc_tob_complete$face_to_face = ifelse(telehealth_alc_tob_complete$telehealth.y == 1, 0,1)
bayes_p_change_al_tob = stan_glm(total_month6~ face_to_face*dep + face_to_face*bipolar + Agegroup.y + Gender.y + RaceWhite.y, prior = my_prior, family = binomial(link = "logit"), data = telehealth_alc_tob_complete, seed = 123)
bayes_p_change_al_tob
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_al_tob_sum = round(bayes_p_change_al_tob$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_al_tob_sum = round(exp(bayes_p_change_al_tob_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_al_tob_sum= bayes_p_change_al_tob_sum - 1
bayes_p_change_al_tob_sum

### Need mean sd change here
telehealth_alc_tob_complete

mean_sd_alc_tob= round(compmeans(telehealth_alc_tob_complete$total_month6, telehealth_alc_tob_complete$telehealth.y),2)
### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
## Change results from results_sat to whatever you are measuring results_(fill in name)

results_alc_tob_diag = data.frame(odds_change_alc_tob = bayes_p_change_al_tob_sum[2,1], sd_odds_change =  bayes_p_change_al_tob_sum[2,2], ci_95 = paste0(bayes_p_change_al_tob_sum[2,3], ",", bayes_p_change_al_tob_sum[2,4]), n_total = mean_sd_sat[3,2], n_pre_telehealth = mean_sd_sat[1,2], n_post_telehealth = mean_sd_sat[2,2], raw_p_change = round((mean_sd_alc_tob[2,1]-mean_sd_alc_tob[1,1])/mean_sd_alc_tob[2,1],2), p_post = mean_sd_alc_tob[2,1], p_pre = mean_sd_alc_tob[1,1])

write.csv(results_alc_tob_diag, "results_alc_tob_diag.csv", row.names = FALSE)
results_alc_tob
prior_summary(bayes_p_change_al_tob)
exp(.2)
```
Alcohol and tobacco with diagnosis with t.test for just telehealth 
### Redo with wilcox test
```{r}
#### Get full outcome don't need to dicotmize
telehealth_alc_tob = telehealth_noms_wide_noms[c("telehealth.y", "Tobacco_Use.x", "Tobacco_Use.y", "Alcohol_Use.x", "Alcohol_Use.y", "Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")]
apply(telehealth_alc_tob,2, function(x){describe.factor(x)})
library(psych)

telehealth_alc_tob$total_month6 = apply(telehealth_alc_tob[c(3,5)], 1, mean, na.rm = TRUE)
hist(telehealth_alc_tob$total_month6)
telehealth_alc_tob_complete = na.omit(telehealth_alc_tob[c("total_month6", "telehealth.y","Agegroup.y", "Gender.y", "RaceWhite.y", "dep", "bipolar")])

alc_tob_diag_tele_only_dat = subset(telehealth_alc_tob_complete, telehealth.y == 1)
hist(alc_tob_diag_tele_only_dat$total_month6)
qqnorm(alc_tob_diag_tele_only_dat$total_month6)
compmeans(alc_tob_diag_tele_only_dat$total_month6, alc_tob_diag_tele_only_dat$dep)

### recode dep so it compares dep first relative to not depressed
alc_tob_diag_tele_only_dat$dep_recode = ifelse(alc_tob_diag_tele_only_dat$dep == 1, 0,1)
dep_alc_tob = wilcox.test(alc_tob_diag_tele_only_dat$total_month6 ~ alc_tob_diag_tele_only_dat$dep_recode, conf.int = TRUE)

dep_alc_tob = data.frame(t_value = dep_alc_tob$statistic, p_value = dep_alc_tob$p.value, lower = dep_alc_tob$conf.int[1], upper = dep_alc_tob$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(alc_tob_diag_tele_only_dat$total_month6 ~ alc_tob_diag_tele_only_dat$dep_recode)

n_dep_alc_tob =describe.factor(alc_tob_diag_tele_only_dat$dep)
n_dep_alc_tob = data.frame(n_dep_alc_tob)
n_dep_alc_tob = n_dep_alc_tob[1,]
n_dep_alc_tob = data.frame(n_dep_alc_tob)
colnames(n_dep_alc_tob) = n_dep_alc_tob
colnames(n_dep_alc_tob) = c("No major depression count", "Major depression count")
n_alc_tob_dat = data.frame(dim(alc_tob_diag_tele_only_dat)[1])
colnames(n_alc_tob_dat) = "Alcohol or tobacco use total n"

alc_tob_dep_means = compmeans(alc_tob_diag_tele_only_dat$total_month6, alc_tob_diag_tele_only_dat$dep)

alc_tob_dep_d = psych::cohen.d(alc_tob_diag_tele_only_dat$total_month6, group = alc_tob_diag_tele_only_dat$dep)


results_alc_tob_dep = data.frame(dep_alc_tob, n_total = n_alc_tob_dat, n_dep_alc_tob[2], n_dep_alc_tob[1], raw_p_change = round((alc_tob_dep_means[2,1] -  alc_tob_dep_means[1,1]) /  alc_tob_dep_means[1,1],3), dep_mean =  alc_tob_dep_means[2,1], no_dep_mean =  alc_tob_dep_means[1,1], dep_sd= alc_tob_dep_means[2,3], no_dep_sd= alc_tob_dep_means[1,3])
results_alc_tob_dep = round(results_alc_tob_dep, 2)
library(gt)
title_results_alc_tob_dep = "Wilcox test results for alcohol or tobacco last 30 days at 6-months for telehealth clients with major depression"
table_results_alc_tob_dep = 
  gt(results_alc_tob_dep) %>%
  tab_header(title = title_results_alc_tob_dep)%>%
  cols_label(t_value = md("W-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Alcohol.or.tobacco.use.total.n
 = md("Alcohol or tobacco total n") , Major.depression.count = md("Major depression count"), No.major.depression.count = md("No major depression count"), raw_p_change = md("raw p change"), dep_mean = md("Depression mean"), no_dep_mean = md("No depression mean"), dep_sd = md("Depression sd"), no_dep_sd = md("No depression sd"))
table_results_alc_tob_dep

gtsave(table_results_alc_tob_dep, "table_results_alc_tob_dep.png")
#############################################################################################################################
### Bipolar now
alc_tob_diag_tele_only_dat = subset(telehealth_alc_tob, telehealth.y == 1)
hist(alc_tob_diag_tele_only_dat$total_month6)
qqnorm(alc_tob_diag_tele_only_dat$total_month6)
compmeans(alc_tob_diag_tele_only_dat$total_month6, alc_tob_diag_tele_only_dat$bipolar)

### recode bipolar so it compares bipolar first relative to not bipolarressed
alc_tob_diag_tele_only_dat$bipolar_recode = ifelse(alc_tob_diag_tele_only_dat$bipolar == 1, 0,1)
bipolar_alc_tob = wilcox.test(alc_tob_diag_tele_only_dat$total_month6 ~ alc_tob_diag_tele_only_dat$bipolar_recode, conf.int = TRUE)
bipolar_alc_tob = data.frame(t_value = bipolar_alc_tob$statistic, p_value = bipolar_alc_tob$p.value, lower = bipolar_alc_tob$conf.int[1], upper = bipolar_alc_tob$conf.int[2])
### Check against wilcox results if both significant than good
wilcox.test(alc_tob_diag_tele_only_dat$total_month6 ~ alc_tob_diag_tele_only_dat$bipolar_recode)

n_bipolar_alc_tob =describe.factor(alc_tob_diag_tele_only_dat$bipolar)
n_bipolar_alc_tob = data.frame(n_bipolar_alc_tob)
n_bipolar_alc_tob = n_bipolar_alc_tob[1,]
n_bipolar_alc_tob = data.frame(n_bipolar_alc_tob)
colnames(n_bipolar_alc_tob) = n_bipolar_alc_tob
colnames(n_bipolar_alc_tob) = c("No bipolar count", "Bipolar count")
n_alc_tob_dat = data.frame(dim(alc_tob_diag_tele_only_dat)[1])
colnames(n_alc_tob_dat) = "Alcohol or tobacco use total n"

alc_tob_bipolar_means = compmeans(alc_tob_diag_tele_only_dat$total_month6, alc_tob_diag_tele_only_dat$bipolar)

alc_tob_bipolar_d = psych::cohen.d(alc_tob_diag_tele_only_dat$total_month6, group = alc_tob_diag_tele_only_dat$bipolar)


results_alc_tob_bipolar = data.frame(bipolar_alc_tob, n_total = n_alc_tob_dat, n_bipolar_alc_tob[2], n_bipolar_alc_tob[1], raw_p_change = round((alc_tob_bipolar_means[2,1] -  alc_tob_bipolar_means[1,1]) /  alc_tob_bipolar_means[1,1],3), bipolar_mean =  alc_tob_bipolar_means[2,1], no_bipolar_mean =  alc_tob_bipolar_means[1,1], bipolar_sd= alc_tob_bipolar_means[2,3], no_bipolar_sd= alc_tob_bipolar_means[1,3],freq_cohen_d = round(alc_tob_bipolar_d$cohen.d[2],3))
results_alc_tob_bipolar = round(results_alc_tob_bipolar, 2)
library(gt)
title_results_alc_tob_bipolar = "Wilcox test results for alcohol or tobacco last 30 days at 6-months for telehealth clients with primary diagnosis as bipolar"
table_results_alc_tob_bipolar = 
  gt(results_alc_tob_bipolar) %>%
  tab_header(title = title_results_alc_tob_bipolar)%>%
  cols_label(t_value = md("W-value"), p_value = md("P-value"), lower = md("Lower"), upper = md("Upper"),Alcohol.or.tobacco.use.total.n
 = md("Alcohol or tobacco total n") , Bipolar.count = md("Bipolar count"), No.bipolar.count = md("No bipolar count"), raw_p_change = md("raw p change"), bipolar_mean = md("Bipolar mean"), no_bipolar_mean = md("No bipolar mean"), bipolar_sd = md("Bipolar sd"), no_bipolar_sd = md("No bipolar sd"), freq_cohen_d = md("Cohen D"))
table_results_alc_tob_bipolar

gtsave(table_results_alc_tob_bipolar, "table_results_alc_tob_bipolar.png")



```

#################
PERCEPTION OF CARE
1.	In order to provide the best possible mental health and related services, we need to know what you think about the services you received during the past 30 days, the people who provided it, and the results. Please indicate your disagreement/agreement with each of the following statements.
a.	Staff here believe that I can grow, change, and recover.
Recover

b.	I felt free to complain.
Complain

c.	I was given information about my rights.
Rights

d.	Staff encouraged me to take responsibility for how I live my life.
Responsibility

e.	Staff told me what side effects to watch out for.
SideEffects

f.	Staff respected my wishes about who is and who is not to be given information about my treatment.
SharingTreatmentInformation

g.	Staff were sensitive to my cultural background (race, religion, language, etc.).
SensitiveToCulture

h.	Staff helped me obtain the information I needed so that I could take charge of managing my illness.
InformationNeeded

i.	I was encouraged to use consumer-run programs (support groups, drop-in centers, crisis phone line, etc.).
ConsumerRunPrograms

j.	I felt comfortable asking questions about my treatment and medication.
ComfortableAskingQuestions

a.	I, not staff, decided my treatment goals.
TreatmentGoals

b.	I like the services I received here.
LikeServices

c.	If I had other choices, I would still get services from this agency.
Choices

d.	I would recommend this agency to a friend or family member.
RecommendAgency

```{r}
telehealth_noms_wide_pc = telehealth_noms_wide_noms[c("telehealth.y", "Recover.x", "Recover.y", "Rights.x", "Rights.y", "Responsibility.x", "Responsibility.y", "SideEffects.x", "SideEffects.y", "SharingTreatmentInformation.x", "SharingTreatmentInformation.y", "SensitiveToCulture.x", "SensitiveToCulture.y", "InformationNeeded.x", "InformationNeeded.y", "ConsumerRunPrograms.x", "ConsumerRunPrograms.y", "ComfortableAskingQuestions.x", "ComfortableAskingQuestions.y", "TreatmentGoals.x", "TreatmentGoals.y", "LikeServices.x", "LikeServices.y", "Choices.x", "Choices.y", "RecommendAgency.x", "RecommendAgency.y")]

apply(telehealth_noms_wide_pc, 2, function(x){describe.factor(x)})

telehealth_noms_wide_pc$total_month6 = apply(telehealth_noms_wide_pc[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], 1, mean, na.rm = TRUE)


### Psychometrics
### Plug in all the .x variables 
omega_pc_6month =  omega(telehealth_noms_wide_pc[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], poly = TRUE)
omega_pc_6month

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_pc[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], cor = "poly")
fa(telehealth_noms_wide_pc[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], cor = "poly", correct = 0)

telehealth_noms_wide_pc_complete = na.omit(telehealth_noms_wide_pc[c("telehealth.y", "total_month6")])
n_total =  dim(telehealth_noms_wide_pc_complete)[1]
### Just do a wilcox test for now.
mean_sd_pc = compmeans(telehealth_noms_wide_pc_complete$total_month6, telehealth_noms_wide_pc_complete$telehealth.y)
mean_sd_pc = round(mean_sd_pc,3)

### This is comparing face to face with telehealth
results_pc = wilcox.test(telehealth_noms_wide_pc_complete$total_month6 ~telehealth_noms_wide_pc_complete$telehealth.y, conf.int = TRUE)
round(results_pc$estimate,3)


results_pc = data.frame(diff_location = round(results_pc$estimate,3), ci_95 = paste0(round(results_pc$conf.int[1],3), ",", round(results_pc$conf.int[2],3)), n_total = n_total, n_pre_telehealth = mean_sd_pc[1,2], n_post_telehealth = mean_sd_pc[2,2], mean_telehealth = mean_sd_pc[2,1], mean_pre_telehealth = mean_sd_pc[1,1])

results_pc
write.csv(results_pc, "results_pc.csv", row.names = FALSE)

#### What percent of staff have a positive perception of care with centertone 
telehealth_only_positive = subset(telehealth_noms_wide_pc_complete, telehealth.y == 1)
telehealth_only_positive$positive = ifelse(telehealth_only_positive$total_month6 > 3, 1, 0)

agree_p_n =  describe.factor(telehealth_only_positive$positive)
agree_p_n = data.frame(agree_p_n)
agree_p_n$total = apply(agree_p_n, 1, sum)
colnames(agree_p_n) = c("agree_or_greater", "less_than_agree", "total")
agree_p_n = round(agree_p_n,2)
write.csv(agree_p_n, "agree_p_n.csv")

#### Recomend
recommend_agree = subset(telehealth_noms_wide_pc, telehealth.y == 1)
recommend_agree = recommend_agree[c("telehealth.y", "RecommendAgency.y")]
recommend_agree_complete = na.omit(recommend_agree)
recommend_agree_complete$recommend_agree = ifelse(recommend_agree_complete$RecommendAgency.y > 3, 1, 0)
recommend_agree_results =  data.frame(describe.factor(recommend_agree_complete$recommend_agree))
recommend_agree_results$total = apply(recommend_agree_results, 1, sum)
colnames(recommend_agree_results) = c("agree_or_greater", "less_than_agree", "total")
recommend_agree_results = round(recommend_agree_results,2)
recommend_agree_results
write.csv(recommend_agree_results, "recommend_agree_results.csv")
```
Perception of care for dep
```{r}
telehealth_noms_wide_pc_dep = telehealth_noms_wide_noms[c("telehealth.y", "Recover.x", "Recover.y", "Rights.x", "Rights.y", "Responsibility.x", "Responsibility.y", "SideEffects.x", "SideEffects.y", "SharingTreatmentInformation.x", "SharingTreatmentInformation.y", "SensitiveToCulture.x", "SensitiveToCulture.y", "InformationNeeded.x", "InformationNeeded.y", "ConsumerRunPrograms.x", "ConsumerRunPrograms.y", "ComfortableAskingQuestions.x", "ComfortableAskingQuestions.y", "TreatmentGoals.x", "TreatmentGoals.y", "LikeServices.x", "LikeServices.y", "Choices.x", "Choices.y", "RecommendAgency.x", "RecommendAgency.y", "dep", "bipolar")]

apply(telehealth_noms_wide_pc_dep, 2, function(x){describe.factor(x)})

telehealth_noms_wide_pc_dep$total_month6 = apply(telehealth_noms_wide_pc_dep[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], 1, mean, na.rm = TRUE)


telehealth_noms_wide_pc_dep_complete = na.omit(telehealth_noms_wide_pc_dep[c("telehealth.y", "total_month6", "dep", "bipolar")])
telehealth_noms_wide_pc_dep_complete = subset(telehealth_noms_wide_pc_dep_complete, dep == 1)
n_total =  dim(telehealth_noms_wide_pc_dep_complete)[1]


#### What percent of staff with dep  have a positive perception of care with centertone 
telehealth_only_positive = subset(telehealth_noms_wide_pc_dep_complete, telehealth.y == 1)
telehealth_only_positive$positive = ifelse(telehealth_only_positive$total_month6 > 3, 1, 0)

agree_p_n =  describe.factor(telehealth_only_positive$positive)
agree_p_n = data.frame(agree_p_n, less_than_agree = c(0,0))
agree_p_n$total = apply(agree_p_n, 1, sum)
colnames(agree_p_n) = c("agree_or_greater", "less_than_agree", "total")
agree_p_n_dep = round(agree_p_n,2)
write.csv(agree_p_n_dep, "agree_p_n_dep.csv")

#### Recomend for dep
recommend_agree = subset(telehealth_noms_wide_pc_dep, telehealth.y == 1)
recommend_agree = subset(recommend_agree, dep == 1)
recommend_agree = recommend_agree[c("telehealth.y", "RecommendAgency.y")]
recommend_agree_complete = na.omit(recommend_agree)
recommend_agree_complete$recommend_agree = ifelse(recommend_agree_complete$RecommendAgency.y > 3, 1, 0)
recommend_agree_results =  data.frame(describe.factor(recommend_agree_complete$recommend_agree))
recommend_agree_results =data.frame(recommend_agree_results, less_than_agree = c(0,0)) 
recommend_agree_results$total = apply(recommend_agree_results, 1, sum)
colnames(recommend_agree_results) = c("agree_or_greater", "less_than_agree", "total")
recommend_agree_results = round(recommend_agree_results,2)
recommend_agree_results
write.csv(recommend_agree_results, "recommend_agree_results.csv")
```
Perception of care for bipolar
```{r}
telehealth_noms_wide_pc_bipolar = telehealth_noms_wide_noms[c("telehealth.y", "Recover.x", "Recover.y", "Rights.x", "Rights.y", "Responsibility.x", "Responsibility.y", "SideEffects.x", "SideEffects.y", "SharingTreatmentInformation.x", "SharingTreatmentInformation.y", "SensitiveToCulture.x", "SensitiveToCulture.y", "InformationNeeded.x", "InformationNeeded.y", "ConsumerRunPrograms.x", "ConsumerRunPrograms.y", "ComfortableAskingQuestions.x", "ComfortableAskingQuestions.y", "TreatmentGoals.x", "TreatmentGoals.y", "LikeServices.x", "LikeServices.y", "Choices.x", "Choices.y", "RecommendAgency.x", "RecommendAgency.y", "bipolar", "bipolar")]

apply(telehealth_noms_wide_pc_bipolar, 2, function(x){describe.factor(x)})

telehealth_noms_wide_pc_bipolar$total_month6 = apply(telehealth_noms_wide_pc_bipolar[,c(3,5,7,9,11,13,15,17,19,21,23,25,27)], 1, mean, na.rm = TRUE)


telehealth_noms_wide_pc_bipolar_complete = na.omit(telehealth_noms_wide_pc_bipolar[c("telehealth.y", "total_month6", "bipolar", "bipolar")])
telehealth_noms_wide_pc_bipolar_complete = subset(telehealth_noms_wide_pc_bipolar_complete, bipolar == 1)
n_total =  dim(telehealth_noms_wide_pc_bipolar_complete)[1]


#### What percent of staff with bipolar  have a positive perception of care with centertone 
telehealth_only_positive = subset(telehealth_noms_wide_pc_bipolar_complete, telehealth.y == 1)
telehealth_only_positive$positive = ifelse(telehealth_only_positive$total_month6 > 3, 1, 0)

agree_p_n =  describe.factor(telehealth_only_positive$positive)
agree_p_n = data.frame(agree_p_n, less_than_agree = c(0,0))
agree_p_n$total = apply(agree_p_n, 1, sum)
colnames(agree_p_n) = c("agree_or_greater", "less_than_agree", "total")
agree_p_n_bipolar = round(agree_p_n,2)
write.csv(agree_p_n_bipolar, "agree_p_n_bipolar.csv")

#### Recomend for bipolar
recommend_agree = subset(telehealth_noms_wide_pc_bipolar, telehealth.y == 1)
recommend_agree = subset(recommend_agree, bipolar == 1)
recommend_agree = recommend_agree[c("telehealth.y", "RecommendAgency.y")]
recommend_agree_complete = na.omit(recommend_agree)
recommend_agree_complete$recommend_agree = ifelse(recommend_agree_complete$RecommendAgency.y > 3, 1, 0)
recommend_agree_results =  data.frame(describe.factor(recommend_agree_complete$recommend_agree))
recommend_agree_results =data.frame(recommend_agree_results) 
recommend_agree_results$total = apply(recommend_agree_results, 1, sum)
colnames(recommend_agree_results) = c("agree_or_greater", "less_than_agree", "total")
recommend_agree_results = round(recommend_agree_results,2)
recommend_agree_results
write.csv(recommend_agree_results, "recommend_agree_results.csv")

```
Health care and job analysis

All nights vairable are 1 to 30.
1 = Full time, 2 = part-time
```{r}
healthcare_dat = data.frame(NightsDetox.x = telehealth_noms_wide_noms$NightsDetox.x, NightsDetox.y = telehealth_noms_wide_noms$NightsDetox.y, NightsHospitalMHC.x = telehealth_noms_wide_noms$NightsHospitalMHC.x, NightsHospitalMHC.y = telehealth_noms_wide_noms$NightsHospitalMHC.y, TimesER.x = telehealth_noms_wide_noms$TimesER.x, TimesER.y = telehealth_noms_wide_noms$TimesER.y, NightsJail.x = telehealth_noms_wide_noms$NightsJail.x, NightsJail.y = telehealth_noms_wide_noms$NightsJail.y, Employment.x = telehealth_noms_wide_noms$Employment.x, Employment.y = telehealth_noms_wide_noms$Employment.y)
```
Check missingness and descriptives
```{r}
healthcare_dat
miss_var_summary(healthcare_dat)
apply(healthcare_dat, 2, function(x){describe.factor(x)})
```
Create employment variables and combine detox and MHC variables
For employment assume 20 for part-time and 30 for full time.  Create variables for both and look at the difference between the two and take that amount times the rate
```{r}
healthcare_dat$part_time.x = ifelse(healthcare_dat$Employment.x == 2, 1, 0)
healthcare_dat$part_time.y = ifelse(healthcare_dat$Employment.y == 2, 1, 0)

healthcare_dat$full_time.x = ifelse(healthcare_dat$Employment.x == 1, 1, 0)
healthcare_dat$full_time.y = ifelse(healthcare_dat$Employment.y == 1, 1, 0)

healthcare_dat$MHC_detox.x = healthcare_dat$NightsHospitalMHC.x+ healthcare_dat$NightsDetox.x
healthcare_dat$MHC_detox.y = healthcare_dat$NightsHospitalMHC.y+ healthcare_dat$NightsDetox.y
```
Look at ER visits.  Get the average number of visits at post minus intake then times by with complete data
```{r}
er_visit_dat = healthcare_dat[c("TimesER.x","TimesER.y")]
er_visit_dat_complete = na.omit(er_visit_dat)
n_er_visit_dat_complete =  dim(er_visit_dat)[1]
er_visit_dat_complete$diff_er =er_visit_dat_complete$TimesER.y  -er_visit_dat_complete$TimesER.x
sum_er_diff = round(sum(er_visit_dat_complete$diff_er), 2)
er_cost= round(sum_er_diff*5680.56,2)
er_cost
```
Hosptial reduction costs
```{r}
hos_visit_dat = healthcare_dat[c("MHC_detox.y","MHC_detox.x")]
hos_visit_dat_complete = na.omit(hos_visit_dat)
n_hos_visit_dat_complete =  dim(hos_visit_dat)[1]
hos_visit_dat_complete$diff_hos =hos_visit_dat_complete$MHC_detox.y -hos_visit_dat_complete$MHC_detox.x
range(hos_visit_dat_complete$diff_hos)
sum_hos_diff = round(sum(hos_visit_dat_complete$diff_hos),2)
hos_cost= round(sum_hos_diff*2534.62,2)
hos_cost
```
Jail time
```{r}
jail_visit_dat = healthcare_dat[c("NightsJail.y","NightsJail.x")]
jail_visit_dat_complete = na.omit(jail_visit_dat)
n_jail_visit_dat_complete =  dim(jail_visit_dat)[1]
jail_visit_dat_complete$diff_jail =jail_visit_dat_complete$NightsJail.y -jail_visit_dat_complete$NightsJail.x
sum_jail_diff = round(sum(jail_visit_dat_complete$diff_jail),2)
jail_cost= round(sum_jail_diff*103.53,2)
jail_cost
```
Employment
Look at the average difference between part time and full jobs.  This is the percentage change which is the percentage of the population that changed that is affected. For example, if the mean of the difference is .15, then 15% of people of increased job so take the amount for that measure times .15 * the number of people in the complete data set.

```{r}
part_dat = healthcare_dat[c("part_time.x","part_time.y")]
part_dat_complete = na.omit(part_dat)
n_part_dat_complete =  dim(part_dat)[1]
part_dat_complete$diff_part =part_dat_complete$part_time.y -part_dat_complete$part_time.x
sum_part_diff = round(sum(part_dat_complete$diff_part),2)
part_cost= round(sum_part_diff*1740,2)
part_cost

full_dat = healthcare_dat[c("full_time.x","full_time.y")]
full_dat_complete = na.omit(full_dat)
n_full_dat_complete =  dim(full_dat)[1]
full_dat_complete$diff_full =full_dat_complete$full_time.y -full_dat_complete$full_time.x
range(full_dat_complete$diff_full)
sum_full_diff = round(sum(full_dat_complete$diff_full),2)
full_cost= round(mean_full_diff*3480, 2)
full_cost


### Get total cost
total_cost = sum(er_cost, hos_cost, jail_cost, -part_cost, -full_cost)
total_cost
```
Make Table with each amount per unit for measure, number of measurses, and the units, then amount.
```{r}
library(dplyr)
tab_dat = matrix(c("ER", "hospital", "jail", "part_time", "full_time", "Total", 5680.56, 2534.62, 103.53, 1740, 3480, "", sum_er_diff, sum_hos_diff, sum_jail_diff, sum_part_diff, sum_full_diff, "", er_cost, hos_cost, jail_cost, part_cost, full_cost, total_cost), nrow = 6)
tab_dat = data.frame(tab_dat)
colnames(tab_dat) = c("measure", "savings_per_unit", "unit_difference", "cost_savings")
tab_dat
library(gt)
title_tab_dat = c(paste0("Cost saving and job benefits from NOMS data 6-1-20", " ", "n","=", dim(healthcare_dat)[1]))
### Add title, change names of measure, make $, add footnote for part and full time cost savings, add footnote for average saving percentage to make it clear what the units are.
tab_dat$measure = recode(tab_dat$measure, "part_time" = "Part time", "jail"= "Jail", "hospital" = "Hospital", "full_time" = "Full time")
write.csv(tab_dat, "tab_dat.csv", row.names = FALSE)
tab_dat = read.csv("tab_dat.csv", header = TRUE)
tab_dat_table =  gt(tab_dat) %>%
  tab_header(title = title_tab_dat) %>%
  tab_footnote(footnote = "Unit is change in number of ER visits",  locations = cells_body(columns = vars(unit_difference), rows = 1)) %>%
  tab_footnote(footnote = "Unit is change in number of days",  locations = cells_body(columns = vars(unit_difference), rows = c(2,3))) %>%
  tab_footnote(footnote = "Unit is change in number for part / full time jobs",  locations = cells_body(columns = vars(unit_difference), rows = c(4,5)))%>%
  fmt_currency(columns = vars(savings_per_unit, cost_savings))%>%
  tab_footnote(footnote = "Full time and part-time amounts have a negative sign added to them before creating the total to put them in the same direction as cost savings",  locations = cells_body(columns = vars(cost_savings), rows = c(4,5)))%>%
  cols_label(measure = md("Measure"), savings_per_unit = md("Savings per unit"), unit_difference = md("Unit"), cost_savings = md("Cost savings"))
tab_dat_table
gtsave(tab_dat_table, "tab_dat_table.png")   
10*5680.56  
```
Response rates
Want telehealth_noms, because that is not merged yet.  
Then keep Assessment_new 0 for baseline and 2 for 6-month

```{r}
library(lubridate)
library(tidyr)
response_dat =  data.frame(ConsumerID = telehealth_noms$ConsumerID, ConductedInterview = telehealth_noms$ConductedInterview, InterviewDate = telehealth_noms$InterviewDate, Assessment_new = telehealth_noms$Assessment_new)
response_dat = subset(response_dat, Assessment_new == 0 | Assessment_new == 2)
response_dat$InterviewDate = mdy(response_dat$InterviewDate)
response_dat
response_dat =subset(response_dat, ConductedInterview == 1)
### Assume everyone in the system counts towards the reassessment rate
## Let's limit the data set to only those who are eligible then get the number of 6-months relative to the total baselines.  So describe.factor for the number of 2's divided by the number of 0's. 
## If you have interview date for 0 that is less than  "2019-11-01" create a variable
### If you have interview date for 2 that is greater than or equal to "2019-11-01" and not "1869-01-01" then a one 
## Then keep if either rule 1 or rule 2 is true
response_dat$rule_1 = ifelse(response_dat$Assessment_new == 0 & response_dat$InterviewDate < "2019-11-01",1, 0)
response_dat$rule_2 = ifelse(response_dat$Assessment_new == 2 & response_dat$InterviewDate >= "2019-11-01" & response_dat$InterviewDate != "1869-01-01", 1, 0)
describe.factor(response_dat$rule_2)
subset(response_dat, rule_2==1)
response_dat_eligible = subset(response_dat, rule_1 == 1 | rule_2 == 1)
tail(response_dat_eligible,100)
response_dat_eligible
### Let's try and see if you limit to only those ID's who 

all_response_rate_dat =  describe.factor(response_dat_eligible$Assessment_new)
elig_n_all = all_response_rate_dat[1,1]
elig_n_6_month =all_response_rate_dat[1,2]
all_response_rate = round(elig_n_6_month /elig_n_all ,2)
all_response_rate
## What is the rate before telehealth?  Exclude anyone with an intake or reassessment before 2020-04-01
response_dat_eligible_face = subset(response_dat, InterviewDate < "2020-04-01")
### Get rid of client 7 months prior to 2020-4-1, because they would not be eligible
response_dat_eligible_face$rule_1 = ifelse(response_dat_eligible_face$Assessment_new == 0 & response_dat_eligible_face$InterviewDate < "2019-09-01",1, 0)
response_dat_eligible_face$rule_2 = ifelse(response_dat_eligible_face$Assessment_new == 2 & response_dat_eligible_face$InterviewDate >= "2019-09-01" & response_dat_eligible_face$InterviewDate != "1869-01-01", 1, 0)
subset(response_dat_eligible_face, rule_2==1)

response_dat_eligible_face = subset(response_dat_eligible_face, rule_1 == 1 | rule_2 == 1)

response_dat_eligible_face

face_response_rate_dat =  describe.factor(response_dat_eligible_face$Assessment_new)
elig_n_all_face = face_response_rate_dat[1,1]
elig_n_6_month_face =face_response_rate_dat[1,2]
response_rate_face = round(elig_n_6_month_face /elig_n_all_face ,2)
response_rate_face

```
Response rate table
Total N is used for N is proportion tests: https://sixsigmastudyguide.com/one-and-two-sample-proportion-hypothesis-tests/
https://stattrek.com/hypothesis-test/difference-in-proportions.aspx
```{r}
tab_response_rate = matrix(c(elig_n_all, elig_n_6_month, all_response_rate, elig_n_all_face, elig_n_6_month_face, response_rate_face), nrow = 2, byrow = TRUE)
tab_response_rate = data.frame(tab_response_rate) 
colnames(tab_response_rate) = c("n_all", "n_6", "rr")
all_telehealth = c("Telehealth", "Face to face")
tab_response_rate = data.frame(all_telehealth, tab_response_rate)
library(gt)
title_tab_response = c("Comparing response rates for face to face and telehealth from NOMS data as of 6-1-20")
tab_response = gt(tab_response_rate)%>%
     tab_header(title = title_tab_response)%>%
    tab_footnote(footnote = "N all for telehealth  means all clients with intake data prior to 11-1-19, which is 7 months prior 6-1-20.  Only including clients who have intake data 7 months prior to 6-1-20 ensures we only include clients who should have received a 6-month reassessment.  However, we also included a client if they had a 6-month reassessment and the intake was after 11-1-19, because that means their data was collected early.",  locations = cells_body(columns = vars(n_all), rows = c(1)))%>%
    tab_footnote(footnote = "N all for face to face first subsetting all data prior to 4-1-2019.  Then all clients with intake data prior to 09-1-19, which is 7 months prior 4-1-20.  Only including clients who have intake data 7 months prior to 4-1-20 ensures we only include clients who should have received a 6-month reassessment.  However, we also included a client if they had a 6-month reassessment and the intake was after 9-1-19, because that means their data was collected early.",  locations = cells_body(columns = vars(n_all), rows = c(2)))%>%
    tab_footnote(footnote = "No statistically significant difference between response rates p = .34.",  locations = cells_body(columns = vars(rr), rows = c(1,2)))%>%
    cols_label(all_telehealth = md("Time label"), n_all = md("Total N eligible"), n_6 = md("Total 6-months eligible"), rr = md("Response rate"))
tab_response

```
Response rate statistical test
Statistical test for client clincian sub p = (p1 * n1 + p2 * n2) / (n1 + n2) 
SE = sqrt[p * ( 1 - p ) * [ (1/n1) + (1/n2) ] 
z = (p1 - p2) / SE 
https://www.cyclismo.org/tutorial/R/pValues.html


```{r}
tab_response_rate
pool_samp_prop_rr = (tab_response_rate$rr[1]*tab_response_rate$n_all[1] + tab_response_rate$rr[2]*tab_response_rate$n_all[2])/(tab_response_rate$n_all[1]+ tab_response_rate$n_all[2])


se_p_rr = sqrt(pool_samp_prop_rr*(1-pool_samp_prop_rr) * ( (1/tab_response_rate$n_all[1]) +(1/tab_response_rate$n_all[1]) ) )

z_rr = (tab_response_rate$rr[1] - tab_response_rate$rr[2])/se_p_rr
z_rr
p_rr = round(2*pnorm(-abs(z_rr)),4)
p_rr

```
CRI reassess 6_18_20 
p1 = June reassessment
p2 = Feburray resasessment
```{r}
p1 = .52
p2 = .41
p_diff = p1-p2
n1 = 2388
n2 = 2403
p = (p1 * n1 + p2 * n2) / (n1 + n2)
se = sqrt(p * ( 1 - p ) * ((1/n1) + (1/n2))) 
z = (p1 - p2) / se
p_reasses = round(2*pnorm(-abs(z)),4)
p_reasses

ci_upper = p_diff+1.96*se
ci_lower = p_diff-1.96*se

```






```{r}
tab_response_rate
pool_samp_prop_rr = (tab_response_rate$rr[1]*tab_response_rate$n_all[1] + tab_response_rate$rr[2]*tab_response_rate$n_all[2])/(tab_response_rate$n_all[1]+ tab_response_rate$n_all[2])


se_p_rr = sqrt(pool_samp_prop_rr*(1-pool_samp_prop_rr) * ( (1/tab_response_rate$n_all[1]) +(1/tab_response_rate$n_all[1]) ) )

z_rr = (tab_response_rate$rr[1] - tab_response_rate$rr[2])/se_p_rr
z_rr
p_rr = round(2*pnorm(-abs(z_rr)),4)
p_rr

```
CRI reassess 6_18_20 
p1 = June reassessment
p2 = Feburray resasessment
```{r}
p1 = .52
p2 = .41
p_diff = p1-p2
n1 = 2388
n2 = 2403
p = (p1 * n1 + p2 * n2) / (n1 + n2)
se = sqrt(p * ( 1 - p ) * ((1/n1) + (1/n2))) 
z = (p1 - p2) / se
p_reasses = round(2*pnorm(-abs(z)),4)
p_reasses

ci_upper = p_diff+1.96*se
ci_lower = p_diff-1.96*se

```




