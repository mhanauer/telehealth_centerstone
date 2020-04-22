---
title: "telehealth_noms"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

State: IN = 1, IL  = 0
adult_youth = 1 = adult, 0 = youth
youth have "c" in their id

Stack variables

Post stacking transformations



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
2 = 6 month reassessment
3 = 9 month reassessment (vitals)
4 = 12 month reassessment
5 = clinical discharge

Now review the missing data
Only include Baseline and 6-month
telehealth.y means they were in telehealth at 6 months which is what we want

# Data mergeing
For CCBHC IN, IL all the same
For FHHC first 186 variables are the same

telehealth: Telehealth = 1; Pre-telehealth = 0 telehealth defined as those with any assessment date on or after 4-2-2020

### Run this prior to any analysis to load data ####
```{r}
library(lubridate)
library(prettyR)
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
IL =  read.csv("CCBHC_IL_4_14_20.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IN =  read.csv("CCBHC_IN_4_15_20.csv", header = TRUE, na.strings =  c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IN_fhhc = read.csv("fhhc_noms_4_22_20.csv", header= TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
dim(IN)
dim(IL)
dim(IN_fhhc)


## Now stack them
IN_IL = rbind(IN, IL)
dim(IN_IL)
head(IN_IL[,c(1:185)])
IN_IL_fhhc = IN_IL[,1:185]
dim(IN_IL_fhhc)
IN_fhhc = IN_fhhc[,1:185]
dim(IN_fhhc)
IN_IL_fhhc = rbind(IN_IL_fhhc, IN_fhhc)
### Figure out how you can stack FHHC data



telehealth_noms = IN_IL_fhhc
describe.factor(telehealth_noms$Assessment)
## Rename to the above

## No one has multiple reassessments
describe.factor(telehealth_noms$ReassessmentNumber_07)
## Reorder data
telehealth_noms = telehealth_noms[order(telehealth_noms$ConsumerID),]

## Create recoded assessment variable
telehealth_noms$Assessment_new = ifelse(telehealth_noms$Assessment == 600, 0, ifelse(telehealth_noms$Assessment == 301, 1, ifelse(telehealth_noms$Assessment == 302, 2, ifelse(telehealth_noms$Assessment == 303, 3, ifelse(telehealth_noms$Assessment == 304, 4, ifelse(telehealth_noms$Assessment == 699,5, "Wrong"))))))
describe.factor(telehealth_noms$Assessment_new, decr.order= FALSE)

### Create full date variable
telehealth_noms$date = paste0(telehealth_noms$FFY, "-", telehealth_noms$Month, "-", "01")
telehealth_noms$date = ymd(telehealth_noms$date)
head(telehealth_noms$date)

telehealth_noms$telehealth = ifelse(telehealth_noms$date >= "2020-04-01", 1, 0)
telehealth_noms[c("date","telehealth")]
### Cannot be greater than 2020-09-30 last day of grant
telehealth_noms = subset(telehealth_noms, date < "2020-09-30")
telehealth_noms[c("date","telehealth")]
dim(telehealth_noms)


### Create a NOMS data set  
telehealth_noms = subset(telehealth_noms, Assessment_new == 0 | Assessment_new == 2 | Assessment_new == 4)
describe.factor(IN_IL_noms$Assessment_new)
#### Create a vitals data set
IN_IL_vital = subset(telehealth_noms, Assessment_new == 1 | Assessment_new == 3)


####################
library(naniar)
miss_var_summary(telehealth_noms)

### Just grab a few and see what the descriptives are like
#Alcohol_Use
#PsychologicalEmotionalProblems, EnoughEnergyForEverydayLife, Depressed
#telehealth_noms_sub_count = IN_IL_noms[c("ConsumerID", "Assessment_new", "NightsHospitalMHC", "NightsDetox", "telehealth", "date")]
head(telehealth_noms)
miss_var_summary(subset(telehealth_noms, Assessment_new == 2))
miss_var_summary(subset(telehealth_noms, Assessment_new == 0))

telehealth_noms_wide = subset(telehealth_noms, Assessment_new <=2)
describe.factor(telehealth_noms_wide$Assessment_new)
describe.factor(telehealth_noms_wide$telehealth)

### These people have two baselines delete them 'A00276''A00295''A00298'
telehealth_noms_wide_test = subset(telehealth_noms_wide, ConsumerID == "'A00276'" | ConsumerID == "'A00295'" | ConsumerID == "'A00298'")
telehealth_noms_wide[c(276,293, 298), c(1,7)]
telehealth_noms_wide = telehealth_noms_wide[-c(276,293, 298),] 

telehealth_noms_base_noms = subset(telehealth_noms_wide,Assessment_new == 0)
telehealth_noms_month6_noms = subset(telehealth_noms_wide,Assessment_new == 2)
head(telehealth_noms_base_noms)
dim(telehealth_noms_month6_noms)
telehealth_noms_wide_noms = merge(telehealth_noms_base_noms, telehealth_noms_month6_noms, by = "ConsumerID", all.y = TRUE)
dim(telehealth_noms_wide_noms)
```
Data set descriptives
telehealth_noms_wide_noms = CCBHC IN and IL both adults and youth, FFHC NOMS data for baseline and 6-month matched pairs 
### Finish this later

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

telehealth.y: 1 = client had a 6-month reassessment during telehealth, 0 = client had a 6-month reassessment pre-telehealth
.x = data from baseline
.y = data from 6-month reassessment

```{r}
### All items should be 1 to 5
telehealth_noms_wide_noms_sat = telehealth_noms_wide_noms[c("telehealth.y", "PerformDailyActivitiesSatisfaction.x", "PerformDailyActivitiesSatisfaction.y", "HealthSatisfaction.x", "HealthSatisfaction.y", "SelfSatisfaction.x", "SelfSatisfaction.y", "RelationshipSatisfaction.x","RelationshipSatisfaction.y")]
apply(telehealth_noms_wide_noms_sat,2, function(x){describe.factor(x)})
library(psych)

### Plug in all the .x variables 
omega_sat_base =  omega(telehealth_noms_wide_noms_sat[c(2,4,6,8)], poly = TRUE)
### Plug in all the .y variables except telehealth.y 
omega_sat_6month =  omega(telehealth_noms_wide_noms_sat[c(3,5,7,9)], poly = TRUE)
omega_sat_6month

### Plug in all the .x variables 
vss(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly")
fa(telehealth_noms_wide_noms_sat[c(2,4,6,8)], cor = "poly", correct = 0)

### Plug in all the .y variables except telehealth.y 
vss(telehealth_noms_wide_noms_sat[c(3,5,7,9)], cor = "poly")
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
telehealth_noms_wide_noms_sat_month6_complete = na.omit(telehealth_noms_wide_noms_sat[c("total_month6", "telehealth.y")])
dim(telehealth_noms_wide_noms_sat_month6_complete)
telehealth_noms_wide_noms_sat_month6_complete


```
Analysis sat
Percentage change: https://stats.idre.ucla.edu/sas/faq/how-can-i-interpret-log-transformed-variables-in-terms-of-percent-change-in-linear-regression/
  
You need to expondiate the parameter estimate
Doesn't work to well when you get above 20% differences: https://people.duke.edu/~rnau/411log.htm

P-change is bad for regression because of: 
(3-2)/2 
(2-3)/3

```{r}
library(rstanarm)
library(descr)
describe.factor(telehealth_noms_wide_noms_sat_month6_complete$telehealth.y)
n_total = dim(telehealth_noms_wide_noms_sat_month6_complete)[1]
## Take log of outcome to get percentage change interpretation
bayes_p_change_sat = stan_glm(log(total_month6)~ telehealth.y, data = telehealth_noms_wide_noms_sat_month6_complete, seed = 123)
### You should not need to change this.  We want the mean, sd, 2.5, and 97.5
## check bayes_p_change_sat$stan_summary if you are unsure
bayes_p_change_sat_sum = round(bayes_p_change_sat$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
bayes_p_change_sat_sum = round(exp(bayes_p_change_sat_sum),3)
### Creates a percentage instead 1 + % 
bayes_p_change_sat_sum= bayes_p_change_sat_sum - 1
bayes_p_change_sat_sum

### Grabing the means, sds, and n's for each group
mean_sd_sat= round(compmeans(telehealth_noms_wide_noms_sat_month6_complete$total_month6, telehealth_noms_wide_noms_sat_month6_complete$telehealth.y))
mean_sd_sat
### Get freq cohen's D, because I don't know how to get bayes cohen's D
month_6_sat_d =  cohen.d(telehealth_noms_wide_noms_sat_month6_complete$total_month6, group = telehealth_noms_wide_noms_sat_month6_complete$telehealth.y)

### Put together the results.  Should not need to change this.  See example telehealth_noms_sat_results in TDrive CRI_Research/telehealth_evaluation/data_codebooks/results
results_sat = data.frame(p_change_sat = bayes_p_change_sat_sum[2,1], sd_p_change =  bayes_p_change_sat_sum[2,2], ci_95 = paste0(bayes_p_change_sat_sum[2,3], ",", bayes_p_change_sat_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_sat[1,2], n_post_telehealth = mean_sd_sat[2,2], freq_cohen_d = round(month_6_sat_d$cohen.d[2],3))
write.csv(results_sat, "results.csv", row.names = FALSE)
results_sat

```



######################################
Next section
Dealing with everyday life with mental health illness

a.	I deal effectively with daily problems.
b.	I am able to control my life.
c.	I am able to deal with crisis.
d.	I am getting along with my family.
e.	I do well in social situations.
f.	I do well in school and/or work.
g.	My housing situation is satisfactory.
h.	My symptoms are not bothering me.

```{r}

```


####################################################
Feeling in last 30 days
During the past 30 days, about how often did you feel …
a.	nervous?
b.	hopeless?
c.	restless or fidgety?
d.	so depressed that nothing could cheer you up?
e.	that everything was an effort?
f.	worthless?
```{r}

```


#######################

6.	The following questions relate to your experience with alcohol, cigarettes, and other drugs. Some of the substances we’ll talk about are prescribed by a doctor (like pain medications). But I will only record those if you have taken them for reasons or in doses other than prescribed.

In the past 30 days, how often have you used …

a.	tobacco products (cigarettes, chewing tobacco, cigars, etc.)?
b.	alcoholic beverages (beer, wine, liquor, etc.)?
In the past 30 days, how often have you used …
k.	prescription opioids (fentanyl, oxycodone [OxyContin, Percocet], hydrocodone [Vicodin], methadone, buprenorphine, etc.)?
j.	street opioids (heroin, opium, etc.)?
```{r}

```



##################################
VIOLENCE AND TRAUMA 
10.	Did any of these experiences feel so frightening, horrible, or upsetting that in the past and/or the present you:
a.	Have had nightmares about it or thought about it when you did not want to?
b.	Tried hard not to think about it or went out of your way to avoid situations that remind you of it?
c.	Were constantly on guard, watchful, or easily startled?
d.	Felt numb and detached from others, activities, or your surroundings?
```{r}

```


#################
PERCEPTION OF CARE
1.	In order to provide the best possible mental health and related services, we need to know what you think about the services you received during the past 30 days, the people who provided it, and the results. Please indicate your disagreement/agreement with each of the following statements.
a.	Staff here believe that I can grow, change, and recover.
b.	I felt free to complain.
c.	I was given information about my rights.
d.	Staff encouraged me to take responsibility for how I live my life.
e.	Staff told me what side effects to watch out for.
f.	Staff respected my wishes about who is and who is not to be given information about my treatment.
g.	Staff were sensitive to my cultural background (race, religion, language, etc.).
h.	Staff helped me obtain the information I needed so that I could take charge of managing my illness.
i.	I was encouraged to use consumer-run programs (support groups, drop-in centers, crisis phone line, etc.).
j.	I felt comfortable asking questions about my treatment and medication.
a.	I, not staff, decided my treatment goals.
b.	I like the services I received here.
c.	If I had other choices, I would still get services from this agency.
d.	I would recommend this agency to a friend or family member.
```{r}

```

Vitals



