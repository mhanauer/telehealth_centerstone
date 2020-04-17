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
0301 = 3 Month Reassessment
0302 = 6 Month Reassessment
0303 = 9 Month Reassessment
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

```{r}
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
IL =  read.csv("CCBHC_IL_4_14_20.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IN =  read.csv("CCBHC_IN_4_15_20.csv", header = TRUE, na.strings =  c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))

dim(IN)
dim(IL)

## State
IN$state = rep(1, length(IN$ConsumerID))
IL$state = rep(0, length(IL$ConsumerID))

### Adult and youth
IN$adult_youth = ifelse(IN$GrantID == "SM81852", 1, 0)
IL$adult_youth = ifelse(IL$GrantID == "SM81851", 1, 0)

## Now stack them
IN_IL = rbind(IN, IL)
dim(IN_IL)


IN_IL_dat = IN_IL
describe.factor(IN_IL_dat$Assessment)
## Rename to the above

## No one has multiple reassessments
describe.factor(IN_IL_dat$ReassessmentNumber_07)
## Reorder data
IN_IL_dat = IN_IL_dat[order(IN_IL_dat$ConsumerID),]

## Create recoded assessment variable
IN_IL_dat$Assessment_new = ifelse(IN_IL_dat$Assessment == 600, 0, ifelse(IN_IL_dat$Assessment == 301, 1, ifelse(IN_IL_dat$Assessment == 302, 2, ifelse(IN_IL_dat$Assessment == 303, 3, ifelse(IN_IL_dat$Assessment == 304, 4, ifelse(IN_IL_dat$Assessment == 699,5, "Wrong"))))))
describe.factor(IN_IL_dat$Assessment_new, decr.order= FALSE)

### Create full date variable

IN_IL_dat$date = paste0(IN_IL_dat$FFY, "-", IN_IL_dat$Month, "-", "01")
IN_IL_dat$date = ymd(IN_IL_dat$date)
head(IN_IL_dat$date)

IN_IL_dat$telehealth = ifelse(IN_IL_dat$date >= "2020-04-01", 1, 0)
IN_IL_dat[c("date","telehealth")]
### Cannot be greater than 2020-09-30 last day of grant
IN_IL_dat = subset(IN_IL_dat, date < "2020-09-30")
IN_IL_dat[c("date","telehealth")]
dim(IN_IL_dat)


### Create a NOMS data set  
IN_IL_noms = subset(IN_IL_dat, Assessment_new == 0 | Assessment_new == 2 | Assessment_new == 4)
describe.factor(IN_IL_noms$Assessment_new)
#### Create a vitals data set
IN_IL_vital = subset(IN_IL_dat, Assessment_new == 0 | Assessment_new == 2)


####################
library(naniar)
miss_var_summary(IN_IL_dat)

### Just grab a few and see what the descriptives are like
#Alcohol_Use
#PsychologicalEmotionalProblems, EnoughEnergyForEverydayLife, Depressed
#IN_IL_dat_sub_count = IN_IL_noms[c("ConsumerID", "Assessment_new", "NightsHospitalMHC", "NightsDetox", "telehealth", "date")]
head(IN_IL_dat)
miss_var_summary(subset(IN_IL_noms, Assessment_new == 2))
miss_var_summary(subset(IN_IL_noms, Assessment_new == 0))

IN_IL_dat_wide = subset(IN_IL_noms, Assessment_new <=2)
describe.factor(IN_IL_dat_wide$Assessment_new)
describe.factor(IN_IL_dat_wide$telehealth)

### These people have two baselines delete them 'A00276''A00295''A00298'
IN_IL_dat_wide_test = subset(IN_IL_dat_wide, ConsumerID == "'A00276'" | ConsumerID == "'A00295'" | ConsumerID == "'A00298'")
IN_IL_dat_wide[c(276,293, 298), c(1,7)]
IN_IL_dat_wide = IN_IL_dat_wide[-c(276,293, 298),] 

IN_IL_dat_base_noms = subset(IN_IL_dat_wide,Assessment_new == 0)
IN_IL_dat_month6_noms = subset(IN_IL_dat_wide,Assessment_new == 2)
head(IN_IL_dat_base_noms)
dim(IN_IL_dat_month6_noms)
IN_IL_dat_wide_noms = merge(IN_IL_dat_base_noms, IN_IL_dat_month6_noms, by = "ConsumerID", all.y = TRUE)
dim(IN_IL_dat_wide_noms)

```
Data set descriptives
IN_IL_noms = IN and IL for the NOMS data across baseline, 6-month, and 12 month including both adults and youth
IN_IL_vitals = IN and IL vital data across 3-months and 9-months included both adults and youth
IN_IL_dat_wide_noms = IN and IL NOMS data for baseline and 6-month matched pairs
### Finish this later

################################################
Full data set created can start data analysis
################################################




Let's try satisfaction 
c.	how satisfied are you with your ability to perform your daily living activities? PerformDailyActivitiesSatisfaction
d.	how satisfied are you with your health? HealthSatisfaction
how satisfied are you with yourself? SelfSatisfaction
how satisfied are you with your personal relationships? RelationshipSatisfaction
```{r}
### Just look at satisfaction
IN_IL_dat_wide_sat = IN_IL_dat_wide[c("telehealth.y", "PerformDailyActivitiesSatisfaction.x", "PerformDailyActivitiesSatisfaction.y", "HealthSatisfaction.x", "HealthSatisfaction.y", "SelfSatisfaction.x", "SelfSatisfaction.y", "RelationshipSatisfaction.x","RelationshipSatisfaction.y")]

### All items should be 1 to five
apply(IN_IL_dat_wide_sat,2, function(x){describe.factor(x)})
library(psych)

omega_sat_base =  omega(IN_IL_dat_wide_sat[c(2,4,6,8)], poly = TRUE)
omega_sat_6month =  omega(IN_IL_dat_wide_sat[c(3,5,7,9)], poly = TRUE)
omega_sat_6month
vss(IN_IL_dat_wide_sat[c(2,4,6,8)], cor = "poly")
fa(IN_IL_dat_wide_sat[c(2,4,6,8)], cor = "poly", correct = 0)

vss(IN_IL_dat_wide_sat[c(3,5,7,9)], cor = "poly")
fa(IN_IL_dat_wide_sat[c(3,5,7,9)], cor = "poly", correct = 0)
```
Create a total score for each and then difference and then t or wilcox test

IN_IL_dat_wide_sat_month6_complete  = satisfaction scores only with complete data
```{r}
IN_IL_dat_wide_sat$total_base = apply(IN_IL_dat_wide_sat[c(2,4,6,8)], 1, mean, na.rm = TRUE)
IN_IL_dat_wide_sat$total_month6 = apply(IN_IL_dat_wide_sat[c(3,5,7,9)], 1, mean, na.rm = TRUE)
hist(IN_IL_dat_wide_sat$total_base)
hist(IN_IL_dat_wide_sat$total_month6)
## Now create total score diff score
 
IN_IL_dat_wide_sat$total_sat_diff = IN_IL_dat_wide_sat$total_month6 -  IN_IL_dat_wide_sat$total_base
hist(IN_IL_dat_wide_sat$total_sat_diff)
IN_IL_dat_wide_sat$total_sat_diff_scale = scale(IN_IL_dat_wide_sat$total_sat_diff)
hist(IN_IL_dat_wide_sat$total_sat_diff_scale)
IN_IL_dat_wide_sat[c("telehealth.y", "total_sat_diff_scale")]


## No data for difference scores so try just 6months
range(IN_IL_dat_wide_sat$total_month6, na.rm = TRUE)
dim(IN_IL_dat_wide_sat)
head(IN_IL_dat_wide_sat)
IN_IL_dat_wide_sat_month6_complete = na.omit(IN_IL_dat_wide_sat[c("total_month6", "telehealth.y")])
dim(IN_IL_dat_wide_sat_month6_complete)
IN_IL_dat_wide_sat_month6_complete


```
Analysis sat
Percentage change: https://stats.idre.ucla.edu/sas/faq/how-can-i-interpret-log-transformed-variables-in-terms-of-percent-change-in-linear-regression/

You need to expondiate the variable
Doesn't work to well when you get above 20% differences: https://people.duke.edu/~rnau/411log.htm

```{r}
describe.factor(IN_IL_dat_wide_sat_month6_complete$telehealth.y)
n_total = dim(IN_IL_dat_wide_sat_month6_complete)[1]
bayes_p_change_sat = stan_glm(log(total_month6)~ telehealth.y, data = IN_IL_dat_wide_sat_month6_complete, seed = 123)
bayes_p_change_sat_sum = round(bayes_p_change_sat$stan_summary[,c(1,3,4,10)],4)
bayes_p_change_sat_sum = round(exp(bayes_p_change_sat_sum),3)
bayes_p_change_sat_sum= bayes_p_change_sat_sum - 1
bayes_p_change_sat_sum
mean_sd_sat= round(compmeans(IN_IL_dat_wide_sat_month6_complete$total_month6, IN_IL_dat_wide_sat_month6_complete$telehealth.y))
mean_sd_sat
month_6_sat_d =  cohen.d(IN_IL_dat_wide_sat_month6_complete$total_month6, group = IN_IL_dat_wide_sat_month6_complete$telehealth.y)
results = data.frame(p_change_sat = bayes_p_change_sat_sum[2,1], sd_p_change =  bayes_p_change_sat_sum[2,2], ci_95 = paste0(bayes_p_change_sat_sum[2,3], ",", bayes_p_change_sat_sum[2,4]), n_total = n_total, n_pre_telehealth = mean_sd_sat[1,2], n_post_telehealth = mean_sd_sat[2,2], freq_cohen_d = round(month_6_sat_d$cohen.d[2],3))
write.csv(results, "results.csv", row.names = FALSE)
results
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

####################################################
Feeling in last 30 days
During the past 30 days, about how often did you feel …
a.	nervous?
b.	hopeless?
c.	restless or fidgety?
d.	so depressed that nothing could cheer you up?
e.	that everything was an effort?
f.	worthless?

###############################
5.	The following questions ask about how you have been feeling during the last 4 weeks
In the last 4 weeks …


#######################

6.	The following questions relate to your experience with alcohol, cigarettes, and other drugs. Some of the substances we’ll talk about are prescribed by a doctor (like pain medications). But I will only record those if you have taken them for reasons or in doses other than prescribed.

In the past 30 days, how often have you used …

a.	tobacco products (cigarettes, chewing tobacco, cigars, etc.)?
b.	alcoholic beverages (beer, wine, liquor, etc.)?
In the past 30 days, how often have you used …
k.	prescription opioids (fentanyl, oxycodone [OxyContin, Percocet], hydrocodone [Vicodin], methadone, buprenorphine, etc.)?
j.	street opioids (heroin, opium, etc.)?

##################################
VIOLENCE AND TRAUMA 
10.	Did any of these experiences feel so frightening, horrible, or upsetting that in the past and/or the present you:
a.	Have had nightmares about it or thought about it when you did not want to?
b.	Tried hard not to think about it or went out of your way to avoid situations that remind you of it?
c.	Were constantly on guard, watchful, or easily startled?
d.	Felt numb and detached from others, activities, or your surroundings?

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




