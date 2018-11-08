---
---
title: "BAHCS-10 Prelim Results"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
```{r}
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(paran)
library(caret)
```
Steps I need to take
Figure out what variables I want
Grab them from each data set
Make sure each variable is in the same order 
Rbind them

Variables: 
FollowUpTimePoint, AvatarClient_ID, all health capital scale items, generate a state ID location indicator



Load data.  Just get the actual data for now don't worry about sub group analyses.  
Add a state ID variable so we can differential them later on
```{r}
setwd("T:/Clinical Model Materials/Clinical Models/Adult Health Home/Evaluation Materials/Data/1. Health Capital Scale/6. October 2018/Matt'sData")
CIL_South_HCS_10052018 = read.csv("CIL_South_HCS_10052018.csv", header = TRUE)
CIL_West_HCS_10052018 = read.csv("CIL_West_HCS_10052018.csv", header = TRUE)
CKY_HCS_10052018 = read.csv("CKY_HCS_10052018.csv", header = TRUE)

head(CIL_South_HCS_10052018)

CIL_South_HCS = CIL_South_HCS_10052018[c("AvatarClient_ID", "FollowUpTimePoint", "GoodHealth",	"ManageHealthProblems",	"KnowHealthConditions",	"PhysicalActivity",	"ManageMentalWellness",	"HopesForFuture",	"NotOverwhelmed",	"Attending_PCP",	"ProvidersSimilarGoals",	"CommunicateHealthcareNeeds",	"NoFutureHospitalization",	"No_ED_Use",	"KnowPrescribedMeds",	"TakePrescribedMeds",	"NoSideEffectsConcerns", "CookNutritiousMeals",	"NutritiousFoodPhysicalBarrier",	"NutritiousFoodFinancialBarrier",	"NutritiousWellBalanced",	"HealthyHomeEnvironment",	"SafeMovingAroundHome",	"LivingSituationSatisfaction",	"HaveHome", "PhysicallySafeNeighborhood",	"LiveCloseLovedOnes",	"TransportationAccess",	"SupportiveFriends",	"ParticipateSocialActivity",	"DifficultHealthWellness", "ProvideFinancialSupportFamily",	"ManageFinances",	"FinancialNegativeAffectHealth",	"EducationSatisfaction",	"EmploymentSatisfaction")]
StateID = rep(0, dim(CIL_South_HCS)[1])
CIL_South_HCS = data.frame(StateID, CIL_South_HCS)


CIL_West_HCS = CIL_West_HCS_10052018[c("AvatarClient_ID", "FollowUpTimePoint", "GoodHealth",	"ManageHealthProblems",	"KnowHealthConditions",	"PhysicalActivity",	"ManageMentalWellness",	"HopesForFuture",	"NotOverwhelmed",	"Attending_PCP",	"ProvidersSimilarGoals",	"CommunicateHealthcareNeeds",	"NoFutureHospitalization",	"No_ED_Use",	"KnowPrescribedMeds",	"TakePrescribedMeds",	"NoSideEffectsConcerns", "CookNutritiousMeals",	"NutritiousFoodPhysicalBarrier",	"NutritiousFoodFinancialBarrier",	"NutritiousWellBalanced",	"HealthyHomeEnvironment",	"SafeMovingAroundHome",	"LivingSituationSatisfaction",	"HaveHome", "PhysicallySafeNeighborhood",	"LiveCloseLovedOnes",	"TransportationAccess",	"SupportiveFriends",	"ParticipateSocialActivity",	"DifficultHealthWellness", "ProvideFinancialSupportFamily",	"ManageFinances",	"FinancialNegativeAffectHealth",	"EducationSatisfaction",	"EmploymentSatisfaction")]
StateID = rep(1, dim(CIL_West_HCS)[1])
CIL_West_HCS = data.frame(StateID, CIL_West_HCS)



head(CKY_HCS_10052018)
CKY_HCS = CKY_HCS_10052018[c("AvatarClient_ID", "FollowUpTimePoint", "GoodHealth",	"ManageHealthProblems",	"KnowHealthConditions",	"PhysicalActivity",	"ManageMentalWellness",	"HopesForFuture",	"NotOverwhelmed",	"Attending_PCP",	"ProvidersSimilarGoals",	"CommunicateHealthcareNeeds",	"NoFutureHospitalization",	"No_ED_Use",	"KnowPrescribedMeds",	"TakePrescribedMeds",	"NoSideEffectsConcerns", "CookNutritiousMeals",	"NutritiousFoodPhysicalBarrier",	"NutritiousFoodFinancialBarrier",	"NutritiousWellBalanced",	"HealthyHomeEnvironment",	"SafeMovingAroundHome",	"LivingSituationSatisfaction",	"HaveHome", "PhysicallySafeNeighborhood",	"LiveCloseLovedOnes",	"TransportationAccess",	"SupportiveFriends",	"ParticipateSocialActivity",	"DifficultHealthWellness", "ProvideFinancialSupportFamily",	"ManageFinances",	"FinancialNegativeAffectHealth",	"EducationSatisfaction",	"EmploymentSatisfaction")]
StateID = rep(2, dim(CKY_HCS)[1])
CKY_HCS = data.frame(StateID, CKY_HCS)

HCS = rbind(CIL_South_HCS, CIL_West_HCS, CKY_HCS)

```
Now we need to clean the data
First see how much missing data there is an delete where 70% or greater is missing
Losing about half the data set here with just getting rid of NAs
We want to keep the 1's, because those are the values with less 70% missing data
Starting N = 2654
Ending N = 1586
```{r}
dim(HCS)
HCSNas = data.frame(is.na(HCS))
head(HCSNas)
HCSNas$NAs = apply(HCSNas, 1, sum)
HCSNas$Nas = HCSNas$NAs / dim(HCSNas)[2]
describe.factor(HCSNas$Nas)
HCS$NAs = ifelse(HCSNas$Nas >=.7, 0,1)

HCS = subset(HCS, NAs == 1)
HCS$NAs = NULL
dim(HCS)

```

####################
I cannot tell if the blanks for time are unique people or for what time point they are at so they must be deleted
######################



Ok get the || out now need to get the first "1" out


Need a unique number of people so only intake and then check duplicate
N = 216

Now we need to get rid of the NA's for ID 
N = 41 with people only actual Avatar IDs
```{r}
# If you want only intake uncomment below
#HCS = subset(HCS, FollowUpTimePoint == "Intake")
names(HCS)[2] = "AvatarID"
dim(HCS)
head(HCS$AvatarID)

HCS$AvatarID = gsub("\\D", "", HCS$AvatarID)
head(HCS$AvatarID)
# Start from the second character
HCS$AvatarID = substring(HCS$AvatarID, 2)
head(HCS$AvatarID)
write.csv(HCS, "HCS.csv", row.names = FALSE)
HCS = read.csv("HCS.csv", header = TRUE, na.strings = "")

HCS = subset(HCS, AvatarID > 0)
sum(is.na((HCS$AvatarID)))
dim(HCS)
```
Now need to get rid of blank TimePoint slots, because I do not know what time point that data is associated with
```{r}
describe.factor(HCS$FollowUpTimePoint)
HCS = subset(HCS,FollowUpTimePoint == "Intake" | FollowUpTimePoint == "3 Month"  | FollowUpTimePoint == "18 Month" | FollowUpTimePoint == "6 Month" | FollowUpTimePoint == "15 Month" | FollowUpTimePoint == "24 Month" | FollowUpTimePoint == "9 Month" | FollowUpTimePoint == "12 Month" | FollowUpTimePoint == "21 Month")
describe.factor(HCS$FollowUpTimePoint)
dim(HCS)
```
Figure out how many people by time point
Now try to merge with Passport data
HCS_Passport
```{r}
HCS_Passport = merge(HCS, InfoMC10_PassPort_10_18, by = "AvatarID",all.x = TRUE)
dim(HCS_Passport)
HCS_Passport = na.omit(HCS_Passport)
dim(HCS_Passport)
describe.factor(HCS_Passport$FollowUpTimePoint)
HCS_Passport
```
Now try linking with the member data sets
Member2017 
Member2018 
SystemIDConverter

Ok try and rbind everything so get rid of variables that do match in 2017

Then merge the combined data set with the system id to get the Avatar IDs
```{r}
head(Member2018)
head(Member2017)
Member2018$DECEASED_DATE = NULL
Member2018$LANGUAGE = NULL
Member2018$ADDR_DATE_FROM = NULL
Member2018$ADDR_DATE_TO = NULL

#Loading data
#Member2017 =  read.csv("MEMBER_20170809_00.csv", header = TRUE)
#Member2018 = read.csv("MEMBER_20180816_00.csv", header = TRUE)
#SystemIDConverter = read.csv("SYSTEM_ID_to_AvatarClient_ID_Mapping.csv", header = TRUE)

Member2017_2018 = rbind(Member2017, Member2018)
head(Member2017_2018)

Member2017_2018Test = merge(Member2017_2018, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)

head(Member2017_2018Test)
```







