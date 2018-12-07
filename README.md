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


#Now we need to clean the data
#First see how much missing data there is an delete where 70% or greater is missing
#Losing about half the data set here with just getting rid of NAs
##We want to keep the 1's, because those are the values with less 70% missing data
#Starting N = 2654
#Ending N = 1586

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



####################
#I cannot tell if the blanks for time are unique people or for what time point they are at so they must be deleted
######################

#Ok get the || out now need to get the first "1" out

#Need a unique number of people so only intake and then check duplicate
#N = 216

#Now we need to get rid of the NA's for ID 
#N = 41 with people only actual Avatar IDs

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
HCS$AvatarID

describe.factor(HCS$FollowUpTimePoint)


#Now need to get rid of blank TimePoint slots, because I do not know what time point that data is associated with
HCS = subset(HCS,FollowUpTimePoint == "Intake" | FollowUpTimePoint == "3 Month"  | FollowUpTimePoint == "18 Month" | FollowUpTimePoint == "6 Month" | FollowUpTimePoint == "15 Month" | FollowUpTimePoint == "24 Month" | FollowUpTimePoint == "9 Month" | FollowUpTimePoint == "12 Month" | FollowUpTimePoint == "21 Month")
describe.factor(HCS$FollowUpTimePoint)
dim(HCS)
describe.factor(HCS$FollowUpTimePoint, decr.order = TRUE)
dim(HCS)

```
Cleaning all the data sets
```{r}
SystemIDConverter= data.frame(SYSTEM_ID = SystemIDConverter$SYSTEM_ID, AvatarClient_ID = SystemIDConverter$AvatarClient_ID)

SystemIDConverter$AvatarClient_ID = gsub("\\D", "", SystemIDConverter$AvatarClient_ID)
head(SystemIDConverter$AvatarClient_ID)
# Start from the second character
SystemIDConverter$AvatarClient_ID = substring(SystemIDConverter$AvatarClient_ID, 2)
head(SystemIDConverter$AvatarClient_ID)
write.csv(SystemIDConverter, "SystemIDConverter.csv", row.names = FALSE)
SystemIDConverter = read.csv("SystemIDConverter.csv", header = TRUE)
head(SystemIDConverter$AvatarClient_ID)

sum(is.na(SystemIDConverter$AvatarClient_ID))

## Need to get rid of NAs for AvatarClient_ID, because cannot match on those
sum(is.na(SystemIDConverter$SYSTEM_ID))
SystemIDConverter = na.omit(SystemIDConverter)
dim(SystemIDConverter)

# Just grab system ID and Avatar ID from this data set
# Need to fix the Avatar ID

## Member data
head(Member2018)
head(Member2017)
Member2018$DECEASED_DATE = NULL
Member2018$LANGUAGE = NULL
Member2018$ADDR_DATE_FROM = NULL
Member2018$ADDR_DATE_TO = NULL

Member2017_2018 = rbind(Member2017, Member2018)
dim(Member2017_2018)
head(Member2017_2018)

Member2017_2018 = merge(Member2017_2018, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
dim(Member2017_2018)
head(Member2017_2018)
# At the end get rid of NA avatar clients
sum(is.na(Member2017_2018$AvatarClient_ID))

## Get rid of uncessary variables
head(Member2017_2018)
Member2017_2018 = Member2017_2018[c("SYSTEM_ID", "SEX", "RACE", "ZIP_CODE", "FOSTER_CARE_INDICATOR", "MEDICARE_DUAL_INDICATOR", "AvatarClient_ID")]

dim(Member2017_2018)
## Get rid of Avatar ID that are empty
Member2017_2018 = subset(Member2017_2018, AvatarClient_ID > 0)
sum(is.na(Member2017_2018$AvatarClient_ID))
sum(is.na(Member2017_2018))

#Pharm Claims
### Now try pharm claims
head(PHARMCLAIMS_2017)
head(PHARMCLAIMS_2018)

PHARMCLAIMS_2017_2018 = rbind(PHARMCLAIMS_2017, PHARMCLAIMS_2018)
dim(PHARMCLAIMS_2017_2018)

# We want all.x for SystemID conversion, because we only want the data from the data set not the system ID
PHARMCLAIMS_2017_2018 = merge(PHARMCLAIMS_2017_2018, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
dim(PHARMCLAIMS_2017_2018)

## Get rid of uncessary variables
head(PHARMCLAIMS_2017_2018)

PHARMCLAIMS_2017_2018 = PHARMCLAIMS_2017_2018[c("SYSTEM_ID", "CLAIMTYPE", "PAIDAMOUNT", "DRUGCODE", "AvatarClient_ID")]
PHARMCLAIMS_2017_2018 = subset(PHARMCLAIMS_2017_2018, AvatarClient_ID > 0)
sum(is.na(PHARMCLAIMS_2017_2018$AvatarClient_ID))
sum(is.na(PHARMCLAIMS_2017_2018))


### Now try InpatClaimsData
names(InpatClaimsData)[1] = "SYSTEM_ID"
InpatClaimsData = merge(InpatClaimsData, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
head(InpatClaimsData)
dim(InpatClaimsData)
head(InpatClaimsData)

InpatClaimsData = InpatClaimsData[c("SYSTEM_ID", "DxCode1", "RowTotal", "MemberTotal", "AvatarClient_ID")]

InpatClaimsData = subset(InpatClaimsData, AvatarClient_ID > 0)
sum(is.na(InpatClaimsData$AvatarClient_ID))
sum(is.na(InpatClaimsData))
dim(InpatClaimsData)

#InpatAvatarData
names(InpatAvatarData)[1] = "SYSTEM_ID"
InpatAvatarData = merge(InpatAvatarData, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
head(InpatAvatarData)
dim(InpatAvatarData)
head(InpatAvatarData)
InpatAvatarData = InpatAvatarData[c("SYSTEM_ID", "DxCode1", "ServiceCode", "RowTotal", "AvatarID", "MissedVisits", "NumServices", "AvatarClient_ID")]

InpatAvatarData = subset(InpatAvatarData, AvatarClient_ID > 0)
sum(is.na(InpatAvatarData$AvatarClient_ID))
sum(is.na(InpatAvatarData))
dim(InpatAvatarData)

#OutpatClaimsData
names(OutpatClaimsData)[1] = "SYSTEM_ID"
OutpatClaimsData = merge(OutpatClaimsData, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
head(OutpatClaimsData)
dim(OutpatClaimsData)
head(OutpatClaimsData)
OutpatClaimsData = OutpatClaimsData[c("SYSTEM_ID", "DxCode1", "RowTotal", "MemberTotal", "AvatarClient_ID")]
OutpatClaimsData = subset(OutpatClaimsData, AvatarClient_ID> 0)
sum(is.na(OutpatClaimsData$AvatarClient_ID))
sum(is.na(OutpatClaimsData))
dim(OutpatClaimsData)

#InfoMC10_PassPort_10_18
head(InfoMC10_PassPort_10_18)
InfoMC10_PassPort_10_18 = InfoMC10_PassPort_10_18[c("AvatarID", "MissedVisits", "NumServices", "ClaimDx", "PharmCost", "PPTotalPaid")]
InfoMC10_PassPort_10_18 = subset(InfoMC10_PassPort_10_18, AvatarID > 0)
sum(is.na(InfoMC10_PassPort_10_18$AvatarID))

#Claims data
head(Claims2017)
Claims2017 = Claims2017[c("CLAIMTYPE", "POS", "PAIDAMOUNT", "SYSTEM_ID")]
head(Claims2018) 
Claims2018 = Claims2018[c("CLAIMTYPE", "POS", "PAIDAMOUNT", "SYSTEM_ID")]
Claims2017_Claims2018 = rbind(Claims2017, Claims2018)
dim(Claims2017_Claims2018)
Claims2017_Claims2018 = merge(Claims2017_Claims2018, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
Claims2017_Claims2018 = subset(Claims2017_Claims2018, AvatarClient_ID > 0)
dim(Claims2017_Claims2018)
sum(is.na(Claims2017_Claims2018$AvatarClient_ID))

```
########################################################
Showing that only info data set can merge with HCS data
########################################################
Here are the different data sets
Member2017 
Member2018
SystemIDConverter
Claims2017 
Claims2018 
PHARMCLAIMS_2017 
PHARMCLAIMS_2018 
Provider2017 - No relevant data 
Provider2018 - No relevant data
InpatClaimsData 
InpatAvatarData 
OutpatClaimsData 
OutpatAvatarData 
Info data set

Ok there are matches, but I need to figure out what the data set are before I start matching on them
Also there are not enough people, to do any fancy stats 
```{r}
names(HCS)[2] = "AvatarClient_ID"
HCS_merge = merge(HCS, Member2017_2018, by = "AvatarClient_ID", all.x = TRUE)
HCS_merge = merge(HCS_merge, InpatClaimsData, by = "AvatarClient_ID", all.x = TRUE)
HCS_merge = merge(HCS_merge, Claims2017_Claims2018, by = "AvatarClient_ID", all.x = TRUE)
#HCS_merge = merge(HCS_merge, PHARMCLAIMS_2017_2018, by = "AvatarClient_ID", all.x = TRUE)
```
