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

describe.factor(HCS$FollowUpTimePoint)
```
Now need to get rid of blank TimePoint slots, because I do not know what time point that data is associated with
```{r}
HCS = subset(HCS,FollowUpTimePoint == "Intake" | FollowUpTimePoint == "3 Month"  | FollowUpTimePoint == "18 Month" | FollowUpTimePoint == "6 Month" | FollowUpTimePoint == "15 Month" | FollowUpTimePoint == "24 Month" | FollowUpTimePoint == "9 Month" | FollowUpTimePoint == "12 Month" | FollowUpTimePoint == "21 Month")
describe.factor(HCS$FollowUpTimePoint)
dim(HCS)
describe.factor(HCS$FollowUpTimePoint, decr.order = TRUE)
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

Lost 3 people in the merging process may not have their IDs in the conversion list

Create a giant spreadsheet with all the data sets then reduce down later so you have them

Steps to reduce the data
1. Try eliminating all the uneccesary data
2. If a variable is time invariant then just repeat
3. Figure out the longitudinal nature and get a time variable
4. Eliminate variables with more than 50% missing data 
5. ??

Info data

### 
Get Cliams later too much data
Claims2017 = read.csv("CLAIMS_20170809_00.csv", head = TRUE) 
Claims2018 = read.csv("CLAIMS_20180816_00.csv", header = TRUE)

PHARMCLAIMS_2017
PHARMCLAIMS_2018 

Provider2017
Provider2018 


######
InpatClaimsData
InpatAvatarData 

OutpatClaimsData
OutpatAvatarData = read.csv("OutpatAvatarData.csv", header = TRUE)


Clean system ID for later aggregation
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
```
Clean member first
Clean and add system ID
```{r}
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

```
Pharm Claims
```{r}
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
```
InpatClaimsData
```{r}
### Now try InpatClaimsData
names(InpatClaimsData)[1] = "SYSTEM_ID"
InpatClaimsData = merge(InpatClaimsData, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
head(InpatClaimsData)
dim(InpatClaimsData)
head(InpatClaimsData)

InpatClaimsData = InpatClaimsData[c("SYSTEM_ID", "DxCode1", "RowTotal", "MemberTotal", "AvatarClient_ID.x")]
names(InpatClaimsData)[5] = "AvatarClient_ID" 

InpatClaimsData = subset(InpatClaimsData, AvatarClient_ID > 0)
sum(is.na(InpatClaimsData$AvatarClient_ID))
sum(is.na(InpatClaimsData))
dim(InpatClaimsData)

```
InpatAvatarData
```{r}
names(InpatAvatarData)[1] = "SYSTEM_ID"
InpatAvatarData = merge(InpatAvatarData, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
head(InpatAvatarData)
dim(InpatAvatarData)
head(InpatAvatarData)
InpatAvatarData = InpatAvatarData[c("SYSTEM_ID", "DxCode1", "RowTotal", "AvatarID", "ServiceCode", "MissedVisits", "NumServices", "AvatarClient_ID")]

InpatAvatarData = subset(InpatAvatarData, AvatarClient_ID > 0)
sum(is.na(InpatAvatarData$AvatarClient_ID))
sum(is.na(InpatAvatarData))
dim(InpatAvatarData)

```
InpatClaimsData
```{r}
names(InpatClaimsData)[1] = "SYSTEM_ID"
InpatClaimsData = merge(InpatClaimsData, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
head(InpatClaimsData)
dim(InpatClaimsData)
InpatClaimsData = InpatClaimsData[c("SYSTEM_ID", "DxCode1", "RowTotal", "MemberTotal", "AvatarClient_ID.x")]
names(InpatClaimsData)[5] = "AvatarClient_ID"
head(InpatClaimsData)
sum(is.na(InpatClaimsData$AvatarClient_ID))
sum(is.na(InpatClaimsData))
```
InpatAvatarData
```{r}
names(InpatAvatarData)[1] = "SYSTEM_ID"
InpatAvatarData = merge(InpatAvatarData, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
head(InpatAvatarData)
dim(InpatAvatarData)

InpatAvatarData = InpatAvatarData[c("SYSTEM_ID", "DxCode1", "RowTotal", "AvatarID", "MissedVisits", "NumServices", "AvatarClient_ID.x")]

names(InpatAvatarData)[7] = "AvatarClient_ID"

```
OutpatClaimsData
```{r}
names(OutpatClaimsData)[1] = "SYSTEM_ID"
OutpatClaimsData = merge(OutpatClaimsData, SystemIDConverter, by = "SYSTEM_ID", all.x = TRUE)
head(OutpatClaimsData)
dim(OutpatClaimsData)
```




```{r}






### InfoMC10_PassPort_10_18
InfoMC10_PassPort_10_18
```
Ok no try mergeing with  the other  Passport data change name of AvatarID

Member2017_2018
PHARMCLAIMS_2017_2018
```{r}
head(Member2017_2018)
names(Member2017_2018)[64] = "AvatarID"

HCS = merge(HCS_Passport, Member2017_2018, by = "AvatarID", all.x = TRUE)
dim(HCS)
```
