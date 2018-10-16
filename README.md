---
---
title: "Passport"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load data.  Just get the actual data for now don't worry about sub group analyses.  
Add a state ID variable so we can differential them later on
```{r, include=FALSE}
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

setwd("T:/PP Claims Data/Matt'sStuff")
InfoMC10_PassPort_10_18 = read.csv("InfoMC10-18.csv", header = TRUE)
CIL_CKYFull = read.csv("CIL_CKYFull10-18.csv", header = TRUE)

names(CIL_CKYFull)[37] = "AvatarID"


# Need to get rid of all data with NA for id
InfoMC10_PassPort_10_18 = na.omit(InfoMC10_PassPort_10_18)
sum(is.na(InfoMC10_PassPort_10_18$AvatarID))

InfoMC10_PassPort_10_18$AvatarID = as.numeric(InfoMC10_PassPort_10_18$AvatarID)
CIL_CKYFull$AvatarID = as.numeric(CIL_CKYFull$AvatarID)


```
Ok now get rid of the extra pieces for 
```{r}
HCS_Passport = merge(CIL_CKYFull, InfoMC10_PassPort_10_18, by = "AvatarID")
dim(HCS_Passport)
head(HCS_Passport)

sum(CIL_CKYFull$AvatarID == 186)
sum(CIL_CKYFull$AvatarID == 504)
HCS_Passport

sum(CIL_CKYFull$AvatarID == 504)
```





