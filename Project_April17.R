install.packages("gapminder")
library(gapminder)
install.packages("dummies");
library(dummies)
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
install.packages("stringr")
library(stringr)
library(lattice)
install.packages("Hmisc")
library(Hmisc)


setwd("E:/Data Science Project/R projects/Predict Cost of worker's compensation New")
rawData <- read.csv("data.csv",na.strings = c(NA,""),header = T,sep=",",stringsAsFactors = T) #[,-c(1,4,6,10,13,15,18,20,24,31,33,35,37,40,42,45)]
str(rawData)
ncol(rawData)
rawDataCopy <- rawData
# Converting data into correct format
# Because there was comma in figure sor it is converted into Factor, first we need to remove , and then convert it into Number
n1 <- rawData$Dependent 
rawData$Dependent <-gsub(",","",n1)  # removing ,
rawData$Dependent <- as.integer(rawData$Dependent) # Converting into Number

# Same treatment will be given to Average.Weekly.Wage 
n2 <- rawData$Average.Weekly.Wage 
rawData$Average.Weekly.Wage <-gsub(",","",n2)
rawData$Average.Weekly.Wage <- as.integer(rawData$Average.Weekly.Wage)

# Renaming the column
colnames(rawData)[colnames(rawData)=='Average.Weekly.Wage'] <- 'Weekly.Wage'


# Conversion into factor
rawData$Body.Part.Code <- factor(rawData$Body.Part.Code)


# $ Cause.Code                     : int  
# Conversion into factor
rawData$Cause.Code <- factor(rawData$Cause.Code)
# $ Claimant.Age                   : int  
# $ Claimant.Atty.Firm.Name        : Factor w/ 453 levels 

# $ Claimant.Gender.Code           : Factor w/ 3 levels "F","M","U"

# $ Claimant.Hire.Date             : Factor w/ 4204 levels 
# Conversion into date
rawData$Claimant.Hire.Date <- as.Date(rawData$Claimant.Hire.Date,'%m/%d/%y')
# Renaming the column
colnames(rawData)[colnames(rawData)=='Claimant.Hire.Date'] <- 'Hire.Date'
# $ Claimant.Marital.Status.Code   : Factor w/ 3 levels "M","S","U":

# $ Claimant.State.Code            : Factor w/ 49 levels "AL","AZ","BC",..
# $ Department.Code                : int 
# Conversion into factor
rawData$Department.Code <- factor(rawData$Department.Code)
# Renaming the column
colnames(rawData)[colnames(rawData)=='Department.Code'] <- 'Dept.Code'
# $ Detail.Cause.Code              : int  
# Conversion into factor
rawData$Detail.Cause.Code  <- factor(rawData$Detail.Cause.Code)
# Renaming the column
colnames(rawData)[colnames(rawData)=='Detail.Cause.Code'] <- 'Cause.Code'

# $ Domestic.vs..Foreign..Code     : Factor w/ 2 levels "D","F"
# $ Dt.Reported.to.Carrier.TPA     : Factor w/ 3633 levels "1/10/2000","1/10/2001"
# Conversion into date
rawData$Dt.Reported.to.Carrier.TPA <- as.Date(rawData$Dt.Reported.to.Carrier.TPA,'%m/%d/%y')
# Renaming the column
colnames(rawData)[colnames(rawData)=='Dt.Reported.to.Carrier.TPA'] <- 'Reported.Carrier.TPA'
# $ Dt.Reported.to.Employer        : Factor w/ 4355 levels 
# Conversion into date
rawData$Dt.Reported.to.Employer <- as.Date(rawData$Dt.Reported.to.Employer,'%m/%d/%y')
# Renaming the column
colnames(rawData)[colnames(rawData)=='Dt.Reported.to.Employer'] <- 'Reported.Emp.Date'

# $ Employment.Status.Code         : Factor w/ 12 levels 
# Renaming the column
colnames(rawData)[colnames(rawData)=='Emp.Status.Code'] <- 'Emp.Status.Code'

# $ Date.of.Injury.Illness         : Factor w/ 4430 levels 
# Conversion into date
rawData$Date.of.Injury.Illness <- as.Date(rawData$Date.of.Injury.Illness,'%m/%d/%y')
# Renaming the column
colnames(rawData)[colnames(rawData)=='Date.of.Injury.Illness'] <- 'Date.of.Injury.Illness'

# $ Handling.Office.Name           : Factor w/ 28 levels 
# $ How.Injury.Occurred            : Factor w/ 15344 levels 
# $ Injury.Illness.Postal          : Factor w/ 1841 levels 

# $ Injury.Illness.State.Code      : Factor w/ 53 levels 
colnames(rawData)[colnames(rawData)=='Injury.Illness.State.Code'] <- 'Incedient.State.Code'
# $ Jurisdiction.Code              : Factor w/ 47 levels 
colnames(rawData)[colnames(rawData)=='Jurisdiction.Code'] <- 'Jurisdiction.Code'

# $ Lost.Time.or.Medical.Only..Code: Factor w/ 2 levels "LT","MO":
colnames(rawData)[colnames(rawData)=='Lost.Time.or.Medical.Only..Code'] <- 'Medical.Only.Code'

# $ Nature.of.Injury.Illness.Code  : int  
# Conversion into factor
rawData$Nature.of.Injury.Illness.Code  <- factor(rawData$Nature.of.Injury.Illness.Code)
colnames(rawData)[colnames(rawData)=='Nature.of.Injury.Illness.Code'] <- 'Nature.Illness.Code'
# $ Number.of.Dependents           : int  
# Conversion into factor
# $ OSHA.Injury.Type.Code          : int  
# Conversion into factor
rawData$OSHA.Injury.Type.Code  <- factor(rawData$OSHA.Injury.Type.Code)
# $ Severity.Index.Code            : Factor w/ 10 levels 
# $ Severity.Index.Code.Code       : int 
# Conversion into factor
rawData$Severity.Index.Code.Code  <- factor(rawData$Severity.Index.Code.Code)
colnames(rawData)[colnames(rawData)=='Severity.Index.Code.Code'] <- 'Severity.Index.Code'
# $ Time.of.Injury.Illness         : int  
# Conversion into time 
# rawData$Time.of.Injury.Illness  <- factor(rawData$Time.of.Injury.Illness) CHANGE INTO TIME

# $ Type.of.Loss.Code              : int  3 3 3 3 3 1 3 3 3 3 ...
# Conversion into factor
rawData$Type.of.Loss.Code  <- factor(rawData$Type.of.Loss.Code)
# $ Policy.Year                    : int
# Conversion into factor
# $ Policy.Year                    : int
rawData$Policy.Year <- factor(rawData$Policy.Year)
# $ Reforms_dummy                  : Factor w/ 3 levels 
# 
# Dropping Dates and Time.of.Injury.Illness  column
rawData_new <- rawData[,! colnames(rawData) %in% c("Hire.Date","Reported.Emp.Date","Date.of.Injury.Illness","Time.of.Injury.Illness")]

# Missing Values

sapply(rawData_new,function(x)(sum(is.na(x))/nrow(rawData_new))*100)



# Dependent 0.00000000
# Weekly.Wage 61.88745375
# Body.Part.Code 0.00000000
# Cause.Code 0.00000000
# Claimant.Age 14.03907315
# Claimant.Atty.Firm.Name 80.54780295
# Claimant.Gender.Code 0.00000000
# Claimant.Marital.Status.Code 82.38463036
# Claimant.State.Code 0.00000000
# Dept.Code 52.72278834
# Cause.Code.1 0.00000000
# Domestic.vs..Foreign..Code 0.00000000
# Reported.Carrier.TPA 0.00000000
# Employment.Status.Code 0.00000000
# Handling.Office.Name 0.00000000
# How.Injury.Occurred 0.00000000
# Injury.Illness.Postal 23.91120919
# Incedient.State.Code 0.07788667
# Jurisdiction.Code 0.00000000
# Medical.Only.Code 0.07139612
# Nature.Illness.Code 0.00000000
# Number.of.Dependents 96.11215681
# OSHA.Injury.Type.Code 0.01298111
# Severity.Index.Code 2.23275135
# Type.of.Loss.Code 0.34399948
# Policy.Year 0.00000000
# Reforms_dummy 40.97488155
# Days_Reporting 0.00000000

# Dropping the columns which have missing values greater than 40%
rawData_noMiss <- rawData_new[,!colnames(rawData_new) %in% c("Weekly.Wage","Reported.Carrier.TPA","Claimant.Atty.Firm.Name","Claimant.Marital.Status.Code","Dept.Code","Number.of.Dependents")]
ncol(rawData_noMiss)
str(rawData_noMiss)

# Deleting the rows which have < 3% NA values
rawData_noMiss <-  rawData_noMiss[!is.na(rawData_noMiss$Medical.Only.Code),]
rawData_noMiss <-  rawData_noMiss[!is.na(rawData_noMiss$Severity.Index.Code),]
rawData_noMiss <-  rawData_noMiss[!is.na(rawData_noMiss$OSHA.Injury.Type.Code),]
rawData_noMiss <-  rawData_noMiss[!is.na(rawData_noMiss$Type.of.Loss.Code),]

# backup ds
rawData_noMissBck <- rawData_noMiss
rawData_noMiss <- rawData_noMissBck
# Imputing  missing value in the variable which have NA values > 3% and ~ <= 40%
# Claimant.Age
rawData_noMiss$Claimant.Age <- impute(rawData_noMiss$Claimant.Age,median)

# Injury.Illness.Postal
rawData_noMiss$Injury.Illness.Postal <- impute(rawData_noMiss$Injury.Illness.Postal,mode)

# Incedient.State.Code
rawData_noMiss$Incedient.State.Code <- impute(rawData_noMiss$Incedient.State.Code,mode)

# Reforms_dummy
rawData_noMiss$Reforms_dummy <- impute(rawData_noMiss$Reforms_dummy,mode)
str(rawData_noMiss)

# Checking NA values in final df
sapply(rawData_noMiss,function(x)(sum(is.na(x))/nrow(rawData_noMiss))*100)

# Checking value frequency in Categorical variables

df <- rawData_noMiss[,-c(1,4,22)]
apply(df,2,table)
str(rawData_noMiss)

# Frequency graph for categorical variables and reduction of levels

#######################  rawData_noMiss$Body.Part.Code  ##########################
 w1 <- table(rawData_noMiss$Body.Part.Code)
 f1 <- as.data.frame(w1)

 vector <- c()
 vector <- ifelse(f1$Freq<=100,as.vector(f1$Var1),NA)  # why inserting index ?
 df <- as.data.frame(vector)
 df <- df[!is.na(vector),]
 vector <- na.omit(vector) # vector of values which have <= 100 frequency
 
table(rawData_noMiss$Body.Part.Code)
rawData_noMiss[rawData_noMiss$Body.Part.Code %in% vector,'Body.Part.Code'] <- 15 # assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss$Body.Part.Code <- as.character(rawData_noMiss$Body.Part.Code) # to remove the levels which have 0 frequency
rawData_noMiss$Body.Part.Code <- factor(rawData_noMiss$Body.Part.Code)  # to remove the levels which have 0 frequency

barchart(rawData_noMiss$Body.Part.Code,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$Body.Part.Code  ##########################

#######################  rawData_noMiss$Cause.Code  ##########################
rawData_noMissBck.caousecode <- rawData_noMiss

w2 <- table(rawData_noMiss$Cause.Code)
f2 <- as.data.frame(w2)
barchart(rawData_noMiss$Cause.Code,rawData_noMiss,horizontal = F)

v2 <- c()
v2 <- ifelse(f2$Freq<=544,as.vector(f2$Var1),NA)  # why inserting index ?
# df <- as.data.frame(v2)
# df <- df[!is.na(v2),]
v2 <- na.omit(v2) # vector of values which have <= 100 frequency

table(rawData_noMiss$Cause.Code)
rawData_noMiss[rawData_noMiss$Cause.Code %in% v2,'Cause.Code'] <- 1100 # assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss$Cause.Code <- as.character(rawData_noMiss$Cause.Code) # to remove the levels which have 0 frequency
rawData_noMiss$Cause.Code <- factor(rawData_noMiss$Cause.Code)  # to remove the levels which have 0 frequency

barchart(rawData_noMiss$Cause.Code,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$Cause.Code  ##########################

#######################  rawData_noMiss$Claimant.State.Code  ##########################
rawData_noMissBck.Claimant.State.Code <- rawData_noMiss

w3 <- table(rawData_noMiss$Claimant.State.Code)
f3 <- as.data.frame(w3)
barchart(rawData_noMiss$Claimant.State.Code,rawData_noMiss,horizontal = F)

v3 <- c()
v3 <- ifelse(f3$Freq<=230,as.vector(f3$Var1),NA)  # why inserting index ?
# df <- as.data.frame(v2)
# df <- df[!is.na(v2),]
v3 <- na.omit(v3) # vector of values which have <= 100 frequency

table(rawData_noMiss$Claimant.State.Code)
rawData_noMiss[rawData_noMiss$Claimant.State.Code %in% v3,'Claimant.State.Code'] <- 'FL' # assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss$Claimant.State.Code <- as.character(rawData_noMiss$Claimant.State.Code) # to remove the levels which have 0 frequency
rawData_noMiss$Claimant.State.Code <- factor(rawData_noMiss$Claimant.State.Code)  # to remove the levels which have 0 frequency

barchart(rawData_noMiss$Claimant.State.Code,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$Claimant.State.Code  ##########################

#######################  rawData_noMiss$Domestic.vs..Foreign..Code  ##########################
rawData_noMissBck.Employment.Status.Code<- rawData_noMiss

w4 <- table(rawData_noMiss$Employment.Status.Code)
f4 <- as.data.frame(w4)
barchart(rawData_noMiss$Employment.Status.Code,rawData_noMiss,horizontal = F)

v4 <- c()
v4 <- ifelse(f4$Freq<=50,as.vector(f4$Var1),NA)  # why inserting index ?
# df <- as.data.frame(v2)
# df <- df[!is.na(v2),]
v4 <- na.omit(v4) # vector of values which have <= 100 frequency

table(rawData_noMiss$Employment.Status.Code)
rawData_noMiss[rawData_noMiss$Employment.Status.Code %in% v4,'Employment.Status.Code'] <- 'C' # assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss$Employment.Status.Code <- as.character(rawData_noMiss$Employment.Status.Code) # to remove the levels which have 0 frequency
rawData_noMiss$Employment.Status.Code <- factor(rawData_noMiss$Employment.Status.Code)  # to remove the levels which have 0 frequency

barchart(rawData_noMiss$Employment.Status.Code,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$Domestic.vs..Foreign..Code  ##########################

#######################  rawData_noMiss$Cause.Code.1  ##########################
rawData_noMissBck.Cause.Code.1<- rawData_noMiss

w5 <- table(rawData_noMiss$Cause.Code.1)
f5 <- as.data.frame(w5)
barchart(rawData_noMiss$Cause.Code.1,rawData_noMiss,horizontal = F)

v5 <- c()
v5 <- ifelse(f5$Freq<=150,as.vector(f5$Var1),NA)  # why inserting index ?
# df <- as.data.frame(v2)
# df <- df[!is.na(v2),]
v5 <- na.omit(v5) # vector of values which have <= 100 frequency

table(rawData_noMiss$Cause.Code.1)
rawData_noMiss[rawData_noMiss$Cause.Code.1 %in% v5,'Cause.Code.1'] <- 81 # assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss$Cause.Code.1 <- as.character(rawData_noMiss$Cause.Code.1) # to remove the levels which have 0 frequency
rawData_noMiss$Cause.Code.1 <- factor(rawData_noMiss$Cause.Code.1)  # to remove the levels which have 0 frequency

barchart(rawData_noMiss$Cause.Code.1,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$Domestic.vs..Foreign..Code  ##########################

#######################  rawData_noMiss$Handling.Office.Name  ##########################
rawData_noMissBck.Handling.Office.Name<- rawData_noMiss

w6 <- table(rawData_noMiss$Handling.Office.Name)
f6 <- as.data.frame(w6)
barchart(rawData_noMiss$Handling.Office.Name,rawData_noMiss,horizontal = F)

v6 <- c()
v6 <- ifelse(f6$Freq<=245,as.vector(f6$Var1),NA)  # why inserting index ?
# df <- as.data.frame(v2)
# df <- df[!is.na(v2),]
v6 <- na.omit(v6) # vector of values which have <= 100 frequency

table(rawData_noMiss$Handling.Office.Name)
rawData_noMiss[rawData_noMiss$Handling.Office.Name %in% v6,'Handling.Office.Name'] <- 'PHILADELPH' # assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss$Handling.Office.Name <- as.character(rawData_noMiss$Handling.Office.Name) # to remove the levels which have 0 frequency
rawData_noMiss$Handling.Office.Name <- factor(rawData_noMiss$Handling.Office.Name)  # to remove the levels which have 0 frequency

barchart(rawData_noMiss$Handling.Office.Name,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$Handling.Office.Name  ##########################

#######################  rawData_noMiss$How.Injury.Occurred  ##########################
rawData_noMissBck.How.Injury.Occurred<- rawData_noMiss

w7 <- table(rawData_noMiss$How.Injury.Occurred)
f7 <- as.data.frame(w7)
barchart(rawData_noMiss$How.Injury.Occurred,rawData_noMiss,horizontal = F)

v7 <- c()
v7 <- ifelse(f7$Freq<=245,as.vector(f7$Var1),NA)  # why inserting index ?
# df <- as.data.frame(v2)
# df <- df[!is.na(v2),]
v7 <- na.omit(v7) # vector of values which have <= 100 frequency

table(rawData_noMiss$How.Injury.Occurred)
rawData_noMiss[rawData_noMiss$How.Injury.Occurred %in% v7,'How.Injury.Occurred'] <- 'PHILADELPH' # assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss$How.Injury.Occurred <- as.character(rawData_noMiss$How.Injury.Occurred) # to remove the levels which have 0 frequency
rawData_noMiss$How.Injury.Occurred <- factor(rawData_noMiss$How.Injury.Occurred)  # to remove the levels which have 0 frequency

barchart(rawData_noMiss$How.Injury.Occurred,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$How.Injury.Occurred  ##########################

#######################  rawData_noMiss$Injury.Illness.Postal  ##########################
rawData_noMissBck.Injury.Illness.Postal<- rawData_noMiss
rawData_noMiss <- rawData_noMissBck.How.Injury.Occurred

w8 <- table(rawData_noMiss$Injury.Illness.Postal)
f8 <- as.data.frame(w8)
barchart(rawData_noMiss$Injury.Illness.Postal,rawData_noMiss,horizontal = F)

v8 <- c()
v8 <- ifelse(f8$Freq<=5,as.vector(f8$Var1),NA)  # why inserting index ?
# df <- as.data.frame(v2)
# df <- df[!is.na(v2),]
v8 <- na.omit(v8) # vector of values which have <= 100 frequency

table(rawData_noMiss$Injury.Illness.Postal)

# to remove the levels which have 0 frequency. Dropping LEVELS
rawData_noMiss$Injury.Illness.Postal <- as.character(rawData_noMiss$Injury.Illness.Postal) 

# assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss[rawData_noMiss$Injury.Illness.Postal %in% as.vector(v8),'Injury.Illness.Postal'] <- "other"
rawData_noMiss$Injury.Illness.Postal <- factor(rawData_noMiss$Injury.Illness.Postal)  # to remove the levels which have 0 frequency


barchart(rawData_noMiss$Injury.Illness.Postal,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$Injury.Illness.Postal  ##########################


#######################  rawData_noMiss$Incedient.State.Code  ##########################
rawData_noMissBck.Incedient.State.Code<- rawData_noMiss


w9 <- table(rawData_noMiss$Incedient.State.Code)
f9 <- as.data.frame(w9)
barchart(rawData_noMiss$Incedient.State.Code,rawData_noMiss,horizontal = F)

v9 <- c()
v9 <- ifelse(f9$Freq<=240,as.vector(f9$Var1),NA)  # why inserting index ?
# df <- as.data.frame(v2)
# df <- df[!is.na(v2),]
v9 <- na.omit(v9) # vector of values which have <= 100 frequency

table(rawData_noMiss$Incedient.State.Code)

# to remove the levels which have 0 frequency. Dropping LEVELS
rawData_noMiss$Incedient.State.Code <- as.character(rawData_noMiss$Incedient.State.Code) 

# assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss[rawData_noMiss$Incedient.State.Code %in% as.vector(v9),'Incedient.State.Code'] <- "other"
rawData_noMiss$Incedient.State.Code <- factor(rawData_noMiss$Incedient.State.Code)  # to remove the levels which have 0 frequency


barchart(rawData_noMiss$Incedient.State.Code,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$Injury.Illness.Postal  ##########################

#######################  rawData_noMiss$Jurisdiction.Code  ##########################
rawData_noMissBck.Jurisdiction.Code<-   rawData_noMiss


w10 <- table(rawData_noMiss$Jurisdiction.Code)
f10 <- as.data.frame(w10)
barchart(rawData_noMiss$Jurisdiction.Code,rawData_noMiss,horizontal = F)


v10 <- c()
v10 <- ifelse(f10$Freq<=50,as.vector(f10$Var1),NA)  # why inserting index ?
# df <- as.data.frame(v2)
# df <- df[!is.na(v2),]
v10 <- na.omit(v10) # vector of values which have <= 100 frequency

table(rawData_noMiss$Jurisdiction.Code)

# to remove the levels which have 0 frequency. Dropping LEVELS
rawData_noMiss$Jurisdiction.Code <- as.character(rawData_noMiss$Jurisdiction.Code) 

# assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss[rawData_noMiss$Jurisdiction.Code %in% as.vector(v10),'Jurisdiction.Code'] <- "other"
rawData_noMiss$Jurisdiction.Code <- factor(rawData_noMiss$Jurisdiction.Code)  # to remove the levels which have 0 frequency


barchart(rawData_noMiss$Jurisdiction.Code,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$Injury.Illness.Postal  ##########################

#######################  rawData_noMiss$Nature.Illness.Code  ##########################
rawData_noMissBck.Nature.Illness.Code<- rawData_noMiss


w11 <- table(rawData_noMiss$Nature.Illness.Code)
f11 <- as.data.frame(w11)
barchart(rawData_noMiss$Nature.Illness.Code,rawData_noMiss,horizontal = F)

v11 <- c()
v11 <- ifelse(f11$Freq<=50,as.vector(f11$Var1),NA)  # why inserting index ?
# df <- as.data.frame(v2)
# df <- df[!is.na(v2),]
v11 <- na.omit(v11) # vector of values which have <= 100 frequency

table(rawData_noMiss$Nature.Illness.Code)

# to remove the levels which have 0 frequency. Dropping LEVELS
rawData_noMiss$Nature.Illness.Code <- as.character(rawData_noMiss$Nature.Illness.Code) 

# assigning a common value to reduce the levels which will help in reducing number of dummy variable
rawData_noMiss[rawData_noMiss$Nature.Illness.Code %in% as.vector(v11),'Nature.Illness.Code'] <- "other"
rawData_noMiss$Nature.Illness.Code <- factor(rawData_noMiss$Nature.Illness.Code)  # to remove the levels which have 0 frequency


barchart(rawData_noMiss$Nature.Illness.Code,rawData_noMiss,horizontal = F)

#######################  END rawData_noMiss$Injury.Illness.Postal  ##########################

nrow(rawData_noMiss)
str(rawData_noMiss)
View(rawData_noMiss)

# Check distribution of continuous variable
# Transformation of rawData_noMiss$Dependent
par(mfrow=c(3,1))
plot(density(rawData_noMiss$Dependent));plot(density(scale(rawData_noMiss$Dependent)));plot(density(log(rawData_noMiss$Dependent)));

# With above plots we can conclude that with logrithmic transformation variable  rawData_noMiss$Dependent is normally distributed

# We can Check with double log transformation
par(mfrow=c(3,1))
plot(density(log(log(rawData_noMiss$Dependent))));plot(density(log2(rawData_noMiss$Dependent)));plot(density(log10(rawData_noMiss$Dependent)));
#  double log is not giving correct result and log2 and log10 are giving same result as log

# Transformation of rawData_noMiss$Dependent
par(mfrow=c(3,1)) 
plot(density(rawData_noMiss$Claimant.Age));plot(density(scale(rawData_noMiss$Claimant.Age)));plot(density(log(rawData_noMiss$Claimant.Age)));

# Transformation of rawData_noMiss$Dependent
par(mfrow=c(3,1))  # right skewed
plot(density(rawData_noMiss$Days_Reporting)); 
plot(density(scale(rawData_noMiss$Days_Reporting)));
plot(density(log(rawData_noMiss$Days_Reporting)));

# By keeping above things in mind we need to use a REGRESSION model where Linear Regression assumption no need to apply
# assumption free regression regression technique

# Check OUTLIERS
par(mfrow=c(1,1))
boxplot(rawData_noMiss$Dependent);boxplot(as.numeric(rawData_noMiss$Claimant.Age));boxplot(rawData_noMiss$Days_Reporting)

summary(rawData_noMiss$Dependent)
summary(scale(rawData_noMiss$Dependent))
x <- boxplot(scale(rawData_noMiss$Dependent))
x$out
length(x$out)
length(x$out)/nrow(rawData_noMiss)

dep1 <- rawData_noMiss$Dependent
dep1 <- scale(dep1, center = TRUE, scale = TRUE)
summary(dep1)

dep2 <- (rawData_noMiss$Dependent-mean(rawData_noMiss$Dependent))/sd(rawData_noMiss$Dependent)
summary(dep2)
boxplot(dep2)






# If outliers are more than 1% we can keep it as it is or we can remove those rows

#########################   Dependent   ###################################
summary.Dependent <- summary(rawData_noMiss$Dependent)
summary.Dependent[5] # 75%
summary.Dependent[2] # 25%
IQR.Dependent <- IQR(rawData_noMiss$Dependent)*1.5

max.Dependent <- summary.Dependent[5]+IQR.Dependent
min.Dependent <- summary.Dependent[2]-IQR.Dependent
rawData_noMiss$Dependent <- ifelse(rawData_noMiss$Dependent < min.Dependent , min.Dependent, rawData_noMiss$Dependent)
rawData_noMiss$Dependent <- ifelse(rawData_noMiss$Dependent > max.Dependent , max.Dependent, rawData_noMiss$Dependent)
boxplot(rawData_noMiss$Dependent)


# which(rawData_noMiss$Dependent > max.Dependent | rawData_noMiss$Dependent < min.Dependent )
# quantT1 <- quantile(rawData_noMiss$Dependent,probs = c(0.01,0.05,0.95,0.99))
# quantT1
# rawData_noMiss$Dependent = ifelse(rawData_noMiss$Dependent < quantT1[2] , quantT1[2], rawData_noMiss$Dependent)
# rawData_noMiss$Dependent = ifelse(rawData_noMiss$Dependent > quantT1[4] , quantT1[4], rawData_noMiss$Dependent)


########################################

# CLAIMANT AGE
summary.age <- summary(rawData_noMiss$Claimant.Age)
summary.age[5] # 75%
summary.age[2] # 25%

IQR.age <- IQR(rawData_noMiss$Claimant.Age)*1.5

max.age <- summary.age[5]+IQR.age
min.age <- summary.age[2]-IQR.age
rawData_noMiss$Claimant.Age <- ifelse(rawData_noMiss$Claimant.Age < min.age , as.numeric(min.age), rawData_noMiss$Claimant.Age)
rawData_noMiss$Claimant.Age <- ifelse(rawData_noMiss$Claimant.Age > max.age ,as.numeric( max.age), rawData_noMiss$Claimant.Age)
boxplot(rawData_noMiss$Claimant.Age)


# x2 <- boxplot(as.numeric(rawData_noMiss$Claimant.Age))
# x2$out
# length(x2$out)
# length(x2$out)/nrow(rawData_noMiss)


#######################################

# Reporting Days

bckAfter_OL <- rawData_noMiss

rawData_noMiss <- rawData_noMiss[,colnames(rawData_noMiss) != c("Days_Reporting")]


###############################  TEST OF ASSOCIATION ###########################################

# Correlation for continious vriable
str(rawData_noMiss)
corAge <- cor(rawData_noMiss$Dependent,rawData_noMiss$Claimant.Age)
corAge # 0.1214776, Week positively correlated

# Anova

colnames(rawData_noMiss)

aov.Body.Part.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Body.Part.Code,data=rawData_noMiss)
aov.Body.Part.Code
summary(aov.Body.Part.Code)[[1]][["Pr(>F)"]][1]     # 1.355309e-198

# Df    Sum Sq  Mean Sq F value Pr(>F)    
# rawData_noMiss$Body.Part.Code    26 2.285e+09 87896490    40.6 <2e-16 ***
#   Residuals                     14970 3.241e+10  2164765                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

######### Cause.Code  #########
aov.Cause.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Cause.Code,data=rawData_noMiss)
aov.Cause.Code
summary(aov.Cause.Code)[[1]][["Pr(>F)"]][1]     # 1.436522e-218

# Df    Sum Sq   Mean Sq F value Pr(>F)    
# rawData_noMiss$Cause.Code     6 2.296e+09 382745712   177.1 <2e-16 ***
#   Residuals                 14990 3.240e+10   2161131                   
# ---

######### Claimant.Gender.Code  #########
aov.Claimant.Gender.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Claimant.Gender.Code,data=rawData_noMiss)
aov.Claimant.Gender.Code
summary(aov.Claimant.Gender.Code)[[1]][["Pr(>F)"]][1]     # 0.0002248326

# Df    Sum Sq  Mean Sq F value   Pr(>F)    
# rawData_noMiss$Claimant.Gender.Code     2 3.885e+07 19424673   8.405 0.000225 ***
#   Residuals

######### Claimant.State.Code  #########
aov.Claimant.State.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Claimant.State.Code,data=rawData_noMiss)
aov.Claimant.State.Code
summary(aov.Claimant.State.Code)[[1]][["Pr(>F)"]][1]     # 1.868158e-17

# Df    Sum Sq  Mean Sq F value Pr(>F)    
# rawData_noMiss$Claimant.State.Code     8 2.234e+08 27928675   12.14 <2e-16 ***
#   Residuals                          14988 3.447e+10  2299733


######### Domestic.vs..Foreign..Code  #########
aov.Domestic.vs..Foreign..Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Domestic.vs..Foreign..Code,data=rawData_noMiss)
aov.Domestic.vs..Foreign..Code
summary(aov.Domestic.vs..Foreign..Code)[[1]][["Pr(>F)"]][1]     # 0.0004404712

# Df    Sum Sq  Mean Sq F value  Pr(>F)    
# rawData_noMiss$Domestic.vs..Foreign..Code     1 2.857e+07 28566701   12.36 0.00044 ***
#   Residuals


######### Employment.Status.Code  #########
aov.Employment.Status.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Employment.Status.Code,data=rawData_noMiss)
aov.Employment.Status.Code
summary(aov.Employment.Status.Code)[[1]][["Pr(>F)"]][1]     # 2.024127e-160

# Df    Sum Sq   Mean Sq F value Pr(>F)    
# rawData_noMiss$Employment.Status.Code     4 1.687e+09 421674943   191.5 <2e-16 ***
#   Residuals                             14992 3.301e+10   2201516                   
# ---

######### Handling.Office.Name  #########
aov.Handling.Office.Name <- aov(rawData_noMiss$Dependent~rawData_noMiss$Handling.Office.Name,data=rawData_noMiss)
aov.Handling.Office.Name
summary(aov.Handling.Office.Name)[[1]][["Pr(>F)"]][1]     # 8.149421e-143

# Df    Sum Sq   Mean Sq F value Pr(>F)    
# rawData_noMiss$Handling.Office.Name    10 1.571e+09 157133426    71.1 <2e-16 ***
#   Residuals

######### How.Injury.Occurred  #########
# aov.How.Injury.Occurred <- aov(rawData_noMiss$Dependent~rawData_noMiss$How.Injury.Occurred,data=rawData_noMiss)
# aov.How.Injury.Occurred
# summary(aov.How.Injury.Occurred)[[1]][["Pr(>F)"]][1]    


######### Injury.Illness.Postal  #########
aov.Injury.Illness.Postal <- aov(rawData_noMiss$Dependent~rawData_noMiss$Injury.Illness.Postal,data=rawData_noMiss)
aov.Injury.Illness.Postal
summary(aov.Injury.Illness.Postal)[[1]][["Pr(>F)"]][1]     # 4.767664e-17

# Df    Sum Sq Mean Sq F value Pr(>F)    
# rawData_noMiss$Injury.Illness.Postal   308 1.289e+09 4183722    1.84 <2e-16 ***
#   Residuals                            14688 3.340e+10 2274186                   
# ---

######### Incedient.State.Code  #########
aov.Incedient.State.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Incedient.State.Code,data=rawData_noMiss)
aov.Incedient.State.Code
summary(aov.Incedient.State.Code)[[1]][["Pr(>F)"]][1]     # 5.456802e-45

# Df    Sum Sq  Mean Sq F value Pr(>F)    
# rawData_noMiss$Incedient.State.Code    11 5.491e+08 49917378    21.9 <2e-16 ***
#   Residual

######### Jurisdiction.Code  #########
aov.Employment.Status.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Employment.Status.Code,data=rawData_noMiss)
aov.Employment.Status.Code
summary(aov.Employment.Status.Code)[[1]][["Pr(>F)"]][1]     # 2.024127e-160

# Df    Sum Sq   Mean Sq F value Pr(>F)    
# rawData_noMiss$Employment.Status.Code     4 1.687e+09 421674943   191.5 <2e-16 ***
#   Residuals                             14992 3.301e+10   2201516    

######### Jurisdiction.Code  #########
aov.Jurisdiction.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Jurisdiction.Code,data=rawData_noMiss)
aov.Jurisdiction.Code
summary(aov.Jurisdiction.Code)[[1]][["Pr(>F)"]][1]     # 1.997057e-32

# Df    Sum Sq  Mean Sq F value Pr(>F)    
# rawData_noMiss$Jurisdiction.Code    17 4.503e+08 26488538   11.59 <2e-16 ***
#   Residuals                        14979 3.424e+10  2285969     

######### Medical.Only.Code  #########
aov.Medical.Only.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Medical.Only.Code,data=rawData_noMiss)
aov.Medical.Only.Code
summary(aov.Medical.Only.Code)[[1]][["Pr(>F)"]][1]     # 0

# Df    Sum Sq   Mean Sq F value Pr(>F)    
# rawData_noMiss$Medical.Only.Code     1 1.470e+10 1.470e+10   11025 <2e-16 ***
#   Residuals                        14995 1.999e+10 1.333e+06                   
---
  
######### Nature.Illness.Code  #########
aov.Nature.Illness.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Nature.Illness.Code,data=rawData_noMiss)
aov.Nature.Illness.Code
summary(aov.Nature.Illness.Code)[[1]][["Pr(>F)"]][1]     # 0
# 
# Df    Sum Sq   Mean Sq F value Pr(>F)    
# rawData_noMiss$Nature.Illness.Code    23 4.469e+09 194308371   96.27 <2e-16 ***
#   Residuals                          14973 3.022e+10   2018483                   
---

######### OSHA.Injury.Type.Code  #########
aov.OSHA.Injury.Type.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$OSHA.Injury.Type.Code,data=rawData_noMiss)
aov.OSHA.Injury.Type.Code
summary(aov.OSHA.Injury.Type.Code)[[1]][["Pr(>F)"]][1]     # 1.944574e-14

# Df    Sum Sq  Mean Sq F value   Pr(>F)    
# rawData_noMiss$OSHA.Injury.Type.Code     5 1.696e+08 33915654   14.73 1.94e-14 ***
#   Residuals                            14991 3.452e+10  2302865 


######### Severity.Index.Code  #########
aov.Severity.Index.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Severity.Index.Code,data=rawData_noMiss)
aov.Severity.Index.Code
summary(aov.Severity.Index.Code)[[1]][["Pr(>F)"]][1]     # 6.455315e-18

# Df    Sum Sq  Mean Sq F value Pr(>F)    
# rawData_noMiss$Severity.Index.Code     9 2.351e+08 26116680   11.36 <2e-16 ***
#   Residuals                          14987 3.446e+10  2299111                   
# ---



######### Type.of.Loss.Code  #########
aov.Type.of.Loss.Code <- aov(rawData_noMiss$Dependent~rawData_noMiss$Type.of.Loss.Code,data=rawData_noMiss)
aov.Type.of.Loss.Code
summary(aov.Type.of.Loss.Code)[[1]][["Pr(>F)"]][1]     # 1.063202e-33

# Df    Sum Sq   Mean Sq F value Pr(>F)    
# rawData_noMiss$Type.of.Loss.Code     1 3.371e+08 337060764   147.1 <2e-16 ***
#   Residuals                        14995 3.435e+10   2291082            


######### Policy.Year  #########
aov.Policy.Year <- aov(rawData_noMiss$Dependent~rawData_noMiss$Policy.Year,data=rawData_noMiss)
aov.Policy.Year
summary(aov.Policy.Year)[[1]][["Pr(>F)"]][1]     # 2.857428e-35
# 
# Terms:
#   rawData_noMiss$Policy.Year   Residuals
# Sum of Squares                   463026295 34228807062
# Deg. of Freedom                         14       14982
# 
# Residual standard error: 1511.51
# Estimated effects may be unbalanced
# > summary(aov.Policy.Year)[[1]][["Pr(>F)"]][1]     
# [1] 2.857428e-35
# > summary(aov.Policy.Year)
# Df    Sum Sq  Mean Sq F value Pr(>F)    
# rawData_noMiss$Policy.Year    14 4.630e+08 33073307   14.48 <2e-16 ***
#   Residuals                  14982 3.423e+10  2284662                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


######### Reforms_dummy  #########
aov.Reforms_dummy <- aov(rawData_noMiss$Dependent~rawData_noMiss$Reforms_dummy,data=rawData_noMiss)
aov.Reforms_dummy
summary(aov.Reforms_dummy)[[1]][["Pr(>F)"]][1]     # 4.833446e-05

# Terms:
#   rawData_noMiss$Reforms_dummy   Residuals
# Sum of Squares                      45953989 34645879368
# Deg. of Freedom                            2       14994
# 
# Residual standard error: 1520.082
# Estimated effects may be unbalanced
# > summary(aov.Reforms_dummy)
# Df    Sum Sq  Mean Sq F value   Pr(>F)    
# rawData_noMiss$Reforms_dummy     2 4.595e+07 22976995   9.944 4.83e-05 ***
#   Residuals                    14994 3.465e+10  2310650                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#-------------------------------------------------------------------#
# DROPPING HOW INJURY occur
rawData_noMiss_bck <- rawData_noMiss
rawData_noMiss <- rawData_noMiss[,colnames(rawData_noMiss) != c("How.Injury.Occurred")]

 # Linear Regression Model

ncol(rawData_noMiss)
colnames(rawData_noMiss)

# Sample

set.seed(1002)

trainingRowIndex <- sample(1:nrow(rawData_noMiss), 0.8*nrow(rawData_noMiss))  # row incices for training data

trainingData <- rawData_noMiss[trainingRowIndex, ]  # model training data

testData  <- rawData_noMiss[-trainingRowIndex, ]   # test data


fit_lm <- lm(Dependent ~ Body.Part.Code + Cause.Code + Claimant.Age + Claimant.Gender.Code + Claimant.State.Code + Cause.Code.1 + Domestic.vs..Foreign..Code + Employment.Status.Code + Handling.Office.Name + Injury.Illness.Postal + Incedient.State.Code + Jurisdiction.Code + Medical.Only.Code + Nature.Illness.Code + OSHA.Injury.Type.Code + Severity.Index.Code +Type.of.Loss.Code + Policy.Year+ Reforms_dummy ,data=trainingData)

summary(fit_lm)

plot(density(fit_lm$residuals))  # Not Normally distributed so assumption of Multicolliniarity is violated 

library(car)
library(MASS)


# Variable Inflanation Function
vif(fit_lm)



#step_fit <- stepAIC(fit_lm)
# 
# Dependent ~ Body.Part.Code + Claimant.Age + Cause.Code.1 + Domestic.vs..Foreign..Code + 
#   Employment.Status.Code + Handling.Office.Name + Incedient.State.Code + 
#   Jurisdiction.Code + Medical.Only.Code + Nature.Illness.Code + 
#   Severity.Index.Code + Type.of.Loss.Code + Policy.Year + Reforms_dummy

step_fit <- stepAIC(fit_lm,direction = "backward") # final AIC=209499.5

fit_final <- lm(Dependent ~ Body.Part.Code + Claimant.Age + Cause.Code.1 + Domestic.vs..Foreign..Code +Employment.Status.Code + Handling.Office.Name + Incedient.State.Code + Jurisdiction.Code + Medical.Only.Code + Nature.Illness.Code + Severity.Index.Code + Type.of.Loss.Code + Policy.Year + Reforms_dummy,data=trainingData)
summary(fit_final)

# Residual standard error: 1083 on 11853 degrees of freedom
# Multiple R-squared:  0.499,	Adjusted R-squared:  0.4929 
# F-statistic: 82.55 on 143 and 11853 DF,  p-value: < 2.2e-16

vif(fit_final)
  
# Heterocidasity
# plot model
par(mfrow=c(4,2))  
plot(fit_final)




#############################  XGBOOST   ################################
install.packages('xgboost')
library(xgboost)
install.packages('magrittr')
library(magrittr)
install.packages('Matrix')
library(Matrix)

bck1 <- rawData_noMiss
# drawing samples with replacement
set.seed(1234)
ind <- sample(2,nrow(rawData_noMiss),replace=T,prob=c(0.8,0.2))
train <- rawData_noMiss[ind==1,]
test <- rawData_noMiss[ind==2,]

trainm <- sparse.model.matrix(Dependent~.-1,data=train)
head(trainm)
train_label <- train[,"Dependent"]
train_matrix <- xgb.DMatrix(data= as.matrix(trainm),label=train_label)

testm <- sparse.model.matrix(Dependent~.-1,data=test)  
head(testm)
test_label <- test[,"Dependent"]
test_matrix <- xgb.DMatrix(data= as.matrix(testm),label=test_label)


# Parameters

nc <- length(unique(train_label))
xgb_params <- list("objective"="multi:softprob",
                   "eval_metric"="mlogloss",
                   "num_class"=nc)


watchlist <- list(train=train_matrix,test=test_matrix)

bst_model <- xgb.train(params = xgb_params,data=train_matrix,
                       nrounds = 100,
                       watchlist = watchlist)
summary(train_label)

library(readr)
library(data.table)

train.feat <- data.table(train)

test.feat <- data.table(test)
testfact <- cbind(test.feat,Dependent=0)

trai_nm <- sparse.model.matrix(Dependent~.-1,data=train.feat)
head(trai_nm)

test_nm <- sparse.model.matrix(Dependent~.-1,data=testfact)
head(test_nm)

trainOutVector <- train.feat$Dependent

train_boost <- xgboost(data=trai_nm,label=trainOutVector,nrounds=80,objective="reg:linear")
# train_boost <- xgboost(data=trai_nm,label=trainOutVector,nrounds=1000,objective="reg:linear",eta=0.05,max_depth=6,min_child_weight=100,subsample=0.75)
trainPreds <- predict(train_boost,test_nm)
plot(trainPreds,train$Dependent[1:3014])

length(trainPreds)
length(train$Dependent)
