#clear workspace
rm(list=ls())

############       Reading data   #################
#using read.table()
loan_data <- read.table("loans data.csv",header = TRUE,sep = ",")
loan <- read.csv("loans data.csv")

#dimension of the data
dim(loan)

#structure of the data
str(loan_data)


#view of the data in a table
View(loan)


#fetching top 6 rows
head(loan)

#fetching last 6 rows
tail(loan)

#summary of the data
summary(loan)

############       Cleaning data   #################
#checking for missing values in the data
any(is.na(loan))  #NA NaN


#checking for the total no. of missing values in the data
sum(is.na(loan))

#cleaning NA values
loan_clean <- na.omit(loan)

sum(is.na(loan_clean))
str(loan_clean)


loan_clean <- loan[complete.cases(loan),]

#imputation - filling the missing values
#cleaning Amount.Requested Column
#checking for the total no. of missing values in a particular column

sum(is.na(loan$Amount.Requested))

unique(loan$Amount.Requested)

#changing to numeric types
loan$Amount.Requested <- as.integer(loan$Amount.Requested)
str(loan)

#unique values in a column
unique(loan$Amount.Requested)

mean(loan$Amount.Requested,na.rm = TRUE)
median(loan$Amount.Requested,na.rm = TRUE)

#library(dplyr)
library(tidyverse)
#Decide whether to impute with mean or median
loan %>%
  summarize(avg=mean(Amount.Requested,na.rm = TRUE),med=median(Amount.Requested,na.rm = TRUE))

loan <- loan %>%
  mutate(Amount.Requested=replace(Amount.Requested,is.na(Amount.Requested),median(Amount.Requested,na.rm = TRUE)))

sum(is.na(loan$Amount.Requested))


#Rename a column
loan <- loan %>%
  rename(Amt_Req=Amount.Requested)
names(loan)

#cleaning Amount.Funded.By.Investors column
sum(is.na(loan$Amount.Funded.By.Investors))

unique(loan$Amount.Funded.By.Investors)

loan <- loan%>%
  rename(Amt_fund=Amount.Funded.By.Investors)
#convert the type to numeric
loan$Amt_fund <- as.numeric(loan$Amt_fund)

#checking for NA values
sum(is.na(loan$Amt_fund))

#check impute with mean or median
loan%>%
  summarize(avg=mean(loan$Amt_fund,na.rm = TRUE),md=median(loan$Amt_fund,na.rm = TRUE))

loan <- loan%>%
  mutate(Amt_fund=replace(Amt_fund,is.na(Amt_fund),median(Amt_fund,na.rm = TRUE)))

sum(is.na(loan$Amt_fund))

str(loan)
#cleaning Interest.Rate column
sum(is.na(loan$Interest.Rate))
#cleaning unwanted substring in a chr column
loan <- loan %>%
  mutate(Interest.Rate=gsub("%","",Interest.Rate))
head(loan$Interest.Rate,2)

loan$Interest.Rate <- as.numeric(loan$Interest.Rate)
head(loan$Interest.Rate,2)
str(loan)


#cleaning Loan.Length column
sum(is.na(loan$Loan.Length))

unique(loan$Loan.Length)

loan <- loan %>%
  mutate(Loan.Length=gsub(" months","",Loan.Length))

loan$Loan.Length <- as.integer(loan$Loan.Length)

sum(is.na(loan$Loan.Length))

unique(loan$Loan.Length)

#filtering the rows with NA values
loan%>%
  filter(is.na(Loan.Length))

#drop the rows with NA values
loan <- loan%>%
  drop_na(Loan.Length)

#checking
sum(is.na(loan$Loan.Length))

unique(loan$Loan.Length)


#cleaning Employment.Length column
sum(is.na(loan$Employment.Length))

unique(loan$Employment.Length)

loan <- loan %>%
  mutate(Employment.Length=gsub(" year| years|<|\\+","",Employment.Length))

loan$Employment.Length <- as.integer(loan$Employment.Length)

#checking
unique(loan$Employment.Length)
sum(is.na(loan$Employment.Length))

table(loan$Employment.Length)
mean(table(loan$Employment.Length))

loan <- loan%>%
  mutate(Employment.Length=replace(Employment.Length,is.na(Employment.Length),2))
#checking
sum(is.na(loan$Employment.Length))
unique(loan$Employment.Length)               

#cleaning FICO.Range column
loan <- loan %>%
  separate(FICO.Range,c("fico-low","fico-high"))
str(loan)

loan$`fico-high` <- as.integer(loan$`fico-high`)
loan$`fico-low` <- as.integer(loan$`fico-low`)
str(loan)
sum(is.na(loan$`fico-high`))
sum(is.na(loan$`fico-low`))
unique(loan$`fico-high`)
unique(loan$`fico-low`)


