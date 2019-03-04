install_github("tomasgreif/riv","tomasgreif",force=TRUE)
install.packages("riv")
install.packages("woe")
install.packages('devtools')
install_github("riv","tomasgreif")
install.packages("DBI",dependencies=TRUE)
install.packages("gsubfn",dependencies=TRUE)
install.packages("RSQLite")
install.packages("ROSE")
install.packages ("randomForest")


library(randomForest)
library(MASS)
library(car)
library(e1071)
library(ggplot2)
library(cowplot)
library(caTools)
library(caret)
library(dummies)
library(woe)
library(riv)
library(devtools)
library(DBI)
library(gsubfn)
library(RSQLite)
library(ROSE)
library(dplyr)
library(plyr)
##------------ CredX Acquisition Analytics---------------------##

#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:- Prospect Profiling
#-------------------------------------------------------

# Loading demographic data in the working directory. 

demog_data<- read.csv("Demographic data.csv",stringsAsFactors = FALSE)

# Checking structure of dataset 

str(demog_data)

# Summary of dataset

summary(demog_data)



# Loading credit bureau data in the working directory. 

bureau_data<- read.csv("Credit Bureau data.csv",stringsAsFactors = FALSE)

# Checking structure of dataset 

str(bureau_data)

# Summary of dataset

summary(bureau_data)


# Checking for duplicate values in the application id for both the data sets :

setdiff(demog_data$Application.ID,bureau_data$Application.ID) # no difference in application ids in 2 sets

# Duplicate values
sum(duplicated(demog_data$Application.ID)) # 3 duplicate values
demog_data_1 <- demog_data[-which(duplicated(demog_data$Application.ID) == T), ]

sum(duplicated(bureau_data$Application.ID)) # 3 duplicate values
bureau_data_1 <- bureau_data[-which(duplicated(bureau_data$Application.ID) == T), ]


#combining demog data and bureau data in one dataset and checking duplicates in Application ID

Combined_data <- merge(demog_data_1,bureau_data_1,by = "Application.ID")

Combined_data$Tag_check <- ifelse(Combined_data$Performance.Tag.x == Combined_data$Performance.Tag.y,"T","F")
which(Combined_data$Tag_check == 'F')  

# both the perrformance tags in bureau and demog data are the same hence dropping one column

Combined_data_EDA<-Combined_data[,-31]

Combined_data<-Combined_data[,-31:-30]


# checking NA values
sum(is.na(Combined_data)) # 3031 NAs

sapply(Combined_data, function(x) length(which(is.na(x))))


# 1425 NA in Performance.Tag
# 272 Na in Outstanding.Balance
# 272 NA in Presence.of.open.home.loan
# 1058 NA in Avgas.CC.Utilization.in.last.12.months
# 3 NA in No.of.dependents
# 1 NA in  No.of.trades.opened.in.last.6.months


##################### DATA SET FOR EXPLORATORY DATA ANALYSIS ################################## 

combined_data_approved <- Combined_data_EDA[-which(is.na(Combined_data_EDA$Performance.Tag.y)==TRUE),] # removing rows with Performance tag NA

# for corelation check
combined_data_approved_correl <- Combined_data[-which(is.na(Combined_data$Performance.Tag)==TRUE),]
str(combined_data_approved_correl)



############################# cORERELATION CHECK ###################3
combined_data_approved_correl_int <- combined_data_approved_correl[,c(-1,-3,-4,-7:-9)]
str(combined_data_approved_correl_int)

correl_matrix<-data.frame(cor(combined_data_approved_correl_int,use="pairwise.complete.obs",method="pearson"))

write.csv(correl_matrix,file="correl_matrix.csv") 


# Duplicate values
sum(duplicated(combined_data_approved$Application.ID)) # 0 duplicate values


sum(combined_data_approved$Performance.Tag.y)
nrow(combined_data_approved)
Bad_performance <- sum(combined_data_approved$Performance.Tag.y)/nrow(combined_data_approved)

Bad_performance # 4.21% default rate in dataset 


##### DATA IS IMBALANCED IN TARGET VARIABLE, WE MIGHT NEED TO USE IMBALANCE CANCELLATION TECHNIQUES FOR MODELLING 


################################################################

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(combined_data_approved) #69867 obs. of 29 variables;




#For categorical variables we are looking at bar plots


# Function for plotting each variable by bad rate

plot_response <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag.y~cat_var, combined_data_approved, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "Performance.Tag.y","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 3),cex = 3)
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = Performance.Tag.y)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}


#-----------------------------------------------------------------#
# the first variable is "Education" variable

# So let's check the summary of this variable 

plot_response(combined_data_approved$Education, "Education") 
# If others is the education level then the bad rate is high - 7%
# All education levels above Bachelors are showing approx 4.2 % bad rate


#-----------------------------------------------------------------#
# the next variable is "Gender" variable

# So let's check the summary of this variable 

plot_response(combined_data_approved$Gender, "Gender") 

# Bad rates are same for males and females as observed . Keeping gender column for further analysis as suggested by mentor


#-----------------------------------------------------------------#
# the next variable is "Maritial Status" variable

# So let's check the summary of this variable 

plot_response(combined_data_approved$Marital.Status..at.the.time.of.application., "Marital_status") 
# marital status is not differentiating between bad rates at 4% each

#-----------------------------------------------------------------#
# the next variable is "No.of.dependents" variable

# So let's check the summary of this variable 


plot_response(combined_data_approved$No.of.dependents, "No_of_dependents") 
# No_of_dependents = 2 the bad rate is low at 3.9%
# For 1 or 4 dependents the bad rate is high at 4.4%
# For 4 and 5 dependents the bad rate is moderate at 4.1%


#-----------------------------------------------------------------#
# the next variable is "Type.of.residence" variable

# So let's check the summary of this variable 

plot_response(combined_data_approved$Type.of.residence, "Type_of_residence") 

#  bad rate is high for living with parents and company provided residences at 4.5% each compared to owned and rented at 4.2%



# Profession 

plot_response(combined_data_approved$Profession, "Profession") 
# BAD RATE IS HIGH FOR SE CLASS at 4.6% compared to other classes



# For quantiative variables we are looking at histograms and box plots 

# Plotting Age histogram
ggplot(combined_data_approved,aes(Age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(combined_data_approved$Age,seq(0,1,0.1))

# Box plot 

boxplot(combined_data_approved$Age)

# capping age to 1 minimum

combined_data_approved[(which(combined_data_approved$Age<=20)),]$Age <- 20



combined_data_approved$New_Age <- ifelse(combined_data_approved$Age <= 41, 'LE_41' ,'GE_42' )

plot_response(combined_data_approved$New_Age, "New_Age") 


# bad rate is higher for age group less than 41  and then it decreases as age increases



#-----------------------------------------------------------------#
# the next variable is "No.of.months.in.current.residence" variable

# So let's check the summary of this variable 

# Let's check the outlier in the variables 

quantile(combined_data_approved$No.of.months.in.current.residence,seq(0,1,0.1))


# Bad rate for No. of months in curr resi. at 10-29 months is highest 6.6% and then gradually decreases to 4.6% till 80-90 months



combined_data_approved$New_No.of.months.in.current.residence <- ifelse(combined_data_approved$No.of.months.in.current.residence == 6, 'Sixmnths' ,
                                                                       ifelse(combined_data_approved$No.of.months.in.current.residence>6 & combined_data_approved$No.of.months.in.current.residence <= 24,'Seven_24mnths' ,
                                                                              ifelse(combined_data_approved$No.of.months.in.current.residence >= 25 ,'GT24mnths',NA)))

plot_response(combined_data_approved$New_No.of.months.in.current.residence, "No.of.months.in.current.residence") 



#-----------------------------------------------------------------#
# the next variable is "No.of.months.in.current.company" variable

# So let's check the summary of this variable 

quantile(combined_data_approved$No.of.months.in.current.company,seq(0,1,0.1))


# Binning the No.of.months.in.current.company variable and store it into "Binned.No.of.months.in.current.company".

plot_response(combined_data_approved$No.of.months.in.current.company, "Binned.No.of.months.in.current.company") 

# Bad rate for No.of.months.in.current.company at 2-13 months is highest 4.8% and then gradually decreases


combined_data_approved$New_No.of.months.in.current.company <- ifelse(combined_data_approved$No.of.months.in.current.company <=24, 'LT24mnths' ,'GT24mnths')

plot_response(combined_data_approved$New_No.of.months.in.current.company, "New_No.of.months.in.current.company") 


#-----------------------------------------------------------------#
# the next variable is "No.of.times.90.DPD.or.worse.in.last.6.months" variable

# So let's check the summary of this variable 


plot_response(combined_data_approved$No.of.times.90.DPD.or.worse.in.last.6.months, "No.of.times.90.DPD.or.worse.in.last.6.months") 

# Bad rate for No.of.times.90.DPD.or.worse.in.last.6.months is highest 11.0% at 3 times and then gradually decreases to 3.3% for 0 times 90+dpd in last 6m



#-----------------------------------------------------------------#
# the next variable is "No.of.times.60.DPD.or.worse.in.last.6.months" variable

# So let's check the summary of this variable 


plot_response(combined_data_approved$No.of.times.60.DPD.or.worse.in.last.6.months, "No.of.times.60.DPD.or.worse.in.last.6.months") 

# Bad rate for No.of.times.90.DPD.or.worse.in.last.6.months is highest 11.0% at 3 times and then gradually decreases to 3.3% for 0 times 90+dpd in last 6m


# Bad rate for No.of.times.60.DPD.or.worse.in.last.6.months is highest 10.0% at 3 times 
# Lowest for 0 times at 3.0%  at 60+dpd in last 6m




#-----------------------------------------------------------------#
# the next variable is "No.of.times.30.DPD.or.worse.in.last.6.months" variable

# So let's check the summary of this variable 



plot_response(combined_data_approved$No.of.times.30.DPD.or.worse.in.last.6.months, "No.of.times.30.DPD.or.worse.in.last.6.months") 

# Bad rate for No.of.times.90.DPD.or.worse.in.last.6.months is highest 11.0% at 3 times and then gradually decreases to 3.3% for 0 times 90+dpd in last 6m


# Bad rate for No.of.times.30.DPD.or.worse.in.last.6.months increases from 1 times to 6 times  
# Lowest for 0 times at 2.9%  for no. of times 30+dpd in last 6m



#-----------------------------------------------------------------#
# the next variable is "No.of.times.90.DPD.or.worse.in.last.12.months" variable

# So let's check the summary of this variable 


# Bad rate for No.of.times.90.DPD.or.worse.in.last.12.months increases from 1 times to 5 times  
# Lowest for 0 times at 3.0% for no of times 90+dpd in last 12m


plot_response(combined_data_approved$No.of.times.90.DPD.or.worse.in.last.12.months, "No.of.times.90.DPD.or.worse.in.last.12.months") 

# Bad rate for No.of.times.90.DPD.or.worse.in.last.12.months is highest 11.0% at 3 times and then gradually decreases to 3.3% for 0 times 90+dpd in last 6m



#-----------------------------------------------------------------#
# the next variable is "No.of.times.60.DPD.or.worse.in.last.12.months" variable

# So let's check the summary of this variable 


plot_response(combined_data_approved$No.of.times.60.DPD.or.worse.in.last.12.months, "No.of.times.60.DPD.or.worse.in.last.12.months") 




#-----------------------------------------------------------------#
# the next variable is "No.of.times.30.DPD.or.worse.in.last.12.months" variable

# So let's check the summary of this variable 
# capping No.of.times.30.DPD.or.worse.in.last.6.months to 6 maximum


plot_response(combined_data_approved$No.of.times.30.DPD.or.worse.in.last.12.months, "No.of.times.30.DPD.or.worse.in.last.12.months") 



# Bad rate for No.of.times.30.DPD.or.worse.in.last.12.months increases from 1 times to 7 times  
# Lowest for 0 times at 2.9% for no of times 30+dpd in last 12m



#-----------------------------------------------------------------#
# the next variable is "Avgas.CC.Utilization.in.last.12.months" variable

# So let's check the summary of this variable 

summary(combined_data_approved$Avgas.CC.Utilization.in.last.12.months)
# Binning the No.of.months.in.current.company variable and store it into "Binned.No.of.months.in.current.residence".

plot_response(combined_data_approved$Avgas.CC.Utilization.in.last.12.months, "Avgas.CC.Utilization.in.last.12.months") 


combined_data_approved$New_Avgas.CC.Utilization.in.last.12.months <- ifelse(combined_data_approved$Avgas.CC.Utilization.in.last.12.months <= 11 , 'LT_11' ,
                                                                            ifelse(combined_data_approved$Avgas.CC.Utilization.in.last.12.months >= 12 & combined_data_approved$Avgas.CC.Utilization.in.last.12.months <= 21 , '12_21' ,'Morethn_21'))

plot_response(combined_data_approved$New_Avgas.CC.Utilization.in.last.12.months, "New_Avgas.CC.Utilization.in.last.12.months") 


#-----------------------------------------------------------------#
# the next variable is "No.of.trades.opened.in.last.6.months" variable

# So let's check the summary of this variable 

summary(combined_data_approved$No.of.trades.opened.in.last.6.months)
# Binning the No.of.months.in.current.company variable and store it into "Binned.No.of.months.in.current.residence".

combined_data_approved$Binned.No.of.trades.opened.in.last.6.months <- as.factor(cut(combined_data_approved$No.of.trades.opened.in.last.6.months, breaks = c(-1,0,1,2,3,4,13)))

plot_response(combined_data_approved$Binned.No.of.trades.opened.in.last.6.months, "Binned.No.of.trades.opened.in.last.6.months") 


# Bad rate for Binned.No.of.trades.opened.in.last.6.months increases no. of trades opened in last 6M increases
# Lowest for 0 utilisation at 2.2%


#-----------------------------------------------------------------#
# the next variable is "No.of.trades.opened.in.last.12.months" variable

# So let's check the summary of this variable 


plot_response(combined_data_approved$No.of.trades.opened.in.last.12.months, "No.of.trades.opened.in.last.12.months") 


combined_data_approved$New_No.of.trades.opened.in.last.12.months <- ifelse(combined_data_approved$No.of.trades.opened.in.last.12.months <= 2 , 'LE_2' ,
                                                                           ifelse(combined_data_approved$No.of.trades.opened.in.last.12.months >= 3 & combined_data_approved$No.of.trades.opened.in.last.12.months <= 5 , 'BET3_5' ,
                                                                                  ifelse(combined_data_approved$No.of.trades.opened.in.last.12.months >= 6 & combined_data_approved$No.of.trades.opened.in.last.12.months <= 12 , 'BET6_12' ,'GE_13')))

plot_response(combined_data_approved$New_No.of.trades.opened.in.last.12.months, "New_No.of.trades.opened.in.last.12.months") 


#-----------------------------------------------------------------#
# the next variable is "No.of.PL.trades.opened.in.last.6.months" variable

# So let's check the summary of this variable 



plot_response(combined_data_approved$No.of.PL.trades.opened.in.last.6.months, "No.of.PL.trades.opened.in.last.6.months") 


combined_data_approved$New_No.of.PL.trades.opened.in.last.6.months <- ifelse(combined_data_approved$No.of.PL.trades.opened.in.last.6.months ==0 , 'Zero' ,                                                                         
                                                                             ifelse(combined_data_approved$No.of.PL.trades.opened.in.last.6.months == 1, 'One' ,'GE_2'))

plot_response(combined_data_approved$New_No.of.PL.trades.opened.in.last.6.months, "New_No.of.PL.trades.opened.in.last.6.months") 


#-----------------------------------------------------------------#
# the next variable is "No.of.PL.trades.opened.in.last.12.months" variable

# So let's check the summary of this variable 



plot_response(combined_data_approved$No.of.PL.trades.opened.in.last.12.months, "No.of.PL.trades.opened.in.last.12.months") 



combined_data_approved$New_No.of.PL.trades.opened.in.last.12.months <- ifelse(combined_data_approved$No.of.PL.trades.opened.in.last.12.months <=1 , 'LE_1' ,                                                                         
                                                                              'GE_2')

plot_response(combined_data_approved$New_No.of.PL.trades.opened.in.last.12.months, "New_No.of.PL.trades.opened.in.last.12.months") 


#-----------------------------------------------------------------#
# the next variable is "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." variable

# So let's check the summary of this variable 



plot_response(combined_data_approved$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.") 


combined_data_approved$New_No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- ifelse(combined_data_approved$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. == 0 , 'Zero' ,                                                                         
                                                                                                    'GE_1')

plot_response(combined_data_approved$New_No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., "New_No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.") 





#-----------------------------------------------------------------#
# the next variable is "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans." variable

# So let's check the summary of this variable 



plot_response(combined_data_approved$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.") 



combined_data_approved$New_No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- ifelse(combined_data_approved$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. == 0 , 'Zero' ,                                                                         
                                                                                                     ifelse(combined_data_approved$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. >= 1 & combined_data_approved$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <= 4 ,'Bet_1_4','GE_5'))

plot_response(combined_data_approved$New_No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., "New_No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.") 


#-----------------------------------------------------------------#
# the next variable is "Presence.of.open.home.loan" variable

# So let's check the summary of this variable 


plot_response(combined_data_approved$Presence.of.open.home.loan, "Presence.of.open.home.loan") 

# Bad rate for Presence.of.open.home.loan is more for 0 existing home loan customers
# For customers with an exxisting home loan the bad rate is low at 3.4%



#-----------------------------------------------------------------#
# the next variable is "Outstanding.Balance" variable

# So let's check the summary of this variable 

summary(combined_data_approved$Outstanding.Balance)

# Plotting Outstanding.Balance histogram
ggplot(combined_data_approved,aes(Outstanding.Balance))+geom_histogram()

# Let's check the outlier in the variables 

quantile(combined_data_approved$Outstanding.Balance,seq(0,1,0.01),na.rm = TRUE)

# Box plot 

boxplot(combined_data_approved$Outstanding.Balance)


plot_response(combined_data_approved$Outstanding.Balance, "Outstanding.Balance") 

combined_data_approved$New_Outstanding.Balance <- ifelse(combined_data_approved$Outstanding.Balance <= 400000 , 'LE_4Lakh' ,
                                                         ifelse(combined_data_approved$Outstanding.Balance >= 400001 & combined_data_approved$Outstanding.Balance <= 1400000 , 'Bet_4_14' ,'GT_14'))

plot_response(combined_data_approved$New_Outstanding.Balance, "New_Outstanding.Balance") 



#-----------------------------------------------------------------#
# the next variable is "Total.No.of.Trades" variable

# So let's check the summary of this variable 

# Binning the No.of.months.in.current.company variable and store it into "Binned.No.of.months.in.current.residence".


plot_response(combined_data_approved$Total.No.of.Trades, "Total.No.of.Trades") 


combined_data_approved$New_Total.No.of.Trades <- ifelse(combined_data_approved$Total.No.of.Trades <= 5 , 'LE_5' ,                                                                         
                                                        'GE_6')

plot_response(combined_data_approved$New_Total.No.of.Trades, "New_Total.No.of.Trades") 


#-----------------------------------------------------------------#
# the next variable is "Presence.of.open.auto.loan" variable

# So let's check the summary of this variable 

plot_response(combined_data_approved$Presence.of.open.auto.loan, "Presence.of.open.auto.loan") 


#-----------------------------------------------------------------#
# the next variable is "Income" variable

# So let's check the summary of this variable 

plot_response(combined_data_approved$Income, "Income") 



combined_data_approved$New_Income <- ifelse(combined_data_approved$Income <= 10 , 'LE_10' ,
                                            ifelse(combined_data_approved$Income >= 11 & combined_data_approved$Income <= 31 , 'Bet_11_31' ,'GE_32'))

plot_response(combined_data_approved$New_Income, "New_Income") 

str(combined_data_approved)



#################################################################################################
########################## BIVARIATE ANALYSIS AND ExPLORATORY DATA ANAYSIS COMPLETED ###################
#################################################################################################



#################################################################################################
########################## WOE ANALYSIS AND INFORMATION VALUE ANALYSIS BEGINS ###################
#################################################################################################




rejected_pop<-Combined_data[which(is.na(Combined_data$Performance.Tag.x)),]

approved_pop<- Combined_data[which(!is.na(Combined_data$Performance.Tag.x)),]

sum(is.na(approved_pop))

master_df <- na.omit(approved_pop)


class(master_df$Performance.Tag.x)

master_df$Performance.Tag.x<-as.factor(master_df$Performance.Tag.x)



row.names(master_df) <- 1:nrow(master_df) 


## From this we can use the variables suitable for prediction
iv.plot.summary(iv.mult(master_df,"Performance.Tag.x",TRUE))

## We have the following variables with High Information value

#Avgas.CC.Utilization.in.last.12.months
#No.of.trades.opened.in.last.12.months,
#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
#No.of.PL.trades.opened.in.last.12.months
#Outstanding.Balance
#Total.No.of.Trades
#No.of.times.30.DPD.or.worse.in.last.6.months
#No.of.PL.trades.opened.in.last.6.months
#No.of.times.90.DPD.or.worse.in.last.12.months
#No.of.times.60.DPD.or.worse.in.last.6.months
#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
#No.of.times.30.DPD.or.worse.in.last.12.months
#No.of.times.60.DPD.or.worse.in.last.12.months
#No.of.trades.opened.in.last.6.months
#No.of.times.90.DPD.or.worse.in.last.6.months
#No.of.months.in.current.residence
#Income

## Replacing values for these coloumns with their WOE values

#1)first variable highest iv
iv.mult(master_df,"Performance.Tag.x",vars=c("Avgas.CC.Utilization.in.last.12.months"))
#Woe is 0.67 and -0.46 for the other bin
##plotting WOE values for this coloumn
iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("Avgas.CC.Utilization.in.last.12.months"),summary=FALSE))
#Imputing these values in master df 
master_df$Avgas.CC.Utilization.in.last.12.months_woe<-ifelse(master_df$Avgas.CC.Utilization.in.last.12.months < 16.5,0.67,
                                                            ifelse(master_df$Avgas.CC.Utilization.in.last.12.months>16.5,-0.46,'NA'))

summary(as.numeric(master_df$Avgas.CC.Utilization.in.last.12.months_woe))


#2)For second Variable Avgas CC Utilization in last 12 months

iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.trades.opened.in.last.12.months"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.trades.opened.in.last.12.months"),summary=FALSE))

master_df$No.of.trades.opened.in.last.12.months_woe<-ifelse(master_df$No.of.trades.opened.in.last.12.months < 2.5,0.9193,
                                                ifelse(master_df$No.of.trades.opened.in.last.12.months>=2.5 & master_df$No.of.trades.opened.in.last.12.months < 5.5,-0.0759,
                                                ifelse(master_df$No.of.trades.opened.in.last.12.months>=5.5 & master_df$No.of.trades.opened.in.last.12.months < 12.5,-0.502,
                                                ifelse(master_df$No.of.trades.opened.in.last.12.months>=12.5,-0.0053,'NA'))))
summary(as.numeric(master_df$No.of.trades.opened.in.last.12.months_woe))
#3)For third Variable No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.

iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."),summary=FALSE))

master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe<-ifelse(master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. < 0.5,1.15,
                                                             ifelse(master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>=0.5,-0.25,'NA'))

summary(as.numeric(master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe))

#4)For the 4 th variable No.of.PL.trades.opened.in.last.12.months
iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.PL.trades.opened.in.last.12.months"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.PL.trades.opened.in.last.12.months"),summary=FALSE))

master_df$No.of.PL.trades.opened.in.last.12.months_woe<-ifelse(master_df$No.of.PL.trades.opened.in.last.12.months < 1.5,0.72,-0.37)


summary(as.numeric(master_df$No.of.PL.trades.opened.in.last.12.months_woe))


#5)For the 5 th Variable Outstanding.Balance
iv.mult(master_df,"Performance.Tag.x",vars=c("Outstanding.Balance"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("Outstanding.Balance"),summary=FALSE))

master_df$Outsranding.Balance_woe <-ifelse(master_df$Outstanding.Balance < 210700,0.81,
                                  ifelse(master_df$Outstanding.Balance >=210700 & master_df$Outstanding.Balance < 1322000,-0.37,
                                  ifelse(master_df$Outstanding.Balance>=1322000 & master_df$Outstanding.Balance < 3262000,0.56,
                                  ifelse(master_df$Outstanding.Balance>=3262000,-0.29,'NA'))))

summary(as.numeric(master_df$Outsranding.Balance_woe))
summary(master_df$Outstanding.Balance)
#6) FOr the 6 th variable 
iv.mult(master_df,"Performance.Tag.x",vars=c("Total.No.of.Trades"))


iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("Total.No.of.Trades"),summary=FALSE))

master_df$Total.No.of.Trades_WOE <- ifelse(master_df$Total.No.of.Trades < 4.5,0.724,
                                    ifelse(master_df$Total.No.of.Trades >=4.5 & master_df$Outstanding.Balance < 7.5,-0.127,
                                    ifelse(master_df$Outstanding.Balance>=7.5 & master_df$Outstanding.Balance < 15.5,-0.5276,
                                    ifelse(master_df$Outstanding.Balance>=15.5,0.098,'NA'))))






#7) FOR THE 7 TH VARIABLE No.of.times.30.DPD.or.worse.in.last.6.months


iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.times.30.DPD.or.worse.in.last.6.months"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.times.30.DPD.or.worse.in.last.6.months"),summary=FALSE))

master_df$No.of.times.30.DPD.or.worse.in.last.6.months_WoE<-ifelse(master_df$No.of.times.30.DPD.or.worse.in.last.6.months < 0.5,0.40,-0.62)
                                                        

summary(as.numeric(master_df$No.of.times.30.DPD.or.worse.in.last.6.months_WoE))

#8)No.of.PL.trades.opened.in.last.6.months

iv.mult(master_df,"Performance.Tag.x",vars = c("No.of.PL.trades.opened.in.last.6.months"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.PL.trades.opened.in.last.6.months"),summary=FALSE))

master_df$No.of.PL.trades.opened.in.last.6.months_woe <- ifelse(master_df$No.of.PL.trades.opened.in.last.6.months < 0.5,0.68,-0.33)

summary(as.numeric(master_df$No.of.PL.trades.opened.in.last.6.months_woe))

### for the 8 th variable No.of.times.90.DPD.or.worse.in.last.12.months

iv.mult(master_df,"Performance.Tag.x",vars = c("No.of.times.90.DPD.or.worse.in.last.12.months"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.times.90.DPD.or.worse.in.last.12.months"),summary=FALSE))

master_df$No.of.times.90.DPD.or.worse.in.last.12.months_woe <- ifelse(master_df$No.of.times.90.DPD.or.worse.in.last.12.months < 0.5,0.36,-0.60)

summary(as.numeric(master_df$No.of.times.90.DPD.or.worse.in.last.12.months_woe))

##For the 9th variable
iv.mult(master_df,"Performance.Tag.x",vars = c("No.of.times.60.DPD.or.worse.in.last.6.months"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.times.60.DPD.or.worse.in.last.6.months"),summary=FALSE))

master_df$No.of.times.60.DPD.or.worse.in.last.6.months_woe <- ifelse(master_df$No.of.times.60.DPD.or.worse.in.last.6.months < 0.5,0.34,-0.62)

summary(as.numeric(master_df$No.of.times.60.DPD.or.worse.in.last.6.months_woe))

###For the 10th variable No.of.Inquiries.in.last.6.months..excluding.home...auto.loans

iv.mult(master_df,"Performance.Tag.x",vars = c("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans."))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans."),summary=FALSE))

master_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe <- ifelse(master_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. < 0.5,0.76,-0.26)

summary(as.numeric(master_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe))

#11) No.of.times.30.DPD.or.worse.in.last.12.months

iv.mult(master_df,"Performance.Tag.x",vars = c("No.of.times.30.DPD.or.worse.in.last.12.months"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.times.30.DPD.or.worse.in.last.12.months"),summary=FALSE))

master_df$No.of.times.30.DPD.or.worse.in.last.12.months_woe<-ifelse(master_df$No.of.times.30.DPD.or.worse.in.last.12.months < 1.5,0.27,-0.71)

summary(as.numeric(master_df$No.of.times.30.DPD.or.worse.in.last.12.months_woe))

#12) No.of.times.60.DPD.or.worse.in.last.12.months

iv.mult(master_df,"Performance.Tag.x",vars = c("No.of.times.60.DPD.or.worse.in.last.12.months"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.times.60.DPD.or.worse.in.last.12.months"),summary=FALSE))

master_df$No.of.times.60.DPD.or.worse.in.last.12.months_WOE <-ifelse(master_df$No.of.times.60.DPD.or.worse.in.last.12.months < 0.5 , 0.36,
                                                              ifelse(master_df$No.of.times.60.DPD.or.worse.in.last.12.months >= 0.5 & master_df$No.of.times.60.DPD.or.worse.in.last.12.months < 1.5,-0.22,
                                                              ifelse(master_df$No.of.times.60.DPD.or.worse.in.last.12.months >= 1.5,-0.69,'NA')))


summary(as.numeric(master_df$No.of.times.60.DPD.or.worse.in.last.12.months_WOE))

#13)No.of.trades.opened.in.last.6.months
iv.mult(master_df,"Performance.Tag.x",vars = c("No.of.trades.opened.in.last.6.months"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.trades.opened.in.last.6.months"),summary=FALSE))

master_df$No.of.trades.opened.in.last.6.months_WOE <-ifelse(master_df$No.of.trades.opened.in.last.6.months < 1.5,0.57,
                                                    ifelse(master_df$No.of.trades.opened.in.last.6.months >= 1.5,-0.32,'NA'))



summary(as.numeric(master_df$No.of.trades.opened.in.last.6.months))


#14)No.of.times.90.DPD.or.worse.in.last.6.months


iv.mult(master_df,"Performance.Tag.x",vars = c("No.of.times.90.DPD.or.worse.in.last.6.months"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.times.90.DPD.or.worse.in.last.6.months"),summary=FALSE))

master_df$No.of.times.90.DPD.or.worse.in.last.6.months_WOE <-ifelse(master_df$No.of.times.90.DPD.or.worse.in.last.6.months < 0.5,0.27,
                                                            ifelse(master_df$No.of.times.90.DPD.or.worse.in.last.6.months >= 0.5,-0.62,'NA'))



summary(as.numeric(master_df$No.of.times.90.DPD.or.worse.in.last.6.months_WOE))


#15)No.of.months.in.current.residence

iv.mult(master_df,"Performance.Tag.x",vars = c("No.of.months.in.current.residence"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("No.of.months.in.current.residence"),summary=FALSE))

master_df$No.of.months.in.current.residence_WOE <-ifelse(master_df$No.of.months.in.current.residence < 6.5,0.322,
                                                            ifelse(master_df$No.of.months.in.current.residence >= 6.5 & master_df$No.of.months.in.current.residence<34.5,-0.510,
                                                            ifelse(master_df$No.of.months.in.current.residence >= 34.5, -0.099,'NA')))



summary(as.numeric(master_df$No.of.months.in.current.residence_WOE))


#16)Income


iv.mult(master_df,"Performance.Tag.x",vars = c("Income"))

iv.plot.woe(iv.mult(master_df,"Performance.Tag.x",vars=c("Income"),summary=FALSE))

master_df$Income_WOE <-ifelse(master_df$Income < 31.5,-0.14,
                  ifelse(master_df$Income >= 31.5,0.23,'NA'))



summary(as.numeric(master_df$Income_WOE))




str(master_df)

master_df$No.of.months.in.current.residence_WOE <- as.numeric(master_df$No.of.months.in.current.residence_WOE)
master_df$No.of.times.90.DPD.or.worse.in.last.6.months_WOE <- as.numeric(master_df$No.of.times.90.DPD.or.worse.in.last.6.months_WOE)
master_df$No.of.trades.opened.in.last.6.months_WOE <- as.numeric(master_df$No.of.trades.opened.in.last.6.months_WOE)
master_df$No.of.times.60.DPD.or.worse.in.last.12.months_WOE <- as.numeric(master_df$No.of.times.60.DPD.or.worse.in.last.12.months_WOE)
master_df$Total.No.of.Trades_WOE <- as.numeric(master_df$Total.No.of.Trades_WOE)
master_df$Outsranding.Balance_woe <- as.numeric(master_df$Outsranding.Balance_woe)
master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe <- as.numeric(master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe)
master_df$No.of.trades.opened.in.last.12.months_woe <- as.numeric(master_df$No.of.trades.opened.in.last.12.months_woe)
master_df$Avgas.CC.Utilization.in.last.12.months_woe <- as.numeric(master_df$Avgas.CC.Utilization.in.last.12.months_woe)
master_df$No.of.trades.opened.in.last.12.months_woe <- as.numeric(master_df$No.of.trades.opened.in.last.12.months_woe)
master_df$Income_WOE <- as.numeric(master_df$Income_WOE)


master_df$Age <- scale(master_df$Age)
master_df$No.of.dependents <- scale(master_df$No.of.dependents)
master_df$No.of.months.in.current.company <- scale(master_df$No.of.months.in.current.company)


str(master_df)

colnames(master_df)


#########
####Final data set to for glm


master_df_fact <- master_df[,c(3,4,7,8,9)]
dummies<- dummy.data.frame(master_df_fact)



log_reg_numeric <- master_df[,c(2,5,11,12,26,29,30:46)]

log_reg <- cbind(log_reg_numeric,dummies)

str(log_reg)


######################           LOGISTIC MODEL 1       #################################
##################### DEMOGRAPHIC  DATA  - LOGISTIC MODEL ################################

# REMOVING DEMOGRAPHIC DATA SEPARATELY FOR THE MODEL 

colnames(log_reg)
log_reg_demographic <- log_reg[,c(1,2,3,4,22:45)]

colnames(log_reg_demographic)


set.seed(1)


split_indices <- sample.split(log_reg_demographic$Performance.Tag.x, SplitRatio = 0.75)

train_d<-log_reg_demographic[split_indices,]

test_d <- log_reg_demographic[!split_indices, ]

nrow(train_d)/nrow(log_reg_demographic)

summary(train_d$Performance.Tag.x) # 0 = 49456 ; 1 = 2174 ; rate = 4.21%

nrow(test_d)/nrow(log_reg_demographic)

summary(test_d$Performance.Tag.x) # 0 = 16486 ; 1 = 725 ; rate = 4.21%


### Logistic Regression

logistic_1_demog <- glm(Performance.Tag.x ~ ., family = "binomial", data = train_d)

summary(logistic_1_demog)

logistic_2_demog<- stepAIC(logistic_1_demog, direction="both")
summary(logistic_2_demog)
vif(logistic_2_demog)

final_model_demog <- logistic_2_demog

save(final_model_demog, file = "final_model_demog.rda")

########################    MODEL EVALUATION : LOGISTIC MODEL 1   ######################################
###################### DEMOGRAPHIC DATA LOGISTIC MODEL ###############


### Test Data ####

## load the model
load("final_model_demog.rda")
summary(final_model_demog)

test_pred_demog = predict(final_model_demog, type = "response", 
                    newdata = test_d[,-4])


# Let's see the summary 

summary(test_pred_demog)

test_d$prob <- test_pred_demog


test_conf <- confusionMatrix(test_pred_bad_rate, test_actual_bad_rate, positive = "Yes")
test_conf

test_actual_bad_rate

perform_fn <- function(cutoff) 
{
  predicted_bad_rate <- factor(ifelse(test_pred_demog >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_bad_rate, test_actual_bad_rate, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_demog <- s[which(abs(OUT[,1]-OUT[,2])<0.06)]

cutoff_demog

# Final cutoff 


library(e1071)


# Let's use the probability cutoff of 5%.

test_pred_bad_rate <- factor(ifelse(test_pred_demog >= 0.03969, "Yes", "No"))
test_actual_bad_rate <- factor(ifelse(test_d$Performance.Tag.x==1,"Yes","No"))

table(test_pred_bad_rate,test_actual_bad_rate)


test_conf <- confusionMatrix(test_pred_bad_rate, test_actual_bad_rate, positive = "Yes")
test_conf

acc <- test_conf$overall[1]

sens <- test_conf$byClass[1]

spec <- test_conf$byClass[2]

acc

sens

spec

#Accuracy 
#0.5369241 

#Sensitivity 
#0.5848276 

#Specificity 
#0.5348174





# plotting the lift chart

# Loading dplyr package 

lift <- function(labels , predicted_probs, groups) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_probs)) predicted_probs <- as.integer(as.character(predicted_probs))
  helper = data.frame(cbind(labels , predicted_probs))
  helper[,"bucket"] = ntile(-helper[,"predicted_probs"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 






LG = lift(test_d$Performance.Tag.x, test_pred_demog , groups = 10)


# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of defaulted")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

View(LG)

LG$Total_nonresp <- LG$total - LG$totalresp
LG$Cum_nonresp <- cumsum(LG$Total_nonresp)
LG$Cumresp_perct <- LG$Cumresp/725*100
LG$Cum_nonresp_perct <- LG$Cum_nonresp/16486*100
LG$KS <- LG$Cumresp_perct - LG$Cum_nonresp_perct 
KS <- max(LG$KS)
KS





accuracy.meas(response = test_actual_bad_rate, predicted = test_pred_bad_rate  )
roc.curve(response = test_actual_bad_rate, predicted = test_pred_bad_rate , plotit = T)
#precision: 0.042
#recall: 1.000
#F: 0.040
#Area under the curve (AUC): 0.56








########################    LOGISTIC MODEL 2   ######################################
###################### cREDIT bUREAU DATA + DEMOGRAPHIC DATA LOGISTIC MODEL ###############





set.seed(1)


split_indices <- sample.split(log_reg$Performance.Tag.x, SplitRatio = 0.75)

train<-log_reg[split_indices,]

test <- log_reg[!split_indices, ]
test_rose <- log_reg[!split_indices, ] 

nrow(train)/nrow(log_reg)

summary(train$Performance.Tag.x) # 0 - 52754 1 - 2319 ; rate = 4.21%

nrow(test)/nrow(log_reg)

summary(test$Performance.Tag.x) # 0 - 13188 1 - 580 ; rate = 4.21%

logistic_1 <- glm(Performance.Tag.x ~ ., family = "binomial", data = train)

summary(logistic_1)

logistic_2<- stepAIC(logistic_1, direction="both")
summary(logistic_2)
vif(logistic_2)

logistic_3  <- glm(formula = Performance.Tag.x ~ No.of.months.in.current.company + 
      Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe + 
      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe 
       + No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
      No.of.times.30.DPD.or.worse.in.last.12.months_woe + ProfessionSE, 
    family = "binomial", data = train)

summary(logistic_3)
vif(logistic_3)

final_model_logistic_original_data<-logistic_3

save(final_model_logistic_original_data, file = "final_model_logistic_original_data.rda")


########################  MODEL EVALUATION :  LOGISTIC MODEL 2   ######################################
###################### cREDIT bUREAU DATA + DEMOGRAPHIC DATA LOGISTIC MODEL ###############


### Test Data ####

## load the model
load("final_model_logistic_original_data.rda")
test <- test[,1:45]
test <- log_reg[!split_indices, ]
 
summary(final_model_logistic_original_data)


test_pred = predict(final_model_logistic_original_data, type = "response", 
                    newdata = test[,-4])

head(test)

# Let's see the summary 

summary(test_pred)

#test$prob <- test_pred
#table(predicted_bad_rate)

perform_fn <- function(cutoff) 
{
  predicted_bad_rate <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_bad_rate, test_actual_bad_rate, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


s = seq(.01,0.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.03)]

cutoff # Final cutoff 4.9%



# Let's use the probability cutoff of 4.95%.

test_pred_bad_rate <- factor(ifelse(test_pred >= 0.04959, "Yes", "No"))
test_actual_bad_rate <- factor(ifelse(test$Performance.Tag.x==1,"Yes","No"))

table(test_pred_bad_rate,test_actual_bad_rate)

test_conf <- confusionMatrix(test_pred_bad_rate, test_actual_bad_rate, positive = "Yes")
test_conf


acc <- test_conf$overall[1]

sens <- test_conf$byClass[1]

spec <- test_conf$byClass[2]

acc

sens

spec

#Accuracy 
# 63.76% 

#Sensitivity 
# 61.62% 

#Specificity 
# 64.07%



accuracy.meas(response = test_actual_bad_rate, predicted = test_pred_bad_rate  )
#precision: 0.042
#recall: 1.000
#F: 0.040
roc.curve(response = test_actual_bad_rate, predicted = test_pred_bad_rate , plotit = T)
#Area under the curve (AUC): 0.625

accmeas <- accuracy.meas(response = test_actual_bad_rate, predicted = test_pred_bad_rate  )
prec<-accmeas$precision
rec<-accmeas$recall

prec
rec


# sorting the probabilities in decreasing order 


test_predit_all_orig <- data.frame(cbind(test_pred,as.numeric(as.character(test$Performance.Tag.x))))
colnames(test_predit_all_orig) <- c("test_pred","Performance.Tag.x")

# sorting the probabilities in decreasing order 
test_predit_all_orig <- test_predit_all_orig[order(test_predit_all_orig$test_pred, decreasing = T), ]




# plotting the lift chart

# Loading dplyr package 

lift <- function(labels , predicted_probs, groups) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_probs)) predicted_probs <- as.integer(as.character(predicted_probs))
  helper = data.frame(cbind(labels , predicted_probs))
  helper[,"bucket"] = ntile(-helper[,"predicted_probs"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 


LG = lift(test_predit_all_orig$Performance.Tag.x, test_predit_all_orig$test_pred, groups = 10)


# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of defaulted")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="Decile",ylab = "Lift")


LG$Total_nonresp <- LG$total - LG$totalresp
LG$Cum_nonresp <- cumsum(LG$Total_nonresp)
LG$Cumresp_perct <- LG$Cumresp/725*100
LG$Cum_nonresp_perct <- LG$Cum_nonresp/16486*100
LG$KS <- LG$Cumresp_perct - LG$Cum_nonresp_perct 
KS <- max(LG$KS)
KS







############################################################################################

############ Imbalanced data tackling SMOTE using ROSE library #####################################



accuracy.meas(response = test_actual_bad_rate, predicted = test_pred_bad_rate  )
roc.curve(response = test_actual_bad_rate, predicted = test_pred_bad_rate , plotit = T)
#precision: 0.042
#recall: 1.000
#F: 0.040
#Area under the curve (AUC): 0.632


names(train) <-make.names(names(train)) 
str(train)

train_factor <- data.frame(sapply(train,function(x)factor(x)))
str(train_factor)

data.rose <- ROSE(Performance.Tag.x ~ .,data=train_factor,seed=1)$data

table(data.rose$Performance.Tag.x)

table(train$Performance.Tag.x)


#sapply(Combined_data, function(x) length(which(is.na(x))))


### Model 1: Logistic Regression

data.rose_final <- data.frame(sapply(data.rose,function(x) as.numeric(as.character(x))))

str(data.rose_final)

logistic_1 <- glm(Performance.Tag.x ~ ., family = "binomial", data = data.rose_final)

summary(logistic_1)

logistic_2_rose<- stepAIC(logistic_1, direction="both")
summary(logistic_2_rose)
vif(logistic_2_rose)




logistic_3_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
  No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
  No.of.PL.trades.opened.in.last.12.months_woe + Outsranding.Balance_woe + 
  No.of.times.30.DPD.or.worse.in.last.6.months_WoE + No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
   No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
  No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
  Income_WOE + GenderF + Marital.Status..at.the.time.of.application.Married + 
  EducationMasters + EducationOthers + Profession + ProfessionSAL + 
  ProfessionSE + Type.of.residence + Type.of.residenceCompany.provided, family = "binomial", data = data.rose_final)

summary(logistic_3_rose)
vif(logistic_3_rose)



logistic_4_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                         No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                         No.of.PL.trades.opened.in.last.12.months_woe + Outsranding.Balance_woe + 
                          No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                         No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                         No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                         Income_WOE + GenderF + Marital.Status..at.the.time.of.application.Married + 
                         EducationMasters + EducationOthers + Profession + ProfessionSAL + 
                         ProfessionSE + Type.of.residence + Type.of.residenceCompany.provided, family = "binomial", data = data.rose_final)

summary(logistic_4_rose)
vif(logistic_4_rose)


logistic_5_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                         No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                         No.of.PL.trades.opened.in.last.12.months_woe + Outsranding.Balance_woe + 
                         No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                         No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                         No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                         Income_WOE + GenderF + Marital.Status..at.the.time.of.application.Married + 
                         EducationMasters + EducationOthers  + ProfessionSAL + 
                         ProfessionSE + Type.of.residence + Type.of.residenceCompany.provided, family = "binomial", data = data.rose_final)

summary(logistic_5_rose)
vif(logistic_5_rose)


logistic_6_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                         No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                         No.of.PL.trades.opened.in.last.12.months_woe + Outsranding.Balance_woe + 
                         No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                         No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                         No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                         Income_WOE + GenderF + Marital.Status..at.the.time.of.application.Married + 
                         EducationMasters + EducationOthers  + ProfessionSAL + 
                         ProfessionSE  + Type.of.residenceCompany.provided, family = "binomial", data = data.rose_final)

summary(logistic_6_rose)
vif(logistic_6_rose)




logistic_7_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                         No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                         No.of.PL.trades.opened.in.last.12.months_woe + Outsranding.Balance_woe + 
                         No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                         No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                         No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                         Income_WOE + GenderF + Marital.Status..at.the.time.of.application.Married + 
                         EducationMasters + EducationOthers  + ProfessionSAL + 
                         ProfessionSE , family = "binomial", data = data.rose_final)

summary(logistic_7_rose)
vif(logistic_7_rose)


logistic_8_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                         No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                          Outsranding.Balance_woe + 
                         No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                         No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                         No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                         Income_WOE + GenderF + Marital.Status..at.the.time.of.application.Married + 
                         EducationMasters + EducationOthers  + ProfessionSAL + 
                         ProfessionSE , family = "binomial", data = data.rose_final)

summary(logistic_8_rose)
vif(logistic_8_rose)


logistic_9_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                         No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                         Outsranding.Balance_woe + 
                         No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                         No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                         No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                         Income_WOE + GenderF + Marital.Status..at.the.time.of.application.Married + 
                         EducationMasters   + ProfessionSAL + 
                         ProfessionSE , family = "binomial", data = data.rose_final)

summary(logistic_9_rose)
vif(logistic_9_rose)



logistic_10_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                         No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                         Outsranding.Balance_woe + 
                         No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                          
                         No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                         Income_WOE + GenderF + Marital.Status..at.the.time.of.application.Married + 
                         EducationMasters   + ProfessionSAL + 
                         ProfessionSE , family = "binomial", data = data.rose_final)

summary(logistic_10_rose)
vif(logistic_10_rose)


logistic_11_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                          No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                          Outsranding.Balance_woe + 
                          No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                          
                          No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                          Income_WOE + GenderF + Marital.Status..at.the.time.of.application.Married + 
                          EducationMasters   + ProfessionSAL  
                           , family = "binomial", data = data.rose_final)

summary(logistic_11_rose)
vif(logistic_11_rose)



logistic_12_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                          No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                          Outsranding.Balance_woe + 
                          No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                          
                          No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                          Income_WOE + GenderF  + 
                          EducationMasters   + ProfessionSAL  
                        , family = "binomial", data = data.rose_final)

summary(logistic_12_rose)
vif(logistic_12_rose)






logistic_13_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                          No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                          Outsranding.Balance_woe + 
                          No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                          
                          No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                           GenderF  + 
                          EducationMasters   + ProfessionSAL  
                        , family = "binomial", data = data.rose_final)

summary(logistic_13_rose)
vif(logistic_13_rose)


logistic_14_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                          No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                          Outsranding.Balance_woe + 
                          No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                          
                          No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_WOE + 
                          GenderF  
                             + ProfessionSAL  
                        , family = "binomial", data = data.rose_final)

summary(logistic_14_rose)
vif(logistic_14_rose)


logistic_15_rose <- glm(formula = data.rose_final$Performance.Tag.x ~ No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months_woe + 
                          No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                          Outsranding.Balance_woe + 
                           
                          
                          No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                            
                        + ProfessionSAL  
                        , family = "binomial", data = data.rose_final)

summary(logistic_15_rose)
vif(logistic_15_rose)



final_model_rose_logistic<-logistic_15_rose

save(final_model_rose_logistic,file = "final_model_rose_logistic.rda")


#######################################################################

### Model Evaluation

### Test Data ####

load("final_model_rose_logistic.rda")

test <- test[,1:45]
test <- data.frame(sapply(test,function(x) as.numeric(as.character(x))))
str(test)

str(data.rose_final)
test_pred = predict(final_model_rose_logistic, type = "response", 
                    newdata = test[,-4])

head(test)

# Let's see the summary 

summary(test_pred)

#test$prob <- test_pred

test_pred_bad_rate <- factor(ifelse(test_pred >= 0.05, "Yes", "No"))
test_actual_bad_rate <- factor(ifelse(test$Performance.Tag.x==1,"Yes","No"))

table(test_pred_bad_rate,test_actual_bad_rate)


test_conf <- confusionMatrix(test_pred_bad_rate, test_actual_bad_rate, positive = "Yes")
test_conf



perform_fn <- function(cutoff) 
{
  predicted_bad_rate <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_bad_rate, test_actual_bad_rate, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.03)]

cutoff

# Final cutoff 

test_pred_bad_rate <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
test_actual_bad_rate <- factor(ifelse(test$Performance.Tag.x==1,"Yes","No"))

table(test_pred_bad_rate,test_actual_bad_rate)


test_conf <- confusionMatrix(test_pred_bad_rate, test_actual_bad_rate, positive = "Yes")
test_conf



acc <- test_conf$overall[1]

sens <- test_conf$byClass[1]

spec <- test_conf$byClass[2]

acc

sens

spec



#Accuracy for logistic_15_rose
#0.6363

#Sensitivity for logistic_15_rose 
#0.6124 

#Specificity for logistic_15_rose
#0.6374

accuracy.meas(response = test_actual_bad_rate, predicted = test_pred_bad_rate  )
#precision: 0.042
#recall: 1.000
#F: 0.040
roc.curve(response = test_actual_bad_rate, predicted = test_pred_bad_rate , plotit = T)
#Area under the curve (AUC): 0.625

accmeas <- accuracy.meas(response = test_actual_bad_rate, predicted = test_pred_bad_rate  )
prec<-accmeas$precision
rec<-accmeas$recall

prec
rec


# sorting the probabilities in decreasing order 


test_predit_all_rose <- data.frame(cbind(test_pred,as.numeric(as.character(test$Performance.Tag.x))))
colnames(test_predit_all_rose) <- c("test_pred","Performance.Tag.x")

# sorting the probabilities in decreasing order 
test_predit_all_rose <- test_predit_all_rose[order(test_predit_all_rose$test_pred, decreasing = T), ]


# plotting the lift chart

# Loading dplyr package 
library(dplyr)

lift <- function(labels , predicted_probs, groups) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_probs)) predicted_probs <- as.integer(as.character(predicted_probs))
  helper = data.frame(cbind(labels , predicted_probs))
  helper[,"bucket"] = ntile(-helper[,"predicted_probs"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 


LG = lift(test_predit_all_rose$Performance.Tag.x, test_predit_all_rose$test_pred, groups = 10)


# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of defaulted")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="Decile",ylab = "Lift")

View(LG)

LG$Total_nonresp <- LG$total - LG$totalresp
LG$Cum_nonresp <- cumsum(LG$Total_nonresp)
LG$Cumresp_perct <- LG$Cumresp/725*100
LG$Cum_nonresp_perct <- LG$Cum_nonresp/16486*100
LG$KS <- LG$Cumresp_perct - LG$Cum_nonresp_perct 
KS <- max(LG$KS)
KS







########################  MODEL 4 Random Forest    ######################################
######################  On Original data CREDIT bUREAU DATA + DEMOGRAPHIC DATA  ###############


##################### Data preparation for random forrest


k1 <- train
nrow(k1)
# checking test data set =  train
str(train)


# checking test data set =  test
str(test)

names(test) <-make.names(names(test)) 


#---------------------------------------------------------    


# Random forrest  Building the model demog + credit bureau
#### Trial 1
k1_rf_orig <- randomForest(k1$Performance.Tag.x ~., data = k1,ntree= 500,  importance =TRUE, proximity = FALSE, do.trace = TRUE, mtry = 5, na.action = na.omit) 
# sensi -  speci-    accu -  


k1_rf_orig

save(k1_rf_orig, file = "k1_rf_orig.rda")



# Predict response for test data

load("k1_rf_orig.rda")


rf_pred_orig <- predict(k1_rf_orig, test[,-4], type = "prob",)
summary(rf_pred_orig)
#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response_rf <- as.factor(ifelse(rf_pred_orig[,2] >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response_rf, test$Performance.Tag.x, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,0.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.15)]

# The plot shows that cutoff value of around 22% optimises sensitivity and accuracy

predicted_response_22_orig <- factor(ifelse(rf_pred_orig[, 2] >= 0.01989899, "1", "0"))

conf_forest <- confusionMatrix(predicted_response_22_orig, test[, 4], positive = "1")

conf_forest

# Sensitivity
conf_forest$byClass[1] #62%

# Specificity 
conf_forest$byClass[2] # 54%

# Accuracy 
conf_forest$overall[1]#54%

test_pred_bad_rate <- factor(ifelse(rf_pred_orig[, 2] >= 0.01989899, "Yes", "No"))



accuracy.meas(response = test_actual_bad_rate, predicted = test_pred_bad_rate  )
#precision: 0.042
#recall: 1.000
#F: 0.040
roc.curve(response = test_actual_bad_rate, predicted = test_pred_bad_rate , plotit = T)
#Area under the curve (AUC): 0.60

accmeas <- accuracy.meas(response = test_actual_bad_rate, predicted = test_pred_bad_rate  )
prec<-accmeas$precision
rec<-accmeas$recall

prec
rec

# sorting the probabilities in decreasing order 

test$RF_pred_orig <- rf_pred_orig[, 2]
rf_test_predit_orig <- data.frame(cbind(test$RF_pred_orig,as.numeric(as.character(test$Performance.Tag.x))))
colnames(rf_test_predit_orig) <- c("RF_pred_orig","Performance.Tag.x")

# sorting the probabilities in decreasing order 
rf_test_predit_orig <- rf_test_predit_orig[order(rf_test_predit_orig$RF_pred, decreasing = T), ]



# plotting the lift chart

# Loading dplyr package 

lift <- function(labels , predicted_probs, groups) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_probs)) predicted_probs <- as.integer(as.character(predicted_probs))
  helper = data.frame(cbind(labels , predicted_probs))
  helper[,"bucket"] = ntile(-helper[,"predicted_probs"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = FALSE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 


LG = lift(rf_test_predit_orig$Performance.Tag.x, rf_test_predit_orig$RF_pred_orig, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of defaulted")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="Decile",ylab = "Lift")

View(LG)

LG$Total_nonresp <- LG$total - LG$totalresp
LG$Cum_nonresp <- cumsum(LG$Total_nonresp)
LG$Cumresp_perct <- LG$Cumresp/725*100
LG$Cum_nonresp_perct <- LG$Cum_nonresp/16486*100
LG$KS <- LG$Cumresp_perct - LG$Cum_nonresp_perct 
KS <- max(LG$KS)
KS


importance_orig <- k1_rf_orig$importance 

importance_orig <- data.frame(importance_orig)
View(importance_orig)











########################  MODEL 5 Random Forest    ######################################
######################  On SMOTE data CREDIT bUREAU DATA + DEMOGRAPHIC DATA  ###############


##################### Data preparation for random forrest



test <- test[,1:45]
str(test)


# checking train data set =  data.rose_final
str(data.rose_final)

test_final <- data.frame(sapply(test,function(x) as.numeric(as.character(x))))

# checking test data set =  test_final
str(test_final)


#---------------------------------------------------------    


data.rose_final$Performance.Tag.x<-as.factor(data.rose_final$Performance.Tag.x)
# Random forrest  Building the model demog + credit bureau
#### Trial 1
k1_rf <- randomForest(data.rose_final$Performance.Tag.x ~., data = data.rose_final,ntree= 500,  importance =TRUE, proximity = FALSE, do.trace = TRUE, mtry = 5, na.action = na.omit) 
# sensi -62 speci- 60   accu -60 


save(k1_rf, file = "k1_rf.rda")

########################    MODEL EVALUATION : LOGISTIC MODEL 1   ######################################
###################### DEMOGRAPHIC DATA LOGISTIC MODEL ###############


### Test Data ####

## load the model
load("k1_rf.rda")


##Trial 2
#  k1_rf_2 <- randomForest(train_rf$Performance.Tag.y ~., data = train_rf,ntree= 500,  importance =TRUE, proximity = FALSE, do.trace = TRUE, mtry = 4, na.action = na.omit)
# sensi -51 speci- 64  accu -64


## Trial 3 
#  k1_rf_3 <- randomForest(train_rf$Performance.Tag.y ~., data = train_rf,ntree= 600,  importance =TRUE, proximity = FALSE, do.trace = TRUE, mtry = 5, na.action = na.omit) 
# sensi -51 speci- 64  accu -64


k1_rf

# Predict response for test data

rf_pred <- predict(k1_rf, test_final[,-4], type = "prob",na.action = na.omit)
summary(rf_pred)
#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response_rf <- as.factor(ifelse(rf_pred[,2] >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response_rf, test_final$Performance.Tag.x, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,0.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.02)]

# The plot shows that cutoff value of around 22% optimises sensitivity and accuracy

predicted_response_22 <- factor(ifelse(rf_pred[, 2] >= 0.287171, "1", "0"))

conf_forest <- confusionMatrix(predicted_response_22, test_final[, 4], positive = "1")

conf_forest

# Sensitivity
conf_forest$byClass[1] #60%

# Specificity 
conf_forest$byClass[2] # 61%

# Accuracy 
conf_forest$overall[1]#61%

test_pred_bad_rate <- factor(ifelse(rf_pred[, 2] >= 0.287171, "Yes", "No"))



accuracy.meas(response = test_actual_bad_rate, predicted = test_pred_bad_rate  )
#precision: 0.042
#recall: 1.000
#F: 0.040
roc.curve(response = test_actual_bad_rate, predicted = test_pred_bad_rate , plotit = T)
#Area under the curve (AUC): 0.60

accmeas <- accuracy.meas(response = test_actual_bad_rate, predicted = test_pred_bad_rate  )
prec<-accmeas$precision
rec<-accmeas$recall

prec
rec

# sorting the probabilities in decreasing order 

test_final$RF_pred <- rf_pred[, 2]
rf_test_predit <- data.frame(cbind(test_final$RF_pred,test_final$Performance.Tag.x))
colnames(rf_test_predit) <- c("RF_pred","Performance.Tag.x")

View(test_final)




# sorting the probabilities in decreasing order 
rf_test_predit <- rf_test_predit[order(rf_test_predit$RF_pred, decreasing = T), ]



# plotting the lift chart

# Loading dplyr package 

lift <- function(labels , predicted_probs, groups) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_probs)) predicted_probs <- as.integer(as.character(predicted_probs))
  helper = data.frame(cbind(labels , predicted_probs))
  helper[,"bucket"] = ntile(-helper[,"predicted_probs"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 


LG = lift(rf_test_predit$Performance.Tag.x, rf_test_predit$RF_pred, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of defaulted")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="Decile",ylab = "Lift")

View(LG)

LG$Total_nonresp <- LG$total - LG$totalresp
LG$Cum_nonresp <- cumsum(LG$Total_nonresp)
LG$Cumresp_perct <- LG$Cumresp/725*100
LG$Cum_nonresp_perct <- LG$Cum_nonresp/16486*100
LG$KS <- LG$Cumresp_perct - LG$Cum_nonresp_perct 
KS <- max(LG$KS)
KS






# Final RF important variables
importance_final <- k1_rf$importance 

importance_final<- data.frame(importance_final)

write.csv(importance_final,file = "importance_final.csv")


View(rf_test_predit)

# capping the minimum prob to 0.0001 to avoid Infinite score

rf_test_predit$RF_pred <- ifelse(rf_test_predit$RF_pred == 0.00,0.0001 ,rf_test_predit$RF_pred)

####################################################################################################
####################################### APPLICAITON SCORECARD ########################################


rf_test_predit$SCORE <- round(333.56 + 28.8539 * log((1-rf_test_predit$RF_pred)/rf_test_predit$RF_pred))
View(rf_test_predit)
summary(rf_test_predit)



################################### CALCULATING CUT OFF SCORE  #######################


### we have a cutoff of 0.554444 for the probability by which we get the optimum accuracy, sensitivity and specificity
## we use this cutoff to calculate the score cutoff 


SCORE_CUTOFF <- max(rf_test_predit$SCORE[which((0.2871717 - round(rf_test_predit$RF_pred,3)) <= 0.0)]) 

SCORE_CUTOFF 
# 360

# we get a score cut off of 360 below which we reject the credit card application





############################ DATA PREPARATION FOR TESTING ON REJECTED POPULATION ###################################3

#####  Creating WOE bins and replicating the treatment for rejected population to pass through logistic test

master_df_reject <- rejected_pop


class(master_df_reject$Performance.Tag.x)

master_df_reject$Performance.Tag.x<-as.factor(master_df_reject$Performance.Tag.x)
row.names(master_df_reject) <- 1:nrow(master_df_reject) 


master_df_reject$Avgas.CC.Utilization.in.last.12.months_woe<-ifelse(master_df_reject$Avgas.CC.Utilization.in.last.12.months < 16.5,0.67,
                                                                    ifelse(master_df_reject$Avgas.CC.Utilization.in.last.12.months>=16.5,-0.46,0.67))


master_df_reject$Avgas.CC.Utilization.in.last.12.months_woe <- ifelse(is.na(master_df_reject$Avgas.CC.Utilization.in.last.12.months_woe) == TRUE ,0.67,master_df_reject$Avgas.CC.Utilization.in.last.12.months_woe)

summary(as.numeric(master_df_reject$Avgas.CC.Utilization.in.last.12.months_woe))
#2)For second Variable Avgas CC Utilization in last 12 months

master_df_reject$No.of.trades.opened.in.last.12.months_woe<-ifelse(master_df_reject$No.of.trades.opened.in.last.12.months < 2.5,0.9193,
                                                                   ifelse(master_df_reject$No.of.trades.opened.in.last.12.months>=2.5 & master_df_reject$No.of.trades.opened.in.last.12.months < 5.5,-0.0759,
                                                                          ifelse(master_df_reject$No.of.trades.opened.in.last.12.months>=5.5 & master_df_reject$No.of.trades.opened.in.last.12.months < 12.5,-0.502,
                                                                                 ifelse(master_df_reject$No.of.trades.opened.in.last.12.months>=12.5,-0.0053,'NA'))))
summary(as.numeric(master_df_reject$No.of.trades.opened.in.last.12.months_woe))
#3)For third Variable No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.

master_df_reject$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe<-ifelse(master_df_reject$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. < 0.5,1.15,
                                                                                             ifelse(master_df_reject$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>=0.5,-0.25,'NA'))

summary(as.numeric(master_df_reject$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe))

#4)For the 4 th variable No.of.PL.trades.opened.in.last.12.months

master_df_reject$No.of.PL.trades.opened.in.last.12.months_woe<-ifelse(master_df_reject$No.of.PL.trades.opened.in.last.12.months < 1.5,0.72,-0.37)


summary(as.numeric(master_df_reject$No.of.PL.trades.opened.in.last.12.months_woe))


#5)For the 5 th Variable Outstanding.Balance

master_df_reject$Outsranding.Balance_woe <-ifelse(master_df_reject$Outstanding.Balance < 210700,0.81,
                                                  ifelse(master_df_reject$Outstanding.Balance >=210700 & master_df_reject$Outstanding.Balance < 1322000,-0.37,
                                                         ifelse(master_df_reject$Outstanding.Balance>=1322000 & master_df_reject$Outstanding.Balance < 3262000,0.56,
                                                                ifelse(master_df_reject$Outstanding.Balance>=3262000,-0.29,'NA'))))

summary(as.numeric(master_df_reject$Outsranding.Balance_woe))
summary(master_df_reject$Outstanding.Balance)
#6) FOr the 6 th variable 

master_df_reject$Total.No.of.Trades_WOE <- ifelse(master_df_reject$Total.No.of.Trades < 4.5,0.724,
                                                  ifelse(master_df_reject$Total.No.of.Trades >=4.5 & master_df_reject$Outstanding.Balance < 7.5,-0.127,
                                                         ifelse(master_df_reject$Outstanding.Balance>=7.5 & master_df_reject$Outstanding.Balance < 15.5,-0.5276,
                                                                ifelse(master_df_reject$Outstanding.Balance>=15.5,0.098,'NA'))))




#7) FOR THE 7 TH VARIABLE No.of.times.30.DPD.or.worse.in.last.6.months


master_df_reject$No.of.times.30.DPD.or.worse.in.last.6.months_WoE<-ifelse(master_df_reject$No.of.times.30.DPD.or.worse.in.last.6.months < 0.5,0.40,-0.62)


summary(as.numeric(master_df_reject$No.of.times.30.DPD.or.worse.in.last.6.months_WoE))

#8)No.of.PL.trades.opened.in.last.6.months


master_df_reject$No.of.PL.trades.opened.in.last.6.months_woe <- ifelse(master_df_reject$No.of.PL.trades.opened.in.last.6.months < 0.5,0.68,-0.33)

summary(as.numeric(master_df_reject$No.of.PL.trades.opened.in.last.6.months_woe))

### for the 8 th variable No.of.times.90.DPD.or.worse.in.last.12.months


master_df_reject$No.of.times.90.DPD.or.worse.in.last.12.months_woe <- ifelse(master_df_reject$No.of.times.90.DPD.or.worse.in.last.12.months < 0.5,0.36,-0.60)

summary(as.numeric(master_df_reject$No.of.times.90.DPD.or.worse.in.last.12.months_woe))

##For the 9th variable

master_df_reject$No.of.times.60.DPD.or.worse.in.last.6.months_woe <- ifelse(master_df_reject$No.of.times.60.DPD.or.worse.in.last.6.months < 0.5,0.34,-0.62)

summary(as.numeric(master_df_reject$No.of.times.60.DPD.or.worse.in.last.6.months_woe))

###For the 10th variable No.of.Inquiries.in.last.6.months..excluding.home...auto.loans

master_df_reject$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe <- ifelse(master_df_reject$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. < 0.5,0.76,-0.26)

summary(as.numeric(master_df_reject$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe))

#11) No.of.times.30.DPD.or.worse.in.last.12.months


master_df_reject$No.of.times.30.DPD.or.worse.in.last.12.months_woe<-ifelse(master_df_reject$No.of.times.30.DPD.or.worse.in.last.12.months < 1.5,0.27,-0.71)

summary(as.numeric(master_df_reject$No.of.times.30.DPD.or.worse.in.last.12.months_woe))

#12) No.of.times.60.DPD.or.worse.in.last.12.months


master_df_reject$No.of.times.60.DPD.or.worse.in.last.12.months_WOE <-ifelse(master_df_reject$No.of.times.60.DPD.or.worse.in.last.12.months < 0.5 , 0.36,
                                                                            ifelse(master_df_reject$No.of.times.60.DPD.or.worse.in.last.12.months >= 0.5 & master_df_reject$No.of.times.60.DPD.or.worse.in.last.12.months < 1.5,-0.22,
                                                                                   ifelse(master_df_reject$No.of.times.60.DPD.or.worse.in.last.12.months >= 1.5,-0.69,'NA')))


summary(as.numeric(master_df_reject$No.of.times.60.DPD.or.worse.in.last.12.months_WOE))

#13)No.of.trades.opened.in.last.6.months

master_df_reject$No.of.trades.opened.in.last.6.months_WOE <-ifelse(master_df_reject$No.of.trades.opened.in.last.6.months < 1.5,0.57,
                                                                   ifelse(master_df_reject$No.of.trades.opened.in.last.6.months >= 1.5,-0.32,'NA'))



summary(as.numeric(master_df_reject$No.of.trades.opened.in.last.6.months))


#14)No.of.times.90.DPD.or.worse.in.last.6.months

master_df_reject$No.of.times.90.DPD.or.worse.in.last.6.months_WOE <-ifelse(master_df_reject$No.of.times.90.DPD.or.worse.in.last.6.months < 0.5,0.27,
                                                                           ifelse(master_df_reject$No.of.times.90.DPD.or.worse.in.last.6.months >= 0.5,-0.62,'NA'))



summary(as.numeric(master_df_reject$No.of.times.90.DPD.or.worse.in.last.6.months_WOE))


#15)No.of.months.in.current.residence


master_df_reject$No.of.months.in.current.residence_WOE <-ifelse(master_df_reject$No.of.months.in.current.residence < 6.5,0.322,
                                                                ifelse(master_df_reject$No.of.months.in.current.residence >= 6.5 & master_df_reject$No.of.months.in.current.residence<34.5,-0.510,
                                                                       ifelse(master_df_reject$No.of.months.in.current.residence >= 34.5, -0.099,'NA')))



summary(as.numeric(master_df_reject$No.of.months.in.current.residence_WOE))


#16)Income


master_df_reject$Income_WOE <-ifelse(master_df_reject$Income < 31.5,-0.14,
                                     ifelse(master_df_reject$Income >= 31.5,0.23,'NA'))



summary(as.numeric(master_df_reject$Income_WOE))




str(master_df_reject)

master_df_reject$No.of.months.in.current.residence_WOE <- as.numeric(master_df_reject$No.of.months.in.current.residence_WOE)
master_df_reject$No.of.times.90.DPD.or.worse.in.last.6.months_WOE <- as.numeric(master_df_reject$No.of.times.90.DPD.or.worse.in.last.6.months_WOE)
master_df_reject$No.of.trades.opened.in.last.6.months_WOE <- as.numeric(master_df_reject$No.of.trades.opened.in.last.6.months_WOE)
master_df_reject$No.of.times.60.DPD.or.worse.in.last.12.months_WOE <- as.numeric(master_df_reject$No.of.times.60.DPD.or.worse.in.last.12.months_WOE)
master_df_reject$Total.No.of.Trades_WOE <- as.numeric(master_df_reject$Total.No.of.Trades_WOE)
master_df_reject$Outsranding.Balance_woe <- as.numeric(master_df_reject$Outsranding.Balance_woe)
master_df_reject$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe <- as.numeric(master_df_reject$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe)
master_df_reject$No.of.trades.opened.in.last.12.months_woe <- as.numeric(master_df_reject$No.of.trades.opened.in.last.12.months_woe)
master_df_reject$Avgas.CC.Utilization.in.last.12.months_woe <- as.numeric(master_df_reject$Avgas.CC.Utilization.in.last.12.months_woe)
master_df_reject$No.of.trades.opened.in.last.12.months_woe <- as.numeric(master_df_reject$No.of.trades.opened.in.last.12.months_woe)
master_df_reject$Income_WOE <- as.numeric(master_df_reject$Income_WOE)


master_df_reject$Age <- scale(master_df_reject$Age)
master_df_reject$No.of.dependents <- scale(master_df_reject$No.of.dependents)
master_df_reject$No.of.months.in.current.company <- scale(master_df_reject$No.of.months.in.current.company)


str(master_df_reject)

colnames(master_df_reject)


#########
####Final data set to for glm


master_df_reject_fact <- master_df_reject[,c(3,4,7,8,9)]
dummies_reject<- dummy.data.frame(master_df_reject_fact)

log_reg_numeric_reject <- master_df_reject[,c(2,5,11,12,26,29,30:46)]

log_reg_reject <- cbind(log_reg_numeric_reject,dummies_reject)

str(log_reg_reject)

str(data.rose_final)

log_reg_reject <- data.frame(sapply(log_reg_reject,function(x) as.numeric(as.character(x))))
str(log_reg_reject)


log_reg_reject$Gender <- 0  
log_reg_reject$Marital.Status..at.the.time.of.application. <- 0  
log_reg_reject$Type.of.residence <- 0  



test_pred_reject = predict(k1_rf, type = "prob", newdata = log_reg_reject[,-4])


# Let's see the summary 

summary(test_pred_reject)
which(is.na(test_pred_reject)== TRUE)



log_reg_reject$prob_reject_final <- test_pred_reject[,2]

log_reg_reject$Score <- round(333.56 + 28.8539 * log((1-log_reg_reject$prob_reject_final)/log_reg_reject$prob_reject_final))

# Final cutoff 

test_pred_bad_rate_reject_final <- factor(ifelse(log_reg_reject$Score <= 360, "Yes", "No"))

table(test_pred_bad_rate_reject_final)
#YES 1359
#No    66

summary(log_reg_reject)





##################################################################################################


#################################### Financial analysis ###########################################



summary(rf_test_predit)


#categorising each class of confusion matrix Treu Negative, True Positive, False Positive and False Negative


# using existing cutoff :
rf_test_predit$Predicted_bad <- ifelse(rf_test_predit$RF_pred >= 0.287171, 1, 0)

summary(rf_test_predit)

rf_test_predit$Class <- ifelse(rf_test_predit$Performance.Tag.x == 1 & rf_test_predit$Predicted_bad == 1, "TP",
                               ifelse(rf_test_predit$Performance.Tag.x == 1 & rf_test_predit$Predicted_bad == 0, "FN",
                                      ifelse(rf_test_predit$Performance.Tag.x == 0 & rf_test_predit$Predicted_bad == 0, "TN","FP")))


table(rf_test_predit$Class)

library(plyr)

#Calculating Avg Prob of default in each class

Grp_by <- aggregate(rf_test_predit$RF_pred,list(rf_test_predit$Class),mean)
colnames(Grp_by) <- c("Class","Avg_Prob_of_default")
View(Grp_by)
Grp_by_1 <- summarise(rf_test_predit, count(rf_test_predit$Class))

View(Grp_by)
Grp_by <- cbind(Grp_by,Grp_by_1$'count(rf_test_predit$Class)')
Grp_by <- Grp_by[,-3]
colnames(Grp_by) <- c("Class","Avg_Prob_of_default","Count_of_application")

Grp_by$Percent_of_total <- Grp_by$Count_of_application/sum(Grp_by$Count_of_application)
View(Grp_by)



#Assumptions for financial analysis:

# Below are assumptions per customer :


#1.Avg .Outstanding Balance  at time of  0 DPD+  =  INR 20000
Avg_Outstanding_Balance_30plusDPD =  20000
  

#2.Interest rate per month
Monthly_Interest_rate  = 0.03

#3.No, of times 0DPD+ in 12 months = 2 time for 1 month
Avg_No_of_times_30plus_in_12months  = 2

#4.Years of usage of CredX card = 5 years
Num_of_Year_of_card_use = 5
  
#5.Avg Transactions per month in INR
Avg_Monthly_amt_in_transactions = 4000


#6.Transactor fee = 1 % of transaction amount 
Transactor_fee_rate = 0.01 

#7.Other late charges etc  
Charges = 250


#8.Number of applications during the scorecard is applicable and valid =  300000
Grp_by$No_of_applicant_during_scorecard <- 300000


#9.Loss given Default for credit cards = 0.70
Grp_by$Loss_given_default <- 0.7

#10.Exposure at default for credit cards = INR 75000
Grp_by$Exposure_at_default <- 75000

#10.Number of credit card applicants during the valid working duration of the scorecard
No_of_applicant_during_scorecard = 300000




View(Grp_by)

Grp_by$Expected_Credit_loss <- (Grp_by$Avg_Prob_of_default*Grp_by$Percent_of_total
                              *Grp_by$No_of_applicant_during_scorecard*Grp_by$Loss_given_default
                              *Grp_by$Exposure_at_default)/10000000 

View(Grp_by)

# We have granted loans to  True Negative and False Negative 
# We would have avoided giving loans to True Positive and False Positive 


# Thus Potential credit loss avoided would be Expected credit loss of True Positive and False Positive

Potential_credit_loss_avoided <- Grp_by$Expected_Credit_loss[which(Grp_by$Class=='TP')] + Grp_by$Expected_Credit_loss[which(Grp_by$Class=='FP')]

Potential_credit_loss_avoided
# INR 273 Crores




###################### For potential revenue loss calculation #######################33



Potential_Revenue_Loss_per_customer_over_lifetime = ((Avg_Outstanding_Balance_30plusDPD * Monthly_Interest_rate * Avg_No_of_times_30plus_in_12months * Num_of_Year_of_card_use)   

+ Charges + (Transactor_fee_rate *  Avg_Monthly_amt_in_transactions * 12 * Num_of_Year_of_card_use ))   


Potential_Revenue_Loss_per_customer_over_lifetime
# INR 8650 per customer is the revenue loss over customer lifetime


### The revenue loss will be for the True Positive and False Positive customers 

View(Grp_by)
Potential_Revenue_Loss_for_CredX = (Potential_Revenue_Loss_per_customer_over_lifetime * (Grp_by$Percent_of_total[which(Grp_by$Class=='TP')] * No_of_applicant_during_scorecard
                                  + Grp_by$Percent_of_total[which(Grp_by$Class=='FP')] * No_of_applicant_during_scorecard) / 10000000)

Potential_Revenue_Loss_for_CredX
# INR 101.95 Crores



Net_gain_for_credX =  Potential_credit_loss_avoided - Potential_Revenue_Loss_for_CredX

Net_gain_for_credX
# INR 171 Crores 












