library(lubridate)
library(stats)
library(stringr)
library(dplyr)
library(ggplot2)
library(DescTools)
library(gridExtra)

setwd("F:/BIG DATA/IIITb/C2.Statistics and Exploratory Data Analytics/EDA CAse study")

#loading the dataset and casting all "" to NAs

EDA <- read.csv("loan.csv",na.strings = c("","n/a","NA"))

################################## Data Cleaning:#########
#Check for duplicate values
sum(duplicated(EDA$id))
sum(duplicated(EDA$member_id))


# selects columns that DO NOT entirely  have NAs

EDA_clean <- EDA[,(colSums(is.na(EDA)) != nrow(EDA))] 

# selects  columns that DO NOT entirely have 0's

EDA_clean <- EDA[,colSums(EDA != 0,na.rm = T)>0] 

#Check for NAs :

Count_NA<-as.data.frame(colSums(is.na(EDA_clean)))
names(Count_NA)="NA's" 

#RAtiowise analysis of NAs
Count_NA$Ratio <- ((Count_NA$`NA's`)/nrow(EDA_clean))*100 
Count_NA$Ratio<- round(Count_NA$Ratio,1)

# Removing the % sign from various columns

EDA_clean$int_rate<- gsub("%"," ",EDA_clean$int_rate)
EDA_clean$revol_util<- gsub("%"," ",EDA_clean$revol_util)

#Formating certain numerics:

EDA_clean$dti <- round(EDA_clean$dti,0)
EDA_clean$int_rate <- round(as.numeric(EDA_clean$int_rate),1)
EDA_clean$revol_util<- round(as.numeric(EDA_clean$revol_util),1)


#Converting  dates to standard form:

EDA_clean$issue_d<-parse_date_time(x=EDA_clean$issue_d, orders="by")

EDA_clean$earliest_cr_line<-parse_date_time(x=EDA_clean$earliest_cr_line, orders="by")

EDA_clean$last_pymnt_d<-parse_date_time(x=EDA_clean$last_pymnt_d, orders="by")

EDA_clean$next_pymnt_d<-parse_date_time(x=EDA_clean$next_pymnt_d, orders="by")

EDA_clean$last_credit_pull_d<-parse_date_time(x=EDA_clean$last_credit_pull_d, orders="by")

#Extracting YEar and Month from Issue date:

EDA_clean$IssueYear<- format(EDA_clean$issue_d, "%Y")
EDA_clean$IssueMonth <-format(EDA_clean$issue_d, "%m")


#converting cases of certain columns to upper case to take care of case sensitiveness

EDA_clean$emp_title<-str_to_upper(EDA_clean$emp_title)

EDA_clean$title<- str_to_upper(EDA_clean$title)

## CREATING BINS FOR MAKING PLOTS easy for analysis

EDA_clean$income_bin<-as.factor(round(EDA_clean$annual_inc/10000,digits = 0)*10000)
EDA_clean$loan_amnt_bin<-round(EDA_clean$loan_amnt/1000,digits = 0)*1000
EDA_clean$dti_bin<-round(EDA_clean$dti,digits = 0)



# Formating the emp_length column: 10+==10 and < 1== 0

EDA_clean$emp_length<- str_replace_all(EDA_clean$emp_length,"[+ years]"," ")
EDA_clean$emp_length<- str_replace_all(EDA_clean$emp_length,"< 1","0")


#removing unimportant columns on the basis of relevance with our analysis and ratio of NA's :
EDA_clean<- subset(EDA_clean, select = -c(sub_grade,pymnt_plan,url,desc,zip_code,emp_title,title, open_acc,
                                          pub_rec,revol_bal,revol_util,total_acc,out_prncp_inv,out_prncp,total_rec_late_fee,
                                          recoveries, last_pymnt_amnt,pub_rec_bankruptcies,installment,
                                          initial_list_status,application_type,policy_code,total_pymnt_inv,
                                          collection_recovery_fee, mths_since_last_record, 
                                          mths_since_last_delinq,emp_title,total_rec_int,total_rec_prncp,
                                          earliest_cr_line,inq_last_6mths,last_pymnt_d,
                                          last_credit_pull_d,next_pymnt_d))

#### Filtering our data on the basis of loan status and ignoring loans with status = current

EDA_clean<- filter(EDA_clean,EDA_clean$loan_status!="Current")

# outliers of Annual income column
boxplot(EDA_clean$annual_inc)
quantile(EDA_clean$annual_inc,c(1:100)/100)
EDA_clean<-subset(EDA_clean,EDA_clean$annual_inc<=89000) #excluding above income above 90% quantile
boxplot(EDA_clean$annual_inc)


#UNIVARIATE ANALYSIS:

###1.Annual Income:

a1=ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(income_bin)) + 
  geom_bar(fill='red') + ylim(0,6000)+geom_text(stat='count',aes(label=..count..),vjust=-1)+
  ggtitle('UA of Annual Income (Loan Paid off)')

a2=ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(income_bin)) + 
  geom_bar(fill='blue') + ylim(0,6000) + geom_text(stat='count',aes(label=..count..),vjust=-1)+
  ggtitle('UA of Annual Income (Loan Defaulted)')

grid.arrange(a1,a2,ncol = 2)

#2. TERM

t1= ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(term))+
geom_bar(fill='red')+ylim(0,15000)+geom_text(stat='count',aes(label=..count..),vjust=-1)+
  ggtitle('UA of Term (Loan Paid off)')

t2= ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(term))+
geom_bar(fill='blue')+ylim(0,15000)+geom_text(stat='count',aes(label=..count..),vjust=-1)+
  ggtitle('UA of Term (Loan Defaulted)')

grid.arrange(t1,t2,ncol = 2)

##3.Interest Rate

R1=ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(int_rate))+geom_bar(fill='red')+
ylim(0,1500)+ggtitle('UA of Interest Rate (Loan Paid off)')

R2=ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(int_rate))+geom_bar(fill='blue')+
ylim(0,1500)+ggtitle('UA of Interest Rate (Loan Defaulted)')

grid.arrange(R1,R2,ncol = 2)

###4. LOAN AMOUNT


 ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(loan_amnt_bin))+
  geom_bar(fill='red')+ylim(0,3000)+geom_text(stat='count',aes(label=..count..),vjust=-1)+
  ggtitle('UA of Loan Amount (Loan Paid off)')

ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(loan_amnt_bin))+
  geom_bar(fill='blue')+ylim(0,3000)+geom_text(stat='count',aes(label=..count..),vjust=-1)+
  ggtitle('UA of Loan Amount (Loan Paid Defaulted)')

##5.Grade

ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(grade))+geom_bar(fill='red')+
  ylim(0,9000)+ggtitle('UA of Grade (Loan Paid off)')+geom_text(stat='count',aes(label=..count..),vjust=-1)

ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(grade))+geom_bar(fill='blue')+
  ylim(0,9000)+ggtitle('UA of Grade (Loan Defaulted)')+geom_text(stat='count',aes(label=..count..),vjust=-1)

##6.Home Ownership
ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(home_ownership))+geom_bar(fill='red')+
  ylim(0,15000)+ggtitle('UA of Home Ownership (Loan Paid off)')+geom_text(stat='count',aes(label=..count..),vjust=-1)

ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(home_ownership))+geom_bar(fill='blue')+
  ylim(0,15000)+ggtitle('UA of Home Ownership (Loan Defaulted)')+geom_text(stat='count',aes(label=..count..),vjust=-1)

##7. PURPOSE

ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(purpose))+geom_bar(fill='red')+
  ylim(0,12500)+ggtitle('UA of Purpose (Loan Paid off)') +geom_text(stat='count',aes(label=..count..),vjust=-1)

ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(purpose))+geom_bar(fill='blue')+
  ylim(0,12500)+ggtitle('UA of Purpose (Loan Defaulted)') + geom_text(stat='count',aes(label=..count..),vjust=-1)

##8.Emp_length
ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(emp_length))+geom_bar(fill='red')+
  ylim(0,5000)+ggtitle('UA of Employee Length (Loan Paid off)') + geom_text(stat='count',aes(label=..count..),vjust=-1)

ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(emp_length))+geom_bar(fill='Blue')+
  ylim(0,5000)+ggtitle('UA of Employee Length (Loan Defaulted)') + geom_text(stat='count',aes(label=..count..),vjust=-1)

##9.Delinq_2yr
ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(delinq_2yrs))+geom_bar(fill='red')+
  ylim(0,25000)+ggtitle('UA of Delinq_2yrs (Loan Paid off)') + geom_text(stat='count',aes(label=..count..),vjust=-1)

ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(delinq_2yrs))+geom_bar(fill='Blue')+
  ylim(0,25000)+ggtitle('UA of Delinq_2yrs (Loan Defaulted)') + geom_text(stat='count',aes(label=..count..),vjust=-1)
##10. DTI
ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(dti_bin))+geom_bar(fill='red')+
  ylim(0,1500)+ggtitle('UA of DTI (Loan Paid off)') +geom_text(stat='count',aes(label=..count..),vjust=-1)

ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(dti_bin))+geom_bar(fill='blue')+
  ylim(0,500)+ggtitle('UA of DTI (Loan Defaulted)') + geom_text(stat='count',aes(label=..count..),vjust=-1)

#11.State
ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(addr_state))+geom_bar(fill='blue')+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+ggtitle('Statewise (Loan Defaulted)') 
  

ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(addr_state))+geom_bar(fill='red')+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+ ggtitle('Statewise (Paid Off)')

#12. Verification:

ggplot(subset(EDA_clean,EDA_clean$loan_status=="Charged Off"),aes(verification_status))+geom_bar(fill='blue')+
  ylim(0,13000)+geom_text(stat='count',aes(label=..count..),vjust=-1)+ggtitle('Statewise (Loan Defaulted)')

ggplot(subset(EDA_clean,EDA_clean$loan_status!="Charged Off"),aes(verification_status))+geom_bar(fill='red')+
  ylim(0,13000)+geom_text(stat='count',aes(label=..count..),vjust=-1)+ggtitle('Statewise (Paid Off)')



# Distributions :

#Grade Distribution among the data set
Desc(EDA_clean$grade, main="Grade Distribution", plotit = 1)

#Loan Term distribution among the data set 
Desc(EDA_clean$term, main="Loan Term Distribution", plotit = 1)

# Loan Amount distribution among the data set
Desc(EDA_clean$loan_amnt, main="Loan Amount Distribution", plotit = 1)

#Loan Amount status distribution among the data set

Desc(EDA_clean$loan_status, main = "Loan amount status distribution", plotit = 1)

#Annual Income distribution of borrowers

Desc(EDA_clean$annual_inc, main = "Annual Income distribution", plotit = 1)

#Purpose distribution of the Loan Amount 
Desc(EDA_clean$purpose, main = "Purpose Distribution of Loan Amount", plotit = 1)


#Bivariate Analysis

#1. Loan Issued per Month 
ggplot(EDA_clean, aes(EDA_clean$IssueMonth, fill = factor(EDA_clean$loan_status))) + 
  geom_histogram(stat="count", bandwidth = 10) + 
  labs(title = "Issued loan and frequency",
       x = "Month",
       y = "Count")


#2.Loan Amount By Term among the data set
ggplot(EDA_clean, aes(EDA_clean$term,EDA_clean$loan_amnt)) + geom_boxplot(aes(fill = EDA_clean$term)) +
  labs(title = "Loan amount by term",
       x = "Term",
       y = "Loan amount")

#3. Interest Rate by Grade among the data set 
ggplot(EDA_clean, aes(EDA_clean$grade,EDA_clean$int_rate)) + geom_boxplot(aes(fill = grade)) +
  labs(title = "Interest rate by grade",
       x = "Grade",
       y = "Interest rate")

#4.Lending amount per month among the data set 

lending_amt = EDA_clean %>% 
  select(IssueMonth, loan_amnt) %>%
  group_by(IssueMonth) %>%
  summarise(Amount = sum(loan_amnt))

summary(lending_amt)
lending_amt

ggplot(lending_amt, aes(x = IssueMonth, y = Amount)) + geom_point() + 
  labs(title = "Loan amount issued by month",
       x = "Issued Month",
       y= "Total Amount")



#5.Loan Status by Grade among the data set 

ggplot(EDA_clean, aes(EDA_clean$grade, fill = factor(EDA_clean$loan_status))) + geom_bar(position = "fill") + 
  labs(title = "Loan status by grade",
       x = "Grade",
       y = "Rate")



#6.Loan disbursement growth rate

amt_group_table = EDA_clean%>% 
  select(IssueMonth, loan_status, loan_amnt) %>% 
  group_by(IssueMonth, loan_status) %>% 
  summarise(Amount = sum(loan_amnt))

summary(amt_group_table)

ggplot(amt_group_table, aes(x = IssueMonth, y = Amount, col = factor(loan_status))) + geom_point() + 
  labs(title = "Loan amount distribution among loan statuses",
       x = "Issued date",
       y = "Amount")



#7. Annual Income by Grade

ggplot(EDA_clean, aes(grade,annual_inc)) + geom_boxplot() + ylim(0,100000) +
  labs(title = "Annual Income by Grade",
       x = "Grade",
       y = "Annual income")


#8. Fully paid vs Charged Off loans excluding "on going"

loan2 <- subset(EDA_clean, EDA_clean$loan_status == "Fully Paid" | EDA_clean$loan_status == "Charged Off")
table(loan2$loan_status)
loan2$is_good = factor(ifelse(loan2$loan_status == "Fully Paid",1,0))
str(loan2$is_good)

loan_chargedoff= subset(EDA_clean,EDA_clean$loan_status=="Charged Off")
#9. Analysing the deafaulters with the purpose and home ownnership:

p1<- ggplot(loan_chargedoff,aes(purpose,fill= home_ownership))+
  geom_bar(position='dodge')+ theme(legend.position = "bottom")


#Since we know from our univariate analysis above,debt_consolidation purpose are the ones 
#who are charged off the most. Here we see that the ones who have debt_consolidation as 
#the purpose are living on rent or on mortgage. They seem to be the highly risky customers

p1+facet_wrap(~verification_status,nrow=3,shrink = F)

#Drilling down further to see regarding the verification status, we see that the ones 
#who are not verified are the highest number of customers who are charged off


#10. Analysing the loan_status and the emp_length
p2<-ggplot(EDA_clean,aes(emp_length,fill=loan_status))+
  geom_bar(position = 'stack')

#The highest number of defaulters are the ones who have employment length of 10 or more years.

p2+facet_wrap(~purpose)

#Most of the dfaulters are having an employment length of 10 years and need loans for debt_collection

##11.Trying to find correlation between some numeric variables:
plot( loan_chargedoff$loan_amnt, loan_chargedoff$funded_amnt_inv,
  main = "Correlation: loan_amnt and funded_amnt_inv",
  xlab = "loan_amnt ",
  ylab = "funded_amnt_inv ",
  pch = 20
)
cor(loan_chargedoff$loan_amnt, loan_chargedoff$funded_amnt_inv)

##12.#DTI ratio is one of the measures lenders can use to assess if the money borrowed will be paid or not
cor(loan_chargedoff$loan_amnt,loan_chargedoff$dti)
#It is a positive correlation, which means that since the dti is high, 
#they take more amount of loan.Fair enough

##13.Grade and loan status:
#using Gradestatus vector from above:
Gradestatus = ggplot(EDA_clean,aes(grade))+geom_bar(fill='red') + geom_text(stat='count',aes(label=..count..),vjust=-1)

Gradestatus+facet_wrap(~loan_status)

  #1. Segmented Bivariate Analysis - Loan disbursement Vs Loan Status and Grade 

Loan_group_table = EDA_clean %>% 
  select(loan_status, loan_amnt, grade) %>% 
  group_by(loan_status, grade) %>% 
  summarise(Amount = sum(loan_amnt))

summary(Loan_group_table)

ggplot(Loan_group_table, aes(x = grade,y = Amount, fill = loan_status)) + 
  geom_bar(stat="identity",position = "dodge") + 
  geom_text(aes(label = Amount), position= position_dodge(width=0.9), vjust=-.5, color="black") +
  theme(legend.position = "bottom") +
  labs(title = "Loan amount distribution Vs Loan Status per Grades",
       x = "Grades",
       y = "Amount")

#2.Segmented Bivariate Analysis - Loan disbursement Vs Verification Status and Loan Status
Loan_group_table = EDA_clean %>% 
  select(loan_status, loan_amnt, verification_status) %>% 
  group_by(loan_status, verification_status) %>% 
  summarise(Amount = sum(loan_amnt))

summary(Loan_group_table)

ggplot(Loan_group_table, aes(x = verification_status,y = Amount, fill = loan_status)) + 
  geom_bar(stat="identity",position = "dodge") + geom_text(aes(label = Amount), position= position_dodge(width=0.9), vjust=-.5, color="black") +
  theme(legend.position = "bottom") +
  labs(title = "Loan amount distribution Vs Loan Status as per  Verification Type ",
       x = "verification Type",
       y = "Amount")






write.csv(loan2,file = "loan_data.csv", na = "")







