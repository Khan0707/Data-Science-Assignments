#Setting the work directory
#setwd("F:/BIG DATA/IIITb/C3. Predictive analysis/HR analytics")

# Install and Load the required packages

#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("zoo")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(zoo)
library(imputeTS)

#Importing Various files:
companyData<- read.csv("general_data.csv",stringsAsFactors = F)
InTime<- read.csv("in_time.csv",stringsAsFactors = F)
OutTime<- read.csv("out_time.csv",stringsAsFactors = F)
ManagerData<-read.csv("manager_survey_data.csv",stringsAsFactors=F)
EmployeeData<- read.csv("employee_survey_data.csv",stringsAsFactors = F)

str(companyData)
str(ManagerData)
str(EmployeeData)

#Removing employee count ,standardhours over18 columns since they have same values throughout
companyData<-companyData[,-which(names(companyData)=="EmployeeCount")]
companyData<-companyData[,-which(names(companyData)=="Over18")]
companyData<-companyData[,-which(names(companyData)=="StandardHours")]

#***************Data Cleaning and Data Preparation:*********************#

#Checking Which column in OutTime and InTime has all values as Na so to treat those dates as holidays
sort(colSums(is.na(OutTime)),decreasing = T) 
sort(colSums(is.na(InTime)),decreasing = T) 

#Remove holidays from "in_time" and "out_time"
remove_holiday_col<-function(time_data)
{
  time_data<-time_data[colSums(!is.na(time_data)) > 0]
  return(time_data)
}

InTime <- remove_holiday_col(InTime)
OutTime<- remove_holiday_col(OutTime)

#Checking NaS for outime intime
sum(is.na(InTime))
sum(is.na(OutTime))

str(InTime)
str(OutTime)

#Convert all the date columns from character to DateTime format
InTime[,2:250] <- lapply(InTime[,2:250], function(x) as_datetime(x))
OutTime[,2:250] <- lapply(OutTime[,2:250], function(x) as_datetime(x))

#Calculate the work hours of each employee everyday outtime-intime
x<-OutTime[,2:250]-InTime[,2:250] 
x_y<-cbind(InTime[,c("X")],x)
names(x_y)[1]<-paste("X")
x_y[,2:250] <- lapply(x_y[,2:250], function(x) as.numeric(x))

#Calculaing the avg work hours of each employee
x_y$work_avg <- rowMeans(x_y[,2:250], na.rm = TRUE)


###Create a new dataframe consisting of Employee ID and Office time hrs.
office_time <- data.frame(x_y$X, x_y$work_avg)
colnames(office_time) <- c("EmployeeID", "OfficeHrs")
office_time$OfficeHrs <- round(office_time$OfficeHrs, 2)

# Collate the data together in one single file
length(unique(tolower(companyData$EmployeeID)))   # 4410, confirming EmployeeID is key
length(unique(tolower(EmployeeData$EmployeeID)))  # 4410, confirming EmployeeID is key
length(unique(tolower(ManagerData$EmployeeID)))   # 4410, confirming EmployeeID is key
length(unique(tolower(office_time$EmployeeID)))   ### 4410, confirming EmployeeID is key

setdiff(EmployeeData$EmployeeID,companyData$EmployeeID) 
setdiff(EmployeeData$EmployeeID,ManagerData$EmployeeID) 
setdiff(EmployeeData$EmployeeID,office_time$EmployeeID) 


hr_analytics <- merge(EmployeeData,companyData, by="EmployeeID", all = F)  
hr_analytics <- merge(hr_analytics, ManagerData, by="EmployeeID", all = F) 
hr_analytics <- merge(hr_analytics, office_time, by= "EmployeeID", all = F) 

length(unique(hr_analytics$EmployeeID)) #4410 unique employee ID- Remove this column: 
hr_analytics<-hr_analytics[,-1]

#Create a new derived column "Over_time"
hr_analytics$Over_time <- ifelse(hr_analytics$OfficeHrs > 8, 1, 0)  
# Overtime, 1 indicates yes while 0 = no

#Create inadequate work time
#Inadequate time means the employee is working much less than the required hours on average.

hr_analytics$inadq_time <- ifelse(hr_analytics$OfficeHrs < 7, 1, 0)  

# Convert all non-numeric columns to lowercase

hr_analytics<- data.frame(lapply(hr_analytics, function(v) {
  if (is.character(v) | (is.factor(v))) return(tolower(v))
  else return(v)
})) 

#create no of leaves as derived metric
#calculated by counting the no of NA's in (outtime-intime) which we have stored as x
for(i in 1:4410)
{
  hr_analytics$no_leaves[i]<-sum(is.na(x[i,]))
}

# *******Exploratory Data Analysis

#Distribution of categorical variables 
par(mfrow = c(4,2))
par(mar =rep(2,4))

barplot(table(hr_analytics$Attrition), main = "Attrition Distribution")

barplot(table(hr_analytics$JobSatisfaction), main="Job Satisfication level")

barplot(table(hr_analytics$BusinessTravel), main = "Business Travel")

barplot(table(hr_analytics$Gender), main ="Gender")

barplot(table(hr_analytics$MaritalStatus), main = "Marital Status")    

barplot(table(hr_analytics$JobInvolvement), main = "Job Involvement")   

barplot(table(hr_analytics$Over_time), main = "Over time")       

barplot(table(hr_analytics$PerformanceRating), main = "Performance rating")

#Observations: 
#The attrition rate is quite low which is a good thing.
#Employees are mostly satisfied but there are many who are not satisfied( with 1 and 2 job satisfaction level). 
#The gender distribution shows males are almost double in number than females
#Most of them are married
#The performance rating is mostly between 3 and 4 which is a good thing
#Almost half of the employess work over time.


#Checking Nas in Hr Analytics df:
#cleaning the individual NA values from the data set
# if we have na values in the dataset the prediction will be highly unstable
sort(colSums(is.na(hr_analytics)),decreasing = T)

#"EnvironmentSatisfaction" "JobSatisfaction""WorkLifeBalance""NumCompaniesWorked"     
#"TotalWorkingYears"      

##Replace above columns with their respective means
## Using imputeTS library na.mean(df)

hr_analytics<- na.mean(hr_analytics)



#checking for outliers for numeric variables:
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(hr_analytics, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
plot_grid(ggplot(hr_analytics, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
plot_grid(ggplot(hr_analytics, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#There are outliers in Monthly income-justified since it could be high for high level employees.
#The distribution is justified as well.

plot_grid(ggplot(hr_analytics, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
plot_grid(ggplot(hr_analytics, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#There are a few senior people which is also justified.

plot_grid(ggplot(hr_analytics, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
plot_grid(ggplot(hr_analytics, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Outliers justified since there are senior people mostly

plot_grid(ggplot(hr_analytics, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
plot_grid(ggplot(hr_analytics, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
plot_grid(ggplot(hr_analytics, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#There are few people not promoted in recent years

plot_grid(ggplot(hr_analytics, aes(OfficeHrs))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=OfficeHrs))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
plot_grid(ggplot(hr_analytics, aes(no_leaves))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=no_leaves))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
  



str(hr_analytics)

#Scaling the continuous variables:
hr_analytics$Age<-scale(hr_analytics$Age)
hr_analytics$DistanceFromHome<-scale(hr_analytics$DistanceFromHome)
hr_analytics$MonthlyIncome<-scale(hr_analytics$MonthlyIncome)
hr_analytics$PercentSalaryHike<-scale(hr_analytics$PercentSalaryHike)
hr_analytics$TotalWorkingYears<-scale(hr_analytics$TotalWorkingYears)
hr_analytics$TrainingTimesLastYear<-scale(hr_analytics$TrainingTimesLastYear)
hr_analytics$YearsAtCompany<-scale(hr_analytics$YearsAtCompany)
hr_analytics$NumCompaniesWorked<-scale(hr_analytics$NumCompaniesWorked)
hr_analytics$YearsWithCurrManager<-scale(hr_analytics$YearsWithCurrManager)
hr_analytics$YearsSinceLastPromotion<-scale(hr_analytics$YearsSinceLastPromotion)
hr_analytics$OfficeHrs<-scale(hr_analytics$OfficeHrs)
hr_analytics$no_leaves<-scale(hr_analytics$no_leaves)

#************Data Preparation for modelling:*******
### finding the  LEVELS IN EACH COLUMN

sapply(colnames(hr_analytics),function(x) levels(hr_analytics[[x]]))

#Below are the list of categorical attributes
#Attrition, Gender  -- 2 levels
# More than 2 levels
#EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,
#JobRole, MaritalStatus,  BusinessTravel, Department,Education, EducationField
#JobInvolvement,JobLevel,PerformanceRating

### CONVERTING COLUMNS WITH TWO LEVELS TO NUMERIC ONES:

hr_analytics$Attrition <- ifelse(hr_analytics$Attrition == "yes", 1,0)
hr_analytics$Gender <- ifelse(hr_analytics$Gender == "female",1,0)

str(hr_analytics)


### CONVERTING COLUMNS WITH MORE THAN TWO LEVELS USING DUMMY VARIABLES:

hr_analytics[,1:3]<-round(hr_analytics[,1:3],digits=0)

#Create a dataframe of categorical attributes with more than 2 levels
hr_analytics_fact <- hr_analytics[,c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance",
                                     "BusinessTravel","Department","EducationField", "Education",
                                     "JobRole","MaritalStatus","JobInvolvement","JobLevel","StockOptionLevel",
                                     "PerformanceRating")]

#Dataframe with numeric columns:
hr_analytics_num<-subset(hr_analytics,select=-c(EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,
                                                BusinessTravel,Department,EducationField, Education,
                                                JobRole,MaritalStatus,JobInvolvement,JobLevel,StockOptionLevel,
                                                PerformanceRating))


#Convert categorical attributes to factors
hr_analytics_fact <- data.frame(sapply(hr_analytics_fact, function(x) factor(x)))
str(hr_analytics_fact)

#Creating dummy attributes for factor attributes
dummies <- data.frame(sapply(hr_analytics_fact, function(x)
  data.frame(model.matrix(~x-1, data = hr_analytics_fact))[,-1]))
  
#combining into final df on which modelling can be done: 
final_df<-cbind(hr_analytics_num,dummies)
str(final_df)

#Checking for NA values in our final dataframe:
sapply(final_df, function(x) sum(is.na(x))) #no NA values


#*******Modeling begins here************#

#Splitting the data:

set.seed(100)
indices = sample.split(final_df$Attrition, SplitRatio = 0.7)
train = final_df[indices,]
test = final_df[!(indices),]


# Logistic Regression: 

#Initial model:

model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection

model_2<- stepAIC(model_1, direction="both")
summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)

#Removing EducationField.xlife.sciences as ith has high vif and is insignificant:

model_3<-glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
               Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
               BusinessTravel.xtravel_rarely + Department.xresearch...development + 
               Department.xsales+ EducationField.xmarketing + 
               EducationField.xmedical + EducationField.xother + EducationField.xtechnical.degree + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + 
               MaritalStatus.xmarried + MaritalStatus.xsingle + JobInvolvement.x2 + 
               JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
             data = train)

summary(model_3)

vif(model_3)

#Removing YearsAtCompany due to high VIF and insignificance:

model_4<-glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
               Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
               BusinessTravel.xtravel_rarely + Department.xresearch...development + 
               Department.xsales+ EducationField.xmarketing + 
               EducationField.xmedical + EducationField.xother + EducationField.xtechnical.degree + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + 
               MaritalStatus.xmarried + MaritalStatus.xsingle + JobInvolvement.x2 + 
               JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
             data = train)

summary(model_4)

vif(model_4)

#At this point all the variables with VIF>2 are significant. Hence cant remove them. 
# Removing the variables that are insignificant one by one:
# Removing MaritalStatus.xmarried:
model_5<-glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
               BusinessTravel.xtravel_rarely + Department.xresearch...development + 
               Department.xsales+ EducationField.xmarketing + 
               EducationField.xmedical + EducationField.xother + EducationField.xtechnical.degree + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + 
                MaritalStatus.xsingle + JobInvolvement.x2 + 
               JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
             data = train)
summary(model_5)

vif(model_5)

#Removing JobInvolvement.x2:

model_6<-glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
               BusinessTravel.xtravel_rarely + Department.xresearch...development + 
               Department.xsales+ EducationField.xmarketing + 
               EducationField.xmedical + EducationField.xother + EducationField.xtechnical.degree + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + 
               MaritalStatus.xsingle + 
               JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
             data = train)
summary(model_6)

vif(model_6)

#Removing EducationField.xmedical:

model_7<-glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
               BusinessTravel.xtravel_rarely + Department.xresearch...development + 
               Department.xsales+ EducationField.xmarketing + 
               EducationField.xother + EducationField.xtechnical.degree + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + 
               MaritalStatus.xsingle + 
               JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
             data = train)
summary(model_7)

vif(model_7)
#Removing Gender:
model_8<-glm(formula = Attrition ~ Age + DistanceFromHome +  MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
               BusinessTravel.xtravel_rarely + Department.xresearch...development + 
               Department.xsales+ EducationField.xmarketing + 
               EducationField.xother + EducationField.xtechnical.degree + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + 
               MaritalStatus.xsingle + 
               JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
             data = train)
summary(model_8)

vif(model_8)

#Removing EducationField.xmarketing:
model_9<-glm(formula = Attrition ~ Age + DistanceFromHome +  MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
               BusinessTravel.xtravel_rarely + Department.xresearch...development + 
               Department.xsales+  
               EducationField.xother + EducationField.xtechnical.degree + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + 
               MaritalStatus.xsingle + 
               JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
             data = train)
summary(model_9)

vif(model_9)
#Removing EducationField.xtechnical.degree:
model_10<-glm(formula = Attrition ~ Age + DistanceFromHome +  MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
               BusinessTravel.xtravel_rarely + Department.xresearch...development + 
               Department.xsales+  
               EducationField.xother + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + 
               MaritalStatus.xsingle + 
               JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
             data = train)
summary(model_10)

vif(model_10)

#Removing StockOptionLevel.x1:
model_11<-glm(formula = Attrition ~ Age + DistanceFromHome +  MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+  
                EducationField.xother + 
                JobRole.xlaboratory.technician + JobRole.xresearch.director + 
                JobRole.xresearch.scientist + JobRole.xsales.executive + 
                MaritalStatus.xsingle + 
                JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
              data = train)
summary(model_11)

vif(model_11)

#Removing EducationField.xother:
model_12<-glm(formula = Attrition ~ Age + DistanceFromHome +  MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+  
                
                JobRole.xlaboratory.technician + JobRole.xresearch.director + 
                JobRole.xresearch.scientist + JobRole.xsales.executive + 
                MaritalStatus.xsingle + 
                JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
              data = train)
summary(model_12)

vif(model_12)
#Removing DistanceFromHome:
model_13<-glm(formula = Attrition ~ Age +  MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+  
                
                JobRole.xlaboratory.technician + JobRole.xresearch.director + 
                JobRole.xresearch.scientist + JobRole.xsales.executive + 
                MaritalStatus.xsingle + 
                JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
              data = train)
summary(model_13)

vif(model_13)

#Removing MonthlyIncome :
model_14<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+  
                
                JobRole.xlaboratory.technician + JobRole.xresearch.director + 
                JobRole.xresearch.scientist + JobRole.xsales.executive + 
                MaritalStatus.xsingle + 
                JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
              data = train)
summary(model_14)

vif(model_14)

#Removing JobInvolvement.x3 :
model_15<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+  
                
                JobRole.xlaboratory.technician + JobRole.xresearch.director + 
                JobRole.xresearch.scientist + JobRole.xsales.executive + 
                MaritalStatus.xsingle + 
                JobLevel.x2 , family = "binomial", 
              data = train)
summary(model_15)

vif(model_15)

#Removing JobRole.xlaboratory.technician:
model_16<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+  
                 JobRole.xresearch.director + 
                JobRole.xresearch.scientist + JobRole.xsales.executive + 
                MaritalStatus.xsingle + 
                JobLevel.x2 , family = "binomial", 
              data = train)
summary(model_16)

vif(model_16)

#Removing JobRole.xresearch.scientist:
model_17<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+  
                JobRole.xresearch.director + 
                 + JobRole.xsales.executive + 
                MaritalStatus.xsingle + 
                JobLevel.x2 , family = "binomial", 
              data = train)
summary(model_17)

vif(model_17)


#The intercept is still insignificant . Hence we remove other variables:

#Removing JobLevel.x2 :

model_18<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+  
                JobRole.xresearch.director + 
                + JobRole.xsales.executive + 
                MaritalStatus.xsingle 
                , family = "binomial", 
              data = train)
summary(model_18)

vif(model_18)

#This has improved the significance on Intercept a little. Continuing, we remove JobRole.xresearch.director

model_19<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+  
               JobRole.xsales.executive + 
                MaritalStatus.xsingle 
              , family = "binomial", 
              data = train)
summary(model_19)

vif(model_19)

#Removing JobRole.xsales.executive:
model_20<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+
                MaritalStatus.xsingle 
              , family = "binomial", 
              data = train)
summary(model_20)

vif(model_20)

#Removing JobSatisfaction.x3:
model_21<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 +  
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+
                MaritalStatus.xsingle 
              , family = "binomial", 
              data = train)
summary(model_21)

vif(model_21)

#Removing JobSatisfaction.x2:
model_22<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+
                MaritalStatus.xsingle 
              , family = "binomial", 
              data = train)
summary(model_22)

vif(model_22)

#Removing TrainingTimesLastYear
model_23<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+
                MaritalStatus.xsingle 
              , family = "binomial", 
              data = train)
summary(model_23)

vif(model_23)

#Removing WorkLifeBalance.x4:
model_24<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+
                MaritalStatus.xsingle 
              , family = "binomial", 
              data = train)
summary(model_24)

vif(model_24)

#Removing WorkLifeBalance.x2:
model_25<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                BusinessTravel.xtravel_frequently + 
                BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                Department.xsales+
                MaritalStatus.xsingle 
              , family = "binomial", 
              data = train)
summary(model_25)

vif(model_25)

final_model<-model_25

#*********Modeling Evaluation:*********

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", newdata = test[,-2])
summary(test_pred)

test$prob <- test_pred
View(test)

#Checking for cutoff as 50%:
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)

#Choosing cutoff:
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#Creating values from 0.0006042 to 0.8474000 for cutoff:(min and max of test_pred )

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.50,.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Let's choose a cutoff value of  0.1616 for final model

test_cutoff_attrition<- factor(ifelse(test_pred >=0.1616, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]


# Hence the accuracy is 0.75, sensitivity is 0.75 and specificity is 0.75.

View(test)


### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition , test_actual_attrition )

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)
lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
#Gain at 4th decile is 81%

#ploting Cumulative Lift
ggplot(Attrition_decile)+ geom_line(aes(Cumlift, bucket, color = "red"))+ xlab("CumLift")+ 
  ylab("Bucket") + ggtitle("LiftCurve") 

#ploting gain :
ggplot(Attrition_decile)+ geom_line(aes(Gain, bucket, color = "red"))+ xlab("Gain")+
  ylab("Bucket")+ggtitle("GainCurve")

#plotting ROC
plot(performance_measures_test, col = "blue", lwd = 2,main="ROC_curve")

# Export gain_lift table
write.csv(Attrition_decile, "lift.csv", row.names = FALSE)


##************************** CONCLUSION******************************

#1.Mostly young, middle aged single employees leave the company. 
#It can be due to not having much family dependency.


#2.Employees also leave due to working over time but very much satisfied with the environment and job

#3.Employees leave because of working under the same Manager for a long time
#and don't get to travel for Business abroad.

#4.employees who Worked for many companies have more chances to leave.


########################## Respective REcommendations########################
#1.They should be given more challenging roles and responsibilities in order to feel the need to stay.

#2,Incase of overtime, they should be compensated for it separately or given a week off on some other day. 
#They could also be given incentives. Some fun activities could be planned once a month in order to boost the morale of the employees

#3.More opportunities should be created in terms of projects and assignments , 
#so that the employees  can work under different managers and also get to travel for business once in a while


