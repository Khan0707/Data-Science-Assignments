##Loading the necessary packages into the R-enviornment
library(dplyr)
library(stringr)
library(ggplot2)

spark_path <- '/usr/local/spark'

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))

##Importing the dataset files

data_2015 <- read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", source = "csv",
                     header = "true", inferSchema = "true")

data_2016 <- read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", source = "csv",
                     header = "true", inferSchema = "true")

data_2017 <- read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", source = "csv",
                     header = "true", inferSchema = "true")



###Fixing Column Names for the imported Datasets:

colnames(data_2015)<- str_trim(colnames(data_2015), side= "both")
colnames(data_2015)<- str_replace_all(colnames(data_2015), pattern=" ", replacement = "_")
colnames(data_2015)<- str_replace_all(colnames(data_2015), pattern="\\?", replacement = "")

colnames(data_2016)<- str_trim(colnames(data_2016), side= "both")
colnames(data_2016)<- str_replace_all(colnames(data_2016), pattern=" ", replacement = "_")
colnames(data_2016)<- str_replace_all(colnames(data_2016), pattern="\\?", replacement = "")

colnames(data_2017)<- str_trim(colnames(data_2017), side= "both")
colnames(data_2017)<- str_replace_all(colnames(data_2017), pattern=" ", replacement = "_")
colnames(data_2017)<- str_replace_all(colnames(data_2017), pattern="\\?", replacement = "")




##Understanding the dimensions and structure of the the Imported Datasets:

##### 2015

head(data_2015)

nrow(data_2015)  ## 11809233 rows

ncol(data_2015) ## 51 Columns

str(data_2015)

printSchema(data_2015)

##### 2016
head(data_2016)

nrow(data_2016)  ## 10626899 rows

ncol(data_2016) ## 51 Columns

str(data_2016)

printSchema(data_2016)

##### 2017
head(data_2017)

nrow(data_2017)  ## 10803028 rows

ncol(data_2017) ## 43 Columns

str(data_2017)

printSchema(data_2017)

####We can see that the dataframes have varying number of columns.
##There are columns which exist in 2015,16 but not in 2017.

# For using SQL, we need to create a temporary view:

createOrReplaceTempView(data_2015, "data_2015_view")
createOrReplaceTempView(data_2016, "data_2016_view")
createOrReplaceTempView(data_2017, "data_2017_view")


# Before executing any hive-sql query from RStudio, you need to add a jar file in RStudio 


sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

####********** DATA CLEANING *******######
#1.Removing any duplicate rows in the dataset

data_2015<- dropDuplicates(data_2015, "Summons_Number")
dim(data_2015)
# After Removing Duplicate records of 2015 dataset.
## we have 10951256 rows & 51 columns

##2.Let us check if there are any missing values in the Issue Date Parameter.

CountNull_IssueDate_2015 <- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                        THEN 1
                                        ELSE 0
                                        END) as nulls_Issue_Date,
                                        COUNT(*) as Num_of_Rows
                                        FROM data_2015_view")
head(CountNull_IssueDate_2015)
### no records with missing Issue Date or Null Issue Date

##3.Converting the Date Paramters to a suitable format for Analysis:

data_2015$Issue_Date <- SparkR::to_date(data_2015$Issue_Date, 'MM/dd/yyyy')


####updating the view:

createOrReplaceTempView(data_2015, "data_2015_view") 



##create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue
data_2015$Issue_Year <- year(data_2015$Issue_Date)
data_2015$Issue_Month <- month(data_2015$Issue_Date)

createOrReplaceTempView(data_2015, "data_2015_view")

#observe the Distribution of Issue Date:
Grouped_Issue_Date_2015 <- SparkR::sql("SELECT Issue_Year,
                                       Issue_Month,
                                       count(*)as Num_of_Records
                                       FROM data_2015_view
                                       GROUP BY Issue_Year,
                                       Issue_Month
                                       ORDER BY 1,2")
dfgrouped_issue_ym_2015 <- data.frame(head(Grouped_Issue_Date_2015, nrow(Grouped_Issue_Date_2015)))
View(dfgrouped_issue_ym_2015)

# Considering A Fiscal Year to extend from the July of Pervious Year to June of the Current Year.

data_2015 <- data_2015[data_2015$Issue_Date >= "2014-07-01" & 
                         data_2015$Issue_Date <= "2015-06-30"]

nrow(data_2015)   ##10598035 

##columns "Latitude", "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" 
##are logged for the fiscal year 2015 and 2016 but not for 2017

createOrReplaceTempView(data_2015, "data_2015_view")

CountNull_ExtraCol_2015 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       SUM(CASE WHEN Plate_ID IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Plate_ID,
                                       SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_stand_stop,
                                       SUM(CASE WHEN Hydrant_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Hydrant_Violation,
                                       SUM(CASE WHEN Double_Parking_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Double_Parking_Violation,
                                       SUM(CASE WHEN Latitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Latitude,
                                       SUM(CASE WHEN Longitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Longitude,
                                       SUM(CASE WHEN Community_Board IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Board,
                                       SUM(CASE WHEN Community_Council IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Council,
                                       SUM(CASE WHEN Census_Tract IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Census_Tract,
                                       SUM(CASE WHEN BIN IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BIN,
                                       SUM(CASE WHEN BBL IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BBL,
                                       SUM(CASE WHEN NTA IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_NTA     
                                       FROM data_2015_view")
head(CountNull_ExtraCol_2015)
####There are 129 records with null registration plate ID which is managable. 
##However, all the additional columns have only null values therefore we will  drop them to standardize 
##the dataset between the years.

#Removing these Null Columns.
data_2015<- drop(data_2015, c("No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation", 
                              "Latitude","Longitude","Community_Board", "Community_Council","Census_Tract","BIN" , "BBL", 
                              "NTA") )
colnames(data_2015)

###the string format of the time attributes include only a partial component of AM/PM. 
##Therefore we will append M to the end of each time attribute before converting it into a timestamp.
data_2015$Concat_M <- "M"
data_2015$Violation_Time<-concat(data_2015$Violation_Time,  data_2015$Concat_M)

data_2015$Time_First_Observed<- concat(data_2015$Time_First_Observed,data_2015$Concat_M)

data_2015$From_Hours_In_Effect<- concat(data_2015$From_Hours_In_Effect, data_2015$Concat_M)
data_2015$To_Hours_In_Effect<- concat(data_2015$To_Hours_In_Effect, data_2015$Concat_M)
data_2015<- drop(data_2015, c("Concat_M"))


#Extracting Violation Hour, Violation Minute and Part of Day.
data_2015$Violation_Hour <- substr(data_2015$Violation_Time, 1, 2)
data_2015$Violation_Minute <- substr(data_2015$Violation_Time, 4, 5)
data_2015$Violation_AMPM <- substr(data_2015$Violation_Time, 6, 7)

#Since  there are records that have both 00xxAM as well as 12xxAM. 
#Therefore we will replace all 00xxAM with 12xxAM
data_2015$Violation_Hour <- regexp_replace(x = data_2015$Violation_Hour,pattern = "\\00",replacement = "12")

#Concatenating the components into a standardized Violation Time.
data_2015$Violation_Time <- concat(data_2015$Violation_Hour, 
                                   data_2015$Violation_Minute, data_2015$Violation_AMPM)
#Converting Violation Time into a TimeStamp
data_2015$Violation_Time<-to_timestamp(x = data_2015$Violation_Time, format = "hhmma")

#Converting the other time attributes into a TimeStamp.
data_2015$Time_First_Observed<- to_timestamp(x= data_2015$Time_First_Observed, format = "hhmma")
data_2015$From_Hours_In_Effect<- to_timestamp(x= data_2015$From_Hours_In_Effect, format = "hhmma")
data_2015$To_Hours_In_Effect<- to_timestamp(x= data_2015$To_Hours_In_Effect, format = "hhmma")

### final 2015 data set
head(data_2015)
dim(data_2015)
str(data_2015)
#2015 Dataset has Rows: 10,598,035 | Columns: 45
printSchema(data_2015)



################################################****2016 *****#################
#1.Removing any duplicate rows in the dataset

data_2016<- dropDuplicates(data_2016, "Summons_Number")
dim(data_2016)
# After Removing Duplicate records of 2016 dataset.
## we have  10626899 rows & 51 columns

##2.Let us check if there are any missing values in the Issue Date Parameter.

CountNull_IssueDate_2016 <- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                        THEN 1
                                        ELSE 0
                                        END) as nulls_Issue_Date,
                                        COUNT(*) as Num_of_Rows
                                        FROM data_2016_view")
head(CountNull_IssueDate_2016)
### no records with missing Issue Date or Null Issue Date

##3.Converting the Date Paramters to a suitable format for Analysis:

data_2016$Issue_Date <- SparkR::to_date(data_2016$Issue_Date, 'MM/dd/yyyy')


####updating the view:

createOrReplaceTempView(data_2016, "data_2016_view") 

##4. Let's Understand the Range of ticket Issue Dates Available in the Dataset




##create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue

data_2016$Issue_Year <- year(data_2016$Issue_Date)
data_2016$Issue_Month <- month(data_2016$Issue_Date)

createOrReplaceTempView(data_2016, "data_2016_view")

#observe the Distribution of Issue Date:
Grouped_Issue_Date_2016 <- SparkR::sql("SELECT Issue_Year,
                                       Issue_Month,
                                       count(*)as Num_of_Records
                                       FROM data_2016_view
                                       GROUP BY Issue_Year,
                                       Issue_Month
                                       ORDER BY 1,2")
dfgrouped_issue_ym_2016 <- data.frame(head(Grouped_Issue_Date_2016, nrow(Grouped_Issue_Date_2016)))
View(dfgrouped_issue_ym_2016)

# Considering A Fiscal Year to extend from the July of Pervious Year to June of the Current Year.

data_2016 <- data_2016[data_2016$Issue_Date >= "2015-07-01" & 
                         data_2016$Issue_Date <= "2016-06-30"]

nrow(data_2016)   ##10396894 

##columns "Latitude", "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" 
##are logged for the fiscal year 2015 and 2016 but not for 2017

createOrReplaceTempView(data_2016, "data_2016_view")

CountNull_ExtraCol_2016 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       SUM(CASE WHEN Plate_ID IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Plate_ID,
                                       SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_stand_stop,
                                       SUM(CASE WHEN Hydrant_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Hydrant_Violation,
                                       SUM(CASE WHEN Double_Parking_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Double_Parking_Violation,
                                       SUM(CASE WHEN Latitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Latitude,
                                       SUM(CASE WHEN Longitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Longitude,
                                       SUM(CASE WHEN Community_Board IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Board,
                                       SUM(CASE WHEN Community_Council IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Council,
                                       SUM(CASE WHEN Census_Tract IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Census_Tract,
                                       SUM(CASE WHEN BIN IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BIN,
                                       SUM(CASE WHEN BBL IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BBL,
                                       SUM(CASE WHEN NTA IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_NTA     
                                       FROM data_2016_view")
head(CountNull_ExtraCol_2016)

####There are 20 records with null registration plate ID . 
##However, all the additional columns have only null values therefore we will  drop them to standardize 
##the dataset between the years.

#Removing these Null Columns.
data_2016<- drop(data_2016, c("No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation", 
                              "Latitude","Longitude","Community_Board", "Community_Council","Census_Tract","BIN" , "BBL", 
                              "NTA") )
colnames(data_2016)

###the string format of the time attributes include only a partial component of AM/PM. 
##Therefore we will append M to the end of each time attribute before converting it into a timestamp.
data_2016$Concat_M <- "M"
data_2016$Violation_Time<-concat(data_2016$Violation_Time,  data_2016$Concat_M)

data_2016$Time_First_Observed<- concat(data_2016$Time_First_Observed,data_2016$Concat_M)

data_2016$From_Hours_In_Effect<- concat(data_2016$From_Hours_In_Effect, data_2016$Concat_M)
data_2016$To_Hours_In_Effect<- concat(data_2016$To_Hours_In_Effect, data_2016$Concat_M)
data_2016<- drop(data_2016, c("Concat_M"))


#Extracting Violation Hour, Violation Minute and Part of Day.

data_2016$Violation_Hour <- substr(data_2016$Violation_Time, 1, 2)
data_2016$Violation_Minute <- substr(data_2016$Violation_Time, 4, 5)
data_2016$Violation_AMPM <- substr(data_2016$Violation_Time, 6, 7)

#Since  there are records that have both 00xxAM as well as 12xxAM. 
#Therefore we will replace all 00xxAM with 12xxAM
data_2016$Violation_Hour <- regexp_replace(x = data_2016$Violation_Hour,pattern = "\\00",replacement = "12")

#Concatenating the components into a standardized Violation Time.
data_2016$Violation_Time <- concat(data_2016$Violation_Hour, 
                                   data_2016$Violation_Minute, data_2016$Violation_AMPM)

#Converting Violation Time into a TimeStamp

data_2016$Violation_Time<-to_timestamp(x = data_2016$Violation_Time, format = "hhmma")

#Converting the other time attributes into a TimeStamp.

data_2016$Time_First_Observed<- to_timestamp(x= data_2016$Time_First_Observed, format = "hhmma")
data_2016$From_Hours_In_Effect<- to_timestamp(x= data_2016$From_Hours_In_Effect, format = "hhmma")
data_2016$To_Hours_In_Effect<- to_timestamp(x= data_2016$To_Hours_In_Effect, format = "hhmma")

### final 2016 data set

head(data_2016)
dim(data_2016)
str(data_2016)

#2016 Dataset has Rows:10396894  | Columns: 45
printSchema(data_2016)

#############*********************************************data Cleaning  2017**************#######################

data_2017<- dropDuplicates(data_2017, "Summons_Number")
dim(data_2017)   #####Rows: 10,803,028 & Columns: 43

##2.Let us check if there are any missing values in the Issue Date Parameter.

CountNull_IssueDate_2017 <- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                        THEN 1
                                        ELSE 0
                                        END) as nulls_Issue_Date,
                                        COUNT(*) as Num_of_Rows
                                        FROM data_2017_view")
head(CountNull_IssueDate_2017)
### no records with missing Issue Date or Null Issue Date

##3.Converting the Date Paramters to a suitable format for Analysis:

data_2017$Issue_Date <- SparkR::to_date(data_2017$Issue_Date, 'MM/dd/yyyy')
# there are cleaning issues here:
#data_2017$Vehicle_Expiration_Date <- SparkR::to_date(data_2017$Vehicle_Expiration_Date,'yyyyMMdd')
#data_2017$Date_First_Observed <- SparkR::to_date(data_2017$Date_First_Observed, 'MM/dd/yyyy')

####updating the view:

createOrReplaceTempView(data_2017, "data_2017_view") 

##4. Let's Understand the Range of ticket Issue Dates Available in the Dataset



##create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue

data_2017$Issue_Year <- year(data_2017$Issue_Date)
data_2017$Issue_Month <- month(data_2017$Issue_Date)

createOrReplaceTempView(data_2017, "data_2017_view")

#observe the Distribution of Issue Date:
Grouped_Issue_Date_2017 <- SparkR::sql("SELECT Issue_Year,
                                       Issue_Month,
                                       count(*)as Num_of_Records
                                       FROM data_2017_view
                                       GROUP BY Issue_Year,
                                       Issue_Month
                                       ORDER BY 1,2")

dfgrouped_issue_ym_2017 <- data.frame(head(Grouped_Issue_Date_2017, nrow(Grouped_Issue_Date_2017)))
View(dfgrouped_issue_ym_2017)

# Considering A Fiscal Year to extend from the July of Pervious Year to June of the Current Year.

data_2017 <- data_2017[data_2017$Issue_Date >= "2016-07-01" & 
                         data_2017$Issue_Date <= "2017-06-30"]

nrow(data_2017)  #10,539,563 

#The columns "No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation" 
#We will Check the Number of Null values in the aforementioned columns for 2017.

createOrReplaceTempView(data_2017, "data_2017_view")

CountNull_ExtraCol_2017 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       SUM(CASE WHEN Plate_ID IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Plate_ID,
                                       SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_stand_stop,
                                       SUM(CASE WHEN Hydrant_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Hydrant_Violation,
                                       SUM(CASE WHEN Double_Parking_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Double_Parking_Violation
                                       
                                       FROM data_2017_view")
head(CountNull_ExtraCol_2017)
####There are 79 records with null registration plate ID which is managable. 
##However, all the additional columns have only null values therefore we will  drop them to standardize 
##the dataset between the years.

#Removing these Null Columns.
data_2017<- drop(data_2017, c("No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation"))
colnames(data_2017)

###the string format of the time attributes include only a partial component of AM/PM. 
##Therefore we will append M to the end of each time attribute before converting it into a timestamp.
data_2017$Concat_M <- "M"
data_2017$Violation_Time<-concat(data_2017$Violation_Time,  data_2017$Concat_M)

data_2017$Time_First_Observed<- concat(data_2017$Time_First_Observed,data_2017$Concat_M)

data_2017$From_Hours_In_Effect<- concat(data_2017$From_Hours_In_Effect, data_2017$Concat_M)
data_2017$To_Hours_In_Effect<- concat(data_2017$To_Hours_In_Effect, data_2017$Concat_M)
data_2017<- drop(data_2017, c("Concat_M"))


#Extracting Violation Hour, Violation Minute and Part of Day.
data_2017$Violation_Hour <- substr(data_2017$Violation_Time, 1, 2)
data_2017$Violation_Minute <- substr(data_2017$Violation_Time, 4, 5)
data_2017$Violation_AMPM <- substr(data_2017$Violation_Time, 6, 7)

#Since  there are records that have both 00xxAM as well as 12xxAM. 
#Therefore we will replace all 00xxAM with 12xxAM
data_2017$Violation_Hour <- regexp_replace(x = data_2017$Violation_Hour,pattern = "\\00",replacement = "12")

#Concatenating the components into a standardized Violation Time.
data_2017$Violation_Time <- concat(data_2017$Violation_Hour, 
                                   data_2017$Violation_Minute, data_2017$Violation_AMPM)
#Converting Violation Time into a TimeStamp
data_2017$Violation_Time<-to_timestamp(x = data_2017$Violation_Time, format = "hhmma")

#Converting the other time attributes into a TimeStamp.
data_2017$Time_First_Observed<- to_timestamp(x= data_2017$Time_First_Observed, format = "hhmma")
data_2017$From_Hours_In_Effect<- to_timestamp(x= data_2017$From_Hours_In_Effect, format = "hhmma")
data_2017$To_Hours_In_Effect<- to_timestamp(x= data_2017$To_Hours_In_Effect, format = "hhmma")

### final 2017 data set
head(data_2017)
dim(data_2017)
str(data_2017)
#2015 Dataset has Rows:10,539,563 & Columns: 45
printSchema(data_2017)







##*********************************Queries for 2015:****************************************#######


#1.Find the total number of tickets for each year.
nrow(unique(select(data_2015,data_2015$Summons_Number)))
#10598035 unique tickets 	

#2.Find out the number of unique states from where the cars that got parking tickets came from
unique_states<-unique(select(data_2015,data_2015$Registration_State))
nrow(unique_states)
#69 states
head(unique_states,69)
#10th state has value 99,replacing it with state having maximum tickets.

unique_states<-SparkR::sql("select registration_state,count(*) as tkts from data_2015_view
                           group by registration_state
                           order by tkts desc")
head(unique_states,69)
# NY 8255530 has maximum number of tickets so replacing 99 with NY.

data_2015$Registration_State<-ifelse(data_2015$Registration_State=="99","NY",data_2015$Registration_State)

#updating the view
createOrReplaceTempView(data_2015, "data_2015_view")

#checking unique states again
nrow(unique(select(data_2015,data_2015$Registration_State)))
#there are 68 unique states

#3.Some parking tickets don't have the address for violation location on them, which is a cause for concern. 
#Write a query to check the number of such tickets.

violation_address<-SparkR::sql("Select * from data_2015_view where violation_location is null")
nrow(violation_address)

#result:1455166

#########################################*Queries for 2016******#################################

#Examine the data:

#1.Find the total number of tickets for each year.
nrow(unique(select(data_2016,data_2016$Summons_Number)))
#10396894 unique  tickets

#2.Find out the number of unique states from where the cars that got parking tickets came from
unique_states<-unique(select(data_2016,data_2016$Registration_State))
nrow(unique_states)
#68 states
head(unique_states,68)
#10th state has value 99,replacing it with state having maximum tickets.

unique_states<-SparkR::sql("select registration_state,count(*) as tkts from data_2016_view
                           group by registration_state
                           order by tkts desc")
head(unique_states,68)
# NY 8083903 has maximum number of tickets so replacing 99 with NY.

data_2016$Registration_State<-ifelse(data_2016$Registration_State=="99","NY",data_2016$Registration_State)

#updating the view
createOrReplaceTempView(data_2016, "data_2016_view")

#checking unique states again
nrow(unique(select(data_2016,data_2016$Registration_State)))
#there are 67 unique states

#3.Some parking tickets don't have the address for violation location on them, which is a cause for concern. 
#Write a query to check the number of such tickets.

violation_address<-SparkR::sql("Select * from data_2016_view where violation_location is null")
nrow(violation_address)

#result:1807140

#######################################****Queries for 2017****##################################

#Examine the data:

#1.Find the total number of tickets for each year.
nrow(unique(select(data_2017,data_2017$Summons_Number)))
#10539563  unique  tickets

#2.Find out the number of unique states from where the cars that got parking tickets came from
unique_states<-unique(select(data_2017,data_2017$Registration_State))
nrow(unique_states)
#67 states
head(unique_states,67)
#10th state has value 99,replacing it with state having maximum tickets.

unique_states<-SparkR::sql("select registration_state,count(*) as tkts from data_2017_view
                           group by registration_state
                           order by tkts desc")
head(unique_states,67)
# NY 8272874 has maximum number of tickets so replacing 99 with NY.

data_2017$Registration_State<-ifelse(data_2017$Registration_State=="99","NY",data_2017$Registration_State)

#updating the view
createOrReplaceTempView(data_2017, "data_2017_view")

#checking unique states again
nrow(unique(select(data_2017,data_2017$Registration_State)))
#there are 66 unique states

#3.Some parking tickets don't have the address for violation location on them, which is a cause for concern. 
#Write a query to check the number of such tickets.

violation_address<-SparkR::sql("Select * from data_2017_view where violation_location is null")
nrow(violation_address)

#result:1950083 

#######################################*************#AGGREGATION TASKS*****************##########################

#Aggregation queries for 2015:


#1.How often does each violation code occur? Display the frequency of the top five violation codes.
violation_freq<-SparkR::sql("Select Violation_code,count(*) as count_violationcode 
                            from data_2015_view 
                            group by Violation_code
                            order by count_violationcode desc")
head(violation_freq)

#Top 5 violation codes are:

# Violation_code count_violationcode                                            
#1             21             1469228
#2             38             1305007
#3             14              908418
#4             36              747098
#5             37              735600
#6             20              605008



#2.How often does each 'vehicle body type' get a parking ticket? How about the 'vehicle make'? 

vehicle_freq<-SparkR::sql("Select vehicle_body_type,count(*) as vehicle_freq
                          from data_2015_view
                          group by vehicle_body_type
                          order by vehicle_freq desc")
head(vehicle_freq)

#top 5 vehicle body type
#vehicle_body_type vehicle_freq                                                
#1              SUBN      3341110
#2              4DSD      3001810
#3               VAN      1570227
#4              DELV       822040
#5               SDN       428571
#6              2DSD       288800

vehiclemake_freq<-SparkR::sql("Select vehicle_make,count(*) as vehiclemake_freq
                              from data_2015_view
                              group by vehicle_make
                              order by vehiclemake_freq desc")
head(vehiclemake_freq)

#top 5 vehicle make
#vehicle_make vehiclemake_freq                                                 
#1         FORD          1373157
#2        TOYOT          1082206
#3        HONDA           982130
#4        CHEVR           811659
#5        NISSA           805572
#6        FRUEH           400562

#3.insights for parking violations in any specific areas of the city

Violation_Precinct_freq<-SparkR::sql("Select Violation_Precinct,count(*) as count_Violation_Precinct
                                     from data_2015_view
                                     where Violation_Precinct!=0
                                     group by Violation_Precinct
                                     order by count_Violation_Precinct desc")
head(Violation_Precinct_freq)

#result:
#Violation_Precinct count_Violation_Precinct                                   
#1                 19                   550797
#2                 18                   393802
#3                 14                   377750
#4                  1                   302737
#5                114                   295855
#6                 13                   281757
#Zone 19 has maximum number of violations


Issuer_Precinct_freq<-SparkR::sql("Select Issuer_Precinct,count(*) as count_Issuer_Precinct
                                  from data_2015_view
                                  where Issuer_Precinct!=0
                                  group by Issuer_Precinct
                                  order by count_Issuer_Precinct desc")
head(Issuer_Precinct_freq)

#result:
#Issuer_Precinct count_Issuer_Precinct                                         
#1              19                536627
#2              18                384863
#3              14                363734
#4               1                293942
#5             114                291100
#6              13                275196

#19 issuer pprecinct has issued maximum tickets


#4.Find the violation code frequency across three precincts which have issued the most number of tickets - 
#do these precinct zones have an exceptionally high frequency of certain violation codes? 
#Are these codes common across precincts? 

Issuer_Precinct_all<-SparkR::sql("(Select Issuer_Precinct,violation_code,count(*) as count_Issuer_Precinct
                                 from data_2015_view
                                 where Issuer_Precinct=19
                                 group by Issuer_Precinct,violation_code
                                 order by count_Issuer_Precinct desc limit 3)
                                 union all
                                 (Select Issuer_Precinct,violation_code,count(*) as count_Issuer_Precinct
                                 from data_2015_view
                                 where Issuer_Precinct=18
                                 group by Issuer_Precinct,violation_code
                                 order by count_Issuer_Precinct desc limit 3)
                                 union all
                                 (Select Issuer_Precinct,violation_code,count(*) as count_Issuer_Precinct
                                 from data_2015_view
                                 where Issuer_Precinct=14
                                 group by Issuer_Precinct,violation_code
                                 order by count_Issuer_Precinct desc limit 3)")
head(Issuer_Precinct_all,9)


#result:

# Issuer_Precinct violation_code count_Issuer_Precinct
#1              19             38                 89102
#2              19             37                 78716
#3              19             14                 59915
#4              18             14                119078
#5              18             69                 56436
#6              18             31                 30030
#7              14             69                 79330
#8              14             14                 75985
#9              14             31                 40410




##5  to find out the properties of parking violations across different times of the day:


createOrReplaceTempView(data_2015, "data_2015_view")
createOrReplaceTempView(data_2016, "data_2016_view")
createOrReplaceTempView(data_2017, "data_2017_view")


#Find a way to deal with missing values, if any.
null_violat_times_2015<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                     SUM(CASE WHEN Violation_Time is NULL
                                     THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                     100*SUM(CASE WHEN Violation_Time IS NULL
                                     THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2015_ViolationTimeMissing
                                     from data_2015_view")
head(null_violat_times_2015)

#2015 dataset has 0.5812% records with Missing Violation Time
##therefore can be removed before analysis.


adjusted_data_2015<- subset(data_2015, isNotNull(data_2015$Violation_Time))

adjusted_data_2015$Violation_Hour <- hour(cast(adjusted_data_2015$Violation_Time,dataType = "string"))

createOrReplaceTempView(adjusted_data_2015, "violt_2015")

#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 

violation_hour_bin_2015 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM violt_2015")


createOrReplaceTempView(violation_hour_bin_2015, "violt_hour_2015_nyc")

hour_bin_tkts_2015 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets,
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  count(*)as Frequency_of_Tickets
                                  FROM violt_hour_2015_nyc
                                  GROUP BY Violation_Hour_Bin,
                                  Violation_Code))
                                  WHERE Rnk <= 3")
df_hour_bin_tkts_2015 <- data.frame(head(hour_bin_tkts_2015, nrow(hour_bin_tkts_2015)))

#2015 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis

top_3_violations_2015 <- SparkR::sql("SELECT Violation_Code,
                                     count(*) no_of_tickets
                                     FROM violt_hour_2015_nyc
                                     GROUP BY Violation_Code
                                     ORDER BY no_of_tickets desc")

head(top_3_violations_2015,3)
#Top-3 Violation Code for 2015 are 21, 38 and 14

common_times_2015 <- SparkR::sql("SELECT Violation_Code,
                                 Violation_Hour_Bin,
                                 count(*) no_of_tickets
                                 FROM violt_hour_2015_nyc
                                 WHERE violation_code IN (21,38,14)
                                 GROUP BY Violation_Code, 
                                 Violation_Hour_Bin
                                 ORDER BY Violation_Code, 
                                 Violation_Hour_Bin,
                                 no_of_tickets desc")	
df_common_times_viol_2015 <- data.frame(head(common_times_2015, nrow(common_times_2015)))
ggplot(df_common_times_viol_2015, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("Plot14A. 2015 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)


#6. Checking for seasonality in dataset:
Season_Binning_2015 <- SparkR::sql("SELECT Summons_Number,
                                   Violation_Code,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 12
                                   THEN 'Fall' 
                                   END AS Season
                                   FROM data_2015_view")

createOrReplaceTempView(Season_Binning_2015, "season_tkt_2015_nyc")

tktseason_2015<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_tkt_2015_nyc
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2015)

freq_tktseason_2015<- data.frame(head(tktseason_2015))
freq_tktseason_2015$Fiscal_Year<- c(2015,2015,2015,2015)
freq_tktseason_2015
#Season Frequency_of_Tickets Fiscal_Year
#1 Spring              2860987        2015
#2 Summer              2838306        2015
#3   Fall              2718502        2015
#4 Winter              2180240        2015

#2015 Season vs. Violation Code Distribution Analysis

season_violation_2015 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_tkt_2015_nyc
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2015 <-  data.frame(head(season_violation_2015, nrow(season_violation_2015)))
df_season_violation_2015
#Season Violation_Code Frequency_of_Tickets
#1    Fall             21               351390
#2    Fall             38               326700
#3    Fall             14               232300
#4  Spring             21               425163
#5  Spring             38               327048
#6  Spring             14               243622
#7  Summer             21               439632
#8  Summer             38               344262
#9  Summer             14               239339
#10 Winter             38               306997
#11 Winter             21               253043
#12 Winter             14               193157

###### 7.

violationcd_frequency_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from data_2015_view
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2015,3)

fine_top3_2015<- data.frame(head(violationcd_frequency_2015,3))
fine_top3_2015$Fiscal_Year <- c(2015,2015,2015)
fine_top3_2015$Average_Fine_PerTicket<- c(55,50,115)
fine_top3_2015$Total_Fine_Amount<- fine_top3_2015$Frequency_of_Tickets * fine_top3_2015$Average_Fine_PerTicket
fine_top3_2015
#Violation_Code Frequency_of_Tickets Fiscal_Year Average_Fine_PerTicket Total_Fine_Amount
#1             21              1469228        2015                     55          80807540
##2             38              1305007        2015                     50          65250350
#3             14               908418        2015                    115         104468070


#AGGREGATION TASKS For 2016:


#1.How often does each violation code occur? Display the frequency of the top five violation codes.
violation_freq<-SparkR::sql("Select Violation_code,count(*) as count_violationcode 
                            from data_2016_view 
                            group by Violation_code
                            order by count_violationcode desc")
head(violation_freq)
#top 5 violation codes are:
#Violation_code count_violationcode                                            
#1             21             1497269
#2             36             1232952
#3             38             1126835
#4             14              860045
#5             37              677805
#6             20              600133

#2.How often does each 'vehicle body type' get a parking ticket? How about the 'vehicle make'? 
vehicle_freq<-SparkR::sql("Select vehicle_body_type,count(*) as vehicle_freq
                          from data_2016_view
                          group by vehicle_body_type
                          order by vehicle_freq desc")
head(vehicle_freq)

#top 5 vehicle body type
#vehicle_body_type vehicle_freq                                                
#1              SUBN      3393838
#2              4DSD      2936729
#3               VAN      1489924
#4              DELV       738747
#5               SDN       401750
#6              2DSD       271364



vehiclemake_freq<-SparkR::sql("Select vehicle_make,count(*) as vehiclemake_freq
                              from data_2016_view
                              group by vehicle_make
                              order by vehiclemake_freq desc")
head(vehiclemake_freq)

#top 5 vehicle make
#vehicle_make vehiclemake_freq                                                 
#1         FORD          1297363
#2        TOYOT          1128909
#3        HONDA           991735
#4        NISSA           815963
#5        CHEVR           743416
#6        FRUEH           415658

#3.insights for parking violations in any specific areas of the city

Violation_Precinct_freq<-SparkR::sql("Select Violation_Precinct,count(*) as count_Violation_Precinct
                                     from data_2016_view
                                     where Violation_Precinct!=0
                                     group by Violation_Precinct
                                     order by count_Violation_Precinct desc")
head(Violation_Precinct_freq)

#Violation_Precinct count_Violation_Precinct                                   
#1                 19                   545669
#2                 18                   325559
#3                 14                   318193
#4                  1                   299074
#5                114                   286741
#6                 13                   283485
#Zone 19 has maximum number of violations

Issuer_Precinct_freq<-SparkR::sql("Select Issuer_Precinct,count(*) as count_Issuer_Precinct
                                  from data_2016_view
                                  where Issuer_Precinct!=0
                                  group by Issuer_Precinct
                                  order by count_Issuer_Precinct desc")
head(Issuer_Precinct_freq)
#Issuer_Precinct count_Issuer_Precinct                                         
#1              19                532298
#2              18                317451
#3              14                309727
#4               1                290472
#5             114                282493
#6              13                278028
#19 issuer pprecinct has issued maximum tickets

#4.Find the violation code frequency across three precincts which have issued the most number of tickets - 
#do these precinct zones have an exceptionally high frequency of certain violation codes? 
#Are these codes common across precincts? 

Issuer_Precinct_all<-SparkR::sql("(Select Issuer_Precinct,violation_code,count(*) as count_Issuer_Precinct
                                 from data_2016_view
                                 where Issuer_Precinct=19
                                 group by Issuer_Precinct,violation_code
                                 order by count_Issuer_Precinct desc limit 3)
                                 union all
                                 (Select Issuer_Precinct,violation_code,count(*) as count_Issuer_Precinct
                                 from data_2016_view
                                 where Issuer_Precinct=18
                                 group by Issuer_Precinct,violation_code
                                 order by count_Issuer_Precinct desc limit 3)
                                 union all
                                 (Select Issuer_Precinct,violation_code,count(*) as count_Issuer_Precinct
                                 from data_2016_view
                                 where Issuer_Precinct=14
                                 group by Issuer_Precinct,violation_code
                                 order by count_Issuer_Precinct desc limit 3)")
head(Issuer_Precinct_all,9)

#Issuer_Precinct violation_code count_Issuer_Precinct
#1              19             38                 76178
#2              19             37                 74758
#3              19             46                 71509
#4              18             14                 98160
#5              18             69                 47129
#6              18             47                 23618




###5..
#Find a way to deal with missing values, if any.
null_violat_times_2016<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                     SUM(CASE WHEN Violation_Time is NULL
                                     THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                     100*SUM(CASE WHEN Violation_Time IS NULL
                                     THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2016_ViolationTimeMissing
                                     from data_2016_view")
head(null_violat_times_2016)
#2016 dataset 0.6114% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

adjusted_data_2016<- subset(data_2016, isNotNull(data_2016$Violation_Time))

adjusted_data_2016$Violation_Hour <- hour(cast(adjusted_data_2016$Violation_Time,dataType = "string"))

createOrReplaceTempView(adjusted_data_2016, "violt_2016")


#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2016 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM violt_2016")

createOrReplaceTempView(violation_hour_bin_2016, "violt_hour_2016_nyc")

hour_bin_tkts_2016 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets,
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  count(*)as Frequency_of_Tickets
                                  FROM violt_hour_2016_nyc
                                  GROUP BY Violation_Hour_Bin,
                                  Violation_Code))
                                  WHERE Rnk <= 3")

df_hour_bin_tkts_2016 <- data.frame(head(hour_bin_tkts_2016, nrow(hour_bin_tkts_2016)))


###Top-3 Violation Code vs. Violation Time Bin Analysis
top_3_violations_2016 <- SparkR::sql("SELECT Violation_Code,
                                     count(*) no_of_tickets
                                     FROM violt_hour_2016_nyc
                                     GROUP BY Violation_Code
                                     ORDER BY no_of_tickets desc")

head(top_3_violations_2016,3)
#Top-3 Violation Codes for 2016 are 21, 36 and 38.

common_times_2016 <- SparkR::sql("SELECT Violation_Code,
                                 Violation_Hour_Bin,
                                 count(*) no_of_tickets
                                 FROM violt_hour_2016_nyc
                                 WHERE violation_code IN (21,36,38)
                                 GROUP BY Violation_Code, 
                                 Violation_Hour_Bin
                                 ORDER BY Violation_Code, 
                                 Violation_Hour_Bin,
                                 no_of_tickets desc")	

df_common_times_viol_2016 <- data.frame(head(common_times_2016, nrow(common_times_2016)))
ggplot(df_common_times_viol_2016, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("Plot14B. 2016 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)


####6. a  Season vs. Frequency Analysis

Season_Binning_2016 <- SparkR::sql("SELECT Summons_Number,
                                   Violation_Code,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 12
                                   THEN 'Fall' 
                                   END AS Season
                                   FROM data_2016_view")

createOrReplaceTempView(Season_Binning_2016, "season_tkt_2016_nyc")

tktseason_2016<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_tkt_2016_nyc
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2016)

freq_tktseason_2016<- data.frame(head(tktseason_2016))
freq_tktseason_2016$Fiscal_Year<- c(2016,2016,2016,2016)
freq_tktseason_2016


## 6 b Season vs. Violation Code Distribution Analysis

season_violation_2016 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_tkt_2016_nyc
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2016 <-  data.frame(head(season_violation_2016, nrow(season_violation_2016)))
df_season_violation_2016

#Seasonwise Violation Code Distribution 2016
ggplot(df_season_violation_2016, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot12B. 2016 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



###7.
violationcd_frequency_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from data_2016_view 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2016,3)

fine_top3_2016<- data.frame(head(violationcd_frequency_2016,3))
fine_top3_2016$Fiscal_Year <- c(2016,2016,2016)
fine_top3_2016$Average_Fine_PerTicket<- c(55,50,50)
fine_top3_2016$Total_Fine_Amount<- fine_top3_2016$Frequency_of_Tickets * fine_top3_2016$Average_Fine_PerTicket
fine_top3_2016

##########################################AGGREGATION TASKS for 2017:

#1.How often does each violation code occur? Display the frequency of the top five violation codes.
violation_freq<-SparkR::sql("Select Violation_code,count(*) as count_violationcode 
                            from data_2017_view 
                            group by Violation_code
                            order by count_violationcode desc")
head(violation_freq)
#top 5 violation codes are:
#Violation_code count_violationcode                                            
#1             21             1500396
#2             36             1345237
#3             38             1050418
#4             14              880152
#5             20              609231
#6             37              588522

#2.How often does each 'vehicle body type' get a parking ticket? How about the 'vehicle make'? 
vehicle_freq<-SparkR::sql("Select vehicle_body_type,count(*) as vehicle_freq
                          from data_2017_view
                          group by vehicle_body_type
                          order by vehicle_freq desc")
head(vehicle_freq)

#top 5 vehicle body type
#vehicle_body_type vehicle_freq                                                
#1              SUBN      3632003
#2              4DSD      3017372
#3               VAN      1384121
#4              DELV       672123
#5               SDN       414984
#6              2DSD       269232



vehiclemake_freq<-SparkR::sql("Select vehicle_make,count(*) as vehiclemake_freq
                              from data_2017_view
                              group by vehicle_make
                              order by vehiclemake_freq desc")
head(vehiclemake_freq)

#top 5 vehicle make
#vehicle_make vehiclemake_freq                                                 
#1         FORD          1250777
#2        TOYOT          1179265
#3        HONDA          1052006
#4        NISSA           895225
#5        CHEVR           698024
#6        FRUEH           420636

#3.insights for parking violations in any specific areas of the city

Violation_Precinct_freq<-SparkR::sql("Select Violation_Precinct,count(*) as count_Violation_Precinct
                                     from data_2017_view
                                     where Violation_Precinct!=0
                                     group by Violation_Precinct
                                     order by count_Violation_Precinct desc")
head(Violation_Precinct_freq)

#Violation_Precinct count_Violation_Precinct                                   
#1                 19                   528317
#2                 14                   347736
#3                  1                   326961
#4                 18                   302008
#5                114                   292682
#6                 13                   242298
#Zone 19 has maximum number of violations


Issuer_Precinct_freq<-SparkR::sql("Select Issuer_Precinct,count(*) as count_Issuer_Precinct
                                  from data_2017_view
                                  where Issuer_Precinct!=0
                                  group by Issuer_Precinct
                                  order by count_Issuer_Precinct desc")
head(Issuer_Precinct_freq)
#Issuer_Precinct count_Issuer_Precinct                                         
#1              19                514786
#2              14                340862
#3               1                316776
#4              18                292237
#5             114                286316
#6              13                236837
#19 issuer pprecinct has issued maximum tickets

#4.Find the violation code frequency across three precincts which have issued the most number of tickets - 
#do these precinct zones have an exceptionally high frequency of certain violation codes? 
#Are these codes common across precincts? 

Issuer_Precinct_all<-SparkR::sql("(Select Issuer_Precinct,violation_code,count(*) as count_Issuer_Precinct
                                 from data_2017_view
                                 where Issuer_Precinct=19
                                 group by Issuer_Precinct,violation_code
                                 order by count_Issuer_Precinct desc limit 3)
                                 union all
                                 (Select Issuer_Precinct,violation_code,count(*) as count_Issuer_Precinct
                                 from data_2017_view
                                 where Issuer_Precinct=18
                                 group by Issuer_Precinct,violation_code
                                 order by count_Issuer_Precinct desc limit 3)
                                 union all
                                 (Select Issuer_Precinct,violation_code,count(*) as count_Issuer_Precinct
                                 from data_2017_view
                                 where Issuer_Precinct=14
                                 group by Issuer_Precinct,violation_code
                                 order by count_Issuer_Precinct desc limit 3)")
head(Issuer_Precinct_all,9)

#Issuer_Precinct violation_code count_Issuer_Precinct
#1              19             46                 84789
#2              19             38                 71631
#3              19             37                 71592
#4              18             14                 90145
#5              18             69                 36246
#6              18             47                 23487
#7              14             14                 73007
#8              14             69                 57316
#9              14             31                 39430


#Find a way to deal with missing values, if any.
null_violat_times_2017<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                     SUM(CASE WHEN Violation_Time is NULL
                                     THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                     100*SUM(CASE WHEN Violation_Time IS NULL
                                     THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2017_ViolationTimeMissing
                                     from data_2017_view")
head(null_violat_times_2017)
#2017 dataset 0.6114% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

adjusted_data_2017<- subset(data_2017, isNotNull(data_2017$Violation_Time))

adjusted_data_2017$Violation_Hour <- hour(cast(adjusted_data_2017$Violation_Time,dataType = "string"))

createOrReplaceTempView(adjusted_data_2017, "violt_2017")


#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2017 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM violt_2017")

createOrReplaceTempView(violation_hour_bin_2017, "violt_hour_2017_nyc")

hour_bin_tkts_2017 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets,
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  count(*)as Frequency_of_Tickets
                                  FROM violt_hour_2017_nyc
                                  GROUP BY Violation_Hour_Bin,
                                  Violation_Code))
                                  WHERE Rnk <= 3")

df_hour_bin_tkts_2017 <- data.frame(head(hour_bin_tkts_2017, nrow(hour_bin_tkts_2017)))

top_3_violations_2017 <- SparkR::sql("SELECT Violation_Code,
                                     count(*) no_of_tickets
                                     FROM violt_hour_2017_nyc
                                     GROUP BY Violation_Code
                                     ORDER BY no_of_tickets desc")

head(top_3_violations_2017,3)
#Top-3 Violation Codes for 2017 are 21, 36 and 38.

common_times_2017 <- SparkR::sql("SELECT Violation_Code,
                                 Violation_Hour_Bin,
                                 count(*) no_of_tickets
                                 FROM violt_hour_2017_nyc
                                 WHERE violation_code IN (21,36,38)
                                 GROUP BY Violation_Code, 
                                 Violation_Hour_Bin
                                 ORDER BY Violation_Code, 
                                 Violation_Hour_Bin,
                                 no_of_tickets desc")	

df_common_times_viol_2017 <- data.frame(head(common_times_2017, nrow(common_times_2017)))
ggplot(df_common_times_viol_2017, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("Plot14C. 2017 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)


###6.Season vs. Frequency Analysis
Season_Binning_2017 <- SparkR::sql("SELECT Summons_Number,
                                   Violation_Code,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 12
                                   THEN 'Fall' 
                                   END AS Season
                                   FROM data_2017_view")
createOrReplaceTempView(Season_Binning_2017, "season_tkt_2017_nyc")

tktseason_2017<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_tkt_2017_nyc
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2017)

freq_tktseason_2017<- data.frame(head(tktseason_2017))
freq_tktseason_2017$Fiscal_Year<- c(2017,2017,2017,2017)
freq_tktseason_2017

## 6b.
#Season vs. Violation Code Distribution Analysis

season_violation_2017 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_tkt_2017_nyc
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2017 <-  data.frame(head(season_violation_2017, nrow(season_violation_2017)))
df_season_violation_2017

#Seasonwise Violation Code Distribution 2017
ggplot(df_season_violation_2017, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot12C. 2017 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

##7.
violationcd_frequency_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from data_2017_view 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2017,3)

fine_top3_2017<- data.frame(head(violationcd_frequency_2017,3))
fine_top3_2017$Fiscal_Year <- c(2017,2017,2017)
fine_top3_2017$Average_Fine_PerTicket<- c(55,50,50)
fine_top3_2017$Total_Fine_Amount<- fine_top3_2017$Frequency_of_Tickets * fine_top3_2017$Average_Fine_PerTicket
fine_top3_2017


fine_top3_combined<- rbind(fine_top3_2015, fine_top3_2016, fine_top3_2017)

ggplot(fine_top3_combined, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot10A. Comparison of Top 3 Violation Code vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

ggplot(fine_top3_combined, aes(x=as.factor(Violation_Code), y=Total_Fine_Amount))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Violation Code") + ylab("Total Fine Amount") + ggtitle("Plot10B. Comparison of Top 3 Violation Code vs Total Fine Amount between Fiscal Years") + geom_text(aes(label=Total_Fine_Amount),vjust=-0.3)


