library(dplyr)
library(stats)
library(tidyr)
library(ggplot2)
library(stringr)
library(MASS)
library(car)


CarPrice<- read.csv("CarPrice_Assignment.csv")
str(CarPrice)


#1. DATA CLEANING:

#Check NA values in the whole data frame

colSums(is.na(CarPrice))  

#Checking for blanks

sapply(CarPrice, function(x) length(which(x == "")))

#Checking For duplicate values

sum(duplicated(CarPrice))

#keeping only the CarName from CarCompany Column,

SeparatedList <- str_split(CarPrice$CarName, pattern=" ")
new_col <- sapply(SeparatedList, function(x) x[1][1])
CarPrice[,"CarName"] <- new_col

# Convert all non-numeric columns to uppercase

CarPrice<- data.frame(lapply(CarPrice, function(v) {
  if (is.character(v) | (is.factor(v))) return(toupper(v))
  else return(v)
}))


#Removing Car ID since its ony a reference
CarPrice<-CarPrice[,-1]


#Checking for outlier values in every numeric variable and treating it if neccessary

boxplot(CarPrice$carheight, ylab= "Car Height")

boxplot(CarPrice$curbweight,ylab= "Curb Weight")

boxplot(CarPrice$boreratio,ylab= "Bore Ratio")


boxplot(CarPrice$enginesize)
quantile(CarPrice$enginesize, seq(0,1,0.01))


boxplot(CarPrice$peakrpm)
quantile(CarPrice$peakrpm, seq(0,1,0.01))


boxplot(CarPrice$carlength)
quantile(CarPrice$carlength, seq(0,1,0.01))


boxplot(CarPrice$wheelbase)
quantile(CarPrice$wheelbase, seq(0,1,0.01))


boxplot(CarPrice$citympg)
quantile(CarPrice$citympg, seq(0,1,0.01))

boxplot(CarPrice$horsepower)
quantile(CarPrice$horsepower, seq(0,1,0.01))


boxplot(CarPrice$stroke)
quantile(CarPrice$stroke, seq(0,1,0.01))


boxplot(CarPrice$compressionratio)
quantile(CarPrice$compressionratio, seq(0,1,0.01))


boxplot(CarPrice$highwaympg)
quantile(CarPrice$highwaympg, seq(0,1,0.01))

#Although there are outliers present in columns but removing them isnt necessary 
#eg compression ratio has outlers but they are for the ones having diesel cars. similarly for other columns


#Correcting Spelling Mistakes in each columns:
# IN CarName:
summary(CarPrice$CarName)

CarPrice$CarName<-gsub("MAXDA","MAZDA",CarPrice$CarName)
CarPrice$CarName<-gsub("PORCSHCE","PORSCHE",CarPrice$CarName)
CarPrice$CarName<-gsub("VOKSWAGEN","VOLKSWAGEN",CarPrice$CarName)
CarPrice$CarName<-gsub("VW","VOLKSWAGEN",CarPrice$CarName)
CarPrice$CarName<-gsub("TOYOUTA","TOYOTA",CarPrice$CarName)

# IN fuel type:
summary(CarPrice$fueltype)

#aspiration
summary(CarPrice$aspiration)

#doornumber
summary(CarPrice$doornumber)

#carbody
summary(CarPrice$carbody)

#drivewheel
summary(CarPrice$drivewheel)


#enginelocation
summary(CarPrice$enginelocation)

#enginetype
summary(CarPrice$enginetype)

summary(CarPrice$fuelsystem)


###### Modifying Data Types####

#checking number of  levels for all categorical columns:
sapply(colnames(CarPrice),function(x) levels(CarPrice[[x]]))

### CONVERTING COLUMNS WITH TWO LEVELS TO NUMERIC ONES:

#For fuel type we have two levels so let diesl=0,gas=1
levels(CarPrice$fueltype)<- c(0,1)
CarPrice$fueltype <- as.numeric(levels(CarPrice$fueltype))[CarPrice$fueltype]

#For Aspiration we have two levels so let STD=0,TURBO=1
levels(CarPrice$aspiration) <-c(0,1)
CarPrice$aspiration <- as.numeric(levels(CarPrice$aspiration))[CarPrice$aspiration]

#For door number  we have two levels so let FOUR=0,Two=1
levels(CarPrice$doornumber) <-c(0,1)
CarPrice$doornumber <- as.numeric(levels(CarPrice$doornumber))[CarPrice$doornumber]

#For enginelocation  we have two levels so let FRONT=0,rear=1
levels(CarPrice$enginelocation) <-c(0,1)
CarPrice$enginelocation <- as.numeric(levels(CarPrice$enginelocation))[CarPrice$enginelocation]


### CONVERTING COLUMNS WITH MORE THAN TWO LEVELS USING DUMMY VARIABLES:

#since symbolling is an ordered variable with integers, hence not creating any dummy varaible fr it

#1. carbody
dummy_cb <- data.frame(model.matrix( ~carbody, data = CarPrice))
dummy_cb <- dummy_cb[,-1]

#2 drive wheel

dummy_dw<- data.frame(model.matrix( ~drivewheel, data = CarPrice))
dummy_dw <- dummy_dw[,-1]


#3.engine type
dummy_et <- data.frame(model.matrix( ~enginetype, data =  CarPrice))
dummy_et <- dummy_et[,-1]

#4. cylindernumber
dummy_cyn <- data.frame(model.matrix( ~cylindernumber, data =  CarPrice))
dummy_cyn <- dummy_cyn[,-1]

#5. fuelsystem
dummy_fsys <- data.frame(model.matrix( ~fuelsystem, data =  CarPrice))
dummy_fsys <- dummy_fsys[,-1]

#6. Company name
dummy_comp <- data.frame(model.matrix( ~CarName, data =  CarPrice))
dummy_comp <- dummy_comp[,-1]

carPrices1 <- cbind(CarPrice[,setdiff(names(CarPrice),c("carbody","drivewheel","enginetype","cylindernumber",
                                                        "fuelsystem","CarName"))], dummy_cb, dummy_dw, dummy_et, dummy_cyn, dummy_fsys,dummy_comp)


####### Derived metrices #########

#1. Overall mpg 
carPrices1$Ompg <- round((carPrices1$citympg + carPrices1$highwaympg),2)

#2. Stroke2Bore Ratio (Engine friction is affected by the stroke-to-bore ratio)
carPrices1$sbr <- round(carPrices1$stroke/carPrices1$boreratio,2)

#3. Overall mpg to Horsepower ratio
carPrices1$Ohp <- round(carPrices1$Ompg/carPrices1$horsepower, 2)

#4. Overall mpg to curbweight ratio (FE)
carPrices1$FE <- round(carPrices1$Ompg/carPrices1$curbweight, 4)


## Setting seed to achieve reproducibility
set.seed(9999)

## seperating Training and test datasets
trainindices= sample(1:nrow(carPrices1), 0.7*nrow(carPrices1))
train = carPrices1[trainindices,]
test = carPrices1[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1) #Adjusted R-squared:  0.9598 


## using stepAIC to estimate the model
step <- stepAIC(model_1, direction = "both")
step


# Now store the last model equation of stepwise method into an object called model_2

model_2<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carwidth + carheight + curbweight + 
              enginesize + stroke + horsepower + peakrpm + citympg + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              enginetypeL + enginetypeOHC + enginetypeOHCF + cylindernumberFIVE + 
              cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
              fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
              CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
              CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
              CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO + sbr + 
              Ohp, data = train)

summary(model_2) #Adjusted R-squared:  0.9641

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)

#using sort to easily find the highest ones

sort(vif(model_2),decreasing =T)  

#Removing Cuberwt since it's vif is too high

model_3<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carwidth + carheight +enginesize + stroke + horsepower + 
              peakrpm + citympg + carbodyHARDTOP +carbodyHATCHBACK +carbodySEDAN +carbodyWAGON +drivewheelRWD + 
              enginetypeL + enginetypeOHC + enginetypeOHCF + cylindernumberFIVE + 
              cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
              fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
              CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
              CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
              CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO + sbr + 
              Ohp, data = train)

summary(model_3)    #Adjusted R-squared:  0.9614

sort(vif(model_3),decreasing =T)




#Removing horsepower since vif is too high and insignificant also.

model_4<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carwidth + carheight +
              enginesize + stroke + peakrpm + citympg + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              enginetypeL + enginetypeOHC + enginetypeOHCF + cylindernumberFIVE + 
              cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
              fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
              CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
              CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
              CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO + sbr + 
              Ohp, data = train)

summary(model_4)  #Adjusted R-squared:  0.9614 

sort(vif(model_4),decreasing =T)

##Removing ohp since vif is too high and insignificant also.

model_5<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carwidth + carheight +
              enginesize + stroke + peakrpm + citympg + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              enginetypeL + enginetypeOHC + enginetypeOHCF + cylindernumberFIVE + 
              cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
              fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
              CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
              CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
              CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO + sbr, data = train)

summary(model_5) #Adjusted R-squared:  0.9591

sort(vif(model_5),decreasing =T)

# Removing sbr
model_6<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carwidth + carheight +
              enginesize + stroke + peakrpm + citympg + carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              enginetypeL + enginetypeOHC + enginetypeOHCF + cylindernumberFIVE + 
              cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
              fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
              CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
              CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
              CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_6)   #Adjusted R-squared:  0.9557

sort(vif(model_6),decreasing =T)

#Removing citympg
model_7<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carwidth + carheight +
              enginesize + stroke + peakrpm +  carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              enginetypeL + enginetypeOHC + enginetypeOHCF + cylindernumberFIVE + 
              cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
              fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
              CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
              CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
              CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_7)  #Adjusted R-squared:  0.956

sort(vif(model_7),decreasing =T)

#Removing enginetypeOHC
model_8<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carwidth + carheight +
              enginesize + stroke + peakrpm +  carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
              cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
              fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
              CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
              CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
              CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_8)  #Adjusted R-squared:  0.9564 

sort(vif(model_8),decreasing =T)

#Removing stroke
model_9<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carwidth + carheight +
              enginesize +  peakrpm +  carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
              cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
              fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
              CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
              CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
              CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_9)  #Adjusted R-squared:  0.9568

sort(vif(model_9),decreasing =T)

#Removing carlength
model_10<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase +  carwidth + carheight +
              enginesize +  peakrpm +  carbodyHARDTOP + 
              carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
              enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
              cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
              fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
              CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
              CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
              CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_10)  #Adjusted R-squared:  0.9528

sort(vif(model_10),decreasing =T)

#Removing carwidth
model_11<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
               wheelbase +  carheight +
               enginesize +  peakrpm +  carbodyHARDTOP + 
               carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
               enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_11)  #Adjusted R-squared:  0.9529

sort(vif(model_11),decreasing =T)

#Removing doornumber   

model_12<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  carheight +
               enginesize +  peakrpm +  carbodyHARDTOP + 
               carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
               enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + CarNameSAAB + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_12)   #Adjusted R-squared:  0.9526

sort(vif(model_12),decreasing =T)

#Removing CarNameSAAB
model_13<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  carheight +
               enginesize +  peakrpm +  carbodyHARDTOP + 
               carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + drivewheelRWD + 
               enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_13) #Adjusted R-squared:  0.9528 

sort(vif(model_13),decreasing =T)

#Removing drivewheelRWD
model_14<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  carheight +
               enginesize +  peakrpm +  carbodyHARDTOP + 
               carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
               enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_14)  #Adjusted R-squared:  0.9526

sort(vif(model_14),decreasing =T)

#Removing peakrpm

model_15<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  carheight +
               enginesize +carbodyHARDTOP + 
               carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
               enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI + CarNameBMW + CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_15) #Adjusted R-squared:  0.9517 

sort(vif(model_15),decreasing =T)

#Removing CarNameBMW  
model_16<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  carheight +
               enginesize +carbodyHARDTOP + 
               carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
               enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI +CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_16)  #Adjusted R-squared:  0.9503

sort(vif(model_16),decreasing =T)

#Removing carbodySEDAN

model_17<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  carheight +
               enginesize +carbodyHARDTOP + 
               carbodyHATCHBACK + carbodyWAGON + 
               enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI +CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_17)  #Adjusted R-squared:  0.9462 

sort(vif(model_17),decreasing =T)

#Removing carbodyHARDTOp

model_18<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  carheight +
               enginesize+ 
               carbodyHATCHBACK + carbodyWAGON + 
               enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI +CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_18) #Adjusted R-squared:  0.9456

sort(vif(model_18),decreasing =T)

#Removing carbodyWAGON

model_19<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  carheight +
               enginesize+ 
               carbodyHATCHBACK +
               enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI +CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_19) #Adjusted R-squared:  0.946 

sort(vif(model_19),decreasing =T)

#Removing carbodyHATCHBACK  

model_20<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  carheight + enginesize+ enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI +CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_20)   #Adjusted R-squared:  0.9434

sort(vif(model_20),decreasing =T)

#Removing carheight

model_21<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  enginesize+ enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI +CarNameCHEVROLET + 
               CarNameDODGE + CarNameISUZU + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_21)  #Adjusted R-squared:  0.9391

sort(vif(model_21),decreasing =T)

#Removing CarNameISUZU
model_22 <-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               wheelbase +  enginesize+ enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
               fuelsystemMPFI + fuelsystemSPDI +CarNameCHEVROLET + 
               CarNameDODGE + CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_22)  #Adjusted R-squared:  0.9343 

sort(vif(model_22),decreasing =T)

#Removing fuelsystemSPDI 
model_23 <-lm(formula = price ~ fueltype + aspiration +enginelocation + 
                wheelbase +  enginesize+ enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + fuelsystem2BBL + 
                fuelsystemMPFI + CarNameCHEVROLET + 
                CarNameDODGE + CarNameMAZDA + CarNameMITSUBISHI + 
                CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
                CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_23) #Adjusted R-squared:  0.9305

sort(vif(model_23),decreasing =T)


# Removing fuelsystem2BBL
model_24 <-lm(formula = price ~ fueltype + aspiration +enginelocation + 
                wheelbase +  enginesize+ enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + 
                fuelsystemMPFI + CarNameCHEVROLET + 
                CarNameDODGE + CarNameMAZDA + CarNameMITSUBISHI + 
                CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
                CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_24) #Adjusted R-squared:  0.9249 

sort(vif(model_24),decreasing =T)

#Removing fueltype

model_25 <-lm(formula = price ~ aspiration +enginelocation + 
                wheelbase +  enginesize+ enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + 
                fuelsystemMPFI + CarNameCHEVROLET + 
                CarNameDODGE + CarNameMAZDA + CarNameMITSUBISHI + 
                CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
                CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_25) #Adjusted R-squared:  0.924 

sort(vif(model_25),decreasing =T)

#RemovingfuelsystemMPFI 
model_26 <-lm(formula = price ~ aspiration +enginelocation + 
                wheelbase +  enginesize+ enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX +CarNameCHEVROLET +CarNameDODGE + CarNameMAZDA + CarNameMITSUBISHI + 
                CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
                CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_26)   #Adjusted R-squared:  0.921

sort(vif(model_26),decreasing =T)

#Removing CarNameCHEVROLET

model_27 <-lm(formula = price ~ aspiration +enginelocation + 
                wheelbase +  enginesize+ enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + 
                CarNameDODGE + CarNameMAZDA + CarNameMITSUBISHI + 
                CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
                CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_27)   #Adjusted R-squared:  0.9173 

sort(vif(model_27),decreasing =T)

#Removing CarNameDODGE

model_28<-lm(formula = price ~ aspiration +enginelocation + 
                wheelbase +  enginesize+ enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
                cylindernumberFOUR + cylindernumberSIX + 
                CarNameMAZDA + CarNameMITSUBISHI + 
                CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
                CarNameTOYOTA + CarNameVOLKSWAGEN + CarNameVOLVO, data = train)

summary(model_28)  #Adjusted R-squared:  0.9135 

sort(vif(model_28),decreasing =T)

#Removing CarNameVOLVO

model_29<-lm(formula = price ~ aspiration +enginelocation + 
               wheelbase +  enginesize+ enginetypeL  + enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + 
               CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN , data = train)

summary(model_29)   #Adjusted R-squared:  0.9105 

sort(vif(model_29),decreasing =T)

#Removing enginetypeL
model_30<-lm(formula = price ~ aspiration +enginelocation + 
               wheelbase +  enginesize+enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + 
               CarNameMAZDA + CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN , data = train)

summary(model_30)  #Adjusted R-squared:  0.9083 
sort(vif(model_30),decreasing =T)

#Removing CarNameMAZDA 
model_31<-lm(formula = price ~ aspiration +enginelocation + 
               wheelbase +  enginesize+enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + 
               CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA + CarNameVOLKSWAGEN , data = train)

summary(model_31)  #Adjusted R-squared:  0.9069 
sort(vif(model_31),decreasing =T)

#Removing CarNameVOLKSWAGEN 
model_32<-lm(formula = price ~ aspiration +enginelocation + 
               wheelbase +  enginesize+enginetypeOHCF + cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + 
               CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA  , data = train)

summary(model_32)  #Adjusted R-squared:  0.9049

sort(vif(model_32),decreasing =T)

#Removing enginetypeOHCF  
model_33<-lm(formula = price ~ aspiration +enginelocation + 
               wheelbase +  enginesize+cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + 
               CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH + CarNameRENAULT + 
               CarNameTOYOTA  , data = train)

summary(model_33)  #Adjusted R-squared:  0.9019 

sort(vif(model_33),decreasing =T)

#Removingv CarNameRENAULT 
model_34<-lm(formula = price ~ aspiration +enginelocation + 
               wheelbase +  enginesize+cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + 
               CarNameMITSUBISHI + 
               CarNameNISSAN + CarNamePLYMOUTH+ 
               CarNameTOYOTA  , data = train)

summary(model_34)   #Adjusted R-squared:  0.8998 

sort(vif(model_34),decreasing =T)

#Removing CarNamePLYMOUTH  
model_35<-lm(formula = price ~ aspiration +enginelocation + 
               wheelbase +  enginesize+cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + 
               CarNameMITSUBISHI + 
               CarNameNISSAN +
               CarNameTOYOTA, data = train)

summary(model_35)   #Adjusted R-squared:  0.8972 

sort(vif(model_35),decreasing =T)

#Removing CarNameNISSAN 
model_36<-lm(formula = price ~ aspiration +enginelocation + 
               wheelbase +  enginesize+cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + 
               CarNameMITSUBISHI +
               CarNameTOYOTA, data = train)

summary(model_36)  #Adjusted R-squared:  0.8926 

sort(vif(model_36),decreasing =T)

#Removing CarNameMITSUBISHI 

model_37<-lm(formula = price ~ aspiration +enginelocation + 
               wheelbase +  enginesize+cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX + 
               CarNameTOYOTA, data = train)

summary(model_37)  #Adjusted R-squared:  0.8865 

sort(vif(model_37),decreasing =T)

#Removing CarNameTOYOTA 

model_38<-lm(formula = price ~ aspiration +enginelocation + 
               wheelbase +  enginesize+cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX, data = train)

summary(model_38) #Adjusted R-squared:  0.8818 

sort(vif(model_38),decreasing =T)

#Removing aspiration 
model_39<-lm(formula = price ~ enginelocation + 
               wheelbase +  enginesize+cylindernumberFIVE + 
               cylindernumberFOUR + cylindernumberSIX, data = train)

summary(model_39)   #Adjusted R-squared:  0.8744 

sort(vif(model_39),decreasing =T)

#Removing cylindernumberFIVE 
model_40<-lm(formula = price ~ enginelocation + 
               wheelbase +  enginesize+
               cylindernumberFOUR + cylindernumberSIX, data = train)

summary(model_40)  ##Adjusted R-squared:  0.8684
sort(vif(model_40),decreasing =T)

## As now our model has only significant parameters so 
## we can use this model i.e model_40 for our prediction. 


# predicting the results in test dataset
Predict_1 <- predict(model_40,test[,-19]) 

# In test data frame the dependent variable price is in 19th column hence excluding it. 

test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared ## 0.821