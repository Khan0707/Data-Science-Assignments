library(stats)
library(dplyr)
library(tidyr)
library(stringr)



#CheckPoint1:

#Load the companies and rounds data into two data frames: companies and rounds2 

companies<- read.delim("companies.txt",header=TRUE, stringsAsFactors = FALSE)
rounds2<- read.csv("rounds2.csv",header = TRUE, stringsAsFactors = FALSE)

#Converting permalinks to lowercase to 
#remove the case sensitivity issue:{Data Cleaning}

companies$permalink=str_to_lower(companies$permalink)
rounds2$company_permalink=str_to_lower(rounds2$company_permalink)

#1.How many unique companies are present in rounds2?

UniqueInRound2<-n_distinct(rounds2$company_permalink)
UniqueInRound2

#2.How many unique companies are present in companies?

UniqueInCompanies<-n_distinct(companies$permalink)
UniqueInCompanies

#3.In the companies data frame, which column can be used as the unique key for each company? 

#Permalink.

#4.Are there any companies in the rounds2 file which are not present in companies?

Difference<-setdiff(rounds2$company_permalink,companies$permalink)
length(Difference)

#5.Merge the two data frames:
#name the merged frame master_frame.

#5a. Merge:

master_frame= merge(companies,rounds2,by.x="permalink",by.y="company_permalink",all.x = T)

#5b.How many observations are present in master_frame?

Observations<-nrow(master_frame)
Observations

#Checkpoint 2:
#Finding the best Funding type based on the average values falling between 5 to 15 million USD :

#Average funding amount of venture type:

#Finding whether there are NA values present in the column "raised_amount_usd" of master_frame:

sum(is.na(master_frame$raised_amount_usd))

#Since the number of values having NA is around 17.3 % they cant be ignored. 
#Hence replacing NA with 0 :

master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0



#Grouping on Fund_Round_type, and calculating average of raised_amount_type:

FundGroup <- group_by(master_frame, funding_round_type)
FundInvestment <- summarise(FundGroup , mean(raised_amount_usd,na.rm = T))
colnames(FundInvestment) <- c("Fund_Type", "Avg_Raised_amt")

#From the FundGroup dataframe, we can extract all the average values required for the Table 1.1 results:
#venture,angel,seed and private equity

#Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round ,
#filtering the avg_raised_amt Funding type that lies between this range :

SuitableType<-filter(FundInvestment, between(FundInvestment$Avg_Raised_amt,5000000,15000000))
SuitableType[which.max(SuitableType$Avg_Raised_amt),]

#Hence at the end of Checkpoint 2, "venture" type is the suitable type of Funding falling between 
#5 to 15 million USD.


#Checkpoint 3: Country Analysis
#Finding the top 9 countries having the highest total funding and choosing the top 3 English 
#speaking countries for further analysis:

#Top nine countries which have received the highest total funding in venture type:

VentureType<-filter(master_frame,funding_round_type=="venture")
CountryGroup<-aggregate(raised_amount_usd~country_code,VentureType,sum)
SortedVenture<-CountryGroup[order(CountryGroup$raised_amount_usd,decreasing = T),]
Top9=head(SortedVenture,9) 

##Since the third value in the top 9 countries is an unknown country, 
#assuming it to be any other country would be a wrong step.
#and extract top 10 countries and consider them as top 9 excluding the blank country.

top9<-head(SortedVenture,10)[-3,]

#As a result of Checkpoint 3, we have extracted the top 9 countries having highest investment.
#Of them, the top 3 English speaking coutries are:
#USA
#GBR(UK)
#IND

#Checkpoint 4: Sector Analysis:
#Finding the best sector to be invested in with the use of the mapping file and the amount invested in each sector:

#Load mapping file

mapping=read.csv("mapping.csv",header = T,stringsAsFactors = F)

##Replace "0" with "na" in the category_list of mapping file:{Data Cleaning-spelling errors}

mapping$category_list <- gsub("0", "na",mapping$category_list)

##Extract the primary sector of each category list from the category_list column of the master_frame:

SeparatedList <- str_split(master_frame$category_list, pattern="\\|")
primary_sector <- sapply(SeparatedList, function(x) x[1][1])
master_frame[,"primary_sector"] <- primary_sector

#Converting the primary_sector and category_list columns to lowercase

master_frame$primary_sector <- str_to_lower(master_frame$primary_sector)

mapping$category_list <- str_to_lower(mapping$category_list)


#Wide to Long Conversion:

mapping_long <- gather(mapping, main_sector, val, 2:10)
mapping_long <- mapping_long[!(mapping_long$val == 0), ]
mapping_long <- mapping_long[,-3]

#Merging the primary sector and the category list:

final_file <- merge(master_frame, mapping_long, by.x = "primary_sector", by.y = "category_list",all.x=T)

#Inorder to  strictly  keep only 8 main sectors for our analysis,we chose the following filter
#to remove NA's and "Blanks" from main_sector and primary_sector columns of the merged file.

final_file<- filter(final_file,primary_sector!="",main_sector!="",main_sector!="Blanks") 

#Even after removing Blanks from main_sector there were about 98 NAs, so again filtered it.
#To verify:

sum(is.na(final_file$main_sector))
sum(is.na(final_file$primary_sector))

#Checkpoint 5:
#Creating dataframes grouped based on main sector.
#Since  the range of funding preferred by Spark Funds is 5 to 15 million USD so we filter our data frames
#on raised amount within this range: 

ED1<-group_by(filter(final_file,country_code=="USA",funding_round_type=="venture",
                     raised_amount_usd >=5000000 & raised_amount_usd <= 15000000),main_sector)

ED2<-group_by(filter(final_file,country_code=="GBR",funding_round_type=="venture",
                     raised_amount_usd >=5000000 & raised_amount_usd <= 15000000),main_sector)

ED3<-group_by(filter(final_file,country_code=="IND",funding_round_type=="venture",
                     raised_amount_usd >=5000000 & raised_amount_usd <=15000000),main_sector)


# Now,Summarise the main sectors with Total raised amount and  number of investments

USA_main_sector <- summarise(ED1, sum(raised_amount_usd), n())
colnames(USA_main_sector) <- c("main_sector","Total_Raised_amt_usd","no. of investments")

GBR_main_sector <- summarise(ED2, sum(raised_amount_usd), n())
colnames(GBR_main_sector) <- c("main_sector","Total_Raised_amt_usd","no. of investments")

IND_main_sector <- summarise(ED3, sum(raised_amount_usd), n())
colnames(IND_main_sector) <- c("main_sector","Total_Raised_amt_usd","no. of investments")



# As asked we need everything to be in one data frame 
#so Merging main_sector data frames to ED1,ED2,ED3 :

##Hence the final 3 dataframes d1,d2,d3 are as follows :-


d1 <- merge(USA_main_sector,ED1, by = "main_sector")
d2 <- merge(GBR_main_sector,ED2, by = "main_sector")
d3 <-merge(IND_main_sector,ED3, by = "main_sector")

##Finding the total count of investments for top 3 countries(USA,GBR,IND):

Total_USA_investments =nrow(d1)
Total_USA_investments

Total_GBR_investments=nrow(d2)
Total_GBR_investments

Total_IND_investments=nrow(d3)
Total_IND_investments

## Finding the total investments in top 3 countries (USA,GBR,IND):

Amount_USA=sum(USA_main_sector$Total_Raised_amt_usd)
Amount_USA

Amount_GBR=sum(GBR_main_sector$Total_Raised_amt_usd)
Amount_GBR

Amount_IND=sum(IND_main_sector$Total_Raised_amt_usd)
Amount_IND



#For the top sector count-wise (point 3), which company received the highest investment?

#Considering highest investment made to be the maximum one

# In USA: TOP_sector = Others

top_US_sector=filter(ED1,main_sector=="Others")
USA_company_received_highest_investment=top_US_sector[which.max(top_US_sector$raised_amount_usd),3]
USA_company_received_highest_investment

# In GBR:TOP_sector= Others

top_GBR_sector=filter(ED2,main_sector=="Others")
GBR_company_received_highest_investment=top_GBR_sector[which.max(top_GBR_sector$raised_amount_usd),3]
GBR_company_received_highest_investment

# In IND:TOP_sector= Others

top_IND_sector=filter(ED3,main_sector=="Others")
IND_company_received_highest_investment=top_IND_sector[which.max(top_IND_sector$raised_amount_usd),3]
IND_company_received_highest_investment

#IN USA: Second_sector=  Social..Finance..Analytics..Advertising

top2_US_sector=filter(ED1,main_sector=="Social..Finance..Analytics..Advertising")
Sec_USA_company_received_highest_investment=top2_US_sector[which.max(top2_US_sector$raised_amount_usd),3]
Sec_USA_company_received_highest_investment


#In GBR: Second_sector= Social..Finance..Analytics..Advertising


top2_GBR_sector=filter(ED2,main_sector=="Social..Finance..Analytics..Advertising")
Sec_GBR_company_received_highest_investment=top2_GBR_sector[which.max(top2_GBR_sector$raised_amount_usd),3]
Sec_GBR_company_received_highest_investment

#In IND: Second_sector= Social..Finance..Analytics..Advertising

top2_IND_sector=filter(ED3,main_sector=="News..Search.and.Messaging")
Sec_IND_company_received_highest_investment=top2_IND_sector[which.max(top2_IND_sector$raised_amount_usd),3]
Sec_IND_company_received_highest_investment

#Exporting To Excel:
write.csv(final_file,"FinalFile4.csv")










