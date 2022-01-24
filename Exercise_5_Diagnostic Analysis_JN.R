# ========================================================================================================================
# This is a script to conduct continued diagnostic analysis using the Exercise2.csv
# and gain more insight about the high cost of Ford and Chervolet and estimate regions with appealing
# volumes and prices.
# This assignment is for use in CIS 6105 diagnostic analysis.
#
# Course: CIS 6105
# Assignment: Exercise 5_Diagnostic Analysis
# Author: Joan Nnadi
# Date: 10/19/2021
#======================================================================================
#
# ------------- Set working directory ------------------------------------------------
setwd("~/Documents/Applied Data Analytics CIS 6105/R ")
#
# --------------- Load the Library ------------------------------------------------------
library(tidyverse)
#
# ------------------ Load the Exercise2 file into the Dataframe -------------------------------------------
Exercise5Data <- read_csv("Exercise2.csv", progress = show_progress())
head(Exercise5Data)
#

# 4 ---- To display the ten Ford models from 2010 -2020 with the highest average sales price ----------------------
# ----- we will create a new dataframe to select those column ranges that we want to investigate -------------------
ModernData<- Exercise5Data[which(Exercise5Data$Year >= 2010 & Exercise5Data$Year <= 2021),]
#
# view(ModernData)
#
# ---------- Aggregate data to find the average Price by Model and Make. This means that for each Year -----------------
# ---------- and car model, we are going to look at the average Price of the used cars----------------------------------------
#--------- Lets create another variable called AvgPriceData -----------------------------------------------------------------
AvgPriceData <-aggregate(ModernData$Price ~ ModernData$Make + ModernData$Model + ModernData$Year, data = ModernData, mean)
#
# ----------------- Round the Price to remove the decimal places -------------------------------------------------------------
#
AvgPriceData$`ModernData$Price` <-round(AvgPriceData$`ModernData$Price`, 0)
#
# ----------------- Rename columns and sort by price --------------------------------------------------------------------------
colnames(AvgPriceData) <-c("Make", "Model","Year", "Price")
AvgPriceData<-AvgPriceData[order(ModernData$Make, ModernData$Model, ModernData$Year),]
#
# --------------- Display the listings of top ten Ford models with the highest average sale price ------------------------------------------------------------
# --- Ten Car make with the highest sales ------------------------------------------------------------- 
#
#---------------- The 10 Ford Models with highest average sales price, we will sortb by price -----------------------------------------------------
AvgPriceData <- AvgPriceData[order(-AvgPriceData$Price),]

head(AvgPriceData[which(AvgPriceData$Make == "ford"),], n=10)
#-----------------------------------------------------------------------------------------------------------------------------------------

# ---------------Looking at this from another perspective -----------------------------------------------------------------------------------------
# ------- To display the ten Ford models (2010-2020) with the highest average sales price----------------------------------
# --------- data values in the dataframe will be matched using %% ------------------------------------------------------------------
ford<-Exercise5Data %>% filter(Make=="ford", Year >=2010 & Year <=2020)
# ------- removing NA values ------------------------------------------------------------------------
ford<-na.omit(ford) # Drop NA Value from the data frame
ford %>% group_by(Model) %>% summarise(Avg_Sale_Price=mean(Price)) %>% arrange(desc(Avg_Sale_Price)) %>% head(10) # chaining the commands using the forward pipe operator
# 
#-------------- To display all Chevrolet models from (2010-2020) and the average sales price for each ----------------------------------
chevrolet<-Exercise5Data %>% filter(Make =="chevrolet", Year >=2010 & Year <= 2020)
chevrolet$Exercise5Data<-chevrolet %>% select(Price,Year,Model)
chevroletExercise5Data<-na.omit(chevroletExercise5Data) # dropping the NA values using na.omit
chevroletExercise5Data %>% group_by(Model) %>% summarise(average_sales_price=round(mean(Price),0)) %>% arrange(desc(average_sales_price))
#
# --------------To display all Chevrolet models from (2010-2020) and the total volume for each in descending order-----------------------------------------------
chevroletExercise5Data %>% group_by(Model) %>% summarise(Total_Volume=n()) %>% arrange(desc(Total_Volume))
#
# --------------To display all Ford models from (2010-2020) and the total volume for each in descending order ------------------------------------------------------------------------------
ford %>% group_by(Model) %>% summarise(Total_Volume=n()) %>% arrange(desc(Total_Volume))
#
# ----------------To display all GMC models from (2010-2020) and the total volume for each--------------------------------------------------------------------------------------
gmc<-Exercise5Data %>% filter(Make=="gmc", Year >=2010 & Year <=2020)
gmcExercise5Data<-na.omit(gmcExercise5Data) # To drop missing values using the na.omit
gmcExercise5Data<-gmc %>% select(Price,Year,Model)
gmcExercise5Data %>% group_by(Model) %>% summarise(Total_Volume=n()) %>% arrange(desc(Total_Volume))
#
# ----------------To display all Toyota models from (2010-2020) and the total volume for each--------------------------------------------------------------------------------------------
toyota<-Exercise5Data %>% filter(Make=="toyota", Year>=2010 & Year <=2020)
toyotaExercise5Data<-toyota %>% select(Price,Year,Model)
toyotaExercise5Data<-na.omit(toyotaExercise5Data) # To drop the missing vaalue usingna.omit
toyotaExercise5Data %>% group_by(Model) %>% summarise(Total_Volume=n()) %>% arrange(desc(Total_Volume))
#
#---------------------- To display the ten regions with the highest total sales price -----------------------------------------------------------------------------------------------------
Exercise5Data %>% select(Region, Price) %>% group_by(Region) %>% summarise(Total_Sales=sum(Price)) %>% arrange(desc(Total_Sales)) %>% head(10)
#
#------------------------To dDisplay the ten regions with the highest average sales price -------------------------------------------------------------------------------------------------------
Exercise5Data %>% select(Region, Price) %>% group_by(Region) %>% summarise(Avg_Sales_Price=mean(Price)) %>% arrange(desc(Avg_Sales_Price)) %>% head(10)
#
# -------------------------To display the ten regions with the largest volume of used cars -----------------------------------------------------------------------------------------------
Exercise5Data %>% select(Region, Price) %>% group_by(Region) %>% summarise(volume=n()) %>% arrange(desc(volume)) %>% head(10)

