# ===============================================================================================================
# This is a Script to load, modify data and create a new calculated field to prepare data
# to identify new regional sales outlet for Ecommerce funiture dealership in CIS 6105 business analysis.
# This measure will give the organization insights that will help in decision making process.
#
# Course : CIS 6105
# Assignment : Final Project_Data Preparation
# Author: Joan Nnadi
# Date: 10/26/2021
# ===============================================================================================================
#
# ------- Set working Directory -------------------------------------------------
setwd("~/Documents/CIS 6105 Final Project/Project/Ecommerce")
#
# ---- Install Libraries---------------------------------------------------------
install.packages("tidyverse") # for data wrangling
#
# ---- Load Libraries -------------------------------------------------------------
#      common data wrangling functions
library(tidyverse)
library(readr)
library(scales)
library(digest)
#
# Load the Ecommerce CSV file into dataframe ---------------------------------
BusinessData <-read_csv("Ecommerce.csv", progress = show_progress())

#---------- Removing Ecommerce columns that will not be used on analysis---------------------------------------------

 BusinessData <-BusinessData[c( "Order Date", "Ship Mode", "Country", "City", "State", "Region", "Product ID","Category", "Product Name","Sales", "Quantity")]

 # =========================================================================================================================================================================

# ------ Round Sales to Whole numbers ----------------------------------------------------------------------------------------------------------------
 
BusinessData$Sales <- round(BusinessData$Sales, digits = 0)



# -------- Removing all business with a sales of 10 or 200 ---------------------------

BusinessData <-BusinessData[BusinessData$Sales !=10 & BusinessData$Sales <= 200,]

#
# ------------ Keeping only where Sales was in California -----------------------------------------------
 
BusinessData<-BusinessData[which(BusinessData$State == "California"),]

head(BusinessData)

#============================================================================================
# --------------- Creating an output file of the dataframe as RProject.csv-----------------------------------------------------------------------------------------------------------------
write.csv(BusinessData, file = "RProject.csv", row.names = FALSE)


#  ==========================================================================================================================

#---------- Perform Descriptive Analysis ------------------------------------------------------------------------------
# -------- Display summary data for columns Sales, Quantity sold ---------------
summary(BusinessData$Sales)
summary(BusinessData$Quantity)

# ----------------- Calculating median, mean for Sales and Quantity to understand the nature of the variables-------------------------
median(BusinessData$Sales, na.rm = TRUE)
mean(BusinessData$Sales, na.rm = TRUE)

median(BusinessData$Quantity, na.rm = TRUE)
mean(BusinessData$Quantity, na.rm = TRUE)

# ==========================================================================================================================================
# ------------------Creating a boxplot for Sales Frequency using Quantity < 100--------------------------------------------
#
boxplot(BusinessData$Sales[BusinessData$Sales < 100],
        main = " Product Sales in California", ylab = "Number Sold", col = "red")

# --------------------------------------------------------------------------------------------------------------------------------------
#
boxplot(BusinessData$Sales[BusinessData$Sales > 100],
        main = " Product Sales in California", ylab = "Number Sold", col = "blue")
# ============================================================================================================================================

# ------ To compare the frequency sales in the city of San Francisco when sales are <100  -----------
boxplot(BusinessData$Sales[BusinessData$Sales < 100 & BusinessData$City == "San Francisco"],
        main = " Product Sales in California", ylab = "Number Sold", col = "purple")

# --------------------------------------------------------------------------------------------------------------------------------------------------
# -------------- Comparing Sales in the City of San Francisco for the sale ranges of > 100
#
boxplot(BusinessData$Sales[BusinessData$Sales >100 & BusinessData$City == "San Francisco"],
        main = " Product Sales in San Francisco", ylab = "Number Sold", col = "green")
#

# =======================================================================================================================================================
#
#----------  For Sales Frequency between 0 and 5000 --------------------------------------------------
bins = seq(0, 5000, by = 1000)
hist(BusinessData$Sales, breaks = bins, include.lowest = TRUE,
     main = "California Ecommerce Sales Frequency Distribution", xlab = "Sales of Category", col = "orange") 

#========================================================================================================================================================
# -------------- Creating a labelled and formatted barchart for Category frequency distribution of the types of products sold -------------------------------------
CategoryCounts <- table(BusinessData$Category)
barplot(sort(CategoryCounts, increasing = True), main = "Products Category Distribution",
        xlab = "Different Categories of Products Sold in California", col = "green")
#
#====================================================================================================================================================================
#
# ----------------- Creating barplot of the 10 most common Products Ship Mode Frequency ----------------------------------------------------
BusinessData$`Ship Mode` <- BusinessData$`Order Date`
OrderDateCounts <- table(BusinessData$`Ship Mode`)
barplot(sort(OrderDateCounts, decreasing = TRUE) [1:10], main = "Top Ten Shipping Methods",
        xlab = "Most common Order Date", col ="yellow")
#
# ======================================================================================================================================================================

# ---------- Creating the barplot of the five most common City ---------------------------------------------------------
ggplot(AgeData, aes(x= reorder(TopFiveMakes, Price), y = Price))+
  geom_bar(stat = "identity", feel = "steelblue")+
  geom_text(aes(label=format(Price, big.mark = ",")),
            hjust = 1, size = 3.5)+
  ggtitle(" Top Five Most Common Average Car Prices")+
  xlab("Make") + ylab("Price")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  coord_flip()

# ==================================================================================================================================================

# -------------- Aggregate Sale by City when quantity of Category of products ordered is 2 to 9 ----------------------------------------------------------------------
# -------------Create a new Dataframe called CitySale -----------------------------------------------------------------------------------------------------------------------------------------------------------
CitySale <- BusinessData[which(BusinessData$Quantity >= 2 & BusinessData$Quantity <= 9),]

CitySale <- aggregate(CitySale$Sales ~ CitySale$City + CitySale$Category, data = CitySale, mean)

# ----------------Rename columns and sort by price -------------------------------------------------------------------------
colnames(CitySale) <- c("City", "Category", "Sales") # Rename columns

 # ----------------- Round the Sales column  to remove the decimal places -------------------------------------------------------------
#
CitySale$Sales <-round(CitySale$Sales, 0) # Remove the decimals 

CitySale<- CitySale[order(-CitySale$Sales),] # order by sales


# --------Create horizontal barchart of average Sales by City of Ten top quantity------------------------------------------------------------------------

ggplot(CitySale [which(CitySale$City !="Technology"),], aes(x= Sales, y = Category)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Average Sales by Category when quantity ordered is (2 9)") +
  xlab ("Sales") + ylab ("Category") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  coord_flip()
#
# ==============================================================================================================================================================
## ------------ Creating a labelled Scatterplot of Quantity and Salesto explore the relationship --------------------------------
# -----------------that exists Ecommerce quantity and the projected sales ------------------------------------------------------------------  # ----------- Since we need to set limits for both the Miles and Price so that the values can fit and be meaningful, xlim and ylim was used --------
#
  ggplot(BusinessData, aes(x = Sales, y = Quantity)) + # Quantity (Independent variable),  Sales = (Dependent variable)
    labs(x = "Quantity", y = " Sales") +
    xlim(1,300)+
    ylim(1, 100)+
    ggtitle("California 2020 Ecommerce Sales and Quantity")+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+  # To center and make the title bold
    geom_point(shape=18)+ # black diamonds to represent the color of the points on both x and y axis
    geom_smooth(method = lm, se=FALSE) # To add linear regression line
  #
#---==================================================================================================================================================================

# ------------------- To uncover more insights about what happened in other regions or other states , let's look look sales thattook place outside the State of California-------------------------
#
#--------- --------- Let's create a new dataframe called RegionSales using a horizontal barplot to calculate the most Common Regions with highest sales------------------------------------------------------------------------
# ------------------- Let's find the distribution of Most common Regions --------------------------------------------------------------------------------------------------------------------

  RegionSales <-table(BusinessData$Region[which(BusinessData$Region != "West")])
  
  barplot(sort(RegionSales,decreasing = TRUE), main = " Most Common Non Western Regions",
          xlab ="Sales", col="red", horiz = TRUE)
  
# --------------- To find which State was driving those non-western regional sales besides California -------------------------------------------------------------------------------------------------
#-------------- Create a new dataframe StateSales ----------------------------------------------------------------------------------------
StateSales <- table(BusinessData$State[which(BusinessData$State != "California")]) 

# ------------ Create a formatted Barplot to represent the Six common States ecommerce Sales except California -----------------------------------------------     
    barplot(sort(StateSales,decreasing = TRUE)[1:6], main = " Six Most Common States Ecommerce in 2020",
          xlab ="Sales", col="green", horiz = TRUE)
  
# ==================================================================================================================================================================  
# -------------- To look at Most common shipping options for Ecommerce in all states ------------------------------------------
  # -------------- Creating a labelled and formatted barchart for Ship Mode frequency distribution of the types of Shipping methods -------------------------------------
  ShippingCounts <- table(BusinessData$`Ship Mode`)
  
  barplot(sort(ShippingCounts, increasing = True), main = "Products Shipping Distribution",
          xlab = "Sales", col = "blue", horiz = TRUE)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

