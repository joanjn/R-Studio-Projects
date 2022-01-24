# ===============================================================================================================
# This is a Script to load, modify data and create a new calculated filed to prepare data
# to identify a new location for a used car dealership for use in CIS 6105 business analysis.
# This measure will give the organization insights that will help in decision making process.
#
# Course : CIS 6105
# Assignment : Excercise 2_Data Preparation
# Author: Joan Nnadi
# Date: 9/25/2021
# ===============================================================================================================
#
# ------- Set working Directory -------------------------------------------------
 setwd("~/Documents/Analytics Projects/CIS 6105/Unit 3_ Excecise 2")
#
# ---- Install Libraries---------------------------------------------------------
 install.packages("tidyverse")
#
# ---- Load Libraries -------------------------------------------------------------
#      common data wrangling functions
 library(tidyverse)
#
# Load the Craglist CSV file into dataframe ---------------------------------
 MNCarsData <- read_csv("MNCars.csv", progress = show_progress())
#
# --- -- View loaded data ------
 View(MNCarsData) 
#
#---------- Removing MNCars columns that will not be used on analysis---------------------------------------------

 MNCarsData <- subset(MNCarsData, select = -c(row, url, region_url,
                                              vin, image_url, county, lat,long))
#
# ------To Rename Column Names on the remaining 18 variables of MNCarsData--------------------
 colnames(MNCarsData) <-c("ID", "Region","Price", "Year", "Make", 'Model', "Condition",
                         "Cylinders", "Fuel", "Miles", "Title", "Transmission", "Drive",
                         "Size", "Type", "Color", "Description", "State")
#
# -------- Removing all vehicles with an asking price of $0 or $50,000 ---------------------------
 MNCarsData <-MNCarsData[MNCarsData$Price != 0 | MNCarsData$Price <= 50000,]
#
 MNCarsData <- MNCarsData[MNCarsData$Price !=0,]
#
 MNCarsData<- MNCarsData[MNCarsData$Price <= 50000,] 
# ---- A final MNCarsData output returns 9968 objects of 18 variables without $0 or $50,000-------------- -----------
#
# ------------ Updating the State Column so that Data values in this column are in UpperCase --------------------------------------
 MNCarsData$State <- toupper(MNCarsData$State)
#
# ------ Keeping only Vehicles listed in Minnesota ----------------
 MNCarsData <- MNCarsData[which(MNCarsData$State == "MN"),]
#
# ---------- Replace zeros in Miles with NA -------------------------------------------
 MNCarsData$Miles[MNCarsData$Miles == 0] <- NA
 MNCarsData$Miles
#
# ----------------- Change NA to blanks -----------------------------------------------------
 MNCarsData$Miles[is.na(MNCarsData$Miles)] <- "  "  
 MNCarsData$Miles
#         
# --------To calculate the Annual Miles ----------------------------------------------
# --- First Create a new variable "Age" to represent the current year -----------------
#
 MNCarsData$Age = 2021  
#
# ------------- Three step method of calculating the Annual Miles ------------------------ 
 MNCarsData$AnnualCarAge <- MNCarsData$Age - MNCarsData$Year   # Finds the difference in Current year 2021 and given data year-------------------------------------------------------
 MNCarsData$AnnualMiles <- as.numeric (MNCarsData$Miles) / MNCarsData$AnnualCarAge  # Actual Annual Miles used as.numeric to avid errors when operating integers and binary operators------
 MNCarsData$AnnualMiles <- round(MNCarsData$AnnualMiles, digits = 0)     # Rounded the decimal place to 0
#
# --------------- Creating an output file of the dataframe as Excercise_2.csv-----------------------------------------------------------------------------------------------------------------
 write.csv(MNCarsData, file = "Excercise_2.csv", row.names = FALSE)
 