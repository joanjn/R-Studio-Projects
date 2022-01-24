# ========================================================================================================================
# This is a script to conduct continued descriptive analysis using the Craglist data to better understand
# and gain more insight about the used car market through additional visualizations.
# This assignment is for use in CIS 6105 descriptive analysis.
#
# Course: CIS 6105
# Assignment: Exercise 3_Descriptive Analysis
# Author: Joan Nnadi
# Date: 10/09/2021
#======================================================================================
#
# ------------- Set working directory ------------------------------------------------
setwd("~/Documents/Applied Data Analytics CIS 6105/R ")
#
# --------------- Load the Library ------------------------------------------------------
library(tidyverse)
library(scales)
library(digest)
#
# ------------------ Load the Exercise2 file into the Dataframe -------------------------------------------
Exercise4Data <- read_csv("Exercise2.csv", progress = show_progress())
#
# ------------- Before running the scatterplot, we need to install the digest library for MacOS ------------------------
install.packages("digest")
#
# ------------ Creating a labelled Scatterplot of Miles and Price to explore the relationship --------------------------------
# -----------------that exists between vehicles miles and the asking price ------------------------------------------------------------------
# ----------- Since we need to set limits for both the Miles and Price so that the values can fit and be meaningful, xlim and ylim was used --------
#
ggplot(Exercise4Data, aes(x = Miles/1000, y = Price/1000)) + # Mileage (Independent variable), Price= (Dependent variable)
        labs(x = "Miles (thousands)", y = " Price (thousands) ") +
        xlim(1,300)+
        ylim(1, 100)+
        ggtitle("Vehicle Miles and Prices")+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))+  # To center and make the title bold
        geom_point(shape=18)+ # black diamonds to represent the color of the points on both x and y axis
        geom_smooth(method = lm, se=FALSE) # To add linear regression line
#
# ---------- Miles and Vehicle Year Bar Chart -----------------------------------------------------------------------------------------------------------------
# ----------Exploring the relationship patterns between the Vehicle Age and miles by creating ------------------------------------------------
# ----------a bar chart of average miles by vehicle year since 2010 ------------------------------------------------------------------------------
#
#---- Creating averages for the subset of Exercise4data -----------------------------------------------------------------------------------------------
Begin <- 2010
End <- 2021 - 1
# ------  Create a new variable called AgeData to prepare average aggregates of the variables---------------------------------------------------------------------------------------------------
AgeData <- aggregate(formula = Miles ~ Year,
           FUN = mean,
           subset = Year >= Begin & Year <= End,
           data = Exercise4Data)
#
# ------------ Rounding the Miles variables --------------------------------------------------------------------------
AgeData$Miles <- round(AgeData$Miles, 0)
#
# ---------------- Barplot of Average miles of Vehicles by Year------------------------------------------------------------
ggplot(AgeData, aes(x = Year, y = Miles)) +
       geom_bar(stat = "identity", fill= "steelblue")+
       scale_x_continuous(breaks = c(Begin : End))+
       scale_y_continuous(labels = scales :: comma)+
       geom_text(aes(label = format(Miles, big.mark = ",")),
                 vjust = 2, size = 3.5)+
       ggtitle("Average Vehicle Miles by Year")+
       xlab("Year")+
       ylab("Miles")+
       theme(plot.title = element_text(hjust = 0.5, face = "bold"))
#
# ---------------- Horizontal Barplot of the average price for five most common vehicle makes ----------------------------------
# ----------- Explore how the price of the five most common vehicle makes is affected in the used car market---------------------
# -----------This will be accomplished by creating a horizontal labelled barchart of the average prices of -----------------------
# ------------------ the five most common vehicle makes ---------------------------------------------------------------------------
#
# ----------- First we will create a new variable called TopFiveMakes to use in the barplot ----------------------------------------------------------
Exercise4Data$TopFiveMakes <- Exercise4Data$Make 
#
#---------  Then repopulate the new variable TopFiveMakes with aggregates of the existing AgeData and finally plot the horizontal barchart----------------------
AgeData <- aggregate(formula = Price ~ TopFiveMakes,
                     FUN = mean,
                     data = Exercise4Data) 
AgeData <- AgeData[order(-AgeData$Price),] [1:5,]
#
# -------- Rounding to remove the decimal points in prices --------------------------------------------------------------
AgeData$Price <- round(AgeData$Price, 0)
#
# ---------- Creating the barplot of the five most common cars ---------------------------------------------------------
ggplot(AgeData, aes(x= reorder(TopFiveMakes, Price), y = Price))+
        geom_bar(stat = "identity", feel = "steelblue")+
        geom_text(aes(label=format(Price, big.mark = ",")),
                  hjust = 1, size = 3.5)+
        ggtitle(" Top Five Most Common Average Car Prices")+
        xlab("Make") + ylab("Price")+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
        coord_flip()
#
# ---------------------- Horizontal barplot of the average vehicle mileage for the five most common makes ----------------------------------------------
# ------------------Explore the mileage of the five most common makes and find the average mileage using a labelled horizontal barchart
#
Exercise4Data$FiveTopMiles <- Exercise4Data$Miles
#
# ------------ Repopulating the new variable with the aggregate
AgeData <- aggregate(formula = Miles~ TopFiveMakes,
                     FUN = mean,
                     data = Exercise4Data) 
AgeData <- AgeData[order(-AgeData$Miles),] [1:5,]
#
# ------------- Rounding to remove the decimal point in miles from the aggregated data -----------------------------------------------------------------
#       
AgeData$Miles <- round(AgeData$Miles, 0)
#
# ---------- Creating the barplot of the five most common cars  and their average miles---------------------------------------------------------
ggplot(AgeData, aes(x= reorder(TopFiveMakes, Miles/1000), y = Miles/1000))+
        geom_bar(stat = "identity", feel = "steelblue")+
        geom_text(aes(label=format(Miles, big.mark = ",")),
            hjust = 1, size = 3.5)+
        ggtitle(" Top Five Most Common Average Vehicle Car Mileages")+
        xlab("Make") + ylab("Miles (in thousands)")+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
        coord_flip()
