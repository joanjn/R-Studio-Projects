# ========================================================================================================================
# This is a script to conduct descriptive analysis using the Craglist data to better understand
# the used car market. This assignment is for use in CIS 6105 descriptive analysis.
#
# Course: CIS 6105
# Assignment: Exercise 3_Descriptive Analysis
# Author: Joan Nnadi
# Date: 10/02/2021
#======================================================================================
#
# ------------- Set working directory ------------------------------------------------
 setwd("~/Documents/Applied Data Analytics CIS 6105/R ")
#
# --------------- Load the Library ------------------------------------------------------
 library(tidyverse)
#
# ------------------ Load the Exercise2 file into the Dataframe -------------------------------------------
 ExerciseData <- read_csv("Exercise2.csv", progress = show_progress())
#
# ----- View Loaded ExerciseData File-----------------------------------------------------
view(ExerciseData)
#
# ------------------ Display summary data for columns Year, Price, Miles, Annual Miles---------------
 summary(ExerciseData$Year)
 summary(ExerciseData$Price)
 summary(ExerciseData$Miles)
 summary(ExerciseData$AnnualMiles)
#
# ----------------- Calculating median, mean for Year, Price, Miles, AnnualMiles -------------------------
 median(ExerciseData$Year, na.rm = TRUE)
 mean(ExerciseData$Year, na.rm = TRUE)
#
  median(ExerciseData$Price, na.rm = TRUE)
  mean(ExerciseData$Price, na.rm = TRUE)
#
  median(ExerciseData$Miles, na.rm = TRUE)
  mean(ExerciseData$Miles, na.rm = TRUE)
#
 median(ExerciseData$AnnualMiles, na.rm = TRUE)
 mean(ExerciseData$AnnualMiles, na.rm = TRUE)
#
# ----------------- Calculating the Standard Deviation from Year, Price, Miles and Annual Miles --------------------------
#
 sd(ExerciseData$Year, na.rm = TRUE)
#
 sd(ExerciseData$Price)
#
 sd(ExerciseData$Miles, na.rm = TRUE)
#
 sd(ExerciseData$AnnualMiles, na.rm = TRUE)
#
# ------------------Creating a boxplot for Miles Frequency using Vehicles < 300,000--------------------------------------------
#
 boxplot(ExerciseData$Miles[ExerciseData$Miles < 300000],
          main = " Vehicles Mileage", ylab = "Cars Odometer Reading", col = "red")
# 
# --------------Creating a Histogram that is labelled and formatted, ----------------------------------------------------------
# -------------- for Price Frequency for Vehicles between $0 and $50,000 -------------------------------------------------------
#
 bins = seq(0, 50000, by = 1000)
 hist(ExerciseData$Price, breaks = bins, include.lowest = TRUE,
      main = "Vehicle Prices Frequency Distribution", xlab = "Prices of Vehicles", col = "purple") 
#
# -------------- Creating a labelled and formatted barchart for color frequency distribution -------------------------------------
 ColorCounts <- table(ExerciseData$Color)
 barplot(sort(ColorCounts, increasing = True), main = "Vehicles Colors Distribution",
         xlab = "Different Colors of Vehicles", col = "green") 
#
# ----------------- Creating barplot of the 10 most common Vehicle Make Frequency ----------------------------------------------------
 ExerciseData$WinMake <- ExerciseData$Make
 MakeCounts <- table(ExerciseData$WinMake)
 barplot(sort(MakeCounts, decreasing = TRUE) [1:10], main = "Top Ten Vehicles Make",
         xlab = "Make of Most Common Cars", col ="blue")
#
# ----------------- Creating barplot of the 10 least common vehicle make
#
 ExerciseData$WinMake <- ExerciseData$Make
 MakeCounts <- table(ExerciseData$WinMake)
 barplot(sort(MakeCounts, increasing  = TRUE) [1:10], main = "Least Ten Vehicle Makes", 
              xlab = "Make of Least Used Cars", col= "orange")
#
# ------------------------------------------------------------------------------------------------


