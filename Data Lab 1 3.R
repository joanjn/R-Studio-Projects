# This is a script to use any database that is large enough to graph something as part of the
# Data storage and retrieval tool. This will be accomplised using Exercise2.csv file.
#
# Course: CIS 6107
# Assignment: DataLab 1
# Author: Joan Nnadi
# Date: 11/5/2021
#======================================================================================
#
# ------------- Set working directory ------------------------------------------------
setwd("~/Documents/CIS 6107/R Studio Project")
#
# --------------- Load the Library ------------------------------------------------------
library(tidyverse)
library(scales)
install.packages("digest")
#
# ------------------ Load the Exercise2 file into the Dataframe -------------------------------------------
Lab1<-read_csv("Exercise2.csv", progress = show_progress())

#================================================================================================================================================================================================================================

# -------- Create a new dataframe for Price and Region for the timeframe  2003 -2019 -------------------
RegionPrice <- Lab1[which(Lab1$Year >=2003 & Lab1$Year <=2019),]

# -------------- Aggregate Price by Car Region -----------------------------------------------------------------------Price <- aggregate(ModelPrice$Price ~ ModelPrice$Region + ModelPrice$State, data = ModelPrice, mean)
RegionPrice <- aggregate(RegionPrice$Price ~ RegionPrice$Region + RegionPrice$State, data = RegionPrice, mean)

# ----------------Rename columns and sort by price -------------------------------------------------------------------------
colnames(RegionPrice) <- c("Region", "State", "Price") # Rename columns
RegionPrice <- RegionPrice[order(-RegionPrice$Price),]


# ============================================================================================================================================================================================================================
#------ (Graph 1) Price by Region for all vehicles manufactured in (2003-2019)

print(RegionPrice)

# ----(Graph 1) --Create horizontal barchart of average price by of all vehicles in all regions manufactured between 2003- 2019 ------------------------------------------------------------------------

ggplot(RegionPrice, aes(x= Region, y = Price)) +
geom_bar(stat = "identity", fill = "steelblue") +
ggtitle("Average Price by Region (2003 and 2019)") +
xlab ("Region") + ylab ("Price") +
theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
coord_flip()

# =========================================================================================================================================================
# ----- To see how prices have been treanding in all regions ---------------------------------------------
# ---(Graph 2) To create horizontal barplot of average price of all vehicles in all regions in 2003
ggplot(Lab1[which(Lab1$Year =="2003"),],aes(x= Region, y = Price)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Average Price by Region (2003") +
  xlab ("Region") + ylab ("Price in thousands") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  coord_flip()

# ====================================================================================================================================================

# ---- (Graph 3) To graph what price looks like in all regions after 3 years , we will graph for the year 2006 ----------------------------------------
# --- To create horizontal barplot of average price of all vehicles in all regions in 2006
ggplot(Lab1[which(Lab1$Year =="2006"),],aes(x= Region, y = Price)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Average Price by Region (2006)") +
  xlab ("Region") + ylab ("Price in thousands") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  coord_flip()
# ========================================================================================================================================
#
# ------( Graph 4) ---Price during the 2008 market crash in all regions ------------------------------------------------------------
# --- To create horizontal barplot of average price of all vehicles in all regions in 2008
ggplot(Lab1[which(Lab1$Year =="2008"),],aes(x= Region, y = Price)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Average Price by Region (2008") +
  xlab ("Region") + ylab ("Price in thousands") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  coord_flip()

#=================================================================================================================================================
# -------( Graph 5) -To graph what the price will look like in next 7 years, we will look at 2010 ------------------------------

# --- To create horizontal barplot of average price of all vehicles in all regions in 2010
ggplot(Lab1[which(Lab1$Year =="2010"),],aes(x= Region, y = Price)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Average Price by Region (2010)") +
  xlab ("Region") + ylab ("Price in thousands") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  coord_flip()