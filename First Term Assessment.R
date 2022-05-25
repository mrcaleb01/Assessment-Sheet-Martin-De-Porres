#LOAD PACKAGES
pacman::p_load(pacman, dplyr, ggvis, ggthemes, ggplot2, GGally,
               httr, lubridate, plotly, rio, rmarkdown, stringr, shiny,
               tidyr, tidyverse)

# IMPORT DATASET
rio_assessment <- import("C:\\Users\\Caleb\\Desktop\\Martin De Porres\\Term 1\\Assessment Scores\\New folder\\First Term Combined Dataset.xlsx")
view(rio_assessment)
head(assessment)
head(rio_assessment)

plot(rio_assessment $ Stream)

#Plotting a Categorical and Quantitative variables 
plot(rio_assessment $ Stream, rio_assessment $ RES)

# Create a data- subset from rio_assessment
# I'm using only Raw Exams scores for this analysis
#Let the subset variable be called "assessment"
Acacia <- rio_assessment [rio_assessment $ stream == "ACACIA",]
view(iris)

Acacia1 <- rio_assessment[rio_assessment $ Stream == "ACACIA",]
Acacia1

Cypress <- rio_assessment [rio_assessment $ Stream == "CYPRESS",]
Cypress

Oak <- rio_assessment[rio_assessment $ Stream == "OAK",]
Oak

Olive <- rio_assessment[rio_assessment $ Stream == "OLIVE",]
Olive

# Making simple Graphs from datasets
# Create a summary table of each sub-variable created for Bar chart
#analysis
#Then run a "plot" OR "Barplot" command

Acacia_RES <- table(Acacia1$RES)
view(Acacia_RES)
plot(Acacia_RES)
barplot(Acacia_RES)

Cypress_RES <- table(Cypress$RES)
view(Cypress_RES)

Oak_RES <- table(oak$RES)
view(Oak_RES)

Olive_RES <- table(Olive$RES)
view(Olive_RES)

#These lines were a complete disaster!
par(mfrow = c(4,1))
hist(Acacia1 $ RES)
hist(Cypress $ RES)
hist(Olive $ RES)
hist(Oak $ RES)

hist(Acacia_RES)
hist(rio_assessment $ RES)

#Attempt number 2 in trying to view each class histogram 
#simultaneously - SUCCESS!
par(mfrow = c(4,1))

#Histogram for each class stream
hist(Acacia1 $ RES,
     xlim = c(0,100),
     main = "Raw Exams Scores for Acacia - SCIENCE",
     xlab = "",
     col = "red")

hist(Cypress $ RES,
     xlim = c(0,100),
     main = "Raw Science Exams Scores for Cypress",
     xlab = "",
     col = "purple")

hist(Oak $ RES,
     xlim = c(0,100),
     main = "Raw Exams Scores for Oak",
     xlab = "",
     col = "green")

hist(Olive $ RES,
     xlim = c(0,100),
     main = "Raw Exams Scores for Olive",
     xlab = "",
     col = "blue")

# Ending the multiple frame parameter function - back to 1,1
par(mfrow = c(1,1))


# Using "Density" to plot a histogram, instead of "frequency
# I'm using the same code lines I used for the previous successful
# code lines for the histogram plot
#Them I'll make an overlapping Normal Distribution curve
hist(Acacia1 $ RES,
     xlim = c(0,100),
     freq = FALSE,      #this causes the axis to show density
     main = "Raw Exams Scores for Acacia - SCIENCE",
     xlab = "",
     col = "red",
     add = FALSE)

curve(dnorm(x, mean = mean(Acacia1 $ RES), sd (Acacia1 $ RES)),
      col = "blue",
      lwd = 2,
      add = TRUE)

# Doing an expanded form of summary using "Psych"
p_load(psych)

describe(Acacia1 $ RES)
            

hist(Cypress $ RES,
     xlim = c(0,100),
     freq = FALSE,
     main = "Raw Science Exams Scores for Cypress",
     xlab = "",
     col = "purple")

curve(dnorm(x, mean = mean(Cypress$RES), sd (Cypress$RES)),
      col = "red",
      lwd = 2,
      add = TRUE)

#Adding Two-Kernel density estimators
lines(density(Cypress$RES), col = "thistle 4", lwd = 2)

#Adding a rug plot
rug(Cypress$RES, lwd = 2, col = "grey")

hist(Oak $ RES,
     xlim = c(0,100),
     freq = FALSE,
     main = "Raw Exams Scores for Oak",
     xlab = "",
     col = "green")

hist(Olive $ RES,
     xlim = c(0,100),
     freq = FALSE,
     main = "Raw Exams Scores for Olive",
     xlab = "",
     col = "blue")

#Comparing Frequency plots to Density plots for histograms
par(mfrow = c(2,1))

# histogram plot for Acacia using (i) frequency and 
# (ii) Density
hist(Acacia1 $ RES,
     xlim = c(0,100),
     main = "Raw Exams Scores for Acacia - SCIENCE",
     xlab = "",
     col = "red")

hist(Acacia1 $ RES,
     xlim = c(0,100),
     freq = FALSE,
     main = "Raw Exams Scores for Acacia - SCIENCE",
     xlab = "",
     col = "red")
#Summary - They basically give the same plot graphs!



#Putting graphs into 3 different rows and a column

par(mfrow = c(3,1))

#Histogram for each class stream
hist(rio_assessment$RES
     [rio_assessment $Stream == "ACACIA"],
      xlim = c(0,4),
       main = "Raw Exams Scores for Acacia",
       xlab = "",
       col = "red")
      
par(mfrow = c(1,1))

hist()
# Do a scatterplot correlation between RES and 