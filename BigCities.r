# This is a comment - line starts with a #
# Project to cluster big cities data

# Load any libraries we may need
library(corrgram) # Print correlation matrix
library(factoextra) # Print clustering

# Set the random seed for reproducability
set.seed(20)

###############################################
# Correlation
###############################################

# Read data from the file (using a relative path)
# Relative paths have . to represent the present working directory
BigCitiesK <- read.csv("./data/BigCities-K-means.csv")


# For correlation we only want the numberic columns
# dplyr is a library which facilitates data manipulation
# https://dplyr.tidyverse.org/index.html
# R has many cheat sheets, dplyr is here:
# https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf

# dplyr::select_if - The double colon means a function from dplyr library
# select_if selects columns from the table if it meets conditions
# in this case is.numeric
corData <- dplyr::select_if(BigCitiesK, is.numeric)

# Create a correlation matrix
corMatrix <- cor(corData, use="all.obs", method="pearson")

# Hmmm...can I have something pretty?
# https://www.statmethods.net/advgraphs/correlograms.html
corrgram(corData, order=TRUE, lower.panel=NULL,
         upper.panel=panel.shade, text.panel=panel.txt,
         main="Big Cities Data")

# Useful tutorial on K-means in R
# https://uc-r.github.io/kmeans_clustering#kmeans
BigKmeans <- kmeans(BigCitiesK, centers = 7, nstart = 25)

# Plot the cluster diagram
# Manual https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster
fviz_cluster(BigKmeans, data=BigCitiesK)

###############################################
# Regression
###############################################

# Read data for regression
BigCitiesR <- read.csv("./data/BigCities-reg.csv")

# Linear model is lm in R
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm

BigCitiesRegression <- lm(formula = BigCitiesR$Life.Expectancy.at.Birth..Years. ~ 
     BigCitiesR$Median.Household.Income..Dollars. +
     BigCitiesR$Percent.Foreign.Born +
     BigCitiesR$Percent.Living.Below.200..Poverty.Level +
     BigCitiesR$Percent.Who.Only.Speak.English.at.Home +
     BigCitiesR$Percent.Who.Speak.Spanish.at.Home +
     BigCitiesR$Percent.of.Children.Living.in.Poverty +
     BigCitiesR$All.Types.of.Cancer.Mortality.Rate..Age.Adjusted..Per.100.000.people. +
     BigCitiesR$Heart.Disease.Mortality.Rate..Age.Adjusted..Per.100.000.people. +
     BigCitiesR$Infant.Mortality.Rate..Per.1.000.live.births. +
     BigCitiesR$Percent.Unemployed +
     BigCitiesR$Percent.of.Adults.Who.Are.Obese +
     BigCitiesR$Percent.of.Adults.Who.Currently.Smoke +
     BigCitiesR$Percent.of.Low.Birth.Weight.Babies.Born +
     BigCitiesR$Percent.of.Population.with.a.Disability +
     BigCitiesR$Tuberculosis.Incidence.Rate..Per.100.000.people. +
     BigCitiesR$All.Cause.Mortality.Rate..Age.Adjusted..Per.100.000.people. +
     BigCitiesR$Asthma.Emergency.Department.Visit.Rate..Age.Adjusted..Per.10.000. +
     BigCitiesR$Diabetes.Mortality.Rate..Age.Adjusted..Per.100.000.people. +
     BigCitiesR$Firearm.Related.Emergency.Department.Visit.Rate..Age.Adjusted..Per.10.000.people. +
     BigCitiesR$Firearm.Related.Mortality.Rate..Age.Adjusted..Per.100.000.people. +
     BigCitiesR$Gonorrhea.Rate..Per.100.000.People. +
     BigCitiesR$Homicide.Rate..Age.Adjusted..Per.100.000.people. +
     BigCitiesR$Lung.Cancer.Mortality.Rate..Age.Adjusted..Per.100.000.people. +
     BigCitiesR$Motor.Vehicle.Mortality.Rate..Age.Adjusted..Per.100.000.people. +
     BigCitiesR$Opioid.Related.Unintentional.Drug.Overdose.Mortality.Rate..Age.Adjusted..Per.100.000.people. +
     BigCitiesR$Percent.of.High.School.Students.Who.Are.Obese, data=BigCitiesR)

BigCsummary <- summary(BigCitiesRegression)
BigCAnova <- anova(BigCitiesRegression)

plot(BigCitiesR$Life.Expectancy.at.Birth..Years., BigCitiesRegression$fitted.values)    


