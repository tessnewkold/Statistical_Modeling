#Tess Newkold
#March 2, 2019 
#Statistical Modeling 
# Project 1 - Part 3

# Packages ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(fastDummies)
library(PerformanceAnalytics)
library(MASS) #to perform AIC function
library(nnet)


# Part 1 Part 1 Questions 1-9 ---------------------------------------------
# Initial Exploration of the Data
# Step 1 ------------------------------------------------------------------
FAA1 <- read_excel("/Users/Tess/Documents/Tess/Ohio/UniversityOfCincinnati/BuisnessAnalytics/Spring2019/StatisticalModeling/Week1/FAA1.xls")
FAA2 <- read_excel("/Users/Tess/Documents/Tess/Ohio/UniversityOfCincinnati/BuisnessAnalytics/Spring2019/StatisticalModeling/Week1/FAA2.xls")

# Step 2 ------------------------------------------------------------------
str(FAA1)
str(FAA2)

# Step 3 ------------------------------------------------------------------
#combinedTEST <- merge(FAA1, FAA2, all.x = TRUE, all.y = TRUE)
combinedFAA <- merge(FAA1, FAA2, all = TRUE)
combinedFAA[duplicated(combinedFAA$speed_ground),]

# Step 4 ------------------------------------------------------------------
str(combinedFAA)
summary(combinedFAA)
sd(combinedFAA$no_pasg)
sd(combinedFAA$speed_ground)
sd(combinedFAA$speed_air, na.rm = TRUE)
sd(combinedFAA$height)
sd(combinedFAA$pitch)
sd(combinedFAA$distance)
sd(combinedFAA$duration, na.rm = TRUE)


# Step 5 ------------------------------------------------------------------
# Step 5. 
# •	There are 850 observations and 8 variables in the combined data set.
# •	There are a significant number of missing values in the Speed_Air variable, there are a few in duration, the rest do not have any missing.
# •	Some of the variables look a little messy, but they will be cleaned up in further steps

# Data Cleaning and Further Exploration -----------------------------------

# Step 6 ------------------------------------------------------------------
filterFAA <- filter(combinedFAA, duration > 40 | is.na(duration))
filterFAA1 <- filter(filterFAA, speed_ground >= 30 & speed_ground <= 140 | is.na(speed_ground))
filterFAA2 <- filter(filterFAA1, speed_air >= 30 & speed_air <= 140 | is.na(speed_air))
filterFAA3 <- filter(filterFAA2, height > 6 | is.na(height))
cleanFAA <- filter(filterFAA3, distance < 6000 | is.na(distance))



# Part 3  -----------------------------------------------------------------

# Question 1 --------------------------------------------------------------
#create multinomial variables
cleanFAA$landing[cleanFAA$distance < 1000] <- 1
cleanFAA$landing[cleanFAA$distance >= 1000 & cleanFAA$distance < 2500] <- 2
cleanFAA$landing[cleanFAA$distance >= 2500] <- 3

FAA <- subset(cleanFAA, select = -c(distance))

hist(FAA$landing, main = "Distribution of Landings", col = 'blue', xlab = ' ')
pct <- round(table(FAA$landing) / length(FAA$landing) * 100, 1)
labs <- c("Short Landings", "Regular Landings", "Long Landings")
labs <- paste(labs, pct)
labs <- paste(labs, "%", sep = " ")
pie(table(FAA$landing), labels = labs, col = rainbow(length(labs)), main = "Pie Chart of Landings")

modelNoPasg <- multinom(FAA$landing ~ FAA$no_pasg)
summary(modelNoPasg)
#model for speed on ground
modelSpeedOnGround <- multinom(FAA$landing ~ FAA$speed_ground)
summary(modelSpeedOnGround)
#model for speed in air
modelSpeedInAir <- multinom(FAA$landing ~ FAA$speed_air)
summary(modelSpeedInAir)
#model for height
modelHeight <- multinom(FAA$landing ~ FAA$height)
summary(modelHeight)
#model for pitch
modelPitch <- multinom(FAA$landing ~ FAA$pitch)
summary(modelPitch)
#make dummy variable for aircraft Airbus = 0, Boeing = 1
dummy <- as.numeric(FAA$aircraft == "airbus", 0, 1)
#model for aircraft
modelAircraft <- multinom(FAA$landing ~ dummy, data = FAA)
summary(modelAircraft)

#model for duration
modelDuration <- multinom(FAA$landing ~ duration, data = FAA)
summary(modelDuration)


#because of multicolinearity we are exculding speed in air, and bc of missing 
#values we are excluding duration 
fullModel <- multinom(landing ~ no_pasg + speed_ground + height + pitch + aircraft, data = FAA)
summary(fullModel)

stepModel <- step(fullModel, na.action = na.omit)
summary(stepModel)

#calculating Odd ratio
exp(9.220361)
plot(jitter(landing,0.1)~jitter(speed_ground), FAA, xlab="Speed On Ground", ylab = "Long Landing", pch = ".")

ggplot(FAA, aes(x = speed_ground, fill = landing)) + 
  geom_histogram(position = "dodge", binwidth = 1) +
  facet_grid(~landing)

ggplot(FAA,aes(x=height,fill=landing))+geom_histogram(position="dodge",binwidth=0.5) +
  facet_grid(~landing)

hist(FAA$no_pasg, main = "Distribution of Number of Pasengers", col = 'red', xlab = ' ')

model6 <- lm(scaledFAA$no_pasg_scaled ~ speed_ground_scaled +distance_scaled + speed_air_scaled + height_scaled + pitch_scaled + duration_scaled + dummy_standard, data = scaledFAA)
summary(model6)


FAA_noAircraft <- subset(FAA, select = -c(aircraft))
FAA_noAircraft

chart.Correlation(FAA_noAircraft, histogram=TRUE, pch=19)
