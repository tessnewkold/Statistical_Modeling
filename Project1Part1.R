#Tess Newkold
#Jan 23, 2019 
#Statistical Modeling 
# Project 1 - Part 1

# Packages ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(fastDummies)
library(PerformanceAnalytics)
library(MASS) #to perform AIC function

# Initial Exploration of the Data -----------------------------------------

FAA1 <- read_excel("/Users/Tess/Documents/Tess/Ohio/UniversityOfCincinnati/BuisnessAnalytics/Spring2019/StatisticalModeling/Week1/FAA1.xls")
FAA2 <- read_excel("/Users/Tess/Documents/Tess/Ohio/UniversityOfCincinnati/BuisnessAnalytics/Spring2019/StatisticalModeling/Week1/FAA2.xls")

str(FAA1)
str(FAA2)

#combinedTEST <- merge(FAA1, FAA2, all.x = TRUE, all.y = TRUE)
combinedFAA <- merge(FAA1, FAA2, all = TRUE)
combinedFAA[duplicated(combinedFAA$speed_ground),]

# numberOccur <- data.frame(table(combinedFAA))
# numberOccur[numberOccur$Freq > 1, ]

str(combinedFAA)
summary(combinedFAA)
sd(combinedFAA$no_pasg)
sd(combinedFAA$speed_ground)
sd(combinedFAA$speed_air, na.rm = TRUE)
sd(combinedFAA$height)
sd(combinedFAA$pitch)
sd(combinedFAA$distance)
sd(combinedFAA$duration, na.rm = TRUE)



# Data Cleaning and Further Exploration -----------------------------------
filterFAA <- filter(combinedFAA, duration > 40)
filterFAA1 <- filter(filterFAA, speed_ground > 30 | speed_ground < 140)
filterFAA2 <- filter(filterFAA1, speed_air > 30 | speed_ground < 140)
filterFAA3 <- filter(filterFAA2, height > 6)
cleanFAA <- filter(filterFAA3, distance < 6000)
str(cleanFAA)
summary(cleanFAA)
sd(cleanFAA$no_pasg)
sd(cleanFAA$speed_ground)
sd(cleanFAA$speed_air, na.rm = TRUE)
sd(cleanFAA$height)
sd(cleanFAA$pitch)
sd(cleanFAA$distance)
sd(cleanFAA$duration, na.rm = TRUE)


hist(cleanFAA$no_pasg, main = " ", xlab = "number of passengers", col = "blue")
hist(cleanFAA$speed_ground, main = " ", xlab = "speed on ground", col = "blue")
hist(cleanFAA$speed_air, main = " ", xlab = "speed in air", col = "blue")
hist(cleanFAA$height, main = " ", xlab = "height", col = "blue")
hist(cleanFAA$pitch, main = " ", xlab = "pitch", col = "blue")
hist(cleanFAA$distance, main = " ", xlab = "distance", col = "blue")
hist(cleanFAA$duration, main = " ", xlab = "duration", col = "blue")

# Initial Analysis --------------------------------------------------------
#making dummy variable for Aircraft
dummyFAA <- dummy_cols(cleanFAA, select_columns = "aircraft")
corData <- dummyFAA[,2:length(dummyFAA)]
round(cor(corData),2)
cor(cleanFAA$distance, cleanFAA$speed_air, use = "complete.obs")

myData <- dummyFAA[, c(2,3,4,5,6,7,8,9,10)]
chart.Correlation(myData, histogram = TRUE, pch = 19, na.rm = TRUE)

# Regression Using a Single Factor ----------------------------------------
#model for number of passengers
modelNoPasg <- lm(cleanFAA$distance ~ no_pasg, data = cleanFAA)
summary(modelNoPasg)

ggplot(cleanFAA, aes(distance, no_pasg)) +
  geom_point() +
  facet_wrap(~aircraft)

#model for speed on ground
modelSpeedOnGround <- lm(cleanFAA$distance ~ speed_ground, data = cleanFAA)
summary(modelSpeedOnGround)

ggplot(cleanFAA, aes(distance, speed_ground)) +
  geom_point() +
  facet_wrap(~aircraft)

#model for speed in air
modelSpeedInAir <- lm(cleanFAA$distance ~ speed_air, data = cleanFAA)
summary(modelSpeedInAir)

ggplot(cleanFAA, aes(distance, speed_air)) +
  geom_point() +
  facet_wrap(~aircraft)

#model for height
modelHeight <- lm(cleanFAA$distance ~ height, data = cleanFAA)
summary(modelHeight)

ggplot(cleanFAA, aes(distance, height)) +
  geom_point() +
  facet_wrap(~aircraft)

#model for pitch
modelPitch <- lm(cleanFAA$distance ~ pitch, data = cleanFAA)
summary(modelPitch)

ggplot(cleanFAA, aes(distance, pitch)) +
  geom_point() +
  facet_wrap(~aircraft)

#model for duration
modelDuration <- lm(cleanFAA$distance ~ duration, data = cleanFAA)
summary(modelDuration)

ggplot(cleanFAA, aes(distance, duration)) +
  geom_point() +
  facet_wrap(~aircraft)

#model for aircraft
dummy <- as.numeric(cleanFAA$aircraft == "airbus")

modelAircraft <- lm(cleanFAA$distance ~ dummy, data = cleanFAA)
summary(modelAircraft)

ggplot(cleanFAA, aes(distance, dummy)) +
  geom_point()


#standarize variables 
scaledFAA <- cleanFAA
scaledFAA$no_pasg_scaled <- scale(scaledFAA$no_pasg)[, 1]
mean(scaledFAA$no_pasg_scaled)
sd(scaledFAA$no_pasg_scaled)
hist(scaledFAA$no_pasg_scaled)

scaledFAA$distance_scaled <- scale(scaledFAA$distance)[, 1]
scaledFAA$speed_air_scaled <- scale(scaledFAA$speed_air)[, 1]
scaledFAA$speed_ground_scaled <- scale(scaledFAA$speed_ground)[, 1]
scaledFAA$duration_scaled <- scale(scaledFAA$duration)[, 1]
scaledFAA$height_scaled <- scale(scaledFAA$height)[, 1]
scaledFAA$pitch_scaled <- scale(scaledFAA$pitch)[, 1]

#models with standardized variables
modelStandardNoPasg <- lm(scaledFAA$distance_scaled ~ scaledFAA$no_pasg_scaled)
summary(modelStandardNoPasg)

modelStandardSpeedAir <- lm(scaledFAA$distance_scaled ~ scaledFAA$speed_air_scaled)
summary(modelStandardSpeedAir)

modelStandardDuration <- lm(scaledFAA$distance_scaled ~ scaledFAA$duration_scaled)
summary(modelStandardDuration)

modelStandardHeight <- lm(scaledFAA$distance_scaled ~ scaledFAA$height_scaled)
summary(modelStandardHeight)

modelStandardPitch <- lm(scaledFAA$distance_scaled ~ scaledFAA$pitch_scaled)
summary(modelStandardPitch)

modelStandardSpeedGround <- lm(scaledFAA$distance_scaled ~ scaledFAA$speed_ground_scaled)
summary(modelStandardSpeedGround)


dummy_standard <- as.numeric(scaledFAA$aircraft == "airbus")
modelStandardAircraft <- lm(scaledFAA$distance ~ dummy_standard, data = scaledFAA)
summary(modelStandardAircraft)
ggplot(scaledFAA, aes(distance, dummy_standard)) +
  geom_point()
# Check Collinearity ------------------------------------------------------
#Model 1
summary(modelStandardSpeedGround)
#Model 2
summary(modelStandardSpeedAir)
#Model 3
modelStandardSpeedGroundAndAir <- lm(scaledFAA$distance_scaled ~ scaledFAA$speed_ground_scaled + scaledFAA$speed_air_scaled)
summary(modelStandardSpeedGroundAndAir)

# Variable Selection ------------------------------------------------------
#Model 1
summary(modelStandardSpeedGround)
#Model 2
summary(modelStandardSpeedGroundAndAir)
#Model 3
model3 <- lm(scaledFAA$distance_scaled ~ speed_ground_scaled + speed_air_scaled + height_scaled, data = scaledFAA)
summary(model3)
#Model 4
model4 <- lm(scaledFAA$distance_scaled ~ speed_ground_scaled + speed_air_scaled + height_scaled + pitch_scaled, data = scaledFAA)
summary(model4)
#Model 5
model5 <- lm(scaledFAA$distance_scaled ~ speed_ground_scaled + speed_air_scaled + height_scaled + pitch_scaled + duration_scaled, data = scaledFAA)
summary(model5)
#Model 6
model6 <- lm(scaledFAA$distance_scaled ~ speed_ground_scaled + speed_air_scaled + height_scaled + pitch_scaled + duration_scaled + no_pasg_scaled, data = scaledFAA)
summary(model6)


r.squared.1<-summary(modelStandardSpeedGround)$r.squared;
r.squared.2<-summary(modelStandardSpeedGroundAndAir)$r.squared;
r.squared.3<-summary(model3)$r.squared;
r.squared.4<-summary(model4)$r.squared;
r.squared.5<-summary(model5)$r.squared;
r.squared.6<-summary(model6)$r.squared;

plot(1:6, c(r.squared.1,r.squared.2,r.squared.3,r.squared.4,r.squared.5,r.squared.6), xlab = "Number of Variables in Model", ylab = "R Squared Value", main = "R Squared as More Variable are Added")

adjr.squared.1<-summary(modelStandardSpeedGround)$adj.r.squared;
adjr.squared.2<-summary(modelStandardSpeedGroundAndAir)$adj.r.squared;
adjr.squared.3<-summary(model3)$adj.r.squared;
adjr.squared.4<-summary(model4)$adj.r.squared;
adjr.squared.5<-summary(model5)$adj.r.squared;
adjr.squared.6<-summary(model6)$adj.r.squared;

plot(1:6, c(adjr.squared.1,adjr.squared.2,adjr.squared.3,adjr.squared.4,adjr.squared.5,adjr.squared.6), xlab = "Number of Variables in Model", ylab = "Adjusted R Squared Value", main = "Adjusted R Squared as More Variable are Added")

AIC1 <- AIC(modelStandardSpeedGround)
AIC2 <- AIC(modelStandardSpeedGroundAndAir)
AIC3 <- AIC(model3)
AIC4 <- AIC(model4)
AIC5 <- AIC(model5)
AIC6 <- AIC(model6)

plot(1:6, c(AIC1, AIC2, AIC3, AIC4, AIC5, AIC6), xlab = "Number of Variables in Model", ylab = "AIC Value", main = "AIC as More Variables are Added")

finalScaledFAA <- scaledFAA[ -c(1:8)]

# Variable Selection based on AIC -----------------------------------------
stepAIC(model6)



