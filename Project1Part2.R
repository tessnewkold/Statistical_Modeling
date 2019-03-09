#Tess Newkold
#Feb 17, 2019 
#Statistical Modeling 
# Project 1 - Part 2

# Packages ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(fastDummies)
library(PerformanceAnalytics)
library(MASS) #to perform AIC function


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

# Step 7 ------------------------------------------------------------------
str(cleanFAA)
summary(cleanFAA)
sd(cleanFAA$no_pasg)
sd(cleanFAA$speed_ground)
sd(cleanFAA$speed_air, na.rm = TRUE)
sd(cleanFAA$height)
sd(cleanFAA$pitch)
sd(cleanFAA$distance)
sd(cleanFAA$duration, na.rm = TRUE)

# Step 8 ------------------------------------------------------------------
hist(cleanFAA$no_pasg, main = " ", xlab = "number of passengers", col = "blue")
hist(cleanFAA$speed_ground, main = " ", xlab = "speed on ground", col = "blue")
hist(cleanFAA$speed_air, main = " ", xlab = "speed in air", col = "blue")
hist(cleanFAA$height, main = " ", xlab = "height", col = "blue")
hist(cleanFAA$pitch, main = " ", xlab = "pitch", col = "blue")
hist(cleanFAA$distance, main = " ", xlab = "distance", col = "blue")
hist(cleanFAA$duration, main = " ", xlab = "duration", col = "blue")


# Step 9 ------------------------------------------------------------------
# •	67 observations were removed resulting in a clean data set of 783 observations and 8 variables 
# •	High percentage of missing variables in Speed in Air variable 
# •	Histograms look good with speed_in_air and distance being skewed to the right.
# •	Standard deviation of variables looks high, so will probably want to standardize variables for the model




###Create Binary Responses -------------------------------------------------

# Step 1 ------------------------------------------------------------------
#creating binary variables for longLanding and riskyLanding
cleanFAA$longLanding <- ifelse(cleanFAA$distance > 2500, 1, 0)
cleanFAA$riskyLanding <- ifelse(cleanFAA$distance > 3000, 1, 0)

#get rid of the distance column
FAA <- subset(cleanFAA, select = -c(distance))


# Identify Importnat Factors Using the Binary Data of longLanding ---------

# Step 2 ------------------------------------------------------------------
#histogram showing distribution of long landings 
hist(FAA$longLanding, main = "Distribution of Long Landings", col = 'blue', xlab = ' ')
pct <- round(table(FAA$longLanding) / length(FAA$longLanding) * 100, 1)
labs <- c("All Other Landings", "Long Landings")
labs <- paste(labs, pct)
labs <- paste(labs, "%", sep = " ")
pie(table(FAA$longLanding), labels = labs, col = rainbow(length(labs)), main = "Pie Chart of Long Landings")

# Step 3 ------------------------------------------------------------------
#model for no-pasg
modelNoPasgB <- glm(FAA$longLanding ~ FAA$no_pasg, family = binomial)
summary(modelNoPasgB)
#model for speed on ground
modelSpeedOnGroundB <- glm(FAA$longLanding ~ FAA$speed_ground, family = binomial)
summary(modelSpeedOnGroundB)
#model for speed in air
modelSpeedInAirB <- glm(FAA$longLanding ~ FAA$speed_air, family = binomial)
summary(modelSpeedInAirB)
#model for height
modelHeightB <- glm(FAA$longLanding ~ FAA$height,family = binomial)
summary(modelHeightB)
#model for pitch
modelPitchB <- glm(FAA$longLanding ~ FAA$pitch, family = binomial)
summary(modelPitchB)
#make dummy variable for aircraft Airbus = 0, Boeing = 1
dummy <- as.numeric(FAA$aircraft == "airbus", 0, 1)
#model for aircraft
modelAircraftB <- lm(FAA$longLanding ~ dummy, data = FAA)
summary(modelAircraftB)

#model for duration
modelDurationB <- lm(FAA$longLanding ~ duration, data = FAA)
summary(modelDurationB)
#Sample output for all variables 


#calculating Odd ratio
exp(modelNoPasgB$coefficients)
exp(modelSpeedOnGroundB$coefficients)
exp(modelSpeedInAirB$coefficients)
exp(modelHeightB$coefficients)
exp(modelPitchB$coefficients)
exp(modelDurationB$coefficients)

exp(modelAircraftB$coefficients)
#Sample output for all variables 


# Step 4 ------------------------------------------------------------------
plot(jitter(longLanding,0.1)~jitter(speed_ground), FAA, xlab="Speed On Ground", ylab = "Long Landing", pch = ".")

ggplot(FAA, aes(x = speed_ground, fill = longLanding)) + geom_histogram(position = "dodge",binwidth = 1) +
  facet_grid(~longLanding)

plot(jitter(longLanding, 0.1)~jitter(speed_air), FAA, xlab = "Speed In Air", ylab = "Long Landing", pch = ".")

ggplot(FAA, aes(x = speed_air, fill = longLanding)) + geom_histogram(position = "dodge",binwidth = 1) +
  facet_grid(~longLanding)

plot(jitter(longLanding, 0.1)~jitter(dummy), FAA, xlab = "Aircraft", ylab = "Long Landing", pch = ".")

ggplot(FAA, aes(x = longLanding, fill = aircraft)) +
  geom_bar(position = "dodge", width = 0.5) +
  facet_grid(~aircraft)

plot(jitter(longLanding, 0.1)~jitter(pitch), FAA, xlab = "Pitch", ylab = "Long Landing", pch = ".")

ggplot(FAA,aes(x=pitch,fill=longLanding))+geom_histogram(position="dodge",binwidth=0.5) +
  facet_grid(~longLanding)


# Step 5 ------------------------------------------------------------------
#plot speed on ground by speed in air to evaluate multicolinearity 
plot(FAA$speed_ground, FAA$speed_air)
#when two variables are correlated to each other there are many things to consider
#1. number of missing values 
#2. AIC, BIC
#3. the size of the coeficients (would pick the bigger one)
#4. general information about the data/variable

proposedModelLong <- glm(longLanding ~ speed_ground + aircraft + pitch, data = FAA, family = binomial(link = "logit"))
summary(proposedModelLong)
exp(coef(proposedModelLong))

#making full and empty model for later steps
fullFAA_omitNA <- na.omit(FAA)
fullModel <- glm(longLanding ~ aircraft + no_pasg + speed_ground + speed_air + 
                   height + pitch + duration, data = fullFAA_omitNA, family = binomial(link = "logit"))
summary(fullModel)

emptyModel <- glm(longLanding ~ 1, data = fullFAA_omitNA, family = binomial(link = "logit"))
summary(emptyModel)



# Step 6 ------------------------------------------------------------------
stepAICModel <- step(proposedModelLong, trace = 0)
summary(stepAICModel)
exp(coef(stepAICModel))

# Step 7 ------------------------------------------------------------------
stepBICModel <- step(proposedModelLong, k = log(195), trace = 0)
stepBICModel
exp(coef(stepBICModel))
# Step 8 ------------------------------------------------------------------
#words for FAA agent

ggplot(FAA, aes(x = speed_ground, fill = longLanding)) + geom_histogram(position = "dodge",binwidth = 1) +
  facet_grid(~longLanding)


# Identify Importnat Factors Using the Binary Data of Risky Landin --------

# Step 9 ------------------------------------------------------------------
hist(FAA$riskyLanding, main = "Distribution of Risky Landings", col = 'red', xlab = ' ')
pct <- round(table(FAA$riskyLanding) / length(FAA$riskyLanding) * 100, 1)
labs <- c("All Other Landings", "Risky Landings")
labs <- paste(labs, pct)
labs <- paste(labs, "%", sep = " ")
pie(table(FAA$riskyLanding), labels = labs, col = rainbow(length(labs)), main = "Pie Chart of Risky Landings")

#model for no-pasg
modelNoPasgR <- glm(FAA$riskyLanding ~ FAA$no_pasg, family = binomial)
summary(modelNoPasgR)

#model for speed on ground
modelSpeedOnGroundR <- glm(FAA$riskyLanding ~ FAA$speed_ground, family = binomial)
summary(modelSpeedOnGroundR)

#model for speed in air
modelSpeedInAirR <- glm(FAA$riskyLanding ~ FAA$speed_air, family = binomial)
summary(modelSpeedInAirR)

#model for height
modelHeightR <- glm(FAA$riskyLanding ~ FAA$height,family = binomial)
summary(modelHeightR)

#model for pitch
modelPitchR <- glm(FAA$riskyLanding ~ FAA$pitch, family = binomial)
summary(modelPitchR)

#model for aircraft
dummy <- as.numeric(FAA$aircraft == "airbus", 0, 1)

modelAircraftR <- lm(FAA$riskyLanding ~ dummy, data = FAA)
summary(modelAircraftR)

#model for duration
modelDurationR <- lm(FAA$riskyLanding ~ duration, data = FAA)
summary(modelDurationR)
#Sample output for all variables 

#calculating the odds ratio
exp(modelNoPasgR$coefficients)
exp(modelSpeedOnGroundR$coefficients)
exp(modelSpeedInAirR$coefficients)
exp(modelHeightR$coefficients)
exp(modelPitchR$coefficients)
exp(modelDurationR$coefficients)
exp(modelAircraftR$coefficients)

exp(modelAircraftR$coefficients)
#Sample output for all variables 

plot(jitter(riskyLanding,0.1)~jitter(speed_ground), FAA, xlab="Speed On Ground", ylab = "Risky Landing", pch = ".")

ggplot(FAA, aes(x = speed_ground, fill = riskyLanding)) + geom_histogram(position = "dodge",binwidth = 1) +
  facet_grid(~riskyLanding)

plot(jitter(riskyLanding, 0.1)~jitter(speed_air), FAA, xlab = "Speed In Air", ylab = "Risky Landing", pch = ".")

ggplot(FAA, aes(x = speed_air, fill = riskyLanding)) + geom_histogram(position = "dodge",binwidth = 1) +
  facet_grid(~riskyLanding)

plot(jitter(riskyLanding, 0.1)~jitter(dummy), FAA, xlab = "Aircraft", ylab = "Risky Landing", pch = ".")

ggplot(FAA, aes(x = riskyLanding, fill = aircraft)) +
  geom_bar(position = "dodge", width = 0.5) +
  facet_grid(~aircraft)

plot(jitter(riskyLanding, 0.1)~jitter(pitch), FAA, xlab = "Pitch", ylab = "Risky Landing", pch = ".")

ggplot(FAA,aes(x=pitch,fill =  riskyLanding))+geom_histogram(position="dodge",binwidth=0.5) +
  facet_grid(~riskyLanding)

#finding the model for risky
proposedModelRisky <- glm(riskyLanding ~ speed_ground + aircraft, data = FAA, family = binomial(link = "logit"))
summary(proposedModelRisky)

fullFAA_omitNA <- na.omit(FAA)
fullModelRisky <- glm(riskyLanding ~ aircraft + no_pasg + speed_ground + speed_air + 
                        height + pitch + duration, data = fullFAA_omitNA, family = binomial(link = "logit"))
summary(fullModelRisky)

emptyModel <- glm(longLanding ~ 1, data = fullFAA_omitNA, family = binomial(link = "logit"))
summary(emptyModel)

stepAICModel <- step(proposedModelRisky, trace = 0)
stepAICModel

stepBICModel <- step(fullModelRisky, k = log(195), trace = 0)
stepBICModel
exp(coef(stepBICModel))

# Step 10 -----------------------------------------------------------------

#words

ggplot(FAA, aes(x = speed_ground, fill = riskyLanding)) + geom_histogram(position = "dodge",binwidth = 1) +
  facet_grid(~riskyLanding)


# Compare the Two Models Built for Long Landing and Risky Landing ---------


# Step 11 -----------------------------------------------------------------
# 1. The two models are similar, however the Risky Landings drop the variables height and pitch as factors impacting risky landing. This could mean that those two are important in predicting a long landing, but not necessarily a risky one. 
# 2. Speed on ground for risky flights has a larger odds ratio, meaning that for the risky flights they have 1.85 times faster speed on the ground than regular landings. Compared to the long landings which has an odds ratio of 1.6, still large, just not as large as in risky landings. 
# 3. The aircraft seems to have a small effect on landing distance as well. Airbus tends to have longer landing distances than boeing, which is definetly something to keep an eye out for, and potentially discuss with Airbus. 


# Step 12 -----------------------------------------------------------------
#ROC for Long model
linPred <- predict(proposedModelLong)
predProb <- predict(proposedModelLong, type = "response")
predOut <- ifelse(predProb < 0.05, "no", "yes")
fullFAA_omitNA <- data.frame(FAA, predProb, predOut)
xtabs(~longLanding+predOut, FAA)

thresh<-seq(0.01,0.5,0.01)
sensitivity<-specificity<-rep(NA,length(thresh))
for(j in seq(along=thresh)){
  pp<-ifelse(fullFAA_omitNA$predProb<thresh[j],"no","yes")
  xx<-xtabs(~longLanding+pp,fullFAA_omitNA)
  specificity[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}
#matplot(thresh,cbind(sensitivity,specificity),type="l",xlab="Threshold",ylab="Proportion",lty=1:2)

#ROC for Risky Model
linPredR <- predict(proposedModelRisky)
predProbR <- predict(proposedModelRisky, type = "response")
predOutR <- ifelse(predProbR < 0.05, "no", "yes")
fullFAA_omitNA <- data.frame(FAA, predProbR, predOutR)
xtabs(~riskyLanding + predOutR, FAA)

thresh<-seq(0.01,0.5,0.01)
sensitivity1<-specificity1<-rep(NA,length(thresh))
for(j in seq(along=thresh)){
  pp<-ifelse(fullFAA_omitNA$predProbR<thresh[j],"no","yes")
  xx<-xtabs(~riskyLanding+pp,fullFAA_omitNA)
  specificity1[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity1[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}
#matplot(thresh,cbind(sensitivity1,specificity1),type="l",xlab="Threshold",ylab="Proportion",lty=1:2)

#plot both together 
plot(1-specificity,sensitivity,type="l", col = "red");abline(0,1,lty=2)
par(new = TRUE)
plot(1-specificity1,sensitivity1,type="l") +   abline(0,1,lty=2)

# Step 13 -----------------------------------------------------------------
newFlight <- data.frame(aircraft = "boeing", duration = 200, no_pasg = 80, speed_ground = 115, speed_air = 120, height = 40, pitch = 4)

predictModelLong <- predict(proposedModelLong, newdata = newFlight, type = 'response' , se = T)
c(predictModelLong$fit, predictModelLong$fit-1.96*predictModelLong$se.fit[1], predictModelLong$fit+1.96*predictModelLong$se.fit[1])

predictModelRisky <- predict(proposedModelRisky, newdata = newFlight, type = "response", se = TRUE)
c(predictModelRisky$fit, predictModelRisky$fit-1.96*predictModelRisky$se.fit[1], predictModelRisky$fit+1.96*predictModelRisky$se.fit[1])


# Compare models with different link functions ----------------------------

# Step 14 -----------------------------------------------------------------
ModelRiskyLogit <- glm(riskyLanding ~ speed_ground + aircraft, data = FAA, family = binomial(link = "logit"))

ModelRiskyProbit <- glm(riskyLanding ~ speed_ground + aircraft, data = FAA, family = binomial(link = probit))

ModelRiskyLogLog <- glm(riskyLanding ~ speed_ground + aircraft, data = FAA, family = binomial(link = cloglog))

round(coef(ModelRiskyLogit),3)
round(coef(ModelRiskyProbit),3)
round(coef(ModelRiskyLogLog),3)

summary(ModelRiskyLogit)
summary(ModelRiskyProbit)
summary(ModelRiskyLogLog)



# Step 15 -----------------------------------------------------------------
#logit model
thresh <- seq(0.01,0.5,0.01)
sensitivity_r <- specificity_r <- rep(NA,length(thresh))
for( j in seq(along=thresh)) {
  pp<- ifelse(predict(ModelRiskyLogit,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~FAA$riskyLanding+pp)
  specificity_r[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity_r[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}
#probit model
thresh <- seq(0.01,0.5,0.01)
sensitivity_probit <- specificity_probit <- rep(NA,length(thresh))
for( j in seq(along=thresh)) {
  pp<- ifelse(predict(ModelRiskyProbit,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~FAA$riskyLanding+pp)
  specificity_probit[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity_probit[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}
#cloglog model
thresh <- seq(0.01,0.5,0.01)
sensitivity_loglog <- specificity_loglog <- rep(NA,length(thresh))
for( j in seq(along=thresh)) {
  pp<- ifelse(predict(ModelRiskyLogLog,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~FAA$riskyLanding+pp)
  specificity_loglog[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity_loglog[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}

#plotting all three together
plot(1 - specificity_r, sensitivity_r, type = "l", col = "black", xlab = " ", ylab = "sensitivity")
par(new = TRUE)
plot(1 - specificity_probit, sensitivity_probit, type = "l", col = "red", xlab = " ", ylab = " ")
par(new = TRUE)
plot(1 - specificity_loglog, sensitivity_loglog, type = "l", col = "green", xlab = "1-specificity", ylab = " ")




# Step 16 -----------------------------------------------------------------
predRiskyLogit <- predict(ModelRiskyLogit,type = 'response')
predRiskyProbit <- predict(ModelRiskyProbit,type = 'response')
predRiskyLoglog <- predict(ModelRiskyLogLog,type = 'response')

#sort by tail, so you get the obs with high risk
FAA[as.numeric(names(tail(sort(predRiskyLogit),5))),]
FAA[as.numeric(names(tail(sort(predRiskyProbit),5))),]
FAA[as.numeric(names(tail(sort(predRiskyLoglog),5))),]



# Step 17 -----------------------------------------------------------------
#make new models with linders in variable name to not get confused
proposedModelRiskyLogit <- glm(riskyLanding ~ speed_ground + aircraft, data = FAA, family = binomial(link = "logit"))
summary(proposedModelRiskyLogit)

proposedModelRiskyProbit <- glm(riskyLanding ~ speed_ground + aircraft, data = FAA, family = binomial(link = "probit"))

proposedModelRiskyCLoglog <- glm(riskyLanding ~ speed_ground + aircraft, data = FAA, family = binomial(link = "cloglog"))

#newFlight <- data.frame(aircraft = "boeing", duration = 200, no_pasg = 80, speed_ground = 115, speed_air = 120, height = 40, pitch = 4)

#straight from notes in class
predLogitRisky <- predict(proposedModelRiskyLogit, newdata = newFlight, type = 'response', se.fit = T)
c(predLogitRisky$fit, predLogitRisky$fit - 1.96 * predLogitRisky$se.fit[1], predLogitRisky$fit + 1.96 * predLogitRisky$se.fit[1])

predProbitRisky <- predict(proposedModelRiskyProbit, newdata = newFlight, type = 'response', se.fit = T)
c(predProbitRisky$fit, predProbitRisky$fit - 1.96 * predProbitRisky$se.fit[1], predProbitRisky$fit + 1.96 * predProbitRisky$se.fit[1])

predCLoglogRisky <- predict(proposedModelRiskyCLoglog, newdata = newFlight, type = 'response', se.fit = T)
c(predCLoglogRisky$fit, predCLoglogRisky$fit - 1.96 * predCLoglogRisky$se.fit[1], predCLoglogRisky$fit + 1.96 * predCLoglogRisky$se.fit[1])































