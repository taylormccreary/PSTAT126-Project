setwd("C:/Users/Taylor/OneDrive/Classes/PSTAT 126/Project")

sleep_data <- read.table("sleep.txt", header = TRUE)

# now remove the rows with na values
good_data <- subset(sleep_data, Dreaming > 0 & !is.na(Dreaming) & !is.na(NonDreaming) & !is.na(LifeSpan) & !is.na(Gestation) & !is.na(Predation) & !is.na(TotalSleep))
good_data <- good_data[-41,]

attach(good_data)

# look at scatterplots with all the variables

pairs(~ TotalSleep + log(BodyWt) + log(BrainWt) + log(NonDreaming) + log(Dreaming))

# some variables seem to not have constant variance, try applying a transformation to these variables
pairs(~ log(BodyWt) + log(BrainWt) + log(NonDreaming) + log(Dreaming) + log(LifeSpan) + log(Gestation) + Predation + Exposure + Danger)

# just look at transformed variables and response, TotalSleep
pairs(TotalSleep ~ log(BodyWt) + log(BrainWt) + log(NonDreaming) + log(Dreaming) + log(LifeSpan))

# create initial linear model
m1 <- lm(TotalSleep ~ log(BodyWt) + log(BrainWt) + log(NonDreaming) + log(Dreaming) + log(LifeSpan) + log(Gestation) + Predation + Exposure + Danger)
summary(m1) # R^2 = .9264

m2 <- update(m1, . ~ . -Exposure)
summary(m2) # R^2 = .9278

m3 <- update(m2, . ~ . -Danger)
summary(m3) # R^2 = .9298

m4 <- update(m3, . ~ . -Predation)
summary(m4) # R^2 = .9313

m5 <- update(m4, . ~ . -log(Gestation))
summary(m5) # R^2 = .9325

m6 <- update(m5, . ~ . -log(LifeSpan))
summary(m6) # now r^2 has actually gone down to .9286, since we removed a significant variable

# check to make sure m6 really is better than the initial model
anova(m1, m5)

library(car)
# step through AIC, basically does what we just did -> gives same variables!
backAIC <- step(m1,direction="backward", data=bridge)
summary(m5)

avPlot(m5, variable=log(BodyWt), ask=FALSE, identify.points=FALSE)
avPlot(m5, variable=log(BrainWt), ask=FALSE, identify.points=FALSE)
avPlot(m5, variable=log(NonDreaming), ask=FALSE, identify.points=FALSE)
avPlot(m5, variable=log(Dreaming), ask=FALSE, identify.points=FALSE)
avPlot(m5, variable=log(LifeSpan), ask=FALSE, identify.points=FALSE)

par(mfrow=c(3,3))
mmp(m5, log(BodyWt))
mmp(m5, log(BrainWt))
mmp(m5, log(NonDreaming))
mmp(m5, log(Dreaming))
mmp(m5, log(LifeSpan))


# actually how about we don't transform NonDreaming
## IMPORTANT STUFF

m6.5 <- lm(TotalSleep ~ log(BodyWt) + log(BrainWt) + (NonDreaming) + log(Dreaming) + log(LifeSpan) + log(Gestation) + Predation + Exposure + Danger)
summary(m6.5)
outlierTest(m6.5)
cd1 <- cooks.distance(m6.5)
leverage1 <- hatvalues(m6.5)
plot(leverage1, cd1)
identify(leverage1, cd1)
backAIC <- step(m6.5,direction="backward", data=bridge)

m7 <- lm(TotalSleep ~ log(BodyWt) + log(BrainWt) + NonDreaming + log(Dreaming) + log(Gestation) + Predation + Danger)
summary(m7)
summary(m5)

m8 <- lm(TotalSleep ~ log(BodyWt) + log(BrainWt) + NonDreaming + log(Dreaming) +  Predation + Danger)
summary(m8)
vif(m8)



m9 <- lm(TotalSleep ~ log(BodyWt)/log(BrainWt) + NonDreaming + log(Dreaming))
summary(m9)

vif(m9)
plot(m9)
library(car)
boxCox(m9)
install.packages("alr3")
library(alr3)
inverse.response.plot(m9)
