setwd("C:/Users/Taylor/OneDrive/Classes/PSTAT 126/Project")

sleep_data <- read.table("sleep.txt", header = TRUE)

# now remove the rows with na values
good_data <- subset(sleep_data, Dreaming > 0 & !is.na(Dreaming) & !is.na(NonDreaming) & !is.na(LifeSpan) & !is.na(Gestation) & !is.na(Predation) & !is.na(TotalSleep))
#good_data <- good_data[-41,]

attach(good_data)

# look at scatterplots with all the variables

pairs(TotalSleep ~ BodyWt + BrainWt + log(LifeSpan) + Gestation + Predation + Exposure + Danger)

mod1 <- lm(TotalSleep ~ BodyWt + BrainWt + (LifeSpan) + Gestation + Predation + Exposure + Danger)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)

par(mfrow=c(3,3))
avPlot(mod1, variable=BodyWt, ask=FALSE, identify.points=FALSE)
avPlot(mod1, variable=BrainWt, ask=FALSE, identify.points=FALSE)
avPlot(mod1, variable=LifeSpan, ask=FALSE, identify.points=FALSE)
avPlot(mod1, variable=Gestation, ask=FALSE, identify.points=FALSE)
avPlot(mod1, variable=Predation, ask=FALSE, identify.points=FALSE)
avPlot(mod1, variable=Exposure, ask=FALSE, identify.points=FALSE)
avPlot(mod1, variable=Danger, ask=FALSE, identify.points=FALSE)

par(mfrow=c(3,3))
mmp(mod1, variable = BodyWt)
mmp(mod1, variable = BrainWt)
mmp(mod1, variable = LifeSpan)
mmp(mod1, variable = Gestation)
mmp(mod1, variable = Predation)
mmp(mod1, variable = Exposure)
mmp(mod1, variable = Danger)

#BodyWt and BrainWt need to be logged
mod2 <- lm(TotalSleep ~ log(BodyWt) + log(BrainWt) + LifeSpan + Gestation + Predation + Exposure + Danger)
summary(mod2)
par(mfrow=c(2,2))
plot(mod2)

par(mfrow=c(3,3))
avPlot(mod2, variable=log(BodyWt), ask=FALSE, identify.points=FALSE)
avPlot(mod2, variable=log(BrainWt), ask=FALSE, identify.points=FALSE)
avPlot(mod2, variable=LifeSpan, ask=FALSE, identify.points=FALSE)
avPlot(mod2, variable=Gestation, ask=FALSE, identify.points=FALSE)
avPlot(mod2, variable=Predation, ask=FALSE, identify.points=FALSE)
avPlot(mod2, variable=Exposure, ask=FALSE, identify.points=FALSE)
avPlot(mod2, variable=Danger, ask=FALSE, identify.points=FALSE)

par(mfrow=c(3,3))
mmp(mod2, variable = log(BodyWt))
mmp(mod2, variable = log(BrainWt))
mmp(mod2, variable = LifeSpan)
mmp(mod2, variable = Gestation)
mmp(mod2, variable = Predation)
mmp(mod2, variable = Exposure)
mmp(mod2, variable = Danger)


# Lifespan needs to be logged
mod3 <- lm(TotalSleep ~ log(BodyWt) + log(BrainWt) + log(LifeSpan) + Gestation + Predation + Exposure + Danger)
summary(mod3)
par(mfrow=c(2,2))
plot(mod3)

par(mfrow=c(3,3))
avPlot(mod3, variable=log(BodyWt), ask=FALSE, identify.points=FALSE)
avPlot(mod3, variable=log(BrainWt), ask=FALSE, identify.points=FALSE)
avPlot(mod3, variable=log(LifeSpan), ask=FALSE, identify.points=FALSE)
avPlot(mod3, variable=Gestation, ask=FALSE, identify.points=FALSE)
avPlot(mod3, variable=Predation, ask=FALSE, identify.points=FALSE)
avPlot(mod3, variable=Exposure, ask=FALSE, identify.points=FALSE)
avPlot(mod3, variable=Danger, ask=FALSE, identify.points=FALSE)

par(mfrow=c(3,3))
mmp(mod3, variable = log(BodyWt))
mmp(mod3, variable = log(BrainWt))
mmp(mod3, variable = log(LifeSpan))
mmp(mod3, variable = Gestation)
mmp(mod3, variable = Predation)
mmp(mod3, variable = Exposure)
mmp(mod3, variable = Danger)

vif(mod3)
cor(good_data[2:11])

# Since Predation and Danger measure basically the same thing, remove Danger (higher vif)
# Since BodyWt and BrainWt measure basically the same thing, remove BrainWt (higher vif)
mod4 <- lm(TotalSleep ~ log(BodyWt) + log(LifeSpan) + Gestation + Predation + Exposure)
summary(mod4)
par(mfrow=c(2,2))
plot(mod4)

par(mfrow=c(2,3))
avPlot(mod4, variable=log(BodyWt), ask=FALSE, identify.points=FALSE)
avPlot(mod4, variable=log(LifeSpan), ask=FALSE, identify.points=FALSE)
avPlot(mod4, variable=Gestation, ask=FALSE, identify.points=FALSE)
avPlot(mod4, variable=Predation, ask=FALSE, identify.points=FALSE)
avPlot(mod4, variable=Exposure, ask=FALSE, identify.points=FALSE)

par(mfrow=c(2,3))
mmp(mod4, variable = log(BodyWt))
mmp(mod4, variable = log(LifeSpan))
mmp(mod4, variable = Gestation)
mmp(mod4, variable = Predation)
mmp(mod4, variable = Exposure)

boxcox(mod4)

# boxcox says maybe we should transform TotalSleep
mod5 <- lm(TotalSleep**.25 ~ log(BodyWt) + log(LifeSpan) + Gestation + Predation + Exposure)
summary(mod5)
par(mfrow=c(2,2))
plot(mod5)

par(mfrow=c(2,3))
avPlot(mod5, variable=log(BodyWt), ask=FALSE, identify.points=FALSE)
avPlot(mod5, variable=log(LifeSpan), ask=FALSE, identify.points=FALSE)
avPlot(mod5, variable=Gestation, ask=FALSE, identify.points=FALSE)
avPlot(mod5, variable=Predation, ask=FALSE, identify.points=FALSE)
avPlot(mod5, variable=Exposure, ask=FALSE, identify.points=FALSE)

par(mfrow=c(2,3))
mmp(mod5, variable = log(BodyWt))
mmp(mod5, variable = log(LifeSpan))
mmp(mod5, variable = Gestation)
mmp(mod5, variable = Predation)
mmp(mod5, variable = Exposure)


library(MASS)
backAIC <- stepAIC(mod5, direction = "backward", k = 2)
summary(backAIC)

mSimple <- lm((TotalSleep**.25) ~ 1)
forwardAIC <- stepAIC(mSimple, direction = "forward", k = 2, scope=list(upper=mod5,lower=mSimple))
summary(forwardAIC)

# So forward and backward AIC are the same

# maybe try transforming Gestation
mod6 <- lm(TotalSleep**.25 ~ log(BodyWt) + log(LifeSpan) + log(Gestation) + Predation + Exposure)
summary(mod6)
par(mfrow=c(2,2))
plot(mod6)

par(mfrow=c(2,3))
avPlot(mod6, variable=log(BodyWt), ask=FALSE, identify.points=FALSE)
avPlot(mod6, variable=log(LifeSpan), ask=FALSE, identify.points=FALSE)
avPlot(mod6, variable=log(Gestation), ask=FALSE, identify.points=FALSE)
avPlot(mod6, variable=Predation, ask=FALSE, identify.points=FALSE)
avPlot(mod6, variable=Exposure, ask=FALSE, identify.points=FALSE)

par(mfrow=c(2,3))
mmp(mod6, variable = log(BodyWt))
mmp(mod6, variable = log(LifeSpan))
mmp(mod6, variable = log(Gestation))
mmp(mod6, variable = Predation)
mmp(mod6, variable = Exposure)

backAIC <- stepAIC(mod6, direction = "backward", k = 2)
summary(backAIC)

mSimple <- lm((TotalSleep**.25) ~ 1)
forwardAIC <- stepAIC(mSimple, direction = "forward", k = 2, scope=list(upper=mod6,lower=mSimple))
summary(forwardAIC)

par(mfrow=c(2,2))
plot(forwardAIC)

# maybe we did boxcox at the wrong time. take away transformation of response variable
mod6 <- lm(TotalSleep ~ log(BodyWt) + log(BrainWt) + log(LifeSpan) + log(Gestation) + Predation + Exposure + Danger)
summary(mod6)
boxcox(mod6)

# apply suggested transformation
mod7 <- lm((TotalSleep**.3) ~ log(BodyWt) + log(BrainWt) + log(LifeSpan) + log(Gestation) + Predation + Exposure + Danger)
summary(mod7)
par(mfrow=c(2,2))
plot(mod7)


backAIC <- stepAIC(mod7, direction = "backward", k = 2)
summary(backAIC)
vif(backAIC) # these are high, but we are ok with that

par(mfrow=c(2,2))
plot(backAIC)

mSimple <- lm((TotalSleep**.3) ~ 1)
forwardAIC <- stepAIC(mSimple, direction = "forward", k = 2, scope=list(upper=mod7,lower=mSimple))
summary(forwardAIC)
vif(forwardAIC)


mSimple <- lm((TotalSleep**.3) ~ 1)
bothAIC <- stepAIC(mSimple, direction = "both", k = 2, scope=list(upper=mod7,lower=mSimple))


par(mfrow=c(2,2))
plot(forwardAIC)
boxcox(forwardAIC)

# compromise backward and forward
mod8 <- lm((TotalSleep**.3) ~ log(BodyWt) + log(LifeSpan) + log(Gestation) + Danger)
summary(mod8)

# but then log(Lifespan) is not significant

