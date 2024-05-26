# Libraries
library(foreign)
library(naniar)
library(dplyr)
library(ggplot2)
library(lme4)
library(sjstats)
library(sjPlot)
library(texreg)
library(ggpubr)
library(interactions)

#### read data ####
df <- read.spss('main_article_df.sav', to.data.frame = T)

#### data preparation ####
## country level
df$scale.FH <- df$FH/100

# individual level 
df$scale.age <- (df$age - mean(df$age))/sd(df$age)
df$scale.health <- (df$health - mean(df$health))/sd(df$health)
df$scale.subfree <- (df$subfree - mean(df$subfree))/sd(df$subfree)

#### Models for SWB ####
###### Null model ######
m0 <- lmer(SWB ~ 1|country, data = df)
summary(m0)
performance::icc(m0) #11.8

# Random intercept model 1 predictor
m1 <- lmer(SWB ~ cRESEMAVAL + (1|country), df)
summary(m1) 
anova(m0, m1)

### Random intercept random slope model
m2 <- lmer(SWB ~ cRESEMAVAL + (cRESEMAVAL|country), df)
summary(m2)
screenreg(list(m1, m2))
anova(m1, m2) # +

###### Freedom House ###### 
## adding 2nd level variable
m3_FH <- lmer(SWB ~ cRESEMAVAL + scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_FH)
screenreg(list(m2, m3_FH)) 
anova(m2, m3_FH)

## adding interaction effect 
m4_FH <- lmer(SWB ~ cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_FH)
screenreg(list(m3_FH, m4_FH)) 
anova(m3_FH, m4_FH) # +

## adding control variables
# gender
m5_FH <- lmer(SWB ~ gender + cRESEMAVAL + cRESEMAVAL*scale.FH +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_FH)
anova(m4_FH, m5_FH) # +

# education
m6_FH <- lmer(SWB ~ gender + edu + cRESEMAVAL + cRESEMAVAL*scale.FH +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_FH)
anova(m5_FH, m6_FH) # +

# age 
m7_FH <- lmer(SWB ~ gender + edu + scale.age + cRESEMAVAL*scale.FH +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_FH)
anova(m6_FH, m7_FH) # +

# age^2
m8_FH <- lmer(SWB ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_FH)
anova(m7_FH, m8_FH) # +

# marital status
m9_FH <- lmer(SWB ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_FH)
anova(m8_FH, m9_FH) # +

# employment
m10_FH <- lmer(SWB ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_FH)
anova(m9_FH, m10_FH) # +

# health
m11_FH <- lmer(SWB ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_FH)
anova(m10_FH, m11_FH) # +

# subjective freedom
m12_FH <- lmer(SWB ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_FH)
anova(m11_FH, m12_FH) # +

##### Electoral Democracy (V-Dem) #####
## adding 2nd level variable
m3_polyarchy <- lmer(SWB ~ cRESEMAVAL + polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_polyarchy)
screenreg(list(m2, m3_polyarchy)) 
anova(m2, m3_polyarchy)

## adding interaction effect 
m4_polyarchy <- lmer(SWB ~ cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_polyarchy)
screenreg(list(m3_polyarchy, m4_polyarchy)) 
anova(m3_polyarchy, m4_polyarchy)

## adding control variables
# gender
m5_polyarchy <- lmer(SWB ~ gender + cRESEMAVAL + cRESEMAVAL*polyarchy +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_polyarchy)
anova(m4_polyarchy, m5_polyarchy) # +

# education
m6_polyarchy <- lmer(SWB ~ gender + edu + cRESEMAVAL + cRESEMAVAL*polyarchy +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_polyarchy)
anova(m5_polyarchy, m6_polyarchy) # +

# age
m7_polyarchy <- lmer(SWB ~ gender + edu + scale.age + cRESEMAVAL*polyarchy +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_polyarchy)
anova(m6_polyarchy, m7_polyarchy) # +

# age^2
m8_polyarchy <- lmer(SWB ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_polyarchy)
anova(m7_polyarchy, m8_polyarchy) # +

# marital status
m9_polyarchy <- lmer(SWB ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_polyarchy)
anova(m8_polyarchy, m9_polyarchy) # +

# employment
m10_polyarchy <- lmer(SWB ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_polyarchy)
anova(m9_polyarchy, m10_polyarchy) # +

# health
m11_polyarchy <- lmer(SWB ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_polyarchy)
anova(m10_polyarchy, m11_polyarchy) # +

# subjective freedom
m12_polyarchy <- lmer(SWB ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_polyarchy)
anova(m11_polyarchy, m12_polyarchy) # +

##### Liberal Democracy (V-Dem) #####
## adding 2nd level variable
m3_libdem <- lmer(SWB ~ cRESEMAVAL + libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_libdem)
screenreg(list(m2, m3_libdem)) 
anova(m2, m3_libdem)

## adding interaction effect 
m4_libdem <- lmer(SWB ~ cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_libdem)
screenreg(list(m3_libdem, m4_libdem)) 
anova(m3_libdem, m4_libdem) # +

## adding control variables
# gender
m5_libdem <- lmer(SWB ~ gender + cRESEMAVAL + cRESEMAVAL*libdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_libdem)
anova(m4_libdem, m5_libdem) # +

# education
m6_libdem <- lmer(SWB ~ gender + edu + cRESEMAVAL + cRESEMAVAL*libdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_libdem)
anova(m5_libdem, m6_libdem) # +

# age
m7_libdem <- lmer(SWB ~ gender + edu + scale.age + cRESEMAVAL*libdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_libdem)
anova(m6_libdem, m7_libdem) # +

# age^2
m8_libdem <- lmer(SWB ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_libdem)
anova(m7_libdem, m8_libdem) # +

# marital status
m9_libdem <- lmer(SWB ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_libdem)
anova(m8_libdem, m9_libdem) # +

# employment
m10_libdem <- lmer(SWB ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_libdem)
anova(m9_libdem, m10_libdem) # +

# health
m11_libdem <- lmer(SWB ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_libdem)
anova(m10_libdem, m11_libdem) # +

# subjective freedom
m12_libdem <- lmer(SWB ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_libdem)
anova(m11_libdem, m12_libdem) # +

##### Particapatory Democracy (V-Dem)
## adding 2nd level variable
m3_partipdem <- lmer(SWB ~ cRESEMAVAL + partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_partipdem)
screenreg(list(m2, m3_partipdem)) 
anova(m2, m3_partipdem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_partipdem <- lmer(SWB ~ cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_partipdem)
screenreg(list(m3_partipdem, m4_partipdem)) 
anova(m3_partipdem, m4_partipdem)
# модель значимо улучшилась по anova, AIC, BIC

## adding control variables
# gender
m5_partipdem <- lmer(SWB ~ gender + cRESEMAVAL + cRESEMAVAL*partipdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_partipdem)
anova(m4_partipdem, m5_partipdem) # +

# education
m6_partipdem <- lmer(SWB ~ gender + edu + cRESEMAVAL + cRESEMAVAL*partipdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_partipdem)
anova(m5_partipdem, m6_partipdem) # +

# age
m7_partipdem <- lmer(SWB ~ gender + edu + scale.age + cRESEMAVAL*partipdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_partipdem)
anova(m6_partipdem, m7_partipdem) # +

# age^2
m8_partipdem <- lmer(SWB ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_partipdem)
anova(m7_partipdem, m8_partipdem) # +

# marital status
m9_partipdem <- lmer(SWB ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_partipdem)
anova(m8_partipdem, m9_partipdem) # +

# employment
m10_partipdem <- lmer(SWB ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_partipdem)
anova(m9_partipdem, m10_partipdem) # +

# health
m11_partipdem <- lmer(SWB ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_partipdem)
anova(m10_partipdem, m11_partipdem) # +

# subjective freedom
m12_partipdem <- lmer(SWB ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_partipdem)
anova(m11_partipdem, m12_partipdem) # +

##### Deliberative Democracy (V-Dem) #####
m3_delibdem <- lmer(SWB ~ cRESEMAVAL + delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_delibdem)
screenreg(list(m2, m3_delibdem)) 
anova(m2, m3_delibdem)

## adding interaction effect 
m4_delibdem <- lmer(SWB ~ cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_delibdem)
screenreg(list(m3_delibdem, m4_delibdem)) 
anova(m3_delibdem, m4_delibdem)

## adding control variables
# gender
m5_delibdem <- lmer(SWB ~ gender + cRESEMAVAL + cRESEMAVAL*delibdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_delibdem)
anova(m4_delibdem, m5_delibdem) # +

# education
m6_delibdem <- lmer(SWB ~ gender + edu + cRESEMAVAL + cRESEMAVAL*delibdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_delibdem)
anova(m5_delibdem, m6_delibdem) # +

# age
m7_delibdem <- lmer(SWB ~ gender + edu + scale.age + cRESEMAVAL*delibdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_delibdem)
anova(m6_delibdem, m7_delibdem) # +

# age^2
m8_delibdem <- lmer(SWB ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_delibdem)
anova(m7_delibdem, m8_delibdem) # +

# marital status
m9_delibdem <- lmer(SWB ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_delibdem)
anova(m8_delibdem, m9_delibdem) # +

# employment
m10_delibdem <- lmer(SWB ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_delibdem)
anova(m9_delibdem, m10_delibdem) # +

# health
m11_delibdem <- lmer(SWB ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_delibdem)
anova(m10_delibdem, m11_delibdem) # +

# subjective freedom
m12_delibdem <- lmer(SWB ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_delibdem)
anova(m11_delibdem, m12_delibdem) # +

##### Egalitarian Democracy (V-Dem) #####
m3_egaldem <- lmer(SWB ~ cRESEMAVAL + egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_egaldem)
screenreg(list(m2, m3_egaldem)) 
anova(m2, m3_egaldem)

## adding interaction effect 
m4_egaldem <- lmer(SWB ~ cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_egaldem)
screenreg(list(m3_egaldem, m4_egaldem)) 
anova(m3_egaldem, m4_egaldem)

## adding control variables
# gender
m5_egaldem <- lmer(SWB ~ gender + cRESEMAVAL + cRESEMAVAL*egaldem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_egaldem)
anova(m4_egaldem, m5_egaldem) # +

# education
m6_egaldem <- lmer(SWB ~ gender + edu + cRESEMAVAL + cRESEMAVAL*egaldem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_egaldem)
anova(m5_egaldem, m6_egaldem) # +

# age
m7_egaldem <- lmer(SWB ~ gender + edu + scale.age + cRESEMAVAL*egaldem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_egaldem)
anova(m6_egaldem, m7_egaldem) # +

# age^2
m8_egaldem <- lmer(SWB ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_egaldem)
anova(m7_egaldem, m8_egaldem) # +

# marital status
m9_egaldem <- lmer(SWB ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem +  (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_egaldem)
anova(m8_egaldem, m9_egaldem) # +

# employment
m10_egaldem <- lmer(SWB ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_egaldem)
anova(m9_egaldem, m10_egaldem) # +

# health
m11_egaldem <- lmer(SWB ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_egaldem)
anova(m10_egaldem, m11_egaldem) # +

# subjective freedom
m12_egaldem <- lmer(SWB ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_egaldem)
anova(m11_egaldem, m12_egaldem) # +

##### Country's EV #####
## adding 2nd level variable
m3_ev <- lmer(SWB ~ cRESEMAVAL + EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_ev)
screenreg(list(m2, m3_ev)) 
anova(m2, m3_ev)

## adding interaction effect 
m4_ev <- lmer(SWB ~ cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_ev)
screenreg(list(m3_ev, m4_ev)) 
anova(m3_ev, m4_ev) # +

## adding control variables
# gender
m5_ev <- lmer(SWB ~ gender + cRESEMAVAL + cRESEMAVAL*EV_country +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_ev)
anova(m4_ev, m5_ev) # +

# education
m6_ev <- lmer(SWB ~ gender + edu + cRESEMAVAL + cRESEMAVAL*EV_country +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_ev)
anova(m5_ev, m6_ev) # +

# age
m7_ev <- lmer(SWB ~ gender + edu + scale.age + cRESEMAVAL*EV_country +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_ev)
anova(m6_ev, m7_ev) # +

# age^2
m8_ev <- lmer(SWB ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_ev)
anova(m7_ev, m8_ev) # +

# marital status
m9_ev <- lmer(SWB ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_ev)
anova(m8_ev, m9_ev) # +

m10_ev <- lmer(SWB ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country +  (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_ev)
anova(m9_ev, m10_ev) # +

m11_ev <- lmer(SWB ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_ev)
anova(m10_ev, m11_ev) # +

# subjective freedom
m12_ev <- lmer(SWB ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_ev)
anova(m11_ev, m12_ev) # +


##### Model comparison ######

# Models without control variables
screenreg(list(m4_FH, m4_polyarchy, m4_libdem, m4_partipdem, m4_delibdem, m4_egaldem, m4_ev)) 

# Models with basic control variables
screenreg(list(m8_FH, m8_polyarchy, m8_libdem, m8_partipdem, m8_delibdem, m8_egaldem, m8_ev)) 

# Models with rigorous control variables
screenreg(list(m12_FH, m12_polyarchy, m12_libdem, m12_partipdem, m12_delibdem, m12_egaldem, m12_ev)) 

##### Visualization models for Participatory Democracy #####
# No control variables
# Johnson-Neyman plot
JN_m4_partipdem <- sim_slopes(m4_partipdem, pred = cRESEMAVAL, modx = partipdem, jnplot = TRUE)
JN_m4_partipdem_plot <- JN_m4_partipdem$jnplot 

JN_m4_partipdem_plot <- JN_m4_partipdem_plot + 
  labs(x = "Participatory Democracy", y = "The Effect of Individual EVI on SWB", title = "Johnson-Neyman plot", fill = NULL) + 
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  xlim(c(0,1)) + ylim(-3, 3) 
JN_m4_partipdem_plot

# Interaction plot
interact_m4_partipdem  <-
  plot_model(m4_partipdem, type = "int",  mdrt.values = "meansd", legend.title = "Participatory\nDemocracy") +
  theme_bw() + 
  scale_color_manual(labels = c("-1SD", "Mean", "+1SD"),
                     values = c("#ff0000","#bdbebd", "steelblue3")) +
  scale_fill_manual(values = c("#ff0000", "#bdbebd", "steelblue3")) +
  labs(x = "Individual EVI", title = "Interaction plot") +   theme(legend.position = "bottom", legend.text = element_text(size = 10), 
                                                                   legend.title=element_text(), 
                                                                   #plot.title = element_text(face="bold"),
                                                                   axis.title.x = element_text(size = 10),
                                                                   axis.title.y = element_text(size = 10)) +
  ylim(0,4)
interact_m4_partipdem 

graph1 <- ggarrange(interact_m4_partipdem, JN_m4_partipdem_plot, labels = c("A", "B"), align = "hv")
graph1

# Interaction plot for model with basic control variables
interact_m8_partipdem  <-
  plot_model(m8_partipdem, type = "int",  mdrt.values = "meansd", legend.title = "Participatory\nDemocracy") +
  theme_bw() + 
  scale_color_manual(labels = c("-1SD", "Mean", "+1SD"),
                     values = c("#ff0000","#bdbebd", "steelblue3")) +
  scale_fill_manual(values = c("#ff0000", "#bdbebd", "steelblue3")) +
  labs(x = "Individual EVI", title = "Interaction plot") +   theme(legend.position = "bottom", legend.text = element_text(size = 10), 
                                                                   legend.title=element_text(), 
                                                                   #plot.title = element_text(face="bold"),
                                                                   axis.title.x = element_text(size = 10),
                                                                   axis.title.y = element_text(size = 10)) +
  ylim(0,4)
interact_m8_partipdem 

# Interaction plot for model with rigorous control variables
interact_m12_partipdem  <-
  plot_model(m12_partipdem, type = "int",  mdrt.values = "meansd", legend.title = "Participatory\nDemocracy") +
  theme_bw() + 
  scale_color_manual(labels = c("-1SD", "Mean", "+1SD"),
                     values = c("#ff0000","#bdbebd", "steelblue3")) +
  scale_fill_manual(values = c("#ff0000", "#bdbebd", "steelblue3")) +
  labs(x = "Individual EVI", title = "Interaction plot") +   theme(legend.position = "bottom", legend.text = element_text(size = 10), 
                                                                   legend.title=element_text(), 
                                                                   #plot.title = element_text(face="bold"),
                                                                   axis.title.x = element_text(size = 10),
                                                                   axis.title.y = element_text(size = 10)) +
  ylim(0,4)
interact_m12_partipdem 

#### Models for Life satisfaction ####
###### Null model ######
m0 <- lmer(lifesat ~ 1|country, data = df)
summary(m0)
performance::icc(m0) #11.8

# Random intercept model 1 predictor
m1 <- lmer(lifesat ~ cRESEMAVAL + (1|country), df)
summary(m1) 
anova(m0, m1)
# ЭЦ не значимы
# модель не стала значимо лучше

### Random intercept random slope model
m2 <- lmer(lifesat ~ cRESEMAVAL + (cRESEMAVAL|country), df)
summary(m2)
screenreg(list(m1, m2))
anova(m1, m2) # +
# Добавление случайного наклона делает модель значимо лучше (и по AIC, BIC и по anova)


###### Freedom House ###### 
## adding 2nd level variable
m3_FH <- lmer(lifesat ~ cRESEMAVAL + scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_FH)
screenreg(list(m2, m3_FH)) 
anova(m2, m3_FH)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_FH <- lmer(lifesat ~ cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_FH)
screenreg(list(m3_FH, m4_FH)) 
anova(m3_FH, m4_FH) # +
# модель значимо улучшилась по anova, AIC, BIC

## adding control variables
# gender
m5_FH <- lmer(lifesat ~ gender + cRESEMAVAL + cRESEMAVAL*scale.FH +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_FH)
anova(m4_FH, m5_FH) # +

# education
m6_FH <- lmer(lifesat ~ gender + edu + cRESEMAVAL + cRESEMAVAL*scale.FH +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_FH)
anova(m5_FH, m6_FH) # +

# age 
m7_FH <- lmer(lifesat ~ gender + edu + scale.age + cRESEMAVAL*scale.FH +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_FH)
anova(m6_FH, m7_FH) # +

# age^2
m8_FH <- lmer(lifesat ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_FH)
anova(m7_FH, m8_FH) # +

# marital status
m9_FH <- lmer(lifesat ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_FH)
anova(m8_FH, m9_FH) # +

# employment
m10_FH <- lmer(lifesat ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_FH)
anova(m9_FH, m10_FH) # +

# health
m11_FH <- lmer(lifesat ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_FH)
anova(m10_FH, m11_FH) # +

# subjective freedom
m12_FH <- lmer(lifesat ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_FH)
anova(m11_FH, m12_FH) # +

##### Electoral Democracy (V-Dem) #####
## adding 2nd level variable
m3_polyarchy <- lmer(lifesat ~ cRESEMAVAL + polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_polyarchy)
screenreg(list(m2, m3_polyarchy)) 
anova(m2, m3_polyarchy)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_polyarchy <- lmer(lifesat ~ cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_polyarchy)
screenreg(list(m3_polyarchy, m4_polyarchy)) 
anova(m3_polyarchy, m4_polyarchy)
# модель значимо улучшилась по anova, AIC, BIC

## adding control variables
# gender
m5_polyarchy <- lmer(lifesat ~ gender + cRESEMAVAL + cRESEMAVAL*polyarchy +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_polyarchy)
anova(m4_polyarchy, m5_polyarchy) # +

# education
m6_polyarchy <- lmer(lifesat ~ gender + edu + cRESEMAVAL + cRESEMAVAL*polyarchy +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_polyarchy)
anova(m5_polyarchy, m6_polyarchy) # +

# age
m7_polyarchy <- lmer(lifesat ~ gender + edu + scale.age + cRESEMAVAL*polyarchy +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_polyarchy)
anova(m6_polyarchy, m7_polyarchy) # +

# age^2
m8_polyarchy <- lmer(lifesat ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_polyarchy)
anova(m7_polyarchy, m8_polyarchy) # +

# marital status
m9_polyarchy <- lmer(lifesat ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_polyarchy)
anova(m8_polyarchy, m9_polyarchy) # +

# employment
m10_polyarchy <- lmer(lifesat ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_polyarchy)
anova(m9_polyarchy, m10_polyarchy) # +

# health
m11_polyarchy <- lmer(lifesat ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_polyarchy)
anova(m10_polyarchy, m11_polyarchy) # +

# subjective freedom
m12_polyarchy <- lmer(lifesat ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_polyarchy)
anova(m11_polyarchy, m12_polyarchy) # +


##### Liberal Democracy (V-Dem) #####
## adding 2nd level variable
m3_libdem <- lmer(lifesat ~ cRESEMAVAL + libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_libdem)
screenreg(list(m2, m3_libdem)) 
anova(m2, m3_libdem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_libdem <- lmer(lifesat ~ cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_libdem)
screenreg(list(m3_libdem, m4_libdem)) 
anova(m3_libdem, m4_libdem) # +
# модель значимо улучшилась по anova, AIC, BIC

## adding control variables
# gender
m5_libdem <- lmer(lifesat ~ gender + cRESEMAVAL + cRESEMAVAL*libdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_libdem)
anova(m4_libdem, m5_libdem) # +

# education
m6_libdem <- lmer(lifesat ~ gender + edu + cRESEMAVAL + cRESEMAVAL*libdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_libdem)
anova(m5_libdem, m6_libdem) # +

# age
m7_libdem <- lmer(lifesat ~ gender + edu + scale.age + cRESEMAVAL*libdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_libdem)
anova(m6_libdem, m7_libdem) # +

# age^2
m8_libdem <- lmer(lifesat ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_libdem)
anova(m7_libdem, m8_libdem) # +

# marital status
m9_libdem <- lmer(lifesat ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_libdem)
anova(m8_libdem, m9_libdem) # +

# employment
m10_libdem <- lmer(lifesat ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_libdem)
anova(m9_libdem, m10_libdem) # +

# health
m11_libdem <- lmer(lifesat ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_libdem)
anova(m10_libdem, m11_libdem) # +

# subjective freedom
m12_libdem <- lmer(lifesat ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_libdem)
anova(m11_libdem, m12_libdem) # +

##### Particapatory Democracy (V-Dem)
## adding 2nd level variable
m3_partipdem <- lmer(lifesat ~ cRESEMAVAL + partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_partipdem)
screenreg(list(m2, m3_partipdem)) 
anova(m2, m3_partipdem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_partipdem <- lmer(lifesat ~ cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_partipdem)
screenreg(list(m3_partipdem, m4_partipdem)) 
anova(m3_partipdem, m4_partipdem)
# модель значимо улучшилась по anova, AIC, BIC

## adding control variables
# gender
m5_partipdem <- lmer(lifesat ~ gender + cRESEMAVAL + cRESEMAVAL*partipdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_partipdem)
anova(m4_partipdem, m5_partipdem) # +

# education
m6_partipdem <- lmer(lifesat ~ gender + edu + cRESEMAVAL + cRESEMAVAL*partipdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_partipdem)
anova(m5_partipdem, m6_partipdem) # +

# age
m7_partipdem <- lmer(lifesat ~ gender + edu + scale.age + cRESEMAVAL*partipdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_partipdem)
anova(m6_partipdem, m7_partipdem) # +

# age^2
m8_partipdem <- lmer(lifesat ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_partipdem)
anova(m7_partipdem, m8_partipdem) # +

# marital status
m9_partipdem <- lmer(lifesat ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_partipdem)
anova(m8_partipdem, m9_partipdem) # +

# employment
m10_partipdem <- lmer(lifesat ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_partipdem)
anova(m9_partipdem, m10_partipdem) # +

# health
m11_partipdem <- lmer(lifesat ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_partipdem)
anova(m10_partipdem, m11_partipdem) # +

# subjective freedom
m12_partipdem <- lmer(lifesat ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_partipdem)
anova(m11_partipdem, m12_partipdem) # +

##### Deliberative Democracy (V-Dem) #####
m3_delibdem <- lmer(lifesat ~ cRESEMAVAL + delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_delibdem)
screenreg(list(m2, m3_delibdem)) 
anova(m2, m3_delibdem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_delibdem <- lmer(lifesat ~ cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_delibdem)
screenreg(list(m3_delibdem, m4_delibdem)) 
anova(m3_delibdem, m4_delibdem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding control variables
# gender
m5_delibdem <- lmer(lifesat ~ gender + cRESEMAVAL + cRESEMAVAL*delibdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_delibdem)
anova(m4_delibdem, m5_delibdem) # +

# education
m6_delibdem <- lmer(lifesat ~ gender + edu + cRESEMAVAL + cRESEMAVAL*delibdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_delibdem)
anova(m5_delibdem, m6_delibdem) # +

# age
m7_delibdem <- lmer(lifesat ~ gender + edu + scale.age + cRESEMAVAL*delibdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_delibdem)
anova(m6_delibdem, m7_delibdem) # +

# age^2
m8_delibdem <- lmer(lifesat ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_delibdem)
anova(m7_delibdem, m8_delibdem) # +

# marital status
m9_delibdem <- lmer(lifesat ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_delibdem)
anova(m8_delibdem, m9_delibdem) # +

# employment
m10_delibdem <- lmer(lifesat ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_delibdem)
anova(m9_delibdem, m10_delibdem) # +

# health
m11_delibdem <- lmer(lifesat ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_delibdem)
anova(m10_delibdem, m11_delibdem) # +

# subjective freedom
m12_delibdem <- lmer(lifesat ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_delibdem)
anova(m11_delibdem, m12_delibdem) # +

##### Egalitarian Democracy (V-Dem) #####
m3_egaldem <- lmer(lifesat ~ cRESEMAVAL + egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_egaldem)
screenreg(list(m2, m3_egaldem)) 
anova(m2, m3_egaldem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_egaldem <- lmer(lifesat ~ cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_egaldem)
screenreg(list(m3_egaldem, m4_egaldem)) 
anova(m3_egaldem, m4_egaldem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding control variables
# gender
m5_egaldem <- lmer(lifesat ~ gender + cRESEMAVAL + cRESEMAVAL*egaldem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_egaldem)
anova(m4_egaldem, m5_egaldem) # +

# education
m6_egaldem <- lmer(lifesat ~ gender + edu + cRESEMAVAL + cRESEMAVAL*egaldem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_egaldem)
anova(m5_egaldem, m6_egaldem) # +

# age
m7_egaldem <- lmer(lifesat ~ gender + edu + scale.age + cRESEMAVAL*egaldem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_egaldem)
anova(m6_egaldem, m7_egaldem) # +

# age^2
m8_egaldem <- lmer(lifesat ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_egaldem)
anova(m7_egaldem, m8_egaldem) # +

# marital status
m9_egaldem <- lmer(lifesat ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem +  (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_egaldem)
anova(m8_egaldem, m9_egaldem) # +

# employment
m10_egaldem <- lmer(lifesat ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_egaldem)
anova(m9_egaldem, m10_egaldem) # +

# health
m11_egaldem <- lmer(lifesat ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_egaldem)
anova(m10_egaldem, m11_egaldem) # +

# subjective freedom
m12_egaldem <- lmer(lifesat ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_egaldem)
anova(m11_egaldem, m12_egaldem) # +

##### country's EV #####
## adding 2nd level variable
m3_ev <- lmer(lifesat ~ cRESEMAVAL + EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_ev)
screenreg(list(m2, m3_ev)) 
anova(m2, m3_ev)
# модель значимо улучшилась по anova, AIC, BIC

## adding interaction effect 
m4_ev <- lmer(lifesat ~ cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_ev)
screenreg(list(m3_ev, m4_ev)) 
anova(m3_ev, m4_ev) # +
# модель значимо улучшилась по anova, AIC, но не по BIC
# эффект взаимодействия для EV не такой сильный, как для переменных отражающих политический режим

## adding control variables
# gender
m5_ev <- lmer(lifesat ~ gender + cRESEMAVAL + cRESEMAVAL*EV_country +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_ev)
anova(m4_ev, m5_ev) # +

# education
m6_ev <- lmer(lifesat ~ gender + edu + cRESEMAVAL + cRESEMAVAL*EV_country +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_ev)
anova(m5_ev, m6_ev) # +

# age
m7_ev <- lmer(lifesat ~ gender + edu + scale.age + cRESEMAVAL*EV_country +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_ev)
anova(m6_ev, m7_ev) # +

# age^2
m8_ev <- lmer(lifesat ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_ev)
anova(m7_ev, m8_ev) # +

# marital status
m9_ev <- lmer(lifesat ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_ev)
anova(m8_ev, m9_ev) # +

m10_ev <- lmer(lifesat ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country +  (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_ev)
anova(m9_ev, m10_ev) # +

m11_ev <- lmer(lifesat ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_ev)
anova(m10_ev, m11_ev) # +

# subjective freedom
m12_ev <- lmer(lifesat ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_ev)
anova(m11_ev, m12_ev) # +


##### Model comparison ######

# Models without control variables
screenreg(list(m4_FH, m4_polyarchy, m4_libdem,  m4_partipdem, m4_delibdem, m4_egaldem, m4_ev)) 

#### Модели для happiness ####
# recode happiness to 1 - not happy at all, 4 - very happy
df$happ <- 5 - df$happiness  
table(df$happiness)
table(df$happ)

###### Null model ######
m0 <- lmer(happ ~ 1|country, data = df)
summary(m0)
performance::icc(m0) #11.8

# Random intercept model 1 predictor
m1 <- lmer(happ ~ cRESEMAVAL + (1|country), df)
summary(m1) 
anova(m0, m1)
# ЭЦ не значимы
# модель не стала значимо лучше

### Random intercept random slope model
m2 <- lmer(happ ~ cRESEMAVAL + (cRESEMAVAL|country), df)
summary(m2)
screenreg(list(m1, m2))
anova(m1, m2) # +
# Добавление случайного наклона делает модель значимо лучше (и по AIC, BIC и по anova)


###### Freedom House ###### 
## adding 2nd level variable
m3_FH <- lmer(happ ~ cRESEMAVAL + scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_FH)
screenreg(list(m2, m3_FH)) 
anova(m2, m3_FH)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_FH <- lmer(happ ~ cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_FH)
screenreg(list(m3_FH, m4_FH)) 
anova(m3_FH, m4_FH) # +
# модель значимо улучшилась по anova, AIC, BIC

## adding control variables
# gender
m5_FH <- lmer(happ ~ gender + cRESEMAVAL + cRESEMAVAL*scale.FH +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_FH)
anova(m4_FH, m5_FH) # +

# education
m6_FH <- lmer(happ ~ gender + edu + cRESEMAVAL + cRESEMAVAL*scale.FH +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_FH)
anova(m5_FH, m6_FH) # +

# age 
m7_FH <- lmer(happ ~ gender + edu + scale.age + cRESEMAVAL*scale.FH +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_FH)
anova(m6_FH, m7_FH) # +

# age^2
m8_FH <- lmer(happ ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_FH)
anova(m7_FH, m8_FH) # +

# marital status
m9_FH <- lmer(happ ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_FH)
anova(m8_FH, m9_FH) # +

# employment
m10_FH <- lmer(happ ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_FH)
anova(m9_FH, m10_FH) # +

# health
m11_FH <- lmer(happ ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_FH)
anova(m10_FH, m11_FH) # +

# subjective freedom
m12_FH <- lmer(happ ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*scale.FH + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_FH)
anova(m11_FH, m12_FH) # +

##### Electoral Democracy (V-Dem) #####
## adding 2nd level variable
m3_polyarchy <- lmer(happ ~ cRESEMAVAL + polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_polyarchy)
screenreg(list(m2, m3_polyarchy)) 
anova(m2, m3_polyarchy)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_polyarchy <- lmer(happ ~ cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_polyarchy)
screenreg(list(m3_polyarchy, m4_polyarchy)) 
anova(m3_polyarchy, m4_polyarchy)
# модель значимо улучшилась по anova, AIC, BIC

## adding control variables
# gender
m5_polyarchy <- lmer(happ ~ gender + cRESEMAVAL + cRESEMAVAL*polyarchy +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_polyarchy)
anova(m4_polyarchy, m5_polyarchy) # +

# education
m6_polyarchy <- lmer(happ ~ gender + edu + cRESEMAVAL + cRESEMAVAL*polyarchy +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_polyarchy)
anova(m5_polyarchy, m6_polyarchy) # +

# age
m7_polyarchy <- lmer(happ ~ gender + edu + scale.age + cRESEMAVAL*polyarchy +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_polyarchy)
anova(m6_polyarchy, m7_polyarchy) # +

# age^2
m8_polyarchy <- lmer(happ ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_polyarchy)
anova(m7_polyarchy, m8_polyarchy) # +

# marital status
m9_polyarchy <- lmer(happ ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_polyarchy)
anova(m8_polyarchy, m9_polyarchy) # +

# employment
m10_polyarchy <- lmer(happ ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_polyarchy)
anova(m9_polyarchy, m10_polyarchy) # +

# health
m11_polyarchy <- lmer(happ ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_polyarchy)
anova(m10_polyarchy, m11_polyarchy) # +

# subjective freedom
m12_polyarchy <- lmer(happ ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*polyarchy + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_polyarchy)
anova(m11_polyarchy, m12_polyarchy) # +


##### Liberal Democracy (V-Dem) #####
## adding 2nd level variable
m3_libdem <- lmer(happ ~ cRESEMAVAL + libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_libdem)
screenreg(list(m2, m3_libdem)) 
anova(m2, m3_libdem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_libdem <- lmer(happ ~ cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_libdem)
screenreg(list(m3_libdem, m4_libdem)) 
anova(m3_libdem, m4_libdem) # +
# модель значимо улучшилась по anova, AIC, BIC

## adding control variables
# gender
m5_libdem <- lmer(happ ~ gender + cRESEMAVAL + cRESEMAVAL*libdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_libdem)
anova(m4_libdem, m5_libdem) # +

# education
m6_libdem <- lmer(happ ~ gender + edu + cRESEMAVAL + cRESEMAVAL*libdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_libdem)
anova(m5_libdem, m6_libdem) # +

# age
m7_libdem <- lmer(happ ~ gender + edu + scale.age + cRESEMAVAL*libdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_libdem)
anova(m6_libdem, m7_libdem) # +

# age^2
m8_libdem <- lmer(happ ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_libdem)
anova(m7_libdem, m8_libdem) # +

# marital status
m9_libdem <- lmer(happ ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_libdem)
anova(m8_libdem, m9_libdem) # +

# employment
m10_libdem <- lmer(happ ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_libdem)
anova(m9_libdem, m10_libdem) # +

# health
m11_libdem <- lmer(happ ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_libdem)
anova(m10_libdem, m11_libdem) # +

# subjective freedom
m12_libdem <- lmer(happ ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*libdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_libdem)
anova(m11_libdem, m12_libdem) # +

##### Particapatory Democracy (V-Dem)
## adding 2nd level variable
m3_partipdem <- lmer(happ ~ cRESEMAVAL + partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_partipdem)
screenreg(list(m2, m3_partipdem)) 
anova(m2, m3_partipdem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_partipdem <- lmer(happ ~ cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_partipdem)
screenreg(list(m3_partipdem, m4_partipdem)) 
anova(m3_partipdem, m4_partipdem)
# модель значимо улучшилась по anova, AIC, BIC

## adding control variables
# gender
m5_partipdem <- lmer(happ ~ gender + cRESEMAVAL + cRESEMAVAL*partipdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_partipdem)
anova(m4_partipdem, m5_partipdem) # +

# education
m6_partipdem <- lmer(happ ~ gender + edu + cRESEMAVAL + cRESEMAVAL*partipdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_partipdem)
anova(m5_partipdem, m6_partipdem) # +

# age
m7_partipdem <- lmer(happ ~ gender + edu + scale.age + cRESEMAVAL*partipdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_partipdem)
anova(m6_partipdem, m7_partipdem) # +

# age^2
m8_partipdem <- lmer(happ ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_partipdem)
anova(m7_partipdem, m8_partipdem) # +

# marital status
m9_partipdem <- lmer(happ ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_partipdem)
anova(m8_partipdem, m9_partipdem) # +

# employment
m10_partipdem <- lmer(happ ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_partipdem)
anova(m9_partipdem, m10_partipdem) # +

# health
m11_partipdem <- lmer(happ ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_partipdem)
anova(m10_partipdem, m11_partipdem) # +

# subjective freedom
m12_partipdem <- lmer(happ ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*partipdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_partipdem)
anova(m11_partipdem, m12_partipdem) # +

##### Deliberative Democracy (V-Dem) #####
m3_delibdem <- lmer(happ ~ cRESEMAVAL + delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_delibdem)
screenreg(list(m2, m3_delibdem)) 
anova(m2, m3_delibdem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_delibdem <- lmer(happ ~ cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_delibdem)
screenreg(list(m3_delibdem, m4_delibdem)) 
anova(m3_delibdem, m4_delibdem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding control variables
# gender
m5_delibdem <- lmer(happ ~ gender + cRESEMAVAL + cRESEMAVAL*delibdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_delibdem)
anova(m4_delibdem, m5_delibdem) # +

# education
m6_delibdem <- lmer(happ ~ gender + edu + cRESEMAVAL + cRESEMAVAL*delibdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_delibdem)
anova(m5_delibdem, m6_delibdem) # +

# age
m7_delibdem <- lmer(happ ~ gender + edu + scale.age + cRESEMAVAL*delibdem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_delibdem)
anova(m6_delibdem, m7_delibdem) # +

# age^2
m8_delibdem <- lmer(happ ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_delibdem)
anova(m7_delibdem, m8_delibdem) # +

# marital status
m9_delibdem <- lmer(happ ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_delibdem)
anova(m8_delibdem, m9_delibdem) # +

# employment
m10_delibdem <- lmer(happ ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_delibdem)
anova(m9_delibdem, m10_delibdem) # +

# health
m11_delibdem <- lmer(happ ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_delibdem)
anova(m10_delibdem, m11_delibdem) # +

# subjective freedom
m12_delibdem <- lmer(happ ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*delibdem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_delibdem)
anova(m11_delibdem, m12_delibdem) # +

##### Egalitarian Democracy (V-Dem) #####
m3_egaldem <- lmer(happ ~ cRESEMAVAL + egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_egaldem)
screenreg(list(m2, m3_egaldem)) 
anova(m2, m3_egaldem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding interaction effect 
m4_egaldem <- lmer(happ ~ cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_egaldem)
screenreg(list(m3_egaldem, m4_egaldem)) 
anova(m3_egaldem, m4_egaldem)
# модель значимо улучшилась по anova, AIC, но не по BIC

## adding control variables
# gender
m5_egaldem <- lmer(happ ~ gender + cRESEMAVAL + cRESEMAVAL*egaldem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_egaldem)
anova(m4_egaldem, m5_egaldem) # +

# education
m6_egaldem <- lmer(happ ~ gender + edu + cRESEMAVAL + cRESEMAVAL*egaldem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_egaldem)
anova(m5_egaldem, m6_egaldem) # +

# age
m7_egaldem <- lmer(happ ~ gender + edu + scale.age + cRESEMAVAL*egaldem +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_egaldem)
anova(m6_egaldem, m7_egaldem) # +

# age^2
m8_egaldem <- lmer(happ ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_egaldem)
anova(m7_egaldem, m8_egaldem) # +

# marital status
m9_egaldem <- lmer(happ ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem +  (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_egaldem)
anova(m8_egaldem, m9_egaldem) # +

# employment
m10_egaldem <- lmer(happ ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_egaldem)
anova(m9_egaldem, m10_egaldem) # +

# health
m11_egaldem <- lmer(happ ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_egaldem)
anova(m10_egaldem, m11_egaldem) # +

# subjective freedom
m12_egaldem <- lmer(happ ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*egaldem + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_egaldem)
anova(m11_egaldem, m12_egaldem) # +

##### country's EV #####
## adding 2nd level variable
m3_ev <- lmer(happ ~ cRESEMAVAL + EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m3_ev)
screenreg(list(m2, m3_ev)) 
anova(m2, m3_ev)
# модель значимо улучшилась по anova, AIC, BIC

## adding interaction effect 
m4_ev <- lmer(happ ~ cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m4_ev)
screenreg(list(m3_ev, m4_ev)) 
anova(m3_ev, m4_ev) # +
# модель значимо улучшилась по anova, AIC, но не по BIC
# эффект взаимодействия для EV не такой сильный, как для переменных отражающих политический режим

## adding control variables
# gender
m5_ev <- lmer(happ ~ gender + cRESEMAVAL + cRESEMAVAL*EV_country +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m5_ev)
anova(m4_ev, m5_ev) # +

# education
m6_ev <- lmer(happ ~ gender + edu + cRESEMAVAL + cRESEMAVAL*EV_country +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m6_ev)
anova(m5_ev, m6_ev) # +

# age
m7_ev <- lmer(happ ~ gender + edu + scale.age + cRESEMAVAL*EV_country +(cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m7_ev)
anova(m6_ev, m7_ev) # +

# age^2
m8_ev <- lmer(happ ~ gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country  + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m8_ev)
anova(m7_ev, m8_ev) # +

# marital status
m9_ev <- lmer(happ ~ married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m9_ev)
anova(m8_ev, m9_ev) # +

m10_ev <- lmer(happ ~ employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country +  (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m10_ev)
anova(m9_ev, m10_ev) # +

m11_ev <- lmer(happ ~ scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m11_ev)
anova(m10_ev, m11_ev) # +

# subjective freedom
m12_ev <- lmer(happ ~ scale.subfree + scale.health + employment + married + gender + edu + scale.age + I(scale.age^2)  +  cRESEMAVAL*EV_country + (cRESEMAVAL|country), df, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m12_ev)
anova(m11_ev, m12_ev) # +


##### Model comparison ######

# Models without control variables
screenreg(list(m4_FH, m4_polyarchy, m4_libdem,  m4_partipdem, m4_delibdem, m4_egaldem, m4_ev)) 

