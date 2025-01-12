# Libraries
library(foreign)
library(naniar)
library(dplyr)
library(ggplot2)
library(haven)

# Data
joint_wvs_evs <- read.spss('EVS_WVS_Joint_Spss_v4_0.sav', to.data.frame = T)

# Relevant Variables
vars <- c("study", # Study
          "wave", # Wave
          "cntrycow", # Country (CoW Numeric code)
          "year", # Year survey
          "cntry_y", # Country - year
          "A008", # Feeling of happiness
          "A170", # Satisfaction with your life
          "A009", # State of health (subjective)
          "A173", # How much freedom of choice and control
          "A029", # Important child qualities: independence
          "A034", # Important child qualities: imagination
          "A042", # Important child qualities: obedience
          "C001", # Jobs scarce: Men should have more right to a job than women (5-point scale)
          "D059", # Men make better political leaders than women do
          "D060", # University is more important for a boy than for a girl
          "E001", # Aims of country: first choice
          "E002", # Aims of country: second choice
          "E003", # Aims of respondent: first choice
          "E004", # Aims of respondent: second choice
          "F118", # Justifiable: Homosexuality
          "F120", # Justifiable: Abortion
          "F121", # Justifiable: Divorce
          "X001", # Gender
          "X003", # Age 
          "X007", # Marital status
          "X011", # Children
          "X025R", # Education (3 groups)
          "X028" # Employment status
          )

wvs_evs <- joint_wvs_evs[vars]

wvs_evs <- wvs_evs %>% 
  rename("country" = "cntrycow")


### --------------------- Control Variables ---------------------

# Gender
wvs_evs <- wvs_evs %>% 
  rename("gender" = "X001")

# Age
wvs_evs$age <- as.numeric(as.character(wvs_evs$X003))

# Marital status
wvs_evs$married <- car::recode(as.numeric(wvs_evs$X007), "1:2=1; 3:6=0") 
wvs_evs$married <- factor(wvs_evs$married, levels = c(0,1),
                          labels = c("No", "Yes"))

# Children
wvs_evs$child <- car::recode(as.numeric(wvs_evs$X011), "1=0; 2:6=1")
wvs_evs$child <- factor(wvs_evs$child, levels = c(0,1),
                          labels = c("No", "Yes"))

# Education 
wvs_evs <- wvs_evs %>% 
  rename("edu" = "X025R")

# Employment status
levels(wvs_evs$X028) <- c("Full", "Part", "Self", "Retired", "Housewife", "Student", "Unemployed", "Other")
wvs_evs <- wvs_evs %>% 
  rename("employment" = "X028")

### Health
wvs_evs$health <- as.numeric(wvs_evs$A009) #1 - very good, 5 - very poor
wvs_evs$health <- 6 - wvs_evs$health #5 - good, 1 - very poor

### Subjective Freedom
wvs_evs$subfree <- as.numeric(wvs_evs$A173) # 10 - a great, 1 - none at all

### Happiness
wvs_evs$happiness <- as.numeric(wvs_evs$A008) #1 - very happy, 4 - not at all happy

### Life Satisfaction
wvs_evs$lifesat <- as.numeric(wvs_evs$A170) #1 - Dissatisfied, 10 - Satisfied


### --------------------- Emancipative values ---------------------

### AUTONOMY ###

# Recode variables from dataset format
wvs_evs$A029 <-  as.numeric(wvs_evs$A029) - 1 # 1 - mentioned, 0 - not mentioned
wvs_evs$A034 <-  as.numeric(wvs_evs$A034) - 1 # 1 - mentioned, 0 - not mentioned
wvs_evs$A042 <-  as.numeric(wvs_evs$A042) - 1 # 1 - mentioned, 0 - not mentioned

# Independence as kid quality
wvs_evs <- wvs_evs %>%
  mutate(indep = case_when(
    wvs_evs$A029 == 1 ~ 1,
    wvs_evs$A029 == 0 ~ 0
  ))

# Imagination as kid quality
wvs_evs <- wvs_evs %>%
  mutate(imagin = case_when(
    wvs_evs$A034 == 1 ~ 1,
    wvs_evs$A034 == 0 ~ 0,
  ))

# Obedience not kid quality
wvs_evs <- wvs_evs %>%
  mutate(nonobed = case_when(
    wvs_evs$A042 == 1 ~ 0,
    wvs_evs$A042 == 0 ~ 1,
  ))

# Step 1. Select dataset without NA for autonomy calculation
autonomy <- wvs_evs %>% select(indep, imagin, nonobed)

# Step 2. Calculate the autonomy index on the full dataset
autonomy$y <- (autonomy$indep + autonomy$imagin + autonomy$nonobed)/3

# Step 3. Obtain weights for calculating the autonomy index if one of the variables is NA
# When nonobed is NA
no_nonobed <- lm(y ~ indep + imagin, data = autonomy)
no_nonobed_intercept <-  no_nonobed$coefficients[1]
no_nonobed_indep <-  no_nonobed$coefficients[2]
no_nonobed_imagin <- no_nonobed$coefficients[3]

# When imagin is NA
no_imagin <- lm(y ~ indep + nonobed, data = autonomy)
no_imagin_intercept <- no_imagin$coefficients[1]
no_imagin_indep <- no_imagin$coefficients[2]
no_imagin_nonobed <- no_imagin$coefficients[3]

# When indep is NA
no_indep <- lm(y ~ imagin + nonobed, data = autonomy)
no_indep_intercept <- no_indep$coefficients[1]
no_indep_imagin <- no_indep$coefficients[2]
no_indep_nonobed <- no_indep$coefficients[3]


# AUTONOMY SUB-INDEX
wvs_evs$AUTONOMY <- 
  ifelse((!is.na(wvs_evs$indep) & !is.na(wvs_evs$imagin) & !is.na(wvs_evs$nonobed)),
         ((wvs_evs$indep+wvs_evs$imagin+wvs_evs$nonobed)/3),
  ifelse((!is.na(wvs_evs$indep) & !is.na(wvs_evs$imagin)),
         (no_nonobed_intercept+no_nonobed_indep*wvs_evs$indep+no_nonobed_imagin*wvs_evs$imagin), 
  ifelse((!is.na(wvs_evs$indep) & !is.na(wvs_evs$nonobed)),
          (no_imagin_intercept+no_imagin_indep*wvs_evs$indep+no_imagin_nonobed*wvs_evs$nonobed),
  ifelse((!is.na(wvs_evs$imagin) & !is.na(wvs_evs$nonobed)),
          (no_indep_intercept+no_indep_imagin*wvs_evs$imagin+no_indep_nonobed*wvs_evs$nonobed), NA))))


### EQUALITY ###

# Recode variables from dataset format
wvs_evs$C001 <-  as.numeric(wvs_evs$C001) # 1 - agree, 2 - disagree, 3 - neither 
wvs_evs$D059 <-  as.numeric(wvs_evs$D059) # 1- strongly agree, 4 - strongly disagree
wvs_evs$D060 <-  as.numeric(wvs_evs$D060) # 1- strongly agree, 4 - strongly disagree

# Gender equality: job
wvs_evs <- wvs_evs %>%
  mutate(womjob = case_when(
    wvs_evs$C001 == 1 ~ 0,
    wvs_evs$C001 == 2 ~ 1,
    wvs_evs$C001 == 3 ~ 0.5,
  ))

# Gender equality: politics
wvs_evs <- wvs_evs %>%
  mutate(wompol = case_when(
    wvs_evs$D059 == 1 ~ 0,
    wvs_evs$D059 == 2 ~ 0.33,
    wvs_evs$D059 == 3 ~ 0.66,
    wvs_evs$D059 == 4 ~ 1,
  ))

# Gender equality: education
wvs_evs <- wvs_evs %>%
  mutate(womedu = case_when(
    wvs_evs$D060 == 1 ~ 0,
    wvs_evs$D060 == 2 ~ 0.33,
    wvs_evs$D060 == 3 ~ 0.66,
    wvs_evs$D060 == 4 ~ 1,
  ))


# Calculation weights for sub-index EQUALITY (same as for AUTONOMY)
# Step 1. Select dataset without NA for EQUALITY calculation
equality <- wvs_evs %>% select(womjob, wompol, womedu)

# Step 2. Calculate EQUALITY index on the full dataset
equality$y <- (equality$womjob + equality$wompol + equality$womedu)/3

# Step 3. Obtain weights for calculating EQUALITY index if one of the variables is NA
# When wompol is NA
no_wompol <- lm(y ~ womedu + womjob, data = equality)
no_wompol_intercept <-  no_wompol$coefficients[1]
no_wompol_edu <-  no_wompol$coefficients[2]
no_wompol_job <- no_wompol$coefficients[3]

# When womedu is NA
no_womedu <- lm(y ~ wompol + womjob, data = equality)
no_womedu_intercept <- no_womedu$coefficients[1]
no_womedu_pol <- no_womedu$coefficients[2]
no_womedu_job <- no_womedu$coefficients[3]

# When womjob is NA
no_womjob <- lm(y ~ wompol + womedu, data = equality)
no_womjob_intercept <- no_womjob$coefficients[1]
no_womjob_pol <- no_womjob$coefficients[2]
no_womjob_edu <- no_womjob$coefficients[3]

# EQUALITY SUB-INDEX
wvs_evs$EQUALITY <- 
  ifelse((!is.na(wvs_evs$wompol) & !is.na(wvs_evs$womedu) & !is.na(wvs_evs$womjob)),                            ((wvs_evs$wompol+wvs_evs$womedu+wvs_evs$womjob)/3),
  ifelse((!is.na(wvs_evs$womedu) & !is.na(wvs_evs$womjob)),                    
         (no_wompol_intercept+no_wompol_edu*wvs_evs$womedu+no_wompol_job*wvs_evs$womjob), 
  ifelse((!is.na(wvs_evs$wompol) & !is.na(wvs_evs$womjob)),                    
         (no_womedu_intercept+no_womedu_pol*wvs_evs$wompol+no_womedu_job*wvs_evs$womjob),
  ifelse((!is.na(wvs_evs$wompol) & !is.na(wvs_evs$womedu)),                    
         (no_womjob_intercept+no_womjob_pol*wvs_evs$wompol+no_womjob_edu*wvs_evs$womedu), NA))))


### CHOICE ###

# Recode variables from dataset format
wvs_evs$F118 <-  as.numeric(wvs_evs$F118) # 1 - never justifiable , 10 - always justifiable 
wvs_evs$F120 <-  as.numeric(wvs_evs$F120) # 1 - never justifiable , 10 - always justifiable 
wvs_evs$F121 <-  as.numeric(wvs_evs$F121) # 1 - never justifiable , 10 - always justifiable 

# Homosexuality acceptance
wvs_evs <- wvs_evs %>%
  mutate(homolib = case_when(
    wvs_evs$F118 > 0 ~ (wvs_evs$F118-1)/9,
  ))

# Abortion acceptable
wvs_evs <- wvs_evs %>%
  mutate(abortlib = case_when(
    wvs_evs$F120 > 0 ~ (wvs_evs$F120-1)/9,
  ))

# Divorce acceptable
wvs_evs <- wvs_evs %>%
  mutate(divorlib = case_when(
    wvs_evs$F121 > 0 ~ (wvs_evs$F121-1)/9,
  ))


# Calculation weights for sub-index CHOICE (same as for AUTONOMY)
# Step 1. Select dataset without NA for CHOICE calculation
choice <- wvs_evs %>% select(homolib, abortlib, divorlib)

# Step 2. Calculate CHOICE index on the dataset without NA
choice$y <- (choice$homolib + choice$abortlib + choice$divorlib)/3

# Step 3. Obtain weights for calculating CHOICE index if one of the variables is NA
# When homolib is NA
no_homolib <- lm(y ~ abortlib + divorlib, data = choice)
no_homolib_intercept <-  no_homolib$coefficients[1]
no_homolib_abort <-  no_homolib$coefficients[2]
no_homolib_divor <- no_homolib$coefficients[3]

# When abortlib is NA
no_abortlib <- lm(y ~ homolib + divorlib, data = choice)
no_abortlib_intercept <- no_abortlib$coefficients[1]
no_abortlib_homo <- no_abortlib$coefficients[2]
no_abortlib_divor <- no_abortlib$coefficients[3]

# When divorlib is NA
no_divorlib <- lm(y ~ homolib + abortlib, data = choice)
no_divorlib_intercept <- no_divorlib$coefficients[1]
no_divorlib_homo <- no_divorlib$coefficients[2]
no_divorlib_abort <- no_divorlib$coefficients[3]

# CHOICE SUB-INDEX
wvs_evs$CHOICE <- 
  ifelse((!is.na(wvs_evs$homolib) & !is.na(wvs_evs$abortlib) & !is.na(wvs_evs$divorlib)),                                ((wvs_evs$homolib + wvs_evs$abortlib + wvs_evs$divorlib)/3),
  ifelse((!is.na(wvs_evs$abortlib) & !is.na(wvs_evs$divorlib)),                      
         (no_homolib_intercept + no_homolib_abort*wvs_evs$abortlib + no_homolib_divor*wvs_evs$divorlib), 
  ifelse((!is.na(wvs_evs$homolib) & !is.na(wvs_evs$divorlib)),                      
         (no_abortlib_intercept + no_abortlib_homo*wvs_evs$homolib + no_abortlib_divor*wvs_evs$divorlib),
  ifelse((!is.na(wvs_evs$homolib) & !is.na(wvs_evs$abortlib)),                      
         (no_divorlib_intercept + no_divorlib_homo*wvs_evs$homolib + no_divorlib_abort*wvs_evs$abortlib), NA))))

### VOICE ### 

# Recode variables from dataset format
wvs_evs$E001 <- as.numeric(wvs_evs$E001)
wvs_evs$E002 <- as.numeric(wvs_evs$E002)
wvs_evs$E003 <- as.numeric(wvs_evs$E003)
wvs_evs$E004 <- as.numeric(wvs_evs$E004)

# Voice 1
wvs_evs$voice1 <- 
  ifelse((wvs_evs$E003==2 & wvs_evs$E004==4)|(wvs_evs$E003==4 & wvs_evs$E004==2), 1,
         ifelse((wvs_evs$E003==2 & wvs_evs$E004!=4)|(wvs_evs$E003==4 & wvs_evs$E004!=2),0.66,
                ifelse((wvs_evs$E003!=2 & wvs_evs$E004==4)|(wvs_evs$E003!=4 & wvs_evs$E004==2),0.33,
                       ifelse((!is.na(wvs_evs$E003) & !is.na(wvs_evs$E004)),0, NA))))

# Voice 2 
wvs_evs$voice2 <- ifelse(wvs_evs$E001 == 3, 1,
                           ifelse(wvs_evs$E002 == 3, 0.5,
                                  ifelse((!is.na(wvs_evs$E001) & !is.na(wvs_evs$E002)), 0, NA)))


# Voi2_00 (auxiliary)
wvs_evs$voi2_00 <- ifelse((!is.na(wvs_evs$voice1) & !is.na(wvs_evs$voice2)), 
                            ((wvs_evs$voice1+wvs_evs$voice2)/2), NA)


# Calculation weights for sub-index VOICE (same as for AUTONOMY)
# Step 1. Select dataset without NA for VOICE calculation
voice <- wvs_evs %>% select(voice1, voice2)

# Step 2. Calculate VOICE index on the dataset without NA
voice$y <- (voice$voice1 + voice$voice2)/2

# Step 3. Obtain weights for calculating VOICE index if one of the variables is NA
# When voice 1 is NA
no_voice1 <- lm(y ~ voice2, data = voice)
no_voice1_intercept <- no_voice1$coefficients[1]
no_voice1_voice2 <- no_voice1$coefficients[2]

# When voice 2 is NA
no_voice2 <- lm(y ~ voice1, data = voice)
no_voice2_intercept <- no_voice2$coefficients[1]
no_voice2_voice1 <- no_voice2$coefficients[2]

## VOICE SUB-INDEX
wvs_evs$VOICE <- 
  ifelse(!is.na(wvs_evs$voi2_00), wvs_evs$voi2_00,
  ifelse(!is.na(wvs_evs$voice1), (no_voice2_voice1*wvs_evs$voice1+no_voice2_intercept),
  ifelse(!is.na(wvs_evs$voice2), (no_voice1_voice2*wvs_evs$voice2+no_voice1_intercept), NA)))


# Emancipative values (EV) index ---------------------------------------------
# Step 1. Calculation weights for EV index (same as for AUTONOMY)
ev <- wvs_evs %>% select(AUTONOMY, EQUALITY, CHOICE, VOICE)

# Step 2. Calculate EV index on the dataset without NA
ev$y <- (ev$AUTONOMY + ev$EQUALITY + ev$CHOICE + ev$VOICE)/4

# Step 3. Obtain weights for calculating VOICE index if one of the variables is NA
# When EQUALITY is NA
NO_EQUALITY <- lm(y ~ AUTONOMY + CHOICE + VOICE, data = ev)
NO_EQUALITY_intercept <- NO_EQUALITY$coefficients[1]
NO_EQUALITY_autonomy <-  NO_EQUALITY$coefficients[2]
NO_EQUALITY_choice <- NO_EQUALITY$coefficients[3]
NO_EQUALITY_voice <- NO_EQUALITY$coefficients[4]

# When AUTONOMY is NA
NO_AUTONOMY <- lm(y ~ EQUALITY + CHOICE + VOICE, data = ev)
NO_AUTONOMY_intercept <-  NO_AUTONOMY$coefficients[1]
NO_AUTONOMY_equality <-  NO_AUTONOMY$coefficients[2]
NO_AUTONOMY_choice <- NO_AUTONOMY$coefficients[3]
NO_AUTONOMY_voice <- NO_AUTONOMY$coefficients[4]

# When CHOICE is NA
NO_CHOICE <- lm(y ~ AUTONOMY + EQUALITY + VOICE, data = ev)
summary(NO_CHOICE)
NO_CHOICE_intercept <-  0
NO_CHOICE_autonomy <-  NO_CHOICE$coefficients[2]
NO_CHOICE_equality <- NO_CHOICE$coefficients[3]
NO_CHOICE_voice <- NO_CHOICE$coefficients[4]

# When VOICE is NA
NO_VOICE <- lm(y ~ AUTONOMY + EQUALITY + CHOICE, data = ev)
NO_VOICE_intercept <-  NO_VOICE$coefficients[1]
NO_VOICE_autonomy <-  NO_VOICE$coefficients[2]
NO_VOICE_equality <- NO_VOICE$coefficients[3]
NO_VOICE_choice <- NO_VOICE$coefficients[4]

# Emancipative laues
wvs_evs$RESEMAVAL <- 
  ifelse((!is.na(wvs_evs$AUTONOMY) & !is.na(wvs_evs$EQUALITY) & !is.na(wvs_evs$CHOICE) & !is.na(wvs_evs$VOICE)),         ((wvs_evs$AUTONOMY + wvs_evs$EQUALITY + wvs_evs$CHOICE + wvs_evs$VOICE)/4), 
  ifelse((!is.na(wvs_evs$AUTONOMY) & !is.na(wvs_evs$CHOICE) & !is.na(wvs_evs$VOICE)), 
         (NO_EQUALITY_intercept+NO_EQUALITY_autonomy*wvs_evs$AUTONOMY+NO_EQUALITY_choice*wvs_evs$CHOICE+NO_EQUALITY_voice*wvs_evs$VOICE),
  ifelse((!is.na(wvs_evs$EQUALITY) & !is.na(wvs_evs$CHOICE) & !is.na(wvs_evs$VOICE)), 
         (NO_AUTONOMY_intercept+NO_AUTONOMY_equality*wvs_evs$EQUALITY+NO_AUTONOMY_choice*wvs_evs$CHOICE+NO_AUTONOMY_voice*wvs_evs$VOICE), 
  ifelse((!is.na(wvs_evs$AUTONOMY) & !is.na(wvs_evs$EQUALITY) & !is.na(wvs_evs$VOICE)), 
         (NO_CHOICE_intercept+NO_CHOICE_autonomy*wvs_evs$AUTONOMY+NO_CHOICE_equality*wvs_evs$EQUALITY+NO_CHOICE_voice*wvs_evs$VOICE),
  ifelse((!is.na(wvs_evs$AUTONOMY) & !is.na(wvs_evs$EQUALITY) & !is.na(wvs_evs$CHOICE)),
         (NO_VOICE_intercept+NO_VOICE_autonomy*wvs_evs$AUTONOMY+NO_VOICE_equality*wvs_evs$EQUALITY+NO_VOICE_choice*wvs_evs$CHOICE), NA)))))


## Emancipative values by  countries
wvs_evs %>% 
  group_by(country) %>% 
  summarize(mean = mean(RESEMAVAL, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(country, -mean), y = mean)) + geom_bar(stat = "identity") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) 





# --------------------- Saving the df with individual level variables ---------------------

# Rename countries to merge them with other data from Freedom House and V-Dem
levels(wvs_evs$country)[levels(wvs_evs$country)== "United States of America"] <- "United States"
levels(wvs_evs$country)[levels(wvs_evs$country)== "Great Britain"] <- "United Kingdom"
levels(wvs_evs$country)[levels(wvs_evs$country)== "Northern Ireland"] <- "United Kingdom"
levels(wvs_evs$country)[levels(wvs_evs$country)== "Taiwan ROC"] <- "Taiwan"
levels(wvs_evs$country)[levels(wvs_evs$country)== "Hong Kong SAR"] <- "Hong Kong"
levels(wvs_evs$country)[levels(wvs_evs$country)== "Malasia"] <- "Malaysia"
levels(wvs_evs$country)[levels(wvs_evs$country)== "Macau SAR"] <- "Macao"


# Select Relevant Variables
vars <- c("study", #Study
          "wave", #Wave
          "country", #Country (CoW Numeric code)
          "year", #Year survey
          "cntry_y", #Country - year",
          "gender", #gender
          "age",
          "married",
          "child", 
          "edu",
          "employment",
          "happiness",
          "lifesat",
          "health",
          "subfree",
          "AUTONOMY",
          "CHOICE",
          "EQUALITY",
          "VOICE",
          "RESEMAVAL"
)

wvs_evs <- wvs_evs[vars]

# Save df
write_sav(wvs_evs, "ind_vars.sav")	

