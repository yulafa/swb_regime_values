# Libraries
library(foreign)
library(dplyr)
library(ggplot2)
library(lme4)
library(sjstats)
library(sjPlot)
library(texreg)
library(ggpubr)
library(haven)


#### creating dataset with country and individual variables #####
individual_level <- read.spss('ind_vars.sav', to.data.frame = T)
country_level <- read.spss('country_vars.sav', to.data.frame = T)
df <- merge(individual_level, country_level, by.x = "country", by.y = "country")
#write_sav(df, "merged_df.sav")	

#### data preparation ####
# df <- read.spss('merged_df.sav', to.data.frame = T)
df$country <- trimws(df$country)
df$country <- as.factor(df$country)

df %>% 
  count(country) # 86 countries

df %>% 
  count(cntry_y) # 94 surveys 

# remove countries that participated twice
df <- df %>%
  filter(!((country == "Armenia" & year == 2021) | 
             (country == "Germany" & year == 2018) | 
             (country == "Netherlands" & year == 2022) | 
             (country == "Serbia" & year == 2017) | 
             (country == "Slovakia" & year == 2022) |
             (country == "United Kingdom" & year == 2022) |
             (country == "Czechia" & year == 2022) |
             (country == "Romania" & study == "WVS") |
             (country == "Russia" & study == "WVS") |
             (country == "Ukraine" & study == "WVS"))) 

# countries amount
df %>% 
  count(country) # 86 countries
df %>% 
  count(cntry_y) # 86 surveys


# Country level variables
# Freedom House index in year of values survey
df$FH_survey_year <- 
  ifelse(df$year == 2022, df$FH_total.2022, 
         ifelse(df$year == 2021, df$FH_total.2021, 
                ifelse(df$year == 2020, df$FH_total.2020, 
                       ifelse(df$year == 2019, df$FH_total.2019, 
                              ifelse(df$year == 2018, df$FH_total.2018, 
                                     ifelse(df$year == 2017, df$FH_total.2017, 0))))))


# liberal democracy in year of values survey
df$libdem_survey_year <- 
  ifelse(df$year == 2022, df$v2x_libdem.2022, 
         ifelse(df$year == 2021, df$v2x_libdem.2021, 
                ifelse(df$year == 2020, df$v2x_libdem.2020, 
                       ifelse(df$year == 2019, df$v2x_libdem.2019, 
                              ifelse(df$year == 2018, df$v2x_libdem.2018, 
                                     ifelse(df$year == 2017, df$v2x_libdem.2017, 0))))))


# electoral democracy in year of values survey
df$polyarchy_survey_year <- 
  ifelse(df$year == 2022, df$v2x_polyarchy.2022, 
         ifelse(df$year == 2021, df$v2x_polyarchy.2021, 
                ifelse(df$year == 2020, df$v2x_polyarchy.2020, 
                       ifelse(df$year == 2019, df$v2x_polyarchy.2019, 
                              ifelse(df$year == 2018, df$v2x_polyarchy.2018, 
                                     ifelse(df$year == 2017, df$v2x_polyarchy.2017, 0))))))

# participatory democracy in year of values survey
df$partipdem_survey_year <- 
  ifelse(df$year == 2022, df$v2x_partipdem.2022, 
         ifelse(df$year == 2021, df$v2x_partipdem.2021, 
                ifelse(df$year == 2020, df$v2x_partipdem.2020, 
                       ifelse(df$year == 2019, df$v2x_partipdem.2019, 
                              ifelse(df$year == 2018, df$v2x_partipdem.2018, 
                                     ifelse(df$year == 2017, df$v2x_partipdem.2017, 0))))))

# deliberative democracy in year of values survey
df$delibdem_survey_year <- 
  ifelse(df$year == 2022, df$v2x_delibdem.2022, 
         ifelse(df$year == 2021, df$v2x_delibdem.2021, 
                ifelse(df$year == 2020, df$v2x_delibdem.2020, 
                       ifelse(df$year == 2019, df$v2x_delibdem.2019, 
                              ifelse(df$year == 2018, df$v2x_delibdem.2018, 
                                     ifelse(df$year == 2017, df$v2x_delibdem.2017, 0))))))

# egalitarian democracy in year of values survey
df$egaldem_survey_year <- 
  ifelse(df$year == 2022, df$v2x_egaldem.2022, 
         ifelse(df$year == 2021, df$v2x_egaldem.2021, 
                ifelse(df$year == 2020, df$v2x_egaldem.2020, 
                       ifelse(df$year == 2019, df$v2x_egaldem.2019, 
                              ifelse(df$year == 2018, df$v2x_egaldem.2018, 
                                     ifelse(df$year == 2017, df$v2x_egaldem.2017, 0))))))



#### Data preparation ####
vars <- c("country",
          "lifesat", 
          "happiness", 
          "gender", 
          "age", 
          "child",
          "married",
          "edu", 
          "employment",
          "health",
          "subfree", 
          "RESEMAVAL", 
          "FH_survey_year", 
          "libdem_survey_year",
          "polyarchy_survey_year",
          "partipdem_survey_year",
          "delibdem_survey_year",
          "egaldem_survey_year") 

df <- df[vars]
rm(vars)

df <- na.omit(df) # 125657 number of observations

# Variables preparation
## country
df$country <- trimws(df$country)
df$country <- as.factor(df$country) 
summary(df$country)

# SWB
df$SWB <- df$lifesat - (2.5*df$happiness)

# Emancipative values of country
df <- df %>%
  group_by(country) %>%
  mutate(EV_country = mean(RESEMAVAL, na.rm = TRUE))


# cRESEMAVAL = EVI country-mean centered (values of individual centered on average values of his/her country)
df <- df %>%
  group_by(country) %>% 
  mutate(cRESEMAVAL = RESEMAVAL-mean(RESEMAVAL, na.rm = TRUE))

# rename country-level variables
df$libdem <- df$libdem_survey_year 
df$polyarchy <- df$polyarchy_survey_year 
df$partipdem <- df$partipdem_survey_year 
df$delibdem <- df$delibdem_survey_year 
df$egaldem <- df$egaldem_survey_year 


###  ----------------------- Save main article df  -----------------------

summary(df)
vars <- c("country",
          "SWB", 
          "lifesat", 
          "happiness", 
          "gender", 
          "age", 
          "edu", 
          "married",
          "employment",
          "health",
          "subfree", 
          "RESEMAVAL", 
          "cRESEMAVAL", 
          "EV_country", 
          "FH_survey_year", 
          "libdem",
          "polyarchy",
          "partipdem",
          "delibdem",
          "egaldem") 
df <- df[vars]
summary(df)


write_sav(df, "main_article_df.sav")	
