# Libraries
library(foreign)
library(dplyr)
library(ggplot2)
library(xlsx)
library(haven)


### ----------------------- V-DEM -----------------------
vdem <- read.spss('V-Dem-CY-Full+Others-v13.sav', to.data.frame = T)

# Relevant Variables
vars <- c("country_name", #country name
          "year", #year
          "v2x_polyarchy", #electoral democracy index
          "v2x_libdem", #liberal democracy index
          "v2x_partipdem", #participatory democracy index
          "v2x_delibdem", #deliberative democracy index
          "v2x_egaldem", #egalitarian democracy index
          "v2x_regime", #the ROW measure
          "v2x_regime_amb" #the ROW measure with categories
)

vdem <- vdem[vars]

vdem$country <- trimws(vdem$country_name)
vdem$country <- as.factor(vdem$country)

# select data only from 2012
vdem <- vdem[vdem$year >= 2012,]

# Change the dataset format so that the country name and country code remain in the columns (wide format)
vdem2 <- reshape(vdem, idvar = "country", timevar = "year", direction = "wide")

# change variable type
vdem2$country <- trimws(vdem2$country)
vdem2$country <- as.factor(vdem2$country)

# change countries name for future merge 
levels(vdem2$country)[levels(vdem2$country)== "United States of America"] <- "United States"
levels(vdem2$country)[levels(vdem2$country)== "Burma/Myanmar"] <- "Myanmar"

# Select relevant variables
vars <- c("country", #country name
          "v2x_polyarchy.2012", # electoral democracy index
          "v2x_polyarchy.2013",
          "v2x_polyarchy.2014",
          "v2x_polyarchy.2015",
          "v2x_polyarchy.2016",
          "v2x_polyarchy.2017",
          "v2x_polyarchy.2018",
          "v2x_polyarchy.2019",
          "v2x_polyarchy.2020",
          "v2x_polyarchy.2021",
          "v2x_polyarchy.2022",
          "v2x_libdem.2012", #liberal democracy index
          "v2x_libdem.2013",
          "v2x_libdem.2014",
          "v2x_libdem.2015",
          "v2x_libdem.2016",
          "v2x_libdem.2017",
          "v2x_libdem.2018",
          "v2x_libdem.2019",
          "v2x_libdem.2020",
          "v2x_libdem.2021",
          "v2x_libdem.2022",
          "v2x_partipdem.2012", # participatory democracy index
          "v2x_partipdem.2013",
          "v2x_partipdem.2014",
          "v2x_partipdem.2015",
          "v2x_partipdem.2016",
          "v2x_partipdem.2017",
          "v2x_partipdem.2018",
          "v2x_partipdem.2019",
          "v2x_partipdem.2020",
          "v2x_partipdem.2021",
          "v2x_partipdem.2022",
          "v2x_delibdem.2012", # deliberative democracy index
          "v2x_delibdem.2013",
          "v2x_delibdem.2014",
          "v2x_delibdem.2015",
          "v2x_delibdem.2016",
          "v2x_delibdem.2017",
          "v2x_delibdem.2018",
          "v2x_delibdem.2019",
          "v2x_delibdem.2020",
          "v2x_delibdem.2021",
          "v2x_delibdem.2022",
          "v2x_egaldem.2012", # egalitarian democracy index
          "v2x_egaldem.2013",
          "v2x_egaldem.2014",
          "v2x_egaldem.2015",
          "v2x_egaldem.2016",
          "v2x_egaldem.2017",
          "v2x_egaldem.2018",
          "v2x_egaldem.2019",
          "v2x_egaldem.2020",
          "v2x_egaldem.2021",
          "v2x_egaldem.2022", 
          "v2x_regime.2012"
)

vdem2 <- vdem2[vars]
rm(vdem)
rm(vars)

###  ----------------------- Freedom House -----------------------
FH <- read.xlsx("All_data_FREEDOM_HOUSE_2013-2023.xlsx", sheetIndex = 2, header = TRUE) 

FH$country <- trimws(FH$Country.Territory)
FH$country <- as.factor(FH$country)
FH$year <- FH$Edition - 1
FH$FH_total <- FH$Total 

vars_fh <- c("country",
             "year",
             "FH_total"
)

FH <- FH[vars_fh]


# Изменить форму датасета, чтобы название страны и код страны остались в колонках
FH2 <- reshape(FH, idvar = "country", timevar = "year", direction = "wide")
table(FH2$country)
rm(FH)
rm(vars_fh)


###  ----------------------- FH and V-Dem data merge  -----------------------
df <- merge(vdem2, FH2, by.x = "country", by.y = "country")

summary(df)
df <- as.data.frame(unclass(df), # Convert all columns to factor
                    stringsAsFactors = TRUE)
summary(df)
table(df$country)

###  ----------------------- Save df with country-level variables -----------------------
write_sav(df, "country_vars.sav")	
