# The Effect of Political Regime on the Association of Values with Subjective Well-Being

Data, code, and instructions to reproduce the findings for the paper 'The Effect of Political Regime on the Association of Values with Subjective Well-Being'

You can read full text of the article here https://doi.org/10.1007/s10902-024-00841-9


# The workflow of the code:
There is four main steps to repeat our model from the article, they are correspond four R files: 
1. ind_vars - preparation of variables on individual level (ex: SWB, age, gender ...)
2. country_vars - preparation of variables on country level (ex: V-Dem index, Freedom House index ...)
3. main_df_preparation - merging the datasets obtained from the previous steps and performing some additional transformations
4. main_artcile_code - building of the models, comparing them and performing some analysis


# Data 
We used 3 datasets. All datasets are free available:
1. World Values Survey: http://www.worldvaluessurvey.org/WVSContents.jsp ('EVS_WVS_Joint_Spss_v4_0.sav')
2. Varieties of Democracy: https://www.v-dem.net/data/the-v-dem-dataset/ ('V-Dem-CY-Full+Others-v13.sav')
3. Freedom House: https://freedomhouse.org/sites/default/files/2023-02/All_data_FIW_2013-2023.xlsx ('All_data_FREEDOM_HOUSE_2013-2023.xlsx')

# Our contacts
If you want to send us feedback or you have any questions please, feel free to contact us afanasye8a@gmail.com