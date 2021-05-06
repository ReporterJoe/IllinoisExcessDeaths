# This code pulled and crunched data for this story: https://www.chicagotribune.com/coronavirus/ct-prem-coronavirus-covid-excess-deaths-2020-illinois-20210311-lnrkqbg56rdvvldmpsykknrqci-htmlstory.html

#This attempts to look at excess death figures for Illinois, including by cause of death:

#Let's first load the libraries we'll need:

library(rvest)
library(dplyr)
library(jsonlite)
library(httr)
library(readr)
library(lubridate)

#First step is to retrieve data from the CDC. Let's download two different datasets from the CDC:

CDC_weekly_by_state <- read_csv("https://data.cdc.gov/api/views/xkkf-xrst/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=")

CDC_weekly_by_state_by_cause <- read_csv("https://data.cdc.gov/api/views/u6jv-9ijr/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=")


#So let's dive into the first one, where we'll pull out Illinois to look at potential 2020 excess deaths by week:

IL_weekly_excess_weighted <- filter(CDC_weekly_by_state,State=="Illinois", Type=="Predicted (weighted)",Outcome=="All causes") %>% 
  arrange(`Week Ending Date`) %>% 
  mutate(weighted_observed=`Observed Number`) %>% 
  select(`Week Ending Date`,
         weighted_observed,
         `Upper Bound Threshold`,
         `Average Expected Count`)

IL_weekly_excess_unweighted <- filter(CDC_weekly_by_state,State=="Illinois", Type=="Unweighted",Outcome=="All causes") %>% 
  arrange(`Week Ending Date`) %>% 
  mutate(week_ending=`Week Ending Date`) %>% 
  mutate(unweighted_observed=`Observed Number`) %>% 
  mutate(upper_bound=`Upper Bound Threshold`) %>% 
  mutate(average=`Average Expected Count`) %>% 
  select(week_ending,
         unweighted_observed,
         upper_bound,
         average)

IL_excess_look <- merge(x=IL_weekly_excess_unweighted, y=IL_weekly_excess_weighted, by.x = "week_ending", by.y = "Week Ending Date", all.x = TRUE) %>% 
  mutate(percent_weighted_diff=round((weighted_observed-unweighted_observed)/unweighted_observed*100,digits=1)) %>% 
  mutate(observed_above_avg=unweighted_observed-average,
         weighted_above_avg=weighted_observed-average,
         observed_above_avg_percent=round((observed_above_avg/average*100), digits=1),
         weighted_above_avg_percent=round((weighted_above_avg/average*100), digits=1)) %>% 
  select(week_ending,
         unweighted_observed,
         weighted_observed,
         upper_bound,
         average,
         percent_weighted_diff,
         observed_above_avg,
         observed_above_avg_percent,
         weighted_above_avg,
         weighted_above_avg_percent)
    

#Now let's look at raw numbers for IL during 10-month span (and percentages)
IL_raw_totals <- filter(IL_excess_look,week_ending >= "2020-03-07" & week_ending <= "2021-01-02") %>% 
  summarize(observed_above_average=sum(observed_above_avg),
            weighted_above_avg=sum(weighted_above_avg),
            average_March_EOY=sum(average)) %>% 
  mutate(observed_percent_above_avg=round(observed_above_average/average_March_EOY*100,digits=1),
         weighted_percent_above_avg=round(weighted_above_avg/average_March_EOY*100,digits=1))


#Now we'll do a look at Illinois from Jan. 3, onward:
IL_raw_totals_2021 <- filter(IL_excess_look, week_ending > "2021-01-02") %>% 
  summarize(observed_above_average=sum(observed_above_avg),
            weighted_above_avg=sum(weighted_above_avg),
            average_Jan3_on=sum(average)) %>% 
  mutate(observed_percent_above_avg=round(observed_above_average/average_Jan3_on*100,digits=1),
         weighted_percent_above_avg=round(weighted_above_avg/average_Jan3_on*100,digits=1))

IL_raw_totals_2021_2 <- filter(IL_excess_look, week_ending > "2021-01-02")

  
#OK, Now we'll  try to see how Illinois compares to other states
#First up, the weighted figures

All_states_weekly_excess_weighted <- filter(CDC_weekly_by_state,Type=="Predicted (weighted)",Outcome=="All causes") %>% 
  arrange(`Week Ending Date`) %>% 
  mutate(weighted_observed=`Observed Number`) %>% 
  select(State,`Week Ending Date`,
         weighted_observed,
         `Upper Bound Threshold`,
         `Average Expected Count`)

#Now let's do unweighted
All_states_weekly_excess_unweighted <- filter(CDC_weekly_by_state,Type=="Unweighted",Outcome=="All causes") %>% 
  arrange(`Week Ending Date`) %>% 
  mutate(week_ending=`Week Ending Date`) %>% 
  mutate(unweighted_observed=`Observed Number`) %>% 
  mutate(upper_bound=`Upper Bound Threshold`) %>% 
  mutate(average=`Average Expected Count`) %>% 
  select(State, week_ending,
         unweighted_observed,
         upper_bound,
         average)

#Now let's try join
All_states_excess_look <- merge(x=All_states_weekly_excess_unweighted, y=All_states_weekly_excess_weighted, by.x = c("State", "week_ending"), by.y = c("State","Week Ending Date"), all.x = TRUE) %>% 
  mutate(percent_weighted_diff=round((weighted_observed-unweighted_observed)/unweighted_observed*100,digits=1)) %>% 
  mutate(observed_above_avg=unweighted_observed-average,
         weighted_above_avg=weighted_observed-average,
         observed_above_avg_percent=round((observed_above_avg/average*100), digits=1),
         weighted_above_avg_percent=round((weighted_above_avg/average*100), digits=1)) %>% 
  select(State, week_ending,
         unweighted_observed,
         weighted_observed,
         upper_bound,
         average,
         percent_weighted_diff,
         observed_above_avg,
         observed_above_avg_percent,
         weighted_above_avg,
         weighted_above_avg_percent)

#Let's calculate tallies, recognizing that we'll need to fix the issue with New York City and New York state being listed separately:
All_states_raw_totals <- filter(All_states_excess_look,week_ending >= "2020-03-07" & week_ending <= "2021-01-02") %>% 
  group_by(State) %>% 
  summarize(observed_above_avg=sum(observed_above_avg),
            weighted_above_avg=sum(weighted_above_avg),
            average_March_EOY=sum(average)) %>% 
  mutate(observed_percent_above_avg=round(observed_above_avg/average_March_EOY*100,digits=1),
         weighted_percent_above_avg=round(weighted_above_avg/average_March_EOY*100,digits=1))


NYS <- filter(All_states_raw_totals,State=="New York" | State=="New York City" ) %>% 
  summarize(observed_above_avg=sum(observed_above_avg),
            weighted_above_avg=sum(weighted_above_avg),
            average_March_EOY=sum(average_March_EOY)) %>% 
  mutate(State="New York") %>% 
  mutate(observed_percent_above_avg=round(observed_above_avg/average_March_EOY*100,digits=1),
         weighted_percent_above_avg=round(weighted_above_avg/average_March_EOY*100,digits=1))


#Now let's clean up to have just states (and entire states):

All_states_raw_totals <- filter(All_states_raw_totals,State!="New York" & State!="New York City" & State!="Puerto Rico" & State!="United States")

All_states_totals <- rbind(All_states_raw_totals,NYS) %>% 
  arrange(State)

All_states_totals <- replace(as.matrix(All_states_totals), is.na(All_states_totals), "")



#So let's do CDC estimated deaths for all states, fixing the New York issue again:


All_states_weekly_totals <- filter(All_states_excess_look,week_ending >= "2020-03-07" & week_ending <= "2021-01-02") %>% 
  mutate(observed_percent_above_avg=round(observed_above_avg/average*100,digits=1),
         weighted_percent_above_avg=round(weighted_above_avg/average*100,digits=1)) %>% 
  select(State,week_ending,average,weighted_above_avg, weighted_percent_above_avg)


NYS_weekly_totals <- filter(All_states_weekly_totals,State=="New York" | State=="New York City" ) %>% 
  group_by(week_ending) %>% 
  summarize(weighted_above_avg=sum(weighted_above_avg),
            average=sum(average)) %>% 
  mutate(State="New York") %>% 
  mutate(weighted_percent_above_avg=round(weighted_above_avg/average*100,digits=1)) %>% 
  select(State,week_ending,average,weighted_above_avg,weighted_percent_above_avg)


#Now let's clean up to have just states (and entire states):

All_states_weekly_totals <- filter(All_states_weekly_totals,State!="New York" & State!="New York City" & State!="Puerto Rico" & State!="United States")

All_states_by_week <- rbind(All_states_weekly_totals,NYS_weekly_totals) %>% 
  arrange(State) %>% 
  select(State,week_ending,weighted_percent_above_avg) %>% 
  spread(week_ending, weighted_percent_above_avg)

All_states_by_week <- replace(as.matrix(All_states_by_week), is.na(All_states_by_week), "") 



#Now let's look more closely at what's being reported in Illinois by IDPH vs CDC.
#We're going to look at IDPH's reports of not just actual but also probable deaths.
#We're going to look at CDC's reports of not just direct cause of death but also underlying.

#First step, let's get IDPH's data

IL_URL <- "https://www.dph.illinois.gov/sitefiles/COVIDHistoricalTestResults.json?nocache=0"

#using a GET command to read the json
IL_data<-GET(IL_URL)

#this is converting the json file to a list and then to a dataframe. then creates a clean date field and calculates seven-day totals for cases, deaths, tests. Then rolling averages! 

decode_IL_data <- jsonlite::fromJSON(content(IL_data, as="text"), flatten=TRUE)

#this is pointing the scraper to the level where the data lives

IL_statewide_data<-decode_IL_data[['state_testing_results']][['values']]

#Now we're going to add a column to do a proper join of dates

IL_statewide_data<- mutate(IL_statewide_data,compare_date=as_date(mdy(testDate)))

#Now let's get CDC data that counts COVID deaths:

CDC_COVID_death_tallies <- read_csv("https://data.cdc.gov/api/views/r8kw-7aab/rows.csv?accessType=DOWNLOAD")

#Now let's tease out Illinois' weekly deaths:

CDC_COVID_death_IL <- filter(CDC_COVID_death_tallies, State=="Illinois" & Group=="By Week") %>% 
  mutate(compare_date=as_date(mdy(`Week Ending Date`))) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

#Now let's combine IL_CDC week_ending data just to sync up weeks,then we'll do math for IL off those weeks to find weekly IDPH totals

IL_statewide_data_weekly <-merge(x=CDC_COVID_death_IL,y=IL_statewide_data,by.x="compare_date",by.y="compare_date",all.x=TRUE) %>% 
  arrange(compare_date) %>% 
  mutate(Weekly_IDPH_confirmed_deaths=deaths-lag(deaths, default = first(deaths))) %>% 
  mutate(Weekly_IDPH_probable_deaths=probable_deaths-lag(probable_deaths, default = first(probable_deaths))) %>% 
  mutate(Weekly_IDPH_confirmed_probable_deaths=Weekly_IDPH_confirmed_deaths+Weekly_IDPH_probable_deaths) %>% 
  mutate(CDC_running_tally=cumsum(`COVID-19 Deaths`)) %>% 
  mutate(IDPH_confirmed_probable_tally=deaths+probable_deaths) %>% 
  rename(Week_ending_date=`Week Ending Date`,
         CDC_weekly_death_count=`COVID-19 Deaths`,
         IDPH_confirmed_tally=deaths)

#To compare IDPH vs CDC:

IDPH_vs_CDC <- select(IL_statewide_data_weekly,
                      Week_ending_date,
                      CDC_running_tally,
                      IDPH_confirmed_tally,
                      IDPH_confirmed_probable_tally)

IDPH_vs_CDC <- replace(as.matrix(IDPH_vs_CDC), is.na(IDPH_vs_CDC), "")


#So now let's figure out the deaths tied and not tied to COVID.

IL_weekly_non_COVID_weighted <- filter(CDC_weekly_by_state,State=="Illinois", Type=="Predicted (weighted)",Outcome=="All causes, excluding COVID-19") %>% 
  arrange(`Week Ending Date`) %>% 
  mutate(weighted_non_COVID=`Observed Number`) %>% 
  select(`Week Ending Date`,
         weighted_non_COVID)


IL_excess_COVID_non_COVID_look <- merge(x=IL_excess_look, y=IL_weekly_non_COVID_weighted, by.x = "week_ending", by.y = "Week Ending Date", all.x = TRUE) %>% 
  filter(week_ending >= "2020-01-04" & week_ending <= "2021-01-02") %>% mutate(weighted_CDC_COVID=weighted_observed-weighted_non_COVID) %>% 
  select(week_ending,
         weighted_non_COVID,
         weighted_CDC_COVID,
         average,
         upper_bound)

#Now, let's look at race, first by percent difference over average in each week:

CDC_by_race <- read_csv("https://data.cdc.gov/api/views/qfhf-uhaa/rows.csv?accessType=DOWNLOAD")

CDC_by_race2 <- CDC_by_race %>% filter(`State Abbreviation`=="IL",
                                      MMWRYear=="2020",
                                      Type=="Predicted (weighted)",
                                      Outcome=="All Cause",
                                      MMWRWeek>9,
                                     # !is.na(`Percent Difference from 2015-2019 to 2020`),
                                     `Race/Ethnicity`!="Other",
                                     `Race/Ethnicity`!="Non-Hispanic American Indian or Alaska Native") %>% 
  select(`Week Ending Date`,
         `Race/Ethnicity`,
         `Percent Difference from 2015-2019 to 2020`) %>% 
  group_by(`Week Ending Date`,
           `Race/Ethnicity`,
           `Percent Difference from 2015-2019 to 2020`) %>% 
  pivot_wider(names_from = `Race/Ethnicity`, values_from = `Percent Difference from 2015-2019 to 2020`)



#Now let's get age data

CDC_by_age <- read_csv("https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD")

#Our first look  will be by percent difference:
#Now let's first pull out Illinois' figures for what actually happened (or is predicted to have happened, with lags) in 2020, from March onward
CDC_by_age_2020 <- CDC_by_age  %>% filter(`State Abbreviation`=="IL",
                                       Year=="2020",
                                       Type=="Predicted (weighted)",
                                       Week>9) %>% 
  select(`Week Ending Date`,
         Week,
         `Age Group`,
         `Number of Deaths`)

#Unfortunately,  this CDC dataset doesn't have the averages baked in, like the last one. So we have to build those averages ourselves. first step is pulling those out and computing:

CDC_by_age_average <- CDC_by_age  %>% filter(`State Abbreviation`=="IL",
                                             Year!="2020",
                                             Type=="Predicted (weighted)",
                                             Week==1|Week>9) %>% 
  select(Week,
         `Age Group`,
         `Number of Deaths`) %>% 
  group_by(Week,`Age Group`) %>% 
  summarize(average_deaths=mean(`Number of Deaths`)) 

#Special note: Because 2021 has 53 weeks listed, and other years have 52, we are converting prior years week 1 to week 53 to allow averages to be computed.

CDC_by_age_average$Week[CDC_by_age_average$Week==1] <-53

#Now we'll do the join (aka merge) of these two tables, then a crunch  to figure out how much higher above avg death rates are,  then final pivot on  those percentages.

CDC_by_age_2 <- merge(CDC_by_age_2020, CDC_by_age_average,
                          by.x=c("Week","Age Group"), by.y=c("Week","Age Group"),
                          all.x= TRUE, all.y= TRUE) 

CDC_by_age_final <- CDC_by_age_2 %>% 
  mutate(`Percent Difference from 2015-2019 to 2020`=round((`Number of Deaths`-average_deaths)/average_deaths*100,digits=1)) %>% 
  select(`Week Ending Date`,
         `Age Group`,
         `Percent Difference from 2015-2019 to 2020`) %>% 
  pivot_wider(names_from = `Age Group`, values_from = `Percent Difference from 2015-2019 to 2020`)




#The above shows wild swings based on % difference. But young people far less likely to die than older people. So let's also figure by rate per 100k pop

#First step is pulling in and cleaning up IL census data:

Illinois_census_age <- read_csv("C:/Users/jmahr/Downloads/IL_state_age_breakdown.txt") %>% 
  mutate(Under_25=`Estimate!!Total!!Total population!!AGE!!Under 5 years`+
           `Estimate!!Total!!Total population!!AGE!!5 to 9 years`+
           `Estimate!!Total!!Total population!!AGE!!10 to 14 years`+
           `Estimate!!Total!!Total population!!AGE!!15 to 19 years`+
           `Estimate!!Total!!Total population!!AGE!!20 to 24 years`,
         Age_25_to_44=`Estimate!!Total!!Total population!!AGE!!25 to 29 years`+
           `Estimate!!Total!!Total population!!AGE!!30 to 34 years`+
           `Estimate!!Total!!Total population!!AGE!!35 to 39 years`+
           `Estimate!!Total!!Total population!!AGE!!40 to 44 years`,
         Age_45_to_64=`Estimate!!Total!!Total population!!AGE!!45 to 49 years`+
           `Estimate!!Total!!Total population!!AGE!!50 to 54 years`+
           `Estimate!!Total!!Total population!!AGE!!55 to 59 years`+
           `Estimate!!Total!!Total population!!AGE!!60 to 64 years`,
         Age_65_to_74=`Estimate!!Total!!Total population!!AGE!!65 to 69 years`+
           `Estimate!!Total!!Total population!!AGE!!70 to 74 years`,
         Age_75_to_84=`Estimate!!Total!!Total population!!AGE!!75 to 79 years`+
           `Estimate!!Total!!Total population!!AGE!!80 to 84 years`,
         Age_85_plus=`Estimate!!Total!!Total population!!AGE!!85 years and over`)  %>% 
  select(Under_25,
         Age_25_to_44,
         Age_45_to_64,
         Age_65_to_74,
         Age_75_to_84,
         Age_85_plus)


CDC_by_age_3 <- CDC_by_age_2 %>% 
  mutate(Death_diff=`Number of Deaths`-average_deaths) %>% 
  mutate(Per_100_k_diff = ifelse(`Age Group`=="Under 25 years",Death_diff/Illinois_census_age$Under_25*100000,
                                 ifelse(`Age Group`=="25-44 years",Death_diff/Illinois_census_age$Age_25_to_44*100000,
                                        ifelse(`Age Group`=="45-64 years",Death_diff/Illinois_census_age$Age_45_to_64*100000,
                                               ifelse(`Age Group`=="65-74 years",Death_diff/Illinois_census_age$Age_65_to_74*100000,
                                                      ifelse(`Age Group`=="75-84 years",Death_diff/Illinois_census_age$Age_75_to_84*100000,
                                                             ifelse(`Age Group`=="85 years and older",Death_diff/Illinois_census_age$Age_85_plus*100000,0
                                        )))))))


#This rounds that final figure
CDC_by_age_3$Per_100_k_diff <- round(CDC_by_age_3$Per_100_k_diff,digits=1)

#Now let's pivot this

CDC_by_age_rate_final <- select(CDC_by_age_3,
                                `Week Ending Date`,
                                `Age Group`,
                                Per_100_k_diff) %>% 
                                pivot_wider(names_from = `Age Group`, 
                                     values_from = Per_100_k_diff)



#Now let's look at raw numbers for IL during 10-month span (and percentages)
IL_raw_totals <- filter(IL_excess_look,week_ending >= "2020-03-07" & week_ending <= "2021-01-02") %>% 
  summarize(observed_above_average=sum(observed_above_avg),
            weighted_above_avg=sum(weighted_above_avg),
            average_March_EOY=sum(average)) %>% 
  mutate(observed_percent_above_avg=round(observed_above_average/average_March_EOY*100,digits=1),
         weighted_percent_above_avg=round(weighted_above_avg/average_March_EOY*100,digits=1))


#Now let's look at causes of death, first step is downloading:

CDC_cause_death_raw <- read_csv("https://data.cdc.gov/api/views/u6jv-9ijr/rows.csv?accessType=DOWNLOAD")

#Now we'll look at the percent difference from week to week to spot any unusual patterns:
CDC_cause_death_percent_diff <- CDC_cause_death_raw %>% 
  filter(`State Abbreviation`=="IL",
         Type=="Predicted (weighted)",
         Year=="2020") %>% 
  group_by(`Cause Subgroup`) %>% 
  mutate(`3_week_avg_per_chg` = round(zoo::rollmean(`Percent Difference from 2015-2019 to 2020`, k = 3, fill = NA),digits=1)) %>% 
  select(`Week Ending Date`,
         `Cause Subgroup`,
         `3_week_avg_per_chg`) %>% 
  pivot_wider(names_from = `Cause Subgroup`, 
              values_from = `3_week_avg_per_chg`) %>% 
  filter(`Week Ending Date`!="2020-01-04",
         `Week Ending Date`!="2021-01-02")

  


#Now let's look at raw difference to see what may have had greatest effect:

CDC_cause_death_number_diff <- CDC_cause_death_raw %>% 
  filter(`State Abbreviation`=="IL",
         Type=="Predicted (weighted)",
         Year=="2020") %>% 
  group_by(`Cause Subgroup`) %>% 
  mutate(`3_week_avg_chg` = round(zoo::rollmean(`Difference from 2015-2019 to 2020`, k = 3, fill = NA),digits=1)) %>% 
  select(`Week Ending Date`,
         `Cause Subgroup`,
         `3_week_avg_chg`) %>% 
  pivot_wider(names_from = `Cause Subgroup`, 
              values_from = `3_week_avg_chg`) %>% 
  filter(`Week Ending Date`!="2020-01-04",
         `Week Ending Date`!="2021-01-02")



#One additional thing: We may want to create our own category called "heart diseases" because CDC data doesn't group them in a way to easily judge them.

joetest <- CDC_cause_death_raw %>% 
  filter(`Cause Subgroup`=="Heart failure"|
           `Cause Subgroup`=="Hypertensive diseases"|
           `Cause Subgroup`=="Ischemic heart disease") %>% 
  filter(`State Abbreviation`=="IL",
         Type=="Predicted (weighted)",
         Year=="2020") %>% 
  select(`Week Ending Date`,
         `Number of Deaths`,
         `Difference from 2015-2019 to 2020`) %>% 
  group_by(`Week Ending Date`) %>% 
  summarize_all(.funs = c(sum)) %>% 
  mutate(`Percent Difference from 2015-2019 to 2020`=round(`Difference from 2015-2019 to 2020`/`Number of Deaths`*100, digits = 1))


#Alright, so here's a more consise way to do disease comparison, by downloading more detailed provisional causes of death, then cleaning to include just IL:

CDC_death_by_cause_code_2020_2021_raw <- read_csv("https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD")

CDC_death_by_cause_codes_2020 <- CDC_death_by_cause_code_2020_2021_raw %>% 
  filter(`Jurisdiction of Occurrence`=="Illinois",
         `MMWR Year`==2020)

CDC_death_by_cause_code_2014_2019_raw <- read_csv("https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD")

CDC_death_by_cause_codes_2014_2019 <- CDC_death_by_cause_code_2014_2019_raw %>% 
  filter(`Jurisdiction of Occurrence`=="Illinois")

#Let's first take our older data and compute averages and upper bounds for each cause

IL_2014_2019_cause_avg <- CDC_death_by_cause_codes_2014_2019 %>% 
  select(`MMWR Week`,
         `All  Cause`, 
         `Natural Cause`,
         `Septicemia (A40-A41)`,
         `Malignant neoplasms (C00-C97)`,
         `Diabetes mellitus (E10-E14)`,
         `Alzheimer disease (G30)`,
         `Influenza and pneumonia (J10-J18)`,
         `Chronic lower respiratory diseases (J40-J47)`,
         `Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)`,
         `Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)`,
         `Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)`,
         `Diseases of heart (I00-I09,I11,I13,I20-I51)`,
         `Cerebrovascular diseases (I60-I69)`) %>% 
  group_by(`MMWR Week`) %>% 
  summarize_all(.funs = c(mean="mean",sd="sd")) %>% 
  mutate(`All  Cause_upper`=round((1.96*(`All  Cause_sd`/sqrt(6)))+`All  Cause_mean`,digits=1),
         `Natural Cause_upper`=round((1.96*(`Natural Cause_sd`/sqrt(6)))+`Natural Cause_mean`,digits=1),
         `Septicemia (A40-A41)_upper`=round((1.96*(`Septicemia (A40-A41)_sd`/sqrt(6)))+`Septicemia (A40-A41)_mean`,digits=1),
         `Malignant neoplasms (C00-C97)_upper`=round((1.96*(`Malignant neoplasms (C00-C97)_sd`/sqrt(6)))+`Malignant neoplasms (C00-C97)_mean`,digits=1),
         `Diabetes mellitus (E10-E14)_upper`=round((1.96*(`Diabetes mellitus (E10-E14)_sd`/sqrt(6)))+`Diabetes mellitus (E10-E14)_mean`,digits=1),
         `Alzheimer disease (G30)_upper`=round((1.96*(`Alzheimer disease (G30)_sd`/sqrt(6)))+`Alzheimer disease (G30)_mean`,digits=1),
         `Influenza and pneumonia (J10-J18)_upper`=round((1.96*(`Influenza and pneumonia (J10-J18)_sd`/sqrt(6)))+`Influenza and pneumonia (J10-J18)_mean`,digits=1),
         `Chronic lower respiratory diseases (J40-J47)_upper`=round((1.96*(`Chronic lower respiratory diseases (J40-J47)_sd`/sqrt(6)))+`Chronic lower respiratory diseases (J40-J47)_mean`,digits=1),
         `Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)_upper`=round((1.96*(`Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)_sd`/sqrt(6)))+`Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)_mean`,digits=1),
         `Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)_upper`=round((1.96*(`Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)_sd`/sqrt(6)))+`Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)_mean`,digits=1),
         `Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)_upper`=round((1.96*(`Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)_sd`/sqrt(6)))+`Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)_mean`,digits=1),
         `Diseases of heart (I00-I09,I11,I13,I20-I51)_upper`=round((1.96*(`Diseases of heart (I00-I09,I11,I13,I20-I51)_sd`/sqrt(6)))+`Diseases of heart (I00-I09,I11,I13,I20-I51)_mean`,digits=1),
         `Cerebrovascular diseases (I60-I69)_upper`=round((1.96*(`Cerebrovascular diseases (I60-I69)_sd`/sqrt(6)))+`Cerebrovascular diseases (I60-I69)_mean`),digits=1)

#Now we can take merge our current year...

CDC_IL_cause_compare <- merge(CDC_death_by_cause_codes_2020,
                              IL_2014_2019_cause_avg,
                              by="MMWR Week") %>% 
  mutate(`All Cause_above_avg`=round(`All Cause`-`All  Cause_mean`,digits=1),
         `Natural Cause_above_avg`=round(`Natural Cause`-`Natural Cause_mean`,digits=1),
         `Septicemia (A40-A41)_above_avg`=round(`Septicemia (A40-A41)`-`Septicemia (A40-A41)_mean`,digits=1),
         `Malignant neoplasms (C00-C97)_above_avg`=round(`Malignant neoplasms (C00-C97)`-`Malignant neoplasms (C00-C97)_mean`,digits=1),
         `Diabetes mellitus (E10-E14)_above_avg`=round(`Diabetes mellitus (E10-E14)`-`Diabetes mellitus (E10-E14)_mean`,digits=1),
         `Alzheimer disease (G30)_above_avg`=round(`Alzheimer disease (G30)`-`Alzheimer disease (G30)_mean`,digits=1),
         `Influenza and pneumonia (J10-J18)_above_avg`=round(`Influenza and pneumonia (J09-J18)`-`Influenza and pneumonia (J10-J18)_mean`,digits=1),
         `Chronic lower respiratory diseases (J40-J47)_above_avg`=round(`Chronic lower respiratory diseases (J40-J47)`-`Chronic lower respiratory diseases (J40-J47)_mean`,digits=1),
         `Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)_above_avg`=round(`Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)`-`Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)_mean`,digits=1),
         `Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)_above_avg`=round(`Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)`-`Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)_mean`,digits=1),
         `Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)_above_avg`=round(`Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)`-`Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)_mean`,digits=1),
         `Diseases of heart (I00-I09,I11,I13,I20-I51)_above_avg`=round(`Diseases of heart (I00-I09,I11,I13,I20-I51)`-`Diseases of heart (I00-I09,I11,I13,I20-I51)_mean`,digits=1),
         `Cerebrovascular diseases (I60-I69)_above_avg`=round(`Cerebrovascular diseases (I60-I69)`-`Cerebrovascular diseases (I60-I69)_mean`,digits=1)) %>% 
  mutate(`All  Cause_percent_diff`=round(`All Cause_above_avg`/`All  Cause_mean`*100,digits=1),
         `Natural Cause_percent_diff`=round(`Natural Cause_above_avg`/`Natural Cause_mean`*100,digits=1),
         `Septicemia (A40-A41)_percent_diff`=round(`Septicemia (A40-A41)_above_avg`/`Septicemia (A40-A41)_mean`*100,digits=1),
         `Malignant neoplasms (C00-C97)_percent_diff`=round(`Malignant neoplasms (C00-C97)_above_avg`/`Malignant neoplasms (C00-C97)_mean`*100,digits=1),
         `Diabetes mellitus (E10-E14)_percent_diff`=round(`Diabetes mellitus (E10-E14)_above_avg`/`Diabetes mellitus (E10-E14)_mean`*100,digits=1),
         `Alzheimer disease (G30)_percent_diff`=round(`Alzheimer disease (G30)_above_avg`/`Alzheimer disease (G30)_mean`*100,digits=1),
         `Influenza and pneumonia (J10-J18)_percent_diff`=round(`Influenza and pneumonia (J10-J18)_above_avg`/`Influenza and pneumonia (J10-J18)_mean`*100,digits=1),
         `Chronic lower respiratory diseases (J40-J47)_percent_diff`=round(`Chronic lower respiratory diseases (J40-J47)_above_avg`/`Chronic lower respiratory diseases (J40-J47)_mean`*100,digits=1),
         `Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)_percent_diff`=round(`Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)_above_avg`/`Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)_mean`*100,digits=1),
         `Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)_percent_diff`=round(`Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)_above_avg`/`Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)_mean`*100,digits=1),
         `Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)_percent_diff`=round(`Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)_above_avg`/`Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)_mean`*100,digits=1),
         `Diseases of heart (I00-I09,I11,I13,I20-I51)_percent_diff`=round(`Diseases of heart (I00-I09,I11,I13,I20-I51)_above_avg`/`Diseases of heart (I00-I09,I11,I13,I20-I51)_mean`*100,digits=1),
         `Cerebrovascular diseases (I60-I69)_percent_diff`=round(`Cerebrovascular diseases (I60-I69)_above_avg`/`Cerebrovascular diseases (I60-I69)_mean`*100,digits=1))
         
  

CDC_IL_cause_compare$`All  Cause_mean`<-round(CDC_IL_cause_compare$`All  Cause_mean`,digits=1)
CDC_IL_cause_compare$`Natural Cause_mean`<-round(CDC_IL_cause_compare$`Natural Cause_mean`,digits=1)
CDC_IL_cause_compare$`Septicemia (A40-A41)_mean`<-round(CDC_IL_cause_compare$`Septicemia (A40-A41)_mean`,digits=1)
CDC_IL_cause_compare$`Malignant neoplasms (C00-C97)_mean`<-round(CDC_IL_cause_compare$`Malignant neoplasms (C00-C97)_mean`,digits=1)
CDC_IL_cause_compare$`Diabetes mellitus (E10-E14)_mean`<-round(CDC_IL_cause_compare$`Diabetes mellitus (E10-E14)_mean`,digits=1)
CDC_IL_cause_compare$`Alzheimer disease (G30)_mean`<-round(CDC_IL_cause_compare$`Alzheimer disease (G30)_mean`,digits=1)
CDC_IL_cause_compare$`Influenza and pneumonia (J10-J18)_mean`<-round(CDC_IL_cause_compare$`Influenza and pneumonia (J10-J18)_mean`,digits=1)
CDC_IL_cause_compare$`Chronic lower respiratory diseases (J40-J47)_mean`<-round(CDC_IL_cause_compare$`Chronic lower respiratory diseases (J40-J47)_mean`,digits=1)
CDC_IL_cause_compare$`Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)_mean`<-round(CDC_IL_cause_compare$`Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)_mean`,digits=1)
CDC_IL_cause_compare$`Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)_mean`<-round(CDC_IL_cause_compare$`Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)_mean`,digits=1)
CDC_IL_cause_compare$`Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)_mean`<-round(CDC_IL_cause_compare$`Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)_mean`,digits=1)
CDC_IL_cause_compare$`Diseases of heart (I00-I09,I11,I13,I20-I51)_mean`<-round(CDC_IL_cause_compare$`Diseases of heart (I00-I09,I11,I13,I20-I51)_mean`,digits=1)
CDC_IL_cause_compare$`Cerebrovascular diseases (I60-I69)_mean`<-round(CDC_IL_cause_compare$`Cerebrovascular diseases (I60-I69)_mean`,digits=1)

#Let's download this big xls for some analysis, then come back to figure out which causes we want to drill down on


#Based on the above look, let's narrow this down to five causes: flu/ alzheimers/ diabetes/ heart/ stroke

CDC_IL_flu <- CDC_IL_cause_compare %>% 
  select(`Week Ending Date`,
         starts_with("Influenza and"))

colnames(CDC_IL_flu)[2:7] <- c("Number", "Average","Standard_dev","Upper_threshold","Raw_dif","Percent_diff")

CDC_IL_flu <- CDC_IL_flu %>% 
  select (-Standard_dev,
          -Raw_dif,
          -Percent_diff) %>% 
  filter(`Week Ending Date`!="2021-01-02")



CDC_IL_Alzheimers <- CDC_IL_cause_compare %>% 
  select(`Week Ending Date`,
         starts_with("Alzheimer"))

colnames(CDC_IL_Alzheimers)[2:7] <- c("Number", "Average","Standard_dev","Upper_threshold","Raw_dif","Percent_diff")

CDC_IL_Alzheimers <- CDC_IL_Alzheimers %>% 
  select (-Standard_dev,
          -Raw_dif,
          -Percent_diff) %>% 
  filter(`Week Ending Date`!="2021-01-02")



CDC_IL_heart <- CDC_IL_cause_compare %>% 
  select(`Week Ending Date`,
         starts_with("Diseases of heart"))

colnames(CDC_IL_heart)[2:7] <- c("Number", "Average","Standard_dev","Upper_threshold","Raw_dif","Percent_diff")

CDC_IL_heart <- CDC_IL_heart %>% 
  select (-Standard_dev,
          -Raw_dif,
          -Percent_diff) %>% 
  filter(`Week Ending Date`!="2021-01-02")



CDC_IL_diabetes <- CDC_IL_cause_compare %>% 
  select(`Week Ending Date`,
         starts_with("Diabetes"))

colnames(CDC_IL_diabetes)[2:7] <- c("Number", "Average","Standard_dev","Upper_threshold","Raw_dif","Percent_diff")

CDC_IL_diabetes <- CDC_IL_diabetes %>% 
  select (-Standard_dev,
          -Raw_dif,
          -Percent_diff) %>% 
  filter(`Week Ending Date`!="2021-01-02")



CDC_IL_stroke <- CDC_IL_cause_compare %>% 
  select(`Week Ending Date`,
         starts_with("Cerebrovascular"))

colnames(CDC_IL_stroke)[2:7] <- c("Number", "Average","Standard_dev","Upper_threshold","Raw_dif","Percent_diff")

CDC_IL_stroke <- CDC_IL_stroke %>% 
  select (-Standard_dev,
          -Raw_dif,
          -Percent_diff) %>% 
  filter(`Week Ending Date`!="2021-01-02")



#One final thing. Let's compare Illinois to the rest of the country for the weekly totals

All_state_combined_weekly <- All_states_weekly_totals %>% 
  filter(State!="North Carolina") %>% 
  group_by(week_ending) %>% 
  summarize(average=sum(average),
            weighted_above_avg=sum(weighted_above_avg)) %>% 
  mutate(US_states_percent_above_avg = round(weighted_above_avg/average*100,digits=1))

US_weekly_excess_weighted <- filter(CDC_weekly_by_state,State=="United States", Type=="Predicted (weighted)",Outcome=="All causes") %>% 
  filter(`Week Ending Date` >= "2020-03-07" & `Week Ending Date` <= "2021-01-02") %>% 
  arrange(`Week Ending Date`) %>% 
  mutate(weighted_observed=`Observed Number`,
         US_percent_difference=round((weighted_observed-`Average Expected Count`)/`Average Expected Count`*100, digits=1)) %>% 
  select(`Week Ending Date`,
         US_percent_difference)


IL_weekly_excess_weighted_2 <- IL_weekly_excess_weighted %>% 
  filter(`Week Ending Date` >= "2020-03-07" & `Week Ending Date` <= "2021-01-02") %>% 
  mutate(IL_percent_difference=round((weighted_observed-`Average Expected Count`)/`Average Expected Count`*100, digits=1)) %>% 
  select(`Week Ending Date`,
         IL_percent_difference)
  
IL_vs_US_by_week <- merge(x=US_weekly_excess_weighted, y=IL_weekly_excess_weighted_2)



#So let's try one more thing - looking overall during this 10 months at percetn incrase in deaths by age group.

IL_by_age_overall <- CDC_by_age_2 %>% 
  group_by(`Age Group`) %>% 
  summarize(March_Dec_2020=sum(`Number of Deaths`),
            March_Dec_average=round(sum(average_deaths),digits=1)) %>% 
  mutate(Percent_dif=round((March_Dec_2020-March_Dec_average)/March_Dec_average*100, digits=1))



#And let's try the same thing by race/ ethnicity

CDC_by_race3 <- CDC_by_race %>% filter(`State Abbreviation`=="IL",
                                       MMWRYear=="2020",
                                       Type=="Predicted (weighted)",
                                       Outcome=="All Cause",
                                       MMWRWeek>9,
                                       # !is.na(`Percent Difference from 2015-2019 to 2020`),
                                       `Race/Ethnicity`!="Other",
                                       `Race/Ethnicity`!="Non-Hispanic American Indian or Alaska Native") %>% 
  group_by(`Race/Ethnicity`) %>% 
  summarize(March_Dec_2020=sum(`Number of Deaths`),
            March_Dec_ave_dif=round(sum(`Difference from 2015-2019 to 2020`),digits=1)) %>%
  mutate(Percent_dif=round(March_Dec_ave_dif/(March_Dec_2020-March_Dec_ave_dif)*100, digits=1))



#Now  let's look at what's going on in 2021:

IL_excess_COVID_non_COVID_look_2020_2021 <- merge(x=IL_excess_look, y=IL_weekly_non_COVID_weighted, by.x = "week_ending", by.y = "Week Ending Date", all.x = TRUE) %>% mutate(weighted_CDC_COVID=weighted_observed-weighted_non_COVID) %>% 
  select(week_ending,
         weighted_non_COVID,
         weighted_CDC_COVID,
         average,
         upper_bound)

