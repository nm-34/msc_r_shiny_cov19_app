library(shiny)
library(shinydashboard)
library(plotly)
library(COVID19)
library(tidyverse)
library(tidyr)
library(tidytext)
library(tibbletime)
library(ggplot2)
library(dplyr)
library(dbplyr)
library(purrr)
library(data.table)
library(lubridate)
library(maps)
library(broom)
library(magrittr)
library(rvest)
library(RColorBrewer)
library(ggiraph)
library (RCurl)
library(leaflet)


# Loading Data for source comparison ----

# WHO Data
download_who <- url("https://covid19.who.int/WHO-COVID-19-global-data.csv")
who_csv <- read_csv(download_who, col_names = TRUE)

# WHO daily values

who_new_case_daily_sum <- who_csv %>%
    group_by(Date_reported) %>%
    transmute(global_daily_new_cases = sum(New_cases)) %>%
    ungroup()

who_daily_new_case_summary <- summary(who_new_case_daily_sum$global_daily_new_cases)

who_daily_new_case_sd <- sd(who_new_case_daily_sum$global_daily_new_cases)

who_new_death_daily_sum <- who_csv %>%
    group_by(Date_reported) %>%
    transmute(global_daily_new_deaths = sum(New_deaths)) %>%
    ungroup()

who_daily_new_death_summary <- summary(who_new_death_daily_sum$global_daily_new_deaths)

who_daily_new_death_sd <- sd(who_new_death_daily_sum$global_daily_new_deaths)

# WHO weekly values 

who_csv_weeks <- who_csv %>%
    group_by(Date_reported)%>%
    mutate(week_number = c(week(Date_reported)))%>%
    ungroup()



who_csv_weeks_cases <- who_csv_weeks %>%
    group_by(week_number) %>%
    summarise(global_weekly_new_cases = sum(New_cases)) %>%
    ungroup()

who_weekly_new_case_summary <- summary(who_csv_weeks_cases$global_weekly_new_cases)

who_weekly_new_case_sd <- sd(who_csv_weeks_cases$global_weekly_new_cases)


who_csv_weeks_deaths <- who_csv_weeks %>%
    group_by(week_number) %>%
    summarise(global_weekly_new_deaths = sum(New_deaths)) %>%
    ungroup()

who_weekly_new_death_summary <- summary(who_csv_weeks_deaths$global_weekly_new_deaths)

who_weekly_new_death_sd <- sd(who_csv_weeks_deaths$global_weekly_new_deaths)



# ECDC Data

ecdc_csv <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                     na.strings = "", fileEncoding = "UTF-8-BOM")


ecdc_csv$dateRep <- as.Date(ecdc_csv$dateRep, format = "%d/%m/%y")



# Daily ECDC values
ecdc_new_case_daily_sum <- ecdc_csv %>%
    group_by(dateRep) %>%
    transmute(global_daily_new_cases = sum(cases)) %>%
    ungroup()

ecdc_daily_new_case_summary <- summary(ecdc_new_case_daily_sum$global_daily_new_cases)

ecdc_daily_new_case_sd <- sd(ecdc_new_case_daily_sum$global_daily_new_cases)


ecdc_new_death_daily_sum <- ecdc_csv %>%
    group_by(dateRep) %>%
    transmute(global_daily_new_deaths = sum(deaths)) %>%
    ungroup()

ecdc_daily_new_death_summary <- summary(ecdc_new_death_daily_sum$global_daily_new_deaths)

ecdc_daily_new_death_sd <- sd(ecdc_new_death_daily_sum$global_daily_new_deaths)

# Weekly 

ecdc_csv_weeks <- ecdc_csv %>%
    group_by(dateRep)%>%
    mutate(week_number = c(week(dateRep)))%>%
    ungroup()

ecdc_csv_weeks_cases <- ecdc_csv_weeks %>%
    group_by(week_number) %>%
    summarise(global_weekly_new_cases = sum(cases)) %>%
    ungroup()

ecdc_weekly_new_case_summary <- summary(ecdc_csv_weeks_cases$global_weekly_new_cases)

ecdc_weekly_new_case_sd <- sd(ecdc_csv_weeks_cases$global_weekly_new_cases)


ecdc_csv_weeks_deaths <- ecdc_csv_weeks %>%
    group_by(week_number) %>%
    summarise(global_weekly_new_deaths = sum(deaths)) %>%
    ungroup()

ecdc_weekly_new_death_summary <- summary(ecdc_csv_weeks_deaths$global_weekly_new_deaths)
ecdc_weekly_new_death_sd <- sd(ecdc_csv_weeks_deaths$global_weekly_new_deaths)

# JHU Data
# Confirmed Cases and Deaths published separately by JHU
# Cases
download_jhu <- url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
jhu_cases_csv <- read_csv(download_jhu, col_names = TRUE)


jhu_cases_csv <- pivot_longer(jhu_cases_csv, cols = 5:ncol(jhu_cases_csv), names_to = "date_reported")



jhu_cases_csv$date_reported <- as.Date(jhu_cases_csv$date_reported, format = "%m/%d/%y")


jhu_cases_csv <- jhu_cases_csv %>%
    group_by(`Country/Region`) %>%
    mutate(new_by_day = c(0, diff(value)))%>%
    ungroup()

# Daily

jhu_new_case_daily_sum <- jhu_cases_csv %>%
    group_by(date_reported) %>%
    transmute(global_daily_new_cases = sum(new_by_day)) %>%
    ungroup()

jhu_daily_new_case_summary <- summary(jhu_new_case_daily_sum$global_daily_new_cases)

jhu_daily_new_case_sd <- sd(jhu_new_case_daily_sum$global_daily_new_cases)

# Weekly

jhu_cases_csv_weeks <- jhu_cases_csv %>%
    group_by(date_reported)%>%
    mutate(week_number = c(week(date_reported)))%>%
    ungroup()

jhu_cases_csv_weeks <- jhu_cases_csv_weeks %>%
    group_by(week_number) %>%
    summarise(global_weekly_new_cases = sum(new_by_day)) %>%
    ungroup()

jhu_weekly_new_case_summary <- summary(jhu_cases_csv_weeks$global_weekly_new_cases)

jhu_weekly_new_case_sd <- sd(jhu_cases_csv_weeks$global_weekly_new_cases)


# Deaths
download_jhu_deaths <- url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
jhu_deaths_csv <- read_csv(download_jhu_deaths, col_names = TRUE)


jhu_deaths_csv <- pivot_longer(jhu_deaths_csv, cols = 5:ncol(jhu_deaths_csv), names_to = "date_reported")


jhu_deaths_csv$date_reported <- as.Date(jhu_deaths_csv$date_reported, format = "%m/%d/%y")

# Daily

jhu_deaths_csv <- jhu_deaths_csv %>%
    group_by(`Country/Region`) %>%
    mutate(new_by_day = c(0, diff(value)))%>%
    ungroup()

jhu_new_death_daily_sum <- jhu_deaths_csv %>%
    group_by(date_reported) %>%
    transmute(global_daily_new_deaths = sum(new_by_day)) %>%
    ungroup()

jhu_daily_new_death_summary <- summary(jhu_new_death_daily_sum$global_daily_new_deaths)

jhu_daily_new_death_sd <- sd(jhu_new_death_daily_sum$global_daily_new_deaths)


# Weekly

jhu_deaths_csv_weeks <- jhu_deaths_csv %>%
    group_by(date_reported)%>%
    mutate(week_number = c(week(date_reported)))%>%
    ungroup()

jhu_deaths_csv_weeks <- jhu_deaths_csv_weeks %>%
    group_by(week_number) %>%
    summarise(global_weekly_new_deaths = sum(new_by_day)) %>%
    ungroup()

jhu_weekly_new_death_summary <- summary(jhu_deaths_csv_weeks$global_weekly_new_deaths)

jhu_weekly_new_death_sd <- sd(jhu_deaths_csv_weeks$global_weekly_new_deaths)

# Comparison stats

sd_daily_cases_comp <- as.data.frame(c(who_daily_new_case_sd, ecdc_daily_new_case_sd, jhu_daily_new_case_sd))

sd_daily_cases_comp$source <- c("WHO", "ECDC", "JHU")

names(sd_daily_cases_comp)[names(sd_daily_cases_comp)== "c(who_daily_new_case_sd, ecdc_daily_new_case_sd, jhu_daily_new_case_sd)"] <- "daily_case_SD"

sd_daily_cases_comp <- sd_daily_cases_comp[,c(2, 1)]

sd_daily_deaths_comp <- as.data.frame(c(who_daily_new_death_sd, ecdc_daily_new_death_sd, jhu_daily_new_death_sd))
sd_daily_deaths_comp$source <- c("WHO", "ECDC", "JHU")
names(sd_daily_deaths_comp)[names(sd_daily_deaths_comp)== "c(who_daily_new_death_sd, ecdc_daily_new_death_sd, jhu_daily_new_death_sd)"] <- "daily_death_SD"
sd_daily_deaths_comp <- sd_daily_deaths_comp[,c(2, 1)]

sd_weekly_cases_comp <- as.data.frame(c(who_weekly_new_case_sd, ecdc_weekly_new_case_sd, jhu_weekly_new_case_sd))
sd_weekly_cases_comp$source <- c("WHO", "ECDC", "JHU")
names(sd_weekly_cases_comp)[names(sd_weekly_cases_comp)== "c(who_weekly_new_case_sd, ecdc_weekly_new_case_sd, jhu_weekly_new_case_sd)"] <- "weekly_case_sd"
sd_weekly_cases_comp <- sd_weekly_cases_comp[,c(2, 1)]

sd_weekly_deaths_comp <- as.data.frame(c(who_weekly_new_death_sd, ecdc_weekly_new_death_sd, jhu_weekly_new_death_sd))
sd_weekly_deaths_comp$source <- c("WHO", "ECDC", "JHU")
names(sd_weekly_deaths_comp)[names(sd_weekly_deaths_comp)=="c(who_weekly_new_death_sd, ecdc_weekly_new_death_sd, jhu_weekly_new_death_sd)"] <- "weekly_death_sd"
sd_weekly_cases_comp <- sd_weekly_cases_comp[,c(2, 1)]


source_global_case_totals_comp <- as.data.frame(c(sum(who_csv_weeks_cases$global_weekly_new_cases), 
                                                  sum(ecdc_csv_weeks_cases$global_weekly_new_cases),
                                                  sum(jhu_cases_csv_weeks$global_weekly_new_cases)))

source_global_case_totals_comp$source <- c("WHO", "ECDC", "JHU") 

source_global_case_totals_comp <- source_global_case_totals_comp[,c(2, 1)]


names(source_global_case_totals_comp)[names(source_global_case_totals_comp)== "c(sum(who_csv_weeks_cases$global_weekly_new_cases), sum(ecdc_csv_weeks_cases$global_weekly_new_cases), sum(jhu_cases_csv_weeks$global_weekly_new_cases))"] <- "case_count"

source_global_death_totals_comp <- as.data.frame(c(sum(who_csv_weeks_deaths$global_weekly_new_deaths), 
                                                   sum(ecdc_csv_weeks_deaths$global_weekly_new_deaths),
                                                   sum(jhu_deaths_csv_weeks$global_weekly_new_deaths)))


source_global_death_totals_comp$source <- c("WHO", "ECDC", "JHU")

source_global_death_totals_comp <- source_global_death_totals_comp[,c(2, 1)]

names(source_global_death_totals_comp)[names(source_global_death_totals_comp)== "c(sum(who_csv_weeks_deaths$global_weekly_new_deaths), sum(ecdc_csv_weeks_deaths$global_weekly_new_deaths), sum(jhu_deaths_csv_weeks$global_weekly_new_deaths))"] <- "death_count"

source_global_totals_comp <- merge(source_global_case_totals_comp, source_global_death_totals_comp)

source_global_daily_case_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 3)), c("Minimum", 
                                                                                       "1st Quartile", 
                                                                                       "Median", 
                                                                                       "Mean", 
                                                                                       "3rd Quartile", 
                                                                                       "Max"))

source_global_daily_case_summary[1,] <- who_daily_new_case_summary 
source_global_daily_case_summary[2,] <- ecdc_daily_new_case_summary 
source_global_daily_case_summary[3,] <- jhu_daily_new_case_summary 

source_global_daily_case_summary$source <- c("WHO", "ECDC", "JHU")

source_global_daily_case_summary <- source_global_daily_case_summary[,c(7, 1:6)]

source_global_daily_death_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 3)), c("Minimum", 
                                                                                        "1st Quartile", 
                                                                                        "Median", 
                                                                                        "Mean", 
                                                                                        "3rd Quartile", 
                                                                                        "Max"))

source_global_daily_death_summary[1,] <- who_daily_new_death_summary 
source_global_daily_death_summary[2,] <- ecdc_daily_new_death_summary 
source_global_daily_death_summary[3,] <- jhu_daily_new_death_summary 

source_global_daily_death_summary$source <- c("WHO", "ECDC", "JHU")
source_global_daily_death_summary <- source_global_daily_death_summary[,c(7, 1:6)]


source_global_weekly_case_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 3)), c("Minimum", 
                                                                                        "1st Quartile", 
                                                                                        "Median", 
                                                                                        "Mean", 
                                                                                        "3rd Quartile", 
                                                                                        "Max"))

source_global_weekly_case_summary[1,] <- who_weekly_new_case_summary 
source_global_weekly_case_summary[2,] <- ecdc_weekly_new_case_summary 
source_global_weekly_case_summary[3,] <- jhu_weekly_new_case_summary

source_global_weekly_case_summary$source <- c("WHO", "ECDC", "JHU")
source_global_weekly_case_summary <- source_global_weekly_case_summary[,c(7, 1:6)]

source_global_weekly_death_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 3)), c("Minimum", 
                                                                                         "1st Quartile", 
                                                                                         "Median", 
                                                                                         "Mean", 
                                                                                         "3rd Quartile", 
                                                                                         "Max"))

source_global_weekly_death_summary[1,] <- who_weekly_new_death_summary 
source_global_weekly_death_summary[2,] <- ecdc_weekly_new_death_summary 
source_global_weekly_death_summary[3,] <- jhu_weekly_new_death_summary

source_global_weekly_death_summary$source <- c("WHO", "ECDC", "JHU")
source_global_weekly_death_summary <- source_global_weekly_death_summary[,c(7, 1:6)]


source_global_timeseries_comp <- who_new_case_daily_sum
names(source_global_timeseries_comp)[names(source_global_timeseries_comp)=="global_daily_new_cases"] <- "WHO_cases"



source_global_timeseries_comp$WHO_deaths <- who_new_death_daily_sum$global_daily_new_deaths 

source_global_timeseries_comp$ECDC_cases <- ecdc_new_case_daily_sum$global_daily_new_cases[match(source_global_timeseries_comp$Date_reported,
                                                                                                 ecdc_new_case_daily_sum$dateRep)]

source_global_timeseries_comp$ECDC_deaths <- ecdc_new_death_daily_sum$global_daily_new_deaths[match(source_global_timeseries_comp$Date_reported,
                                                                                                    ecdc_new_death_daily_sum$dateRep)]


source_global_timeseries_comp$JHU_cases <- jhu_new_case_daily_sum$global_daily_new_cases[match(source_global_timeseries_comp$Date_reported,
                                                                                               jhu_new_case_daily_sum$date_reported)]

source_global_timeseries_comp$JHU_deaths <- jhu_new_death_daily_sum$global_daily_new_deaths[match(source_global_timeseries_comp$Date_reported,
                                                                                                  jhu_new_death_daily_sum$date_reported)]



# creating dataframe of covid19() package data ----
x_worldwide <- covid19()

# altering names to match further datasets

names(x_worldwide)[names(x_worldwide) == "id"] <- "country_id"
names(x_worldwide)[names(x_worldwide) == "iso_alpha_3"] <- "ISO3"

# Altering scientific numbering to make plots more readable to the average user

options(scipen = 999)


# loading population density data----

pop_density <- read_csv("www/world_bank_pop_density_2018.csv")

# Matching pop density data to x_worldwide countries and adding as column

x_worldwide["pop_density_km2"] <- pop_density$`2018 [YR2018]`[match(x_worldwide$ISO3,
                                                                    pop_density$`Country Code`)]


x_worldwide$pop_density_km2 <- suppressWarnings(as.numeric(x_worldwide$pop_density_km2),)
x_worldwide$pop_density_km2 <- round(x_worldwide$pop_density_km2, 2)


# Adding daily values columns for time series plots ----
x_worldwide_with_dailies <- x_worldwide %>%
    group_by(country_id)%>%   
    mutate(new_cases = c(0, diff(confirmed)), new_deaths = c(0, diff(deaths)), 
           new_recovered= c(0, diff(recovered))) %>%
    ungroup()

x_dailies_long <- pivot_longer(x_worldwide_with_dailies, cols = 37:39, names_to = "new_caseType",
                               values_to = "new_value")

global_daily_figures <- x_dailies_long %>%
    group_by(date, new_caseType) %>%
    summarise(global_by_day = sum(new_value)) %>%
    ungroup()

global_daily_figures <- pivot_wider(global_daily_figures, 
                                    names_from = "new_caseType", values_from = "global_by_day")



# Creating longform x_worldwide_dailies df for map plots----

x_worldwide_long <- pivot_longer(x_worldwide, cols = 3:6, names_to = "caseType",
                                 values_to = "Value")


# Loading excess deaths data from Human Mortality Database----

download <- getURL("https://www.mortality.org/Public/STMF/Outputs/stmf.csv")
human_mortality_db <- read_csv(download, skip = 2)

human_mortality_db$CountryCode[human_mortality_db$CountryCode=="DEUTNP"] <- "DEU"

human_mortality_db$CountryCode[human_mortality_db$CountryCode=="FRATNP"] <- "FRA"

human_mortality_GBR_fix <- human_mortality_db

human_mortality_GBR_fix$CountryCode[human_mortality_GBR_fix$CountryCode=="GBR_NIR"] <- "GBR"
human_mortality_GBR_fix$CountryCode[human_mortality_GBR_fix$CountryCode=="GBR_SCO"] <- "GBR"
human_mortality_GBR_fix$CountryCode[human_mortality_GBR_fix$CountryCode=="GBRTENW"] <- "GBR"

human_mortality_GBR_fix <- human_mortality_GBR_fix %>%
    filter(CountryCode=="GBR" & Sex=="b") %>%
    group_by(Year, Week, CountryCode) %>%
    summarise(DTotal = sum(DTotal)) %>%
    ungroup()


gb_nir_week_trim <- max(human_mortality_db$Week[human_mortality_db$Year==2020 & 
                                                    human_mortality_db$CountryCode=="GBR_NIR"])


# 2020 subset
excess_death_2020 <- subset(human_mortality_db, Sex=="b") %>%
    subset(Year==2020)

excess_death_2020["date"] <- as.Date(paste(2020, 
                                           excess_death_2020$Week, 
                                           1, sep="-"), "%Y-%U-%u")

# GB excess
gb_excess_2020 <- subset(human_mortality_GBR_fix, Year==2020 & Week<=gb_nir_week_trim)


gb_excess_2020["date"] <- as.Date(paste(2020, 
                                        gb_excess_2020$Week, 
                                        1, sep="-"), "%Y-%U-%u")


# 5 year average subset

excess_death_2015_2019_average <- subset(human_mortality_db, Year>=2015 & Year<=2019) %>%
    group_by(CountryCode, Week) %>%
    mutate(avg_deaths = mean(DTotal[Sex=="b"])) %>%
    ungroup()

excess_death_2015_2019_average["date"] <- as.Date(paste(2020, 
                                                        excess_death_2015_2019_average$Week, 
                                                        1, sep="-"), "%Y-%U-%u")

gb_average <- subset(human_mortality_GBR_fix, Year>=2015 & Year<=2019) %>%
    group_by(Week) %>%
    mutate(avg_deaths = mean(DTotal))

gb_average["date"] <- as.Date(paste(2020, 
                                    gb_average$Week, 
                                    1, sep="-"), "%Y-%U-%u")

# Joining excess deaths columns to data from Covid19 package----

x_worldwide_with_weeks <- x_worldwide_with_dailies %>%
    group_by(date)%>%
    mutate(week_number = c(week(date)))%>%
    ungroup() 

x_worldwide_weekly_death_values <- x_worldwide_with_weeks%>%
    group_by(ISO3, administrative_area_level_1, population, 
             pop_density_km2, week_number)%>%
    mutate(weekly_deaths = sum(new_deaths))%>%
    ungroup()

x_worldwide_weekly_death_values["date"] <- as.Date(paste(2020, 
                                                         x_worldwide_weekly_death_values$week_number, 
                                                         1, sep="-"), "%Y-%U-%u")

names(excess_death_2020)[names(excess_death_2020) == "CountryCode"] <- "ISO3"
names(excess_death_2020)[names(excess_death_2020) == "DTotal"] <- "excess_2020"
names(gb_excess_2020)[names(gb_excess_2020) == "CountryCode"] <- "ISO3"
names(gb_excess_2020)[names(gb_excess_2020) == "DTotal"] <- "excess_2020"

names(excess_death_2015_2019_average)[names(excess_death_2015_2019_average) == "CountryCode"] <- "ISO3"



x_worldwide_weekly_death_values<- x_worldwide_weekly_death_values%>%
    left_join(excess_death_2020 %>% 
                  select(excess_2020, ISO3, date), 
              by=c("ISO3", "date"))



x_worldwide_weekly_death_values$excess_2020[
    x_worldwide_weekly_death_values$ISO3=="GBR"] <- gb_excess_2020$excess_2020[match(x_worldwide_weekly_death_values$date,
                                                                                     gb_excess_2020$date)]


x_worldwide_weekly_death_values <- x_worldwide_weekly_death_values%>%
    left_join(excess_death_2015_2019_average %>%
                  select(ISO3, date, avg_deaths), 
              by=c("ISO3", "date"))

x_worldwide_weekly_death_values$avg_deaths[
    x_worldwide_weekly_death_values$ISO3=="GBR"] <- gb_average$avg_deaths[match(x_worldwide_weekly_death_values$date,
                                                                                gb_average$date)]

# Setting date values in line with most recent update data in Covid19() package----

date_set <- tail(x_worldwide$date, n=1)
week_set <- tail(week(x_worldwide$date), n=1)
x_worldwide_min_date = as.Date(min(x_worldwide$date))

most_recent_totals <- subset(x_worldwide, date==date_set) 
most_recent_totals <- most_recent_totals[order(most_recent_totals$confirmed),]

# Creating variables for top 5 most effected countries to start UI with

top_5_countries_by_case_count <- tail(most_recent_totals, n=5)


# Setting panel values for dashboard info boxes
x_confirmed_panel <- sum(x_worldwide$confirmed[x_worldwide$date==date_set])


x_deaths_panel <-  sum(x_worldwide$deaths[x_worldwide$date==date_set])

x_recovered_panel <- sum(x_worldwide$recovered[x_worldwide$date==date_set]) 

# Creating population density subsets----

density_subset_upto_10_per_km2 <- subset(x_worldwide, pop_density_km2 <10 
                                         & date == date_set) 

density_subset_between_10_50_per_km2 <- subset(x_worldwide, pop_density_km2>=10 & pop_density_km2<50 
                                               & date == date_set)

density_subset_between_50_100_per_km2 <- subset(x_worldwide, pop_density_km2>=50 & pop_density_km2<100
                                                & date == date_set)

density_subset_between_100_150_per_km2 <- subset(x_worldwide, pop_density_km2>=100 & pop_density_km2<150 
                                                 & date == date_set)

density_subset_between_150_250_per_km2 <- subset(x_worldwide, pop_density_km2>=150 & pop_density_km2<250 
                                                 & date == date_set)

density_subset_between_250_500_per_km2 <- subset(x_worldwide, pop_density_km2>=250 & pop_density_km2<500 
                                                 & date == date_set)

density_subset_between_500_1000_per_km2 <- subset(x_worldwide, pop_density_km2>=500 & pop_density_km2<1000 
                                                  & date == date_set)

density_subset_excess_1000_per_km2 <- subset(x_worldwide, pop_density_km2>=1000 
                                             & date == date_set)


# Up to 10 per km2 data prep
dens_10_conf_summary_with_outliers <- summary(density_subset_upto_10_per_km2$confirmed)

confirmed_outliers <- boxplot(density_subset_upto_10_per_km2$confirmed)$out

outlier_checker<- density_subset_upto_10_per_km2

dens_10_km2_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
dens_10_conf_summary_without_outliers <- summary(dens_10_km2_without_confirmed_outliers$confirmed)

dens_10_km2_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

dens_10_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_10_confirmed_summary[1,] <- dens_10_conf_summary_with_outliers
dens_10_confirmed_summary[2,] <- dens_10_conf_summary_without_outliers 

dens_10_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
dens_10_confirmed_summary <- dens_10_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(density_subset_upto_10_per_km2$deaths)$out

dens_10_deaths_summary_with_outliers <- summary(density_subset_upto_10_per_km2$deaths)

outlier_checker<- density_subset_upto_10_per_km2

dens_10_km2_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

dens_10_deaths_summary_without_outliers <- summary(dens_10_km2_without_death_outliers$deaths)
dens_10_km2_death_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

dens_10_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_10_deaths_summary[1,] <- dens_10_deaths_summary_with_outliers
dens_10_deaths_summary[2,] <- dens_10_deaths_summary_without_outliers 

dens_10_deaths_summary$source <- c("With Outliers", "Outliers Removed")
dens_10_deaths_summary <- dens_10_deaths_summary[,c(7, 1:6)]



# 10-50perkm2 density group data prep

dens_10_50_conf_summary_with_outliers <- summary(density_subset_between_10_50_per_km2$confirmed)

confirmed_outliers <- boxplot(density_subset_between_10_50_per_km2$confirmed)$out

outlier_checker<- density_subset_between_10_50_per_km2

dens_10_50_km2_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
dens_10_50_conf_summary_without_outliers <- summary(dens_10_50_km2_without_confirmed_outliers$confirmed)

dens_10_50_km2_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

dens_10_50_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_10_50_confirmed_summary[1,] <- dens_10_50_conf_summary_with_outliers
dens_10_50_confirmed_summary[2,] <- dens_10_50_conf_summary_without_outliers 

dens_10_50_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
dens_10_50_confirmed_summary <- dens_10_50_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(density_subset_between_10_50_per_km2$deaths)$out

dens_10_50_deaths_summary_with_outliers <- summary(density_subset_between_10_50_per_km2$deaths)

outlier_checker<- density_subset_between_10_50_per_km2

dens_10_50_km2_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

dens_10_50_deaths_summary_without_outliers <- summary(dens_10_50_km2_without_death_outliers$deaths)
dens_10_50_km2_deaths_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

dens_10_50_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_10_50_deaths_summary[1,] <- dens_10_50_deaths_summary_with_outliers
dens_10_50_deaths_summary[2,] <- dens_10_50_deaths_summary_without_outliers 

dens_10_50_deaths_summary$source <- c("With Outliers", "Outliers Removed")
dens_10_50_deaths_summary <- dens_10_50_deaths_summary[,c(7, 1:6)]

# 50 - 100 density data prep

dens_50_100_conf_summary_with_outliers <- summary(density_subset_between_50_100_per_km2$confirmed)

confirmed_outliers <- boxplot(density_subset_between_50_100_per_km2$confirmed)$out

outlier_checker<- density_subset_between_50_100_per_km2

dens_50_100_km2_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
dens_50_100_conf_summary_without_outliers <- summary(dens_50_100_km2_without_confirmed_outliers$confirmed)

dens_50_100_km2_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

dens_50_100_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_50_100_confirmed_summary[1,] <- dens_50_100_conf_summary_with_outliers
dens_50_100_confirmed_summary[2,] <- dens_50_100_conf_summary_without_outliers 

dens_50_100_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
dens_50_100_confirmed_summary <- dens_50_100_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(density_subset_between_50_100_per_km2$deaths)$out

dens_50_100_deaths_summary_with_outliers <- summary(density_subset_between_50_100_per_km2$deaths)

outlier_checker<- density_subset_between_50_100_per_km2

dens_50_100_km2_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

dens_50_100_deaths_summary_without_outliers <- summary(dens_50_100_km2_without_death_outliers$deaths)
dens_50_100_km2_deaths_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

dens_50_100_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_50_100_deaths_summary[1,] <- dens_50_100_deaths_summary_with_outliers
dens_50_100_deaths_summary[2,] <- dens_50_100_deaths_summary_without_outliers 

dens_50_100_deaths_summary$source <- c("With Outliers", "Outliers Removed")
dens_50_100_deaths_summary <- dens_50_100_deaths_summary[,c(7, 1:6)]



# 100-150 per km2 density group data prep

dens_100_150_conf_summary_with_outliers <- summary(density_subset_between_100_150_per_km2$confirmed)

confirmed_outliers <- boxplot(density_subset_between_100_150_per_km2$confirmed)$out

outlier_checker<- density_subset_between_100_150_per_km2

dens_100_150_km2_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
dens_100_150_conf_summary_without_outliers <- summary(dens_100_150_km2_without_confirmed_outliers$confirmed)

dens_100_150_km2_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

dens_100_150_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_100_150_confirmed_summary[1,] <- dens_100_150_conf_summary_with_outliers
dens_100_150_confirmed_summary[2,] <- dens_100_150_conf_summary_without_outliers 

dens_100_150_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
dens_100_150_confirmed_summary <- dens_100_150_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(density_subset_between_100_150_per_km2$deaths)$out

dens_100_150_deaths_summary_with_outliers <- summary(density_subset_between_100_150_per_km2$deaths)

outlier_checker<- density_subset_between_100_150_per_km2

dens_100_150_km2_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

dens_100_150_deaths_summary_without_outliers <- summary(dens_100_150_km2_without_death_outliers$deaths)
dens_100_150_km2_deaths_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

dens_100_150_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_100_150_deaths_summary[1,] <- dens_100_150_deaths_summary_with_outliers
dens_100_150_deaths_summary[2,] <- dens_100_150_deaths_summary_without_outliers 

dens_100_150_deaths_summary$source <- c("With Outliers", "Outliers Removed")
dens_100_150_deaths_summary <- dens_100_150_deaths_summary[,c(7, 1:6)]


# 150-250 per km2 density group data prep

dens_150_250_conf_summary_with_outliers <- summary(density_subset_between_150_250_per_km2$confirmed)

confirmed_outliers <- boxplot(density_subset_between_150_250_per_km2$confirmed)$out

outlier_checker<- density_subset_between_150_250_per_km2

dens_150_250_km2_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
dens_150_250_conf_summary_without_outliers <- summary(dens_150_250_km2_without_confirmed_outliers$confirmed)

dens_150_250_km2_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

dens_150_250_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_150_250_confirmed_summary[1,] <- dens_150_250_conf_summary_with_outliers
dens_150_250_confirmed_summary[2,] <- dens_150_250_conf_summary_without_outliers 

dens_150_250_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
dens_150_250_confirmed_summary <- dens_150_250_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(density_subset_between_150_250_per_km2$deaths)$out

dens_150_250_deaths_summary_with_outliers <- summary(density_subset_between_150_250_per_km2$deaths)

outlier_checker<- density_subset_between_150_250_per_km2

dens_150_250_km2_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

dens_150_250_deaths_summary_without_outliers <- summary(dens_150_250_km2_without_death_outliers$deaths)
dens_150_250_km2_deaths_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

dens_150_250_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_150_250_deaths_summary[1,] <- dens_150_250_deaths_summary_with_outliers
dens_150_250_deaths_summary[2,] <- dens_150_250_deaths_summary_without_outliers 

dens_150_250_deaths_summary$source <- c("With Outliers", "Outliers Removed")
dens_150_250_deaths_summary <- dens_150_250_deaths_summary[,c(7, 1:6)]

# 250 - 500 per km2 data prep

dens_250_500_conf_summary_with_outliers <- summary(density_subset_between_250_500_per_km2$confirmed)

confirmed_outliers <- boxplot(density_subset_between_250_500_per_km2$confirmed)$out

outlier_checker<- density_subset_between_250_500_per_km2

dens_250_500_km2_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
dens_250_500_conf_summary_without_outliers <- summary(dens_250_500_km2_without_confirmed_outliers$confirmed)

dens_250_500_km2_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

dens_250_500_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_250_500_confirmed_summary[1,] <- dens_250_500_conf_summary_with_outliers
dens_250_500_confirmed_summary[2,] <- dens_250_500_conf_summary_without_outliers 

dens_250_500_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
dens_250_500_confirmed_summary <- dens_250_500_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(density_subset_between_250_500_per_km2$deaths)$out

dens_250_500_deaths_summary_with_outliers <- summary(density_subset_between_250_500_per_km2$deaths)

outlier_checker<- density_subset_between_250_500_per_km2

dens_250_500_km2_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

dens_250_500_deaths_summary_without_outliers <- summary(dens_250_500_km2_without_death_outliers$deaths)
dens_250_500_km2_deaths_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

dens_250_500_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_250_500_deaths_summary[1,] <- dens_250_500_deaths_summary_with_outliers
dens_250_500_deaths_summary[2,] <- dens_250_500_deaths_summary_without_outliers 

dens_250_500_deaths_summary$source <- c("With Outliers", "Outliers Removed")
dens_250_500_deaths_summary <- dens_250_500_deaths_summary[,c(7, 1:6)]

# 500 - 1000 per km2 data prep

dens_500_1000_conf_summary_with_outliers <- summary(density_subset_between_500_1000_per_km2$confirmed)

confirmed_outliers <- boxplot(density_subset_between_500_1000_per_km2$confirmed)$out

outlier_checker<- density_subset_between_500_1000_per_km2

dens_500_1000_km2_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
dens_500_1000_conf_summary_without_outliers <- summary(dens_500_1000_km2_without_confirmed_outliers$confirmed)

dens_500_1000_km2_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

dens_500_1000_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_500_1000_confirmed_summary[1,] <- dens_500_1000_conf_summary_with_outliers
dens_500_1000_confirmed_summary[2,] <- dens_500_1000_conf_summary_without_outliers 

dens_500_1000_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
dens_500_1000_confirmed_summary <- dens_500_1000_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(density_subset_between_500_1000_per_km2$deaths)$out

dens_500_1000_deaths_summary_with_outliers <- summary(density_subset_between_500_1000_per_km2$deaths)

outlier_checker<- density_subset_between_500_1000_per_km2

dens_500_1000_km2_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

dens_500_1000_deaths_summary_without_outliers <- summary(dens_500_1000_km2_without_death_outliers$deaths)
dens_500_1000_km2_deaths_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

dens_500_1000_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_500_1000_deaths_summary[1,] <- dens_500_1000_deaths_summary_with_outliers
dens_500_1000_deaths_summary[2,] <- dens_500_1000_deaths_summary_without_outliers 

dens_500_1000_deaths_summary$source <- c("With Outliers", "Outliers Removed")
dens_500_1000_deaths_summary <- dens_500_1000_deaths_summary[,c(7, 1:6)]

# 1000+ per km2 data prep

dens_1000_conf_summary_with_outliers <- summary(density_subset_excess_1000_per_km2$confirmed)

confirmed_outliers <- boxplot(density_subset_excess_1000_per_km2$confirmed)$out

outlier_checker<- density_subset_excess_1000_per_km2

dens_1000_km2_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
dens_1000_conf_summary_without_outliers <- summary(dens_1000_km2_without_confirmed_outliers$confirmed)

dens_1000_km2_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

dens_1000_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_1000_confirmed_summary[1,] <- dens_1000_conf_summary_with_outliers
dens_1000_confirmed_summary[2,] <- dens_1000_conf_summary_without_outliers 

dens_1000_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
dens_1000_confirmed_summary <- dens_1000_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(density_subset_excess_1000_per_km2$deaths)$out

dens_1000_deaths_summary_with_outliers <- summary(density_subset_excess_1000_per_km2$deaths)

outlier_checker<- density_subset_excess_1000_per_km2

dens_1000_km2_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

dens_1000_deaths_summary_without_outliers <- summary(dens_1000_km2_without_death_outliers$deaths)
dens_1000_km2_deaths_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

dens_1000_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

dens_1000_deaths_summary[1,] <- dens_1000_deaths_summary_with_outliers
dens_1000_deaths_summary[2,] <- dens_1000_deaths_summary_without_outliers 

dens_1000_deaths_summary$source <- c("With Outliers", "Outliers Removed")
dens_1000_deaths_summary <- dens_1000_deaths_summary[,c(7, 1:6)]


# Creating population subsets----
pop_subset_upto_1million <- subset(x_worldwide, population<1000000 
                                   & date == date_set) 

pop_subset_between_1_10million <- subset(x_worldwide, population>=1000000 & population<10000000 
                                         & date == date_set)

pop_subset_between_10_50million <- subset(x_worldwide, population>=10000000 & population<50000000 
                                          & date == date_set)

pop_subset_between_50_100million <- subset(x_worldwide, population>=50000000 & population<100000000 
                                           & date == date_set)

pop_subset_between_100_250million <- subset(x_worldwide, population>=100000000 & population<250000000 
                                            & date == date_set)

pop_subset_between_250_1000million <- subset(x_worldwide, population>=250000000 & population<1000000000 
                                             & date == date_set)

pop_subset_excess_1000million <- subset(x_worldwide, population>=1000000000 
                                        & date == date_set)

# Up to 1 million data prep

pop_1mil_conf_summary_with_outliers <- summary(pop_subset_upto_1million$confirmed)

confirmed_outliers <- boxplot(pop_subset_upto_1million$confirmed)$out

outlier_checker<- pop_subset_upto_1million

pop_1mil_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
pop_1mil_conf_summary_without_outliers <- summary(pop_1mil_without_confirmed_outliers$confirmed)

pop_1mil_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

pop_1mil_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_1mil_confirmed_summary[1,] <- pop_1mil_conf_summary_with_outliers
pop_1mil_confirmed_summary[2,] <- pop_1mil_conf_summary_without_outliers 

pop_1mil_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
pop_1mil_confirmed_summary <- pop_1mil_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(pop_subset_upto_1million$deaths)$out

pop_1mil_deaths_summary_with_outliers <- summary(pop_subset_upto_1million$deaths)

outlier_checker<- pop_subset_upto_1million

pop_1mil_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

pop_1mil_deaths_summary_without_outliers <- summary(pop_1mil_without_death_outliers$deaths)
pop_1mil_death_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

pop_1mil_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_1mil_deaths_summary[1,] <- pop_1mil_deaths_summary_with_outliers
pop_1mil_deaths_summary[2,] <- pop_1mil_deaths_summary_without_outliers 

pop_1mil_deaths_summary$source <- c("With Outliers", "Outliers Removed")
pop_1mil_deaths_summary <- pop_1mil_deaths_summary[,c(7, 1:6)]

# 1 - 10 million data prep


pop_1_10mil_conf_summary_with_outliers <- summary(pop_subset_between_1_10million$confirmed)

confirmed_outliers <- boxplot(pop_subset_between_1_10million$confirmed)$out

outlier_checker<- pop_subset_between_1_10million

pop_1_10mil_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
pop_1_10mil_conf_summary_without_outliers <- summary(pop_1_10mil_without_confirmed_outliers$confirmed)

pop_1_10mil_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

pop_1_10mil_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_1_10mil_confirmed_summary[1,] <- pop_1_10mil_conf_summary_with_outliers
pop_1_10mil_confirmed_summary[2,] <- pop_1_10mil_conf_summary_without_outliers 

pop_1_10mil_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
pop_1_10mil_confirmed_summary <- pop_1_10mil_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(pop_subset_between_1_10million$deaths)$out

pop_1_10mil_deaths_summary_with_outliers <- summary(pop_subset_between_1_10million$deaths)

outlier_checker<- pop_subset_between_1_10million

pop_1_10mil_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

pop_1_10mil_deaths_summary_without_outliers <- summary(pop_1_10mil_without_death_outliers$deaths)
pop_1_10mil_death_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

pop_1_10mil_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_1_10mil_deaths_summary[1,] <- pop_1_10mil_deaths_summary_with_outliers
pop_1_10mil_deaths_summary[2,] <- pop_1_10mil_deaths_summary_without_outliers 

pop_1_10mil_deaths_summary$source <- c("With Outliers", "Outliers Removed")
pop_1_10mil_deaths_summary <- pop_1_10mil_deaths_summary[,c(7, 1:6)]

# 10 - 50 million data prep


pop_10_50mil_conf_summary_with_outliers <- summary(pop_subset_between_10_50million$confirmed)

confirmed_outliers <- boxplot(pop_subset_between_10_50million$confirmed)$out

outlier_checker<- pop_subset_between_10_50million

pop_10_50mil_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
pop_10_50mil_conf_summary_without_outliers <- summary(pop_10_50mil_without_confirmed_outliers$confirmed)

pop_10_50mil_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

pop_10_50mil_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_10_50mil_confirmed_summary[1,] <- pop_10_50mil_conf_summary_with_outliers
pop_10_50mil_confirmed_summary[2,] <- pop_10_50mil_conf_summary_without_outliers 

pop_10_50mil_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
pop_10_50mil_confirmed_summary <- pop_10_50mil_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(pop_subset_between_10_50million$deaths)$out

pop_10_50mil_deaths_summary_with_outliers <- summary(pop_subset_between_10_50million$deaths)

outlier_checker<- pop_subset_between_10_50million

pop_10_50mil_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

pop_10_50mil_deaths_summary_without_outliers <- summary(pop_10_50mil_without_death_outliers$deaths)
pop_10_50mil_death_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

pop_10_50mil_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_10_50mil_deaths_summary[1,] <- pop_10_50mil_deaths_summary_with_outliers
pop_10_50mil_deaths_summary[2,] <- pop_10_50mil_deaths_summary_without_outliers 

pop_10_50mil_deaths_summary$source <- c("With Outliers", "Outliers Removed")
pop_10_50mil_deaths_summary <- pop_10_50mil_deaths_summary[,c(7, 1:6)]

# 50 to 100 million data prep


pop_50_100mil_conf_summary_with_outliers <- summary(pop_subset_between_50_100million$confirmed)

confirmed_outliers <- boxplot(pop_subset_between_50_100million$confirmed)$out

outlier_checker<- pop_subset_between_50_100million

pop_50_100mil_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
pop_50_100mil_conf_summary_without_outliers <- summary(pop_50_100mil_without_confirmed_outliers$confirmed)

pop_50_100mil_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

pop_50_100mil_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_50_100mil_confirmed_summary[1,] <- pop_50_100mil_conf_summary_with_outliers
pop_50_100mil_confirmed_summary[2,] <- pop_50_100mil_conf_summary_without_outliers 

pop_50_100mil_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
pop_50_100mil_confirmed_summary <- pop_50_100mil_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(pop_subset_between_50_100million$deaths)$out

pop_50_100mil_deaths_summary_with_outliers <- summary(pop_subset_between_50_100million$deaths)

outlier_checker<- pop_subset_between_50_100million

pop_50_100mil_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

pop_50_100mil_deaths_summary_without_outliers <- summary(pop_50_100mil_without_death_outliers$deaths)
pop_50_100mil_death_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

pop_50_100mil_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_50_100mil_deaths_summary[1,] <- pop_50_100mil_deaths_summary_with_outliers
pop_50_100mil_deaths_summary[2,] <- pop_50_100mil_deaths_summary_without_outliers 

pop_50_100mil_deaths_summary$source <- c("With Outliers", "Outliers Removed")
pop_50_100mil_deaths_summary <- pop_50_100mil_deaths_summary[,c(7, 1:6)]

# 100 - 250 million data prep

pop_100_250mil_conf_summary_with_outliers <- summary(pop_subset_between_100_250million$confirmed)

confirmed_outliers <- boxplot(pop_subset_between_100_250million$confirmed)$out

outlier_checker<- pop_subset_between_100_250million

pop_100_250mil_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
pop_100_250mil_conf_summary_without_outliers <- summary(pop_100_250mil_without_confirmed_outliers$confirmed)

pop_100_250mil_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

pop_100_250mil_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_100_250mil_confirmed_summary[1,] <- pop_100_250mil_conf_summary_with_outliers
pop_100_250mil_confirmed_summary[2,] <- pop_100_250mil_conf_summary_without_outliers 

pop_100_250mil_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
pop_100_250mil_confirmed_summary <- pop_100_250mil_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(pop_subset_between_100_250million$deaths)$out

pop_100_250mil_deaths_summary_with_outliers <- summary(pop_subset_between_100_250million$deaths)

outlier_checker<- pop_subset_between_100_250million

pop_100_250mil_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

pop_100_250mil_deaths_summary_without_outliers <- summary(pop_100_250mil_without_death_outliers$deaths)
pop_100_250mil_death_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

pop_100_250mil_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_100_250mil_deaths_summary[1,] <- pop_100_250mil_deaths_summary_with_outliers
pop_100_250mil_deaths_summary[2,] <- pop_100_250mil_deaths_summary_without_outliers 

pop_100_250mil_deaths_summary$source <- c("With Outliers", "Outliers Removed")
pop_100_250mil_deaths_summary <- pop_100_250mil_deaths_summary[,c(7, 1:6)]

# 250 to 1000 million data prep

pop_250_1000mil_conf_summary_with_outliers <- summary(pop_subset_between_250_1000million$confirmed)

confirmed_outliers <- boxplot(pop_subset_between_250_1000million$confirmed)$out

outlier_checker<- pop_subset_between_250_1000million

pop_250_1000mil_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
pop_250_1000mil_conf_summary_without_outliers <- summary(pop_250_1000mil_without_confirmed_outliers$confirmed)

pop_250_1000mil_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

pop_250_1000mil_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_250_1000mil_confirmed_summary[1,] <- pop_250_1000mil_conf_summary_with_outliers
pop_250_1000mil_confirmed_summary[2,] <- pop_250_1000mil_conf_summary_without_outliers 

pop_250_1000mil_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
pop_250_1000mil_confirmed_summary <- pop_250_1000mil_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(pop_subset_between_250_1000million$deaths)$out

pop_250_1000mil_deaths_summary_with_outliers <- summary(pop_subset_between_250_1000million$deaths)

outlier_checker<- pop_subset_between_250_1000million

pop_250_1000mil_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

pop_250_1000mil_deaths_summary_without_outliers <- summary(pop_250_1000mil_without_death_outliers$deaths)
pop_250_1000mil_death_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

pop_250_1000mil_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_250_1000mil_deaths_summary[1,] <- pop_250_1000mil_deaths_summary_with_outliers
pop_250_1000mil_deaths_summary[2,] <- pop_250_1000mil_deaths_summary_without_outliers 

pop_250_1000mil_deaths_summary$source <- c("With Outliers", "Outliers Removed")
pop_250_1000mil_deaths_summary <- pop_250_1000mil_deaths_summary[,c(7, 1:6)]

# Excess of 1000 million people data prep

pop_1000mil_conf_summary_with_outliers <- summary(pop_subset_excess_1000million$confirmed)

confirmed_outliers <- boxplot(pop_subset_excess_1000million$confirmed)$out

outlier_checker<- pop_subset_excess_1000million

pop_1000mil_without_confirmed_outliers<- outlier_checker[-which(outlier_checker$confirmed %in% confirmed_outliers),]
pop_1000mil_conf_summary_without_outliers <- summary(pop_1000mil_without_confirmed_outliers$confirmed)

pop_1000mil_confirmed_outliers<- outlier_checker[which(outlier_checker$confirmed %in% confirmed_outliers),]

pop_1000mil_confirmed_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_1000mil_confirmed_summary[1,] <- pop_1000mil_conf_summary_with_outliers
pop_1000mil_confirmed_summary[2,] <- pop_1000mil_conf_summary_without_outliers 

pop_1000mil_confirmed_summary$source <- c("With Outliers", "Outliers Removed")
pop_1000mil_confirmed_summary <- pop_1000mil_confirmed_summary[,c(7, 1:6)]


deaths_outliers <- boxplot(pop_subset_excess_1000million$deaths)$out

pop_1000mil_deaths_summary_with_outliers <- summary(pop_subset_excess_1000million$deaths)

outlier_checker<- pop_subset_excess_1000million

pop_1000mil_without_death_outliers<- outlier_checker[-which(outlier_checker$deaths %in% deaths_outliers),]

pop_1000mil_deaths_summary_without_outliers <- summary(pop_1000mil_without_death_outliers$deaths)
pop_1000mil_death_outliers<- outlier_checker[which(outlier_checker$deaths %in% deaths_outliers),]

pop_1000mil_deaths_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))

pop_1000mil_deaths_summary[1,] <- pop_1000mil_deaths_summary_with_outliers
pop_1000mil_deaths_summary[2,] <- pop_1000mil_deaths_summary_without_outliers 

pop_1000mil_deaths_summary$source <- c("With Outliers", "Outliers Removed")
pop_1000mil_deaths_summary <- pop_1000mil_deaths_summary[,c(7, 1:6)]


# Creating map data and functions to generate geographic map of COVID-19 ----
# This code and the ggiraph output in the server section was based on information gathered from the "Building Interactive World Maps in Shiny"
# tutorial by Florianne Verkroost
# Available at: https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# Changes were made to the original template as follows:
# Original template drew ISO3 codes from the web, however the COVID19 package contains ISO3 codes so this was not necessary
# The process of changing the country names remains the same but the country names themselves have been altered to match the COVID-19 formats
# The log transformation on the second plot was original


world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)


head(world_data)

# Altering country names to match Covid19() package

old_names <- c("French Southern and Antarctic Lands", "Antigua", "Barbuda", "Saint Barthelemy", "Brunei", "Ivory Coast",
               "Democratic Republic of the Congo", "Republic of Congo", "Falkland Islands", "Micronesia", "UK", 
               "Heard Island", "Cocos Islands", "Nevis", "Saint Kitts", "South Korea", "Laos", "Saint Martin",
               "Macedonia", "Pitcairn Islands", "North Korea", "Palestine", "South Sandwich Islands",
               "South Georgia", "Trinidad", "Tobago", "Taiwan", "Tanzania", "USA", "Vatican", "Grenadines",
               "Saint Vincent", "Wallis and Fortuna")
new_names <- c("French Southern Territories", rep("Antigua and Barbuda", 2), "Saint-Barthélemy",
               "Brunei Darussalam", "Cote d'Ivoire", "Congo, the Democratic Republic of the", "Congo", 
               "Falkland Islands (Malvinas)", "Micronesia, Federated States of", "United Kingdom",
               "Heard and Mcdonald Islands", "Cocos (Keeling) Islands",
               rep("Saint Kitts and Nevis", 2), "Korea (South)", "Lao PDR", "Saint-Martin (French part)",
               "Macedonia, Republic of", "Pitcairn", "Korea (North)", "Palestinian Territory",
               rep("South Georgia and the South Sandwich Islands", 2), 
               rep("Trinidad and Tobago", 2), "Taiwan, Republic of China",
               "Tanzania, United Republic of", "United States", "Holy See (Vatican City State)",
               rep("Saint Vincent and Grenadines", 2), "Wallis and Futuna Islands")

for (i in 1:length(old_names)){
    world_data$region[world_data$region == old_names[i]] <- new_names[i]
}

world_data["ISO3"] <- x_worldwide_long$ISO3[match(world_data$region, x_worldwide_long$administrative_area_level_1)]
head(world_data)


x_worldwide_long$Value <- as.numeric(x_worldwide_long$Value)
x_worldwide_long$caseType <- as.factor(x_worldwide_long$caseType)


# Worldmaps function ----
worldMaps <- function(x_worldwide_long, world_data, case_type, date2){
    
    # Function for setting the aesthetics of the plot
    my_theme <- function () { 
        theme_bw() + theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "right",
            legend.direction =  "vertical",
            panel.border = element_blank(),
            strip.background = element_rect(fill = 'white', colour = 'white'))
        
    }
    
    # Select only the data that the user has selected to view
    
    plotx_worldwide_daily <- x_worldwide_long[x_worldwide_long$caseType == case_type &
                                                  x_worldwide_long$date == date2,]
    
    plotx_worldwide_daily <- plotx_worldwide_daily[!is.na(plotx_worldwide_daily$ISO3), ]
    
    
    # Add the data the user wants to see to the geographical world data
    world_data['case_type'] <- rep(case_type, nrow(world_data))
    world_data['date2'] <- rep(date2, nrow(world_data))
    world_data['Value'] <- plotx_worldwide_daily$Value[match(world_data$ISO3, 
                                                             plotx_worldwide_daily$ISO3)]
    
    # Create caption with the data source to show underneath the map
    capt <- paste0("Source: COVID-19 DATAHUB")
    
    # Specify the plot for the world map
    g <- ggplot() + 
        geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                                 aes(x = long, y = lat, fill = Value, group = group, 
                                     tooltip = sprintf("%s<br/><br/>%s", region,
                                                       Value))) + 
        scale_fill_gradientn(colours = brewer.pal(5, "YlOrRd"), na.value = 'white') + 
        labs(fill = case_type, color = case_type, title = "Global COVID-19 Spread", 
             x = NULL, y = NULL, caption = capt)+
        my_theme()
    
    return(g)
}

# Worldmaps function ----
worldMaps_log10 <- function(x_worldwide_long, world_data, case_type, date2){
    
    # Function for setting the aesthetics of the plot
    my_theme <- function () { 
        theme_bw() + theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "right",
            legend.direction =  "vertical",
            panel.border = element_blank(),
            strip.background = element_rect(fill = 'white', colour = 'white'))
        
    }
    
    # Select only the data that the user has selected to view
    
    plotx_worldwide_daily <- x_worldwide_long[x_worldwide_long$caseType == case_type &
                                                  x_worldwide_long$date == date2,]
    
    plotx_worldwide_daily <- plotx_worldwide_daily[!is.na(plotx_worldwide_daily$ISO3), ]
    
    
    # Add the data the user wants to see to the geographical world data
    world_data['case_type'] <- rep(case_type, nrow(world_data))
    world_data['date2'] <- rep(date2, nrow(world_data))
    world_data['Value'] <- plotx_worldwide_daily$Value[match(world_data$ISO3, 
                                                             plotx_worldwide_daily$ISO3)]
    
    # Create caption with the data source to show underneath the map
    capt <- paste0("Source: COVID-19 DATAHUB")
    
    # Specify the plot for the world map
    
    g <- ggplot() + 
        geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                                 aes(x = long, y = lat, fill = log(Value), group = group, 
                                     tooltip = sprintf("%s<br/><br/>%s", region,
                                                       
                                                       Value))) + 
        scale_fill_gradientn(colours = brewer.pal(5, "YlOrRd"), na.value = 'white') + 
        labs(fill = case_type, color = case_type, title = "Global COVID-19 Spread", 
             x = NULL, y = NULL, caption = capt)+
        my_theme()
    
    return(g)
}


# Defining UI ----
ui <- dashboardPage(skin = "blue",
                    
                    
                    
            dashboardHeader(title = "COVID-19"),
            
            dashboardSidebar( tags$script(HTML("$('body').addClass('fixed');")), 
                              sidebarMenu ( 
                                  menuItem("Overview", icon = icon("option-horizontal", lib = "glyphicon"),
                                           selected=TRUE,
                                           startExpanded = TRUE,
                                           br(),
                                           h4(strong("SARS-CoV-2")),
                                           p("At the end of 2019, a new form of",br(),
                                             "Coronavirus emerged in the wet food",br(),
                                             "markets of Wuhan, China. From the ",br(),
                                             "same family of viruses as SARS and  ",br(),
                                             "MERS, SARS-CoV-2 appeared to have  ",br(),
                                             "greater transmisability and quickly ",br(),
                                             "spread across the world, dwarfing  ",br(),
                                             "the respective case numbers of the ", br(),
                                             "previous outbreaks.", br()),
                                           br(),
                                           h5(strong("The purpose of this application", br(),
                                                     "is two-fold:", br())),
                                           p(strong("1."),"To compare source information ", br(),
                                             "from key repositories of data on", br(),
                                             "COVID-19 to assess if there are ",br(),
                                             "any reporting anomalies.", br(),
                                             br(),
                                             strong("2."), "To display data on COVID-19", br(),
                                             "as aggregated by the COVID-19", br(),
                                             "Data Hub, to identify countries", br(),
                                             "whose figures identify them as ", br(),
                                             "outliers in their group"),br()),
                                  
                                  
                                  menuItem("Global Analysis", tabName ="global_maps", icon = icon("globe"),
                                           selected = TRUE,
                                           startExpanded = TRUE,
                                           selectInput(inputId = "case_type",
                                                       label = "Choose the type of data you want to see:",
                                                       choices = list("Confirmed" = "confirmed", 
                                                                      "Recovered" = "recovered", 
                                                                      "Deaths" = "deaths",
                                                                      "Tests" = "tests")),
                                           sliderInput("date2",
                                                       label = h5("Choose date to map globally"),
                                                       min = as.Date(x_worldwide_min_date),
                                                       max = as.Date(date_set),
                                                       value = as.Date(date_set),
                                                       timeFormat = "%d %b", 
                                                       animate=animationOptions(interval = 3000, loop = FALSE))
                                  ),
                                  
                                  
                                  menuItem("Analysis by Country", tabName ="country_analysis", icon = icon("dashboard"), 
                                           startExpanded = FALSE,
                                           
                                           selectInput("country", label = "Country",
                                                       choices = unique(x_worldwide$administrative_area_level_1), 
                                                       selected = top_5_countries_by_case_count$administrative_area_level_1, multiple = TRUE),
                                           
                                           selectInput("type", label = "type", choices = list("Confirmed" = "confirmed", 
                                                                                              "Recovered" = "recovered", 
                                                                                              "Deaths" = "deaths",
                                                                                              "Tests" = "tests")),
                                           selectInput("level", label = "Granularity", 
                                                       choices = c("Country" = 1), selected = 1),
                                           
                                           dateRangeInput("date", label = "Date", start = "2020-01-01")
                                           
                                  )
                              )
            ), 
                    
            dashboardBody(
                
                fluidRow(
                    
                    infoBoxOutput("globalCasesBox"),
                    infoBoxOutput("globalDeathsBox"),
                    infoBoxOutput("globalRecoveriesBox")
                ),
                
                tabsetPanel(
                    tabPanel("Global",
                             tabsetPanel(
                                 tabPanel("Map",
                                          fluidRow(
                                              girafeOutput("distPlot")
                                          )   
                                          
                                 ),
                                 tabPanel("Map (log10)",
                                          fluidRow(
                                              girafeOutput("distPlot_log10")
                                          ) 
                                 )
                             ),
                             
                             fluidRow(
                                 plotlyOutput("global_timeseries"),
                             ),
                             
                             fluidRow(
                                 wellPanel(style = "background: white", 
                                           h3("User Instructions for Analysis by Country:"),
                                           h4("1. Countries, Dates and Data to plot can be altered in the sidebar"),
                                           h4("2. Hover over graph lines for exact figures"),
                                           h4("3. Clicking on the Country name within the legend will deselect it from plots."),
                                           h4("4. Click and drag mouse to zoom in on data"),
                                           h4("5. Double-click to return to orginal view"),
                                 )
                             ),
                             
                             fluidRow(
                                 tabsetPanel(
                                     tabPanel("Population Analysis",
                                              plotlyOutput("total_population_plot")
                                     )
                                     
                                 ),
                                 tabsetPanel(     
                                     tabPanel("Rates Per 100k",
                                              plotlyOutput("population_plot_per100K")
                                     )
                                 ),
                                 tabsetPanel( 
                                     tabPanel("Population Density",
                                              plotlyOutput("popdensityVsType")
                                     )
                                 ),
                                 plotlyOutput("deaths_over_time"),
                                 # Output excess deaths vs confirmed Covid Deaths
                                 plotlyOutput("excess_deaths_plot")
                                 
                             )
                             
                    ),
                    
                    tabPanel("Data Subsetter",
                             fluidRow(
                                 wellPanel(style = "background: white", 
                                           h3("User Instructions for Data Subsetting:"),
                                           h4("The information on this page can be used to identify outliers within Population Density and Population subsets."),
                                           h4("The Population Density per km2 and Population plots contain log10 views to alter the distribution of the data."),
                                           h4("The boxplots will list outliers identified within them for further analysis."),
                                           br(),
                                           h4("1. Hover over points in the Global Density and Population plots display the country."),
                                           h4("2. The outliers will appear between the box plots"),
                                           h4("3. The Analysis by Country sidebar is still active and can be populated with the outlier countries by the user."),
                                 )
                             ),      
                             h2("Global Population Density Analysis"),
                             tabsetPanel(
                                 tabPanel("Cumulative",
                                          plotlyOutput("globalpopdensplot")
                                 ),
                                 tabPanel( "log10",
                                           plotlyOutput("globalpopdensplot_log10")
                                 ),
                                 tabPanel("log10 - Countries with more than 1million people",
                                          plotlyOutput("globalpopdensplot_log10_1m_plus")
                                 )
                             ),
                             h2("Outlier Detection - Population Density per km2"),
                             tabsetPanel(
                                 tabPanel("Up to 10 people per km2",
                                          plotOutput("density_10_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_10_outliers_detected")),
                                          plotOutput("density_10_confirmed_no_outliers"),
                                          tableOutput("density_10_confirmed_summary"),
                                          
                                          
                                          plotOutput("density_10_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_10_outliers_detected")),
                                          plotOutput("density_10_deaths_no_outliers"),
                                          tableOutput("density_10_deaths_summary")
                                 ),
                                 
                                 tabPanel("10-50 people per km2",
                                          plotOutput("density_10_50_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_10_50_outliers_detected")),
                                          plotOutput("density_10_50_confirmed_no_outliers"),
                                          tableOutput("density_10-50_confirmed_summary"),
                                          
                                          plotOutput("density_10_50_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_10_50_outliers_detected")),
                                          plotOutput("density_10_50_deaths_no_outliers"),
                                          tableOutput("density_10_50_deaths_summary")
                                 ),
                                 tabPanel("50-100 people per km2",
                                          plotOutput("density_50_100_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_50_100_outliers_detected")),
                                          plotOutput("density_50_100_confirmed_no_outliers"),
                                          tableOutput("density_50_100_confirmed_summary"),
                                          
                                          plotOutput("density_50_100_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_50_100_outliers_detected")),
                                          plotOutput("density_50_100_deaths_no_outliers"),
                                          tableOutput("density_50_100_deaths_summary")
                                 ),
                                 tabPanel("100-150 people per km2",
                                          plotOutput("density_100_150_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_100_150_outliers_detected")),
                                          plotOutput("density_100_150_confirmed_no_outliers"),
                                          tableOutput("density_100-150_confirmed_summary"),
                                          
                                          plotOutput("density_100_150_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_100_150_outliers_detected")),
                                          plotOutput("density_100_150_deaths_no_outliers"),
                                          tableOutput("density_100_150_deaths_summary")
                                 ),
                                 tabPanel("150-250 people per km2",
                                          plotOutput("density_150_250_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_150_250_outliers_detected")),
                                          plotOutput("density_150_250_confirmed_no_outliers"),
                                          tableOutput("density_150_250_confirmed_summary"),
                                          
                                          plotOutput("density_150_250_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_150_250_outliers_detected")),
                                          plotOutput("density_150_250_deaths_no_outliers"),
                                          tableOutput("density_150_250_deaths_summary")
                                 ),
                                 tabPanel("250-500 people per km2",
                                          plotOutput("density_250_500_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_250_500_outliers_detected")),
                                          plotOutput("density_250_500_confirmed_no_outliers"),
                                          tableOutput("density_250_500_confirmed_summary"),
                                          
                                          plotOutput("density_250_500_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_250_500_outliers_detected")),
                                          plotOutput("density_250_500_deaths_no_outliers"),
                                          tableOutput("density_250_500_deaths_summary")
                                 ),
                                 
                                 tabPanel("500-1000 people per km2",
                                          plotOutput("density_500_1000_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_500_1000_outliers_detected")),
                                          plotOutput("density_500_1000_confirmed_no_outliers"),
                                          tableOutput("density_500_1000_confirmed_summary"),
                                          
                                          plotOutput("density_500_1000_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_500_1000_outliers_detected")),
                                          plotOutput("density_500_1000_deaths_no_outliers"),
                                          tableOutput("density_500_1000_deaths_summary")
                                 ),
                                 
                                 tabPanel("1000+ people per km2",
                                          plotOutput("density_1000_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_1000_outliers_detected")),
                                          plotOutput("density_1000_confirmed_no_outliers"),
                                          tableOutput("density_1000_confirmed_summary"),
                                          
                                          plotOutput("density_1000_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_1000_outliers_detected")),
                                          plotOutput("density_1000_deaths_no_outliers"),
                                          tableOutput("density_1000_deaths_summary")
                                 )
                                 
                             ),
                             
                             
                             h2("Global Population Analysis"),
                             tabsetPanel(
                                 tabPanel("Cumulative",
                                          plotlyOutput("globalpopulationplot")
                                 ),
                                 tabPanel( "log10",
                                           plotlyOutput("globalpopulationplot_log10")
                                 ),
                                 tabPanel("log10 - Countries with more than 1million people",
                                          plotlyOutput("globalpopulationplot_log10_1mplus")
                                 )
                             ),
                             
                             h2("Outlier Detection - Total Population"),
                             tabsetPanel(
                                 tabPanel("Up to 1 million people",
                                          plotOutput("pop_1mil_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_1mil_outliers_detected")),
                                          plotOutput("pop_1mil_confirmed_no_outliers"),
                                          tableOutput("pop_1mil_confirmed_summary"),
                                          
                                          
                                          plotOutput("pop_1mil_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_1mil_outliers_detected")),
                                          plotOutput("pop_1mil_deaths_no_outliers"),
                                          tableOutput("pop_1mil_deaths_summary")
                                 ),
                                 tabPanel("1 - 10 million people",
                                          plotOutput("pop_1_10mil_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_1_10mil_outliers_detected")),
                                          plotOutput("pop_1_10mil_confirmed_no_outliers"),
                                          tableOutput("pop_1_10mil_confirmed_summary"),
                                          
                                          
                                          plotOutput("pop_1_10mil_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_1_10mil_outliers_detected")),
                                          plotOutput("pop_1_10mil_deaths_no_outliers"),
                                          tableOutput("pop_1_10mil_deaths_summary")
                                 ),
                                 tabPanel("10 - 50 million people",
                                          plotOutput("pop_10_50mil_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_10_50mil_outliers_detected")),
                                          plotOutput("pop_10_50mil_confirmed_no_outliers"),
                                          tableOutput("pop_10_50mil_confirmed_summary"),
                                          
                                          
                                          plotOutput("pop_10_50mil_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_10_50mil_outliers_detected")),
                                          plotOutput("pop_10_50mil_deaths_no_outliers"),
                                          tableOutput("pop_10_50mil_deaths_summary")
                                 ),
                                 
                                 tabPanel("50 - 100 million people",
                                          plotOutput("pop_50_100mil_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_50_100mil_outliers_detected")),
                                          plotOutput("pop_50_100mil_confirmed_no_outliers"),
                                          tableOutput("pop_50_100mil_confirmed_summary"),
                                          
                                          
                                          plotOutput("pop_50_100mil_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_50_100mil_outliers_detected")),
                                          plotOutput("pop_50_100mil_deaths_no_outliers"),
                                          tableOutput("pop_50_100mil_deaths_summary")
                                 ),
                                 
                                 tabPanel("100 - 250 million people",
                                          plotOutput("pop_100_250mil_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_100_250mil_outliers_detected")),
                                          plotOutput("pop_100_250mil_confirmed_no_outliers"),
                                          tableOutput("pop_100_250mil_confirmed_summary"),
                                          
                                          
                                          plotOutput("pop_100_250mil_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_100_250mil_outliers_detected")),
                                          plotOutput("pop_100_250mil_deaths_no_outliers"),
                                          tableOutput("pop_100_250mil_deaths_summary")
                                 ),
                                 
                                 tabPanel("250 - 1000 million people",
                                          plotOutput("pop_250_1000mil_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_250_1000mil_outliers_detected")),
                                          plotOutput("pop_250_1000mil_confirmed_no_outliers"),
                                          tableOutput("pop_250_1000mil_confirmed_summary"),
                                          
                                          
                                          plotOutput("pop_250_1000mil_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_250_1000mil_outliers_detected")),
                                          plotOutput("pop_250_1000mil_deaths_no_outliers"),
                                          tableOutput("pop_250_1000mil_deaths_summary")
                                 ),
                                 
                                 tabPanel("Over 1000 million people",
                                          plotOutput("pop_1000mil_confirmed"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("conf_1000mil_outliers_detected")),
                                          plotOutput("pop_1000mil_confirmed_no_outliers"),
                                          tableOutput("pop_1000mil_confirmed_summary"),
                                          
                                          
                                          plotOutput("pop_1000mil_deaths"),
                                          wellPanel(style = "background: white", h5("Outliers detected and removed:"),
                                                    tableOutput("deaths_1000mil_outliers_detected")),
                                          plotOutput("pop_1000mil_deaths_no_outliers"),
                                          tableOutput("pop_1000mil_deaths_summary")
                                 )
                                 
                             ),
                             
                    ),
                    
                    tabPanel("Source Comparison",
                             
                             fluidRow(
                                 wellPanel(style = "background: white", 
                                           h3("User Instructions for Source Comparison:"),
                                           h4("The information on this page can be used to examine similarities and disparities between some of the main repositories of data on COVID-19."),
                                           h4("The data sources compared are from the World Health Organisation (WHO), Johns Hopkins University (JHU) and the European Centre for Disease Prevention and Control (ECDC)."),
                                           br(),
                                           h4("1. For exact figures on a given day, hover over the line on the plot below."),
                                           h4("2. Click and drag to zoom on a particular time window, double-click to return to original view."),
                                 )
                             ),         
                             
                             
                             fluidRow(
                                 box( "Global Totals", width=4,
                                      tableOutput("global_totals")
                                 ),
                                 box( width = 8,
                                      plotlyOutput("source_timeseries")
                                 )
                             ),
                             
                             
                             fluidRow(
                                 box("Standard Deviation Comparison:", width = 6,
                                     fluidRow( "Daily Case rates:",
                                               tableOutput("daily_case_sd_comparison")        
                                     ),
                                     
                                     fluidRow( "Daily Death Rates",
                                               tableOutput("daily_death_sd_comparison")        
                                     )
                                 ),
                                 
                                 box("                             ", width = 6,
                                     fluidRow( "Weekly Case Rates",
                                               tableOutput("weekly_case_sd_comparison")        
                                     ),
                                     fluidRow( "Weekly Death Rates",
                                               tableOutput("weekly_death_sd_comparison")        
                                     ),
                                     
                                 ),
                                 
                             ),
                             
                             wellPanel(style = "background: white",
                                       fluidRow( "Daily Case Rates",    
                                                 tableOutput("daily_case_summary_comparison")        
                                       ),
                                       
                                       fluidRow( "Daily Death Rates",
                                                 tableOutput("daily_death_summary_comparison")        
                                       ),
                                       
                                       fluidRow( "Weekly Case Rates",
                                                 tableOutput("weekly_case_summary_comparison")        
                                       ),
                                       fluidPage( "Weekly Death Rates",
                                                  tableOutput("weekly_death_summary_comparison")        
                                       ),
                             ),
                    )
                )
            )
)


# Define server logic
server <- function(input, output) {
    
    # Outputs relating to the Global tab----
    # Info boxes 
    output$globalCasesBox <- renderInfoBox({
        
        infoBox(
            "Total Cases",x_confirmed_panel, icon =icon("eye-open", lib = "glyphicon"),
            color = "purple"
        )
    })
    output$globalDeathsBox <- renderInfoBox({
        
        infoBox(
            "Deaths:",x_deaths_panel, icon = icon("heart-empty", lib = "glyphicon"),
            color = "red"
        )
    })
    output$globalRecoveriesBox <- renderInfoBox({
        infoBox(
            "Recoveries:", x_recovered_panel, icon = icon("thumbs-up", lib = "glyphicon"),
            color = "green"
        )
    })
    
    # Geographical plot
    
    
    output$distPlot <- renderGirafe({
        
        ggiraph(code = print(worldMaps(x_worldwide_long, world_data, input$case_type, input$date2)))
        
    })
    
    # Geo log 10
    output$distPlot_log10 <- renderGirafe({
        ggiraph(code = print(worldMaps_log10(x_worldwide_long, world_data, input$case_type, input$date2)))
    })
    
    output$global_timeseries <- renderPlotly({
        
        global_timeseries <- ggplot(global_daily_figures, aes(x=date)) + 
            geom_line(aes(y = new_cases), colour="blue")+
            geom_line(aes(y = new_deaths), colour="red")+
            labs(title="Global Time series of COVID-19 Outbreak", 
                 subtitle = "Hover over line for exact values",
                 caption="Source: COVID-19",
                 x= "Date",
                 y="Rate of Occurence")
        
        
        ggplotly(global_timeseries)  %>% config(displayModeBar = F)
        
    })
    
    # Country Analysis plots
    
    # Rates vs Population Plot
    
    output$total_population_plot <- renderPlotly({
        if(!is.null(input$country)){
            x <- covid19(country = input$country, level = input$level, 
                         start = input$date[1], end = input$date[2])
            
            color <- paste0("administrative_area_level_", input$level)
            
            plot_ly(x = x[["population"]], y = x[[input$type]], color = x[[color]]) %>%
                layout(title = paste("Total ", input$type, " vs Total Population"),
                       xaxis=list(title = "Population"),
                       yaxis=list(title = input$type)) %>% 
                config(displayModeBar = F)
        }
    })
    
    
    # Rates per 100k population plot
    output$population_plot_per100K <- renderPlotly({
        if(!is.null(input$country)){
            x <- covid19(country = input$country, level = input$level, 
                         start = input$date[1], end = input$date[2])
            
            x$rate_per_100k <- (x[[input$type]]/x$population)*100000
            
            color <- paste0("administrative_area_level_", input$level)
            
            plot_ly(x, x = x[["population"]], y = x[["rate_per_100k"]], color = x[[color]]) %>%
                layout(title = paste(input$type, "per 100K Population"),
                       xaxis=list(title = "Total Population"),
                       yaxis=list(title = paste(input$type, "per 100K People"))) %>% 
                config(displayModeBar = F)
        }
    })
    
    
    # Population density plot
    output$popdensityVsType <- renderPlotly({
        if(!is.null(input$country)){
            x <-  covid19(country = input$country, level = input$level, 
                          start = input$date[1], end = input$date[2]) 
            x["pop_density_km2"] <- x_worldwide$pop_density_km2[match(x$iso_alpha_3, 
                                                                      x_worldwide$ISO3)]
            color <- paste0("administrative_area_level_", input$level)
            
            plot_ly(x = x[["pop_density_km2"]], y = x[[input$type]], color = x[[color]]) %>%
                
                layout(title = paste("Total", input$type, " Vs Population Density"),
                       xaxis=list(title = "Population Density - People per Km2"),
                       yaxis=list(title = input$type)) %>% 
                config(displayModeBar = F)
            
        }
    })
    
    # Excess Deaths Plot
    output$excess_deaths_plot <- renderPlotly({
        if(!is.null(input$country)){
            
            x <-subset(x_worldwide_weekly_death_values, administrative_area_level_1 == input$country)
            
            linetype = rep(c("solid", "dashed", "dotted"))
            
            plot_work <- ggplot(x, aes(x=date, col=administrative_area_level_1))+
                geom_line(aes(y= weekly_deaths), linetype=1)+
                geom_line(aes(y = excess_2020), linetype=2)+
                geom_line(aes(y = avg_deaths), linetype=3)+ 
                ggtitle("Excess Deaths Vs Confirmed COVID-19 Deaths")+
                xlab("Date")+
                ylab("Deaths")+
                labs(col="Country")
            
            ggplotly(plot_work)   %>% config(displayModeBar = F)
            
            
        }
    })
    # Country plots continued ----
    
    # Time series plot
    output$deaths_over_time <- renderPlotly({
        if(!is.null(input$country)){
            
            x<- subset(x_worldwide_with_dailies, administrative_area_level_1 == input$country)
            
            
            gg_5 <- ggplot(x, aes(x=date, col = administrative_area_level_1)) + 
                geom_line(aes(y=new_cases), linetype=1) + 
                geom_line(aes(y=new_deaths), linetype = 2) + 
                geom_line(aes(y=new_recovered), linetype = 3) +
                labs(title="Time Series Comparison of Selected Countries", 
                     subtitle="Cases and Deaths by Day", 
                     caption="Source: COVID-19",
                     y="Rate of Occurence",
                     x="Date",
                     col = "Country")
            
            
            ggplotly(gg_5)  %>% config(displayModeBar = F)
            
        }
    })
    
    # Outputs relating to the Data Subsetter Tab----
    
    # Global pop density scatter graph
    
    output$globalpopdensplot <- renderPlotly({
        
        num_of_colours = length(unique(x_worldwide_long$administrative_area_level_1))
        palette_loader = colorRampPalette(brewer.pal(9, "Set1"))
        
        
        if(!is.null(input$case_type)){
            x <- subset(x_worldwide_long, caseType == input$case_type & date == input$date2)
            
            plot_work <- ggplot(x)+
                geom_point(aes(x=pop_density_km2, y=Value, col=administrative_area_level_1))+
                geom_smooth(aes(x=pop_density_km2, y=Value),method = lm)+
                
                labs(title="Population Density Cumulative", 
                     caption="Source: COVID-19",
                     x= "Population Density - People per km2",
                     y= input$case_type,
                     col="Country")+
                scale_colour_manual(values = palette_loader(num_of_colours))
            
            ggplotly(plot_work)  %>% config(displayModeBar = F)
            
        }
    })
    
    
    
    output$globalpopdensplot_log10 <- renderPlotly({
        
        num_of_colours = length(unique(x_worldwide_long$administrative_area_level_1))
        palette_loader = colorRampPalette(brewer.pal(9, "Set1"))
        
        if(!is.null(input$case_type)){
            x <- subset(x_worldwide_long, caseType == input$case_type & date == input$date2)
            
            
            plot_work <- ggplot(x)+
                geom_point(aes(x=pop_density_km2, y=Value, col=administrative_area_level_1))+
                geom_smooth(aes(x=pop_density_km2, y=Value),method = lm)+
                
                
                ggtitle("Population Density log10")+
                xlab("Population Density - People per km2 (log10 scale)")+
                ylab(paste(input$case_type, "(log10 scale)"))+
                labs(col="Country")+
                scale_colour_manual(values = palette_loader(num_of_colours))+
                scale_y_log10()+
                scale_x_log10()
            
            
            ggplotly(plot_work)  %>% config(displayModeBar = F)
            
        }
    })
    
    output$globalpopdensplot_log10_1m_plus <- renderPlotly({
        
        num_of_colours = length(unique(x_worldwide_long$administrative_area_level_1))
        palette_loader = colorRampPalette(brewer.pal(9, "Set1"))
        
        
        
        if(!is.null(input$case_type)){
            x <- subset(x_worldwide_long, caseType == input$case_type & date == input$date2
                        & population>=1000000)
            
            
            plot_work <- ggplot(x)+
                geom_point(aes(x=pop_density_km2, y=Value, col=administrative_area_level_1))+
                geom_smooth(aes(x=pop_density_km2, y=Value),method = lm)+
                
                ggtitle("Population Density log10")+
                xlab("Population Density - People per km2 (log10 scale)")+
                ylab(paste(input$case_type, "(log10 scale)"))+
                labs(col="Country")+
                scale_colour_manual(values = palette_loader(num_of_colours))+
                scale_y_log10()+
                scale_x_log10()
            
            
            ggplotly(plot_work)  %>% config(displayModeBar = F)
        }
    })
    
    # Global pop vs cases scatter graph
    output$globalpopulationplot <- renderPlotly({
        
        num_of_colours = length(unique(x_worldwide_long$administrative_area_level_1))
        palette_loader = colorRampPalette(brewer.pal(9, "Set1"))
        
        if(!is.null(input$case_type)){
            x <- subset(x_worldwide_long, caseType == input$case_type & date == input$date2)
            
            plot_work <- ggplot(x)+
                geom_point(aes(x=population, y=Value, col=administrative_area_level_1))+
                geom_smooth(aes(x=population, y=Value), method = "lm")+
                
                ggtitle("Population")+
                xlab("Population")+
                ylab(input$type)+
                labs(col="Country")+
                scale_colour_manual(values = palette_loader(num_of_colours))
            
            ggplotly(plot_work)  %>% config(displayModeBar = F)
        }
    })
    # Global pop vs cases scatter graph - log10
    output$globalpopulationplot_log10 <- renderPlotly({
        
        num_of_colours = length(unique(x_worldwide_long$administrative_area_level_1))
        palette_loader = colorRampPalette(brewer.pal(9, "Set1"))
        
        if(!is.null(input$case_type)){
            x <- subset(x_worldwide_long, caseType == input$case_type & date == input$date2)
            
            plot_work <- ggplot(x)+
                geom_point(aes(x=population, y=Value, col=administrative_area_level_1))+
                geom_smooth(aes(x=population, y=Value), method = "lm")+
                
                ggtitle("Population Log10")+
                xlab("Population (log10 scale)")+
                ylab(paste(input$case_type, "(log10 scale)"))+
                labs(col="Country")+
                scale_colour_manual(values = palette_loader(num_of_colours))+
                scale_y_log10()+
                scale_x_log10()
            
            ggplotly(plot_work)  %>% config(displayModeBar = F)
        }
    })
    
    # Global pop vs cases scatter graph - log10
    output$globalpopulationplot_log10_1mplus <- renderPlotly({
        num_of_colours = length(unique(x_worldwide_long$administrative_area_level_1))
        palette_loader = colorRampPalette(brewer.pal(9, "Set1"))
        
        if(!is.null(input$case_type)){
            
            x <- subset(x_worldwide_long, caseType == input$case_type & date == input$date2
                        & population>=1000000)
            
            plot_work <- ggplot(x)+
                geom_point(aes(x=population, y=Value, col=administrative_area_level_1))+
                geom_smooth(aes(x=population, y=Value), method = "lm")+
                
                ggtitle("Population Log10")+
                xlab("Population (log10 scale)")+
                ylab(paste(input$case_type, "(log10 scale)"))+
                labs(col="Country")+
                scale_colour_manual(values = palette_loader(num_of_colours))+
                scale_y_log10()+
                scale_x_log10()
            
            ggplotly(plot_work)  %>% config(displayModeBar = F)
            
        }
    })
    # less than 10 per km
    output$density_10_confirmed <- renderPlot({
        
        case_plot <- ggplot(density_subset_upto_10_per_km2) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col=administrative_area_level_1)) +
            labs(title="Population Density Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: Up to 10 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    
    
    output$density_10_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(dens_10_km2_without_confirmed_outliers) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: Up to 10 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_10_outliers_detected <- renderText({
        
        dens_10_km2_confirmed_outliers$administrative_area_level_1
    })
    
    output$density_10_confirmed_summary <- renderTable({
        
        dens_10_confirmed_summary
        
    })
    
    output$density_10_deaths <- renderPlot({
        
        death_plot <- ggplot(density_subset_upto_10_per_km2) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot", 
                 subtitle="Country Subset: Up to 10 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        
        plot(death_plot)
    })
    
    output$density_10_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(dens_10_km2_without_death_outliers) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: Up to 10 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Deaths Cases",
                 col = "Country")
        
        
        
        plot(death_plot)
    })
    
    output$deaths_10_outliers_detected <- renderText({
        
        dens_10_km2_death_outliers$administrative_area_level_1
    })
    
    output$density_10_deaths_summary <- renderTable({
        
        dens_10_deaths_summary
        
    })
    # 10 - 50 per km2
    output$density_10_50_confirmed <- renderPlot({
        
        case_plot <- ggplot(density_subset_between_10_50_per_km2) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 10 and 50 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        
        plot(case_plot)
    })
    
    output$density_10_50_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(dens_10_50_km2_without_confirmed_outliers) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 10 and 50 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_10_50_outliers_detected <- renderText({
        
        dens_10_50_km2_confirmed_outliers$administrative_area_level_1
    })
    
    output$density_10_50_confirmed_summary <- renderTable({
        
        dens_10_50_confirmed_summary
        
    })
    
    output$density_10_50_deaths <- renderPlot({
        
        death_plot <- ggplot(density_subset_between_10_50_per_km2) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))
        labs(title="Population Density Vs Deaths Boxplot", 
             subtitle="Country Subset: 10 and 50 people per km2", 
             caption="Source: COVID-19",
             x= "Population Density - People per km2",
             y="Confirmed Deaths",
             col = "Country")
        
        
        plot(death_plot)
    })
    
    output$density_10_50_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(dens_10_50_km2_without_death_outliers) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 10 and 50 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_10_50_outliers_detected <- renderText({
        
        dens_10_50_km2_deaths_outliers$administrative_area_level_1
    })
    
    output$density_10_50_deaths_summary <- renderTable({
        
        dens_10_50_deaths_summary
        
    })
    
    # 50 - 100 per km2
    output$density_50_100_confirmed <- renderPlot({
        
        case_plot <- ggplot(density_subset_between_50_100_per_km2) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 50 to 100 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$density_50_100_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(dens_50_100_km2_without_confirmed_outliers) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 50 to 100 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_50_100_outliers_detected <- renderText({
        
        dens_50_100_km2_confirmed_outliers$administrative_area_level_1
    })
    
    output$density_50_100_confirmed_summary <- renderTable({
        
        dens_50_100_confirmed_summary
        
    })
    
    output$density_50_100_deaths <- renderPlot({
        
        death_plot <- ggplot(density_subset_between_50_100_per_km2) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot", 
                 subtitle="Country Subset: 50 to 100 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$density_50_100_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(dens_50_100_km2_without_death_outliers) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 50 to 100 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_50_100_outliers_detected <- renderText({
        
        dens_50_100_km2_deaths_outliers$administrative_area_level_1
    })
    
    output$density_50_100_deaths_summary <- renderTable({
        
        dens_50_100_deaths_summary
        
    })
    
    
    # 100 - 150 per km2
    output$density_100_150_confirmed <- renderPlot({
        
        case_plot <- ggplot(density_subset_between_100_150_per_km2) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 100 to 150 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$density_100_150_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(dens_100_150_km2_without_confirmed_outliers) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 100 to 150 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_100_150_outliers_detected <- renderText({
        
        dens_100_150_km2_confirmed_outliers$administrative_area_level_1
    })
    
    output$density_100_150_confirmed_summary <- renderTable({
        
        dens_100_150_confirmed_summary
        
    })
    
    output$density_100_150_deaths <- renderPlot({
        
        death_plot <- ggplot(density_subset_between_100_150_per_km2) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot", 
                 subtitle="Country Subset: 100 to 150 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$density_100_150_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(dens_100_150_km2_without_death_outliers) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 100 to 150 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_100_150_outliers_detected <- renderText({
        
        dens_100_150_km2_deaths_outliers$administrative_area_level_1
    })
    
    output$density_100_150_deaths_summary <- renderTable({
        
        dens_100_150_deaths_summary
        
    })
    
    # 150 - 250 per km2
    output$density_150_250_confirmed <- renderPlot({
        
        case_plot <- ggplot(density_subset_between_150_250_per_km2) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 150 to 250 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$density_150_250_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(dens_150_250_km2_without_confirmed_outliers) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 150 to 250 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        
        plot(case_plot)
        
    })
    
    output$conf_150_250_outliers_detected <- renderText({
        
        dens_150_250_km2_confirmed_outliers$administrative_area_level_1
    })
    
    output$density_150_250_confirmed_summary <- renderTable({
        
        dens_150_250_confirmed_summary
        
    })
    
    output$density_150_250_deaths <- renderPlot({
        
        death_plot <- ggplot(density_subset_between_150_250_per_km2) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot", 
                 subtitle="Country Subset: 150 to 250 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        
        plot(death_plot)
    })
    
    output$density_150_250_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(dens_150_250_km2_without_death_outliers) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 150 to 250 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_150_250_outliers_detected <- renderText({
        
        dens_150_250_km2_deaths_outliers$administrative_area_level_1
    })
    
    output$density_150_250_deaths_summary <- renderTable({
        
        dens_150_250_deaths_summary
        
    })
    
    # 250 - 500 per km2
    output$density_250_500_confirmed <- renderPlot({
        
        case_plot <- ggplot(density_subset_between_250_500_per_km2) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 250 and 500 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        
        plot(case_plot)
    })
    
    output$density_250_500_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(dens_250_500_km2_without_confirmed_outliers) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 250 and 500 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_250_500_outliers_detected <- renderText({
        
        dens_250_500_km2_confirmed_outliers$administrative_area_level_1
    })
    
    output$density_250_500_confirmed_summary <- renderTable({
        
        dens_250_500_confirmed_summary
        
    })
    
    output$density_250_500_deaths <- renderPlot({
        
        death_plot <- ggplot(density_subset_between_250_500_per_km2) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot", 
                 subtitle="Country Subset: 250 and 500 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$density_250_500_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(dens_250_500_km2_without_death_outliers) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 250 and 500 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y= "Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_250_500_outliers_detected <- renderText({
        
        dens_250_500_km2_deaths_outliers$administrative_area_level_1
    })
    
    output$density_250_500_deaths_summary <- renderTable({
        
        dens_250_500_deaths_summary
        
    })
    
    # 500 - 1000 per km2
    output$density_500_1000_confirmed <- renderPlot({
        
        case_plot <- ggplot(density_subset_between_500_1000_per_km2) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 500 to 1000 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$density_500_1000_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(dens_500_1000_km2_without_confirmed_outliers) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 500 to 1000 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_500_1000_outliers_detected <- renderText({
        
        dens_500_1000_km2_confirmed_outliers$administrative_area_level_1
    })
    
    output$density_500_1000_confirmed_summary <- renderTable({
        
        dens_500_1000_confirmed_summary
        
    })
    
    output$density_500_1000_deaths <- renderPlot({
        
        death_plot <- ggplot(density_subset_between_500_1000_per_km2) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot", 
                 subtitle="Country Subset: 500 to 1000 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$density_500_1000_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(dens_500_1000_km2_without_death_outliers) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 500 to 1000 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_500_1000_outliers_detected <- renderText({
        
        dens_500_1000_km2_deaths_outliers$administrative_area_level_1
    })
    
    output$density_500_1000_deaths_summary <- renderTable({
        
        dens_500_1000_deaths_summary
        
    })
    
    # Over 1000 per km2
    output$density_1000_confirmed <- renderPlot({
        
        case_plot <- ggplot(density_subset_excess_1000_per_km2) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))+
            labs(title="Population Density Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: Over 1000 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$density_1000_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(dens_1000_km2_without_confirmed_outliers) +
            geom_boxplot(aes(pop_density_km2, confirmed))+
            geom_point(aes(pop_density_km2, confirmed, col = administrative_area_level_1))
        labs(title="Population Density Vs Confirmed Cases Boxplot - Outliers Removed", 
             subtitle="Country Subset: Over 1000 people per km2", 
             caption="Source: COVID-19",
             x= "Population Density - People per km2",
             y="Confirmed Cases",
             col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_1000_outliers_detected <- renderText({
        
        dens_1000_km2_confirmed_outliers$administrative_area_level_1
    })
    
    output$density_1000_confirmed_summary <- renderTable({
        
        dens_1000_confirmed_summary
        
    })
    
    output$density_1000_deaths <- renderPlot({
        
        death_plot <- ggplot(density_subset_excess_1000_per_km2) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot", 
                 subtitle="Country Subset: Over 1000 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$density_1000_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(dens_1000_km2_without_death_outliers) +
            geom_boxplot(aes(pop_density_km2, deaths))+
            geom_point(aes(pop_density_km2, deaths, col = administrative_area_level_1))+
            labs(title="Population Density Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: Over 1000 people per km2", 
                 caption="Source: COVID-19",
                 x= "Population Density - People per km2",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_1000_outliers_detected <- renderText({
        
        dens_1000_km2_deaths_outliers$administrative_area_level_1
    })
    
    output$density_1000_deaths_summary <- renderTable({
        
        dens_1000_deaths_summary
        
    })
    
    # Total population plots
    
    # less than 1 million people
    
    output$pop_1mil_confirmed <- renderPlot({
        
        case_plot <- ggplot(pop_subset_upto_1million) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1)) +
            labs(title="Total Country Population Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: Under 1 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$pop_1mil_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(pop_1mil_without_confirmed_outliers) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: Under 1 million Citizens - Outliers Removed", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_1mil_outliers_detected <- renderText({
        
        pop_1mil_confirmed_outliers$administrative_area_level_1
    })
    
    output$pop_1mil_confirmed_summary <- renderTable({
        
        pop_1mil_confirmed_summary
        
    })
    
    output$pop_1mil_deaths <- renderPlot({
        
        death_plot <- ggplot(pop_subset_upto_1million) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot", 
                 subtitle="Country Subset: Under 1 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$pop_1mil_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(pop_1mil_without_death_outliers) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: Under 1 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_1mil_outliers_detected <- renderText({
        
        pop_1mil_death_outliers$administrative_area_level_1
    })
    
    output$pop_1mil_deaths_summary <- renderTable({
        
        pop_1mil_deaths_summary
        
    })
    
    # between 1 and 10 million people
    
    output$pop_1_10mil_confirmed <- renderPlot({
        
        case_plot <- ggplot(pop_subset_between_1_10million) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 1 to 10 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$pop_1_10mil_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(pop_1_10mil_without_confirmed_outliers) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 1 to 10 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_1_10mil_outliers_detected <- renderText({
        
        pop_1_10mil_confirmed_outliers$administrative_area_level_1
    })
    
    output$pop_1_10mil_confirmed_summary <- renderTable({
        
        pop_1_10mil_confirmed_summary
        
    })
    
    output$pop_1_10mil_deaths <- renderPlot({
        
        death_plot <- ggplot(pop_subset_between_1_10million) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot", 
                 subtitle="Country Subset: 1 to 10 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$pop_1_10mil_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(pop_1_10mil_without_death_outliers) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 1 to 10 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_1_10mil_outliers_detected <- renderText({
        
        pop_1_10mil_death_outliers$administrative_area_level_1
    })
    
    output$pop_1_10mil_deaths_summary <- renderTable({
        
        pop_1_10mil_deaths_summary
        
    })
    
    
    # 10 to 50 million
    
    output$pop_10_50mil_confirmed <- renderPlot({
        
        case_plot <- ggplot(pop_subset_between_10_50million) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 10 to 50 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$pop_10_50mil_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(pop_10_50mil_without_confirmed_outliers) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 10 to 50 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_10_50mil_outliers_detected <- renderText({
        
        pop_10_50mil_confirmed_outliers$administrative_area_level_1
    })
    
    output$pop_10_50mil_confirmed_summary <- renderTable({
        
        pop_10_50mil_confirmed_summary
        
    })
    
    output$pop_10_50mil_deaths <- renderPlot({
        
        death_plot <- ggplot(pop_subset_between_10_50million) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1)) + 
            labs(title="Total Country Population Vs Deaths Boxplot", 
                 subtitle="Country Subset: 10 to 50 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$pop_10_50mil_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(pop_10_50mil_without_death_outliers) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 10 to 50 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_10_50mil_outliers_detected <- renderText({
        
        pop_10_50mil_death_outliers$administrative_area_level_1
    })
    
    output$pop_10_50mil_deaths_summary <- renderTable({
        
        pop_10_50mil_deaths_summary
        
    })
    
    
    # 50 to 100 million plots
    output$pop_50_100mil_confirmed <- renderPlot({
        
        case_plot <- ggplot(pop_subset_between_50_100million) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 50 to 100 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$pop_50_100mil_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(pop_50_100mil_without_confirmed_outliers) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 50 to 100 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_50_100mil_outliers_detected <- renderText({
        
        pop_50_100mil_confirmed_outliers$administrative_area_level_1
    })
    
    output$pop_50_100mil_confirmed_summary <- renderTable({
        
        pop_50_100mil_confirmed_summary
        
    })
    
    output$pop_50_100mil_deaths <- renderPlot({
        
        death_plot <- ggplot(pop_subset_between_50_100million) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot", 
                 subtitle="Country Subset: 50 to 100 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$pop_50_100mil_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(pop_50_100mil_without_death_outliers) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 50 to 100 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_50_100mil_outliers_detected <- renderText({
        
        pop_50_100mil_death_outliers$administrative_area_level_1
    })
    
    output$pop_50_100mil_deaths_summary <- renderTable({
        
        pop_50_100mil_deaths_summary
        
    })
    
    
    # 100-250 million 
    output$pop_100_250mil_confirmed <- renderPlot({
        
        case_plot <- ggplot(pop_subset_between_100_250million) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 100 to 250 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$pop_100_250mil_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(pop_100_250mil_without_confirmed_outliers) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 100 to 250 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
        
    })
    
    output$conf_100_250mil_outliers_detected <- renderText({
        
        pop_100_250mil_confirmed_outliers$administrative_area_level_1
    })
    
    output$pop_100_250mil_confirmed_summary <- renderTable({
        
        pop_100_250mil_confirmed_summary
        
    })
    
    output$pop_100_250mil_deaths <- renderPlot({
        
        death_plot <- ggplot(pop_subset_between_100_250million) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs ~Deaths Boxplot", 
                 subtitle="Country Subset: 100 to 250 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$pop_100_250mil_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(pop_100_250mil_without_death_outliers) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 100 to 250 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        plot(death_plot)
    })
    
    output$deaths_100_250mil_outliers_detected <- renderText({
        
        pop_100_250mil_death_outliers$administrative_area_level_1
    })
    
    output$pop_100_250mil_deaths_summary <- renderTable({
        
        pop_100_250mil_deaths_summary
        
    })
    
    # 250 to 1000 million
    
    output$pop_250_1000mil_confirmed <- renderPlot({
        
        case_plot <- ggplot(pop_subset_between_250_1000million) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: 250 to 1000 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        plot(case_plot)
    })
    
    output$pop_250_1000mil_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(pop_250_1000mil_without_confirmed_outliers) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 250 to 1000 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        
        plot(case_plot)
        
    })
    
    output$conf_250_1000mil_outliers_detected <- renderText({
        
        pop_250_1000mil_confirmed_outliers$administrative_area_level_1
    })
    
    output$pop_250_1000mil_confirmed_summary <- renderTable({
        
        pop_250_1000mil_confirmed_summary
        
    })
    
    output$pop_250_1000mil_deaths <- renderPlot({
        
        death_plot <- ggplot(pop_subset_between_250_1000million) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot", 
                 subtitle="Country Subset: 250 to 1000 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        
        plot(death_plot)
    })
    
    output$pop_250_1000mil_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(pop_250_1000mil_without_death_outliers) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 250 to 1000 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        
        plot(death_plot)
    })
    
    output$deaths_250_1000mil_outliers_detected <- renderText({
        
        pop_250_1000mil_death_outliers$administrative_area_level_1
    })
    
    output$pop_250_1000mil_deaths_summary <- renderTable({
        
        pop_250_1000mil_deaths_summary
        
    })
    
    
    # excess of 1 million people
    
    output$pop_1000mil_confirmed <- renderPlot({
        
        case_plot <- ggplot(pop_subset_excess_1000million) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot", 
                 subtitle="Country Subset: Over 1000 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        
        plot(case_plot)
    })
    
    output$pop_1000mil_confirmed_no_outliers <- renderPlot({
        
        case_plot <- ggplot(pop_1000mil_without_confirmed_outliers) +
            geom_boxplot(aes(population, confirmed))+
            geom_point(aes(population, confirmed, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Confirmed Cases Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 250 to 1000 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Cases",
                 col = "Country")
        
        
        plot(case_plot)
        
    })
    
    output$conf_1000mil_outliers_detected <- renderText({
        
        pop_1000mil_confirmed_outliers$administrative_area_level_1
    })
    
    output$pop_1000mil_confirmed_summary <- renderTable({
        
        pop_1000mil_confirmed_summary
        
    })
    
    output$pop_1000mil_deaths <- renderPlot({
        
        death_plot <- ggplot(pop_subset_excess_1000million) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot", 
                 subtitle="Country Subset: 250 to 1000 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        
        plot(death_plot)
    })
    
    output$pop_1000mil_deaths_no_outliers <- renderPlot({
        
        death_plot <- ggplot(pop_1000mil_without_death_outliers) +
            geom_boxplot(aes(population, deaths))+
            geom_point(aes(population, deaths, col = administrative_area_level_1))+
            labs(title="Total Country Population Vs Deaths Boxplot - Outliers Removed", 
                 subtitle="Country Subset: 250 to 1000 million Citizens", 
                 caption="Source: COVID-19",
                 x= "Population",
                 y="Confirmed Deaths",
                 col = "Country")
        
        
        
        plot(death_plot)
    })
    
    output$deaths_1000mil_outliers_detected <- renderText({
        
        pop_1000mil_death_outliers$administrative_area_level_1
    })
    
    output$pop_1000mil_deaths_summary <- renderTable({
        
        pop_1000mil_deaths_summary
        
    })
    
    #Source comparison outputs ----
    
    # Time series plot for Source Comparison tab
    output$source_timeseries <- renderPlotly({
        
        linetype = rep(c("solid", "dashed"))
        
        time_plot <- ggplot(source_global_timeseries_comp, aes(x=Date_reported))+
            geom_line(aes(y=WHO_cases), colour="green", linetype=1)+
            geom_line(aes(y=WHO_deaths), colour="green", linetype=2)+
            geom_line(aes(y=ECDC_cases), colour="blue", linetype=1)+
            geom_line(aes(y=ECDC_deaths), colour="blue", linetype=2)+
            geom_line(aes(y=JHU_cases), colour="purple", linetype=1)+
            geom_line(aes(y=JHU_deaths), colour="purple",linetype=2)+
            labs(title="Time Series Plot of Reported Figures from WHO, ECDC and JHU", 
                 caption="Source: WHO, ECDC, JHU",
                 x= "Date",
                 y="Reported Figures that Day")
        
        
        ggplotly(time_plot)  %>% config(displayModeBar = F)
        
    })
    output$global_totals <- renderTable({
        
        source_global_totals_comp
    })
    
    output$daily_case_summary_comparison<- renderTable({
        
        source_global_daily_case_summary
    })
    
    output$daily_death_summary_comparison<- renderTable({
        
        source_global_daily_death_summary
        
        
    })
    
    
    output$weekly_case_summary_comparison <- renderTable({
        
        source_global_weekly_case_summary
    })
    
    output$weekly_death_summary_comparison <- renderTable({
        
        source_global_weekly_death_summary
    })
    
    output$daily_case_sd_comparison <- renderTable({
        
        sd_daily_cases_comp
    })
    
    output$daily_death_sd_comparison <- renderTable({
        
        sd_daily_deaths_comp
    })
    
    output$weekly_case_sd_comparison <- renderTable({
        
        sd_weekly_cases_comp
    })
    
    output$weekly_death_sd_comparison <- renderTable({
        
        sd_weekly_deaths_comp
    })
    
    
}


shinyApp(ui = ui, server = server)
