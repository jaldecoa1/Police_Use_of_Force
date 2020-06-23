library(readr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(tidyverse)
library(lubridate)
library(viridis)
library(flexdashboard)
library(plotly)
library(knitr)

#############
############

#Import datasets

#############
#############

#Import relational data
Incident_2018 <- read_csv(url("https://data-openjustice.doj.ca.gov/sites/default/files/dataset/2019-07/URSUS_Incident_2018.csv"))
Incident_2017 <- read_csv(url("https://data-openjustice.doj.ca.gov/sites/default/files/dataset/2018-08/URSUS_Incident_2017.csv"))
Incident_2016 <- read_csv(url("https://data-openjustice.doj.ca.gov/sites/default/files/dataset/2018-08/URSUS_Incident_2016.csv"))

#############
#############

#Clean up Incident_2016 dataset before binding datasets

#############
#############

#Ensure variable names are the same
Incident_2016 <- rename(Incident_2016,
                        "INCIDENT_ID" = "Incident_ID",
                        "ORI" = "ORI",
                        "INCIDENT_DATE_STR" = "Incident_Date_Str",
                        "INCIDENT_TIME_STR" = "Incident_Time_Str",
                        "CITY" = "City",
                        "COUNTY" = "County",
                        "STATE" = "State",
                        "ZIP CODE" = "Zip_Code",
                        "MULTIPLE_LOCATIONS" = "Multiple_Locations",
                        "ON_K12_CAMPUS" = "On_K12_Campus",
                        "ARREST_MADE" = "Arrest_Made",
                        "CRIME_REPORT_FILED" = "Crime_Report_Filed",
                        "CONTACT_REASON" = "Contact_Reason",
                        "IN_CUSTODY_REASON" = "In_Custody_Reason",
                        "NUM_INVOLVED_CIVILIANS" = "Num_Involved_Civilians",
                        "NUM_INVOLVED_OFFICERS" = "Num_Involved_Officers")

Incident_2016 <- Incident_2016 %>%
        mutate(INCIDENT_TIME_STR = as.hms(Incident_2016$INCIDENT_TIME_STR))


##################
##################

#Bind datsets

##################
##################

Incident_data <- Incident_2018 %>%
        bind_rows(Incident_2017)

Incident_data <- Incident_data %>%
        bind_rows(Incident_2016)

##################
##################

#Exploratory data analysis

##################
##################

#Convert INCIDENT_DATE_STR to a date variable
Incident_data <- Incident_data %>%
        mutate(INCIDENT_DATE_STR = mdy(INCIDENT_DATE_STR))

#Parse the date variable
Incident_data <- Incident_data %>%
        tibble(date = INCIDENT_DATE_STR,
               year = year(INCIDENT_DATE_STR),
               month = month(INCIDENT_DATE_STR),
               day = day(INCIDENT_DATE_STR))

#Tabulate and plot incident counts by year
Incident_data %>%
        count(year)

ggplot(Incident_data) +
        geom_bar(mapping = aes(x = year), fill = "blue")

##################
#################

#Segment data by COUNTY
Incident_data <- Incident_data %>%
        mutate(COUNTY = recode(COUNTY, 
                               "ALAMEDA" = "Alameda County",
                               "BUTTE" = "Butte County",
                               "CALAVERAS" = "Calaveras County",
                               "CONTRA COSTA" = "Contra Costa County",
                               "DEL NORTE" = "Del Norte County",
                               "EL DORADO" = "El Dorado County",
                               "FRESNO" = "Fresno County",
                               "GLENN" = "Glenn County",
                               "HUMBOLDT" = "Humboldt County",
                               "IMPERIAL" = "Imperial County",
                               "KERN" = "Kern County",
                               "KINGS" = "Kings County",
                               "LAKE" = "Lake County",
                               "LASSEN" = "Lassen County",
                               "LOS ANGELES" = "Los Angeles County",
                               "MADERA" = "Madera County",
                               "MARIN" = "Marin County",
                               "MARIPOSA" = "Mariposa County",
                               "MENDOCINO" = "Mendocino County",
                               "MERCED" = "Merced County",
                               "MONTEREY" = "Monterey County",
                               "NAPA" = "Napa County",
                               "NEVADA" = "Nevada County",
                               "ORANGE" = "Orange County",
                               "PLACER" = "Placer County",
                               "PLUMAS" = "Plumas County",
                               "RIVERSIDE" = "Riverside County",
                               "SACRAMENTO" = "Sacramento County",
                               "SAN BERNARDINO" = "San Bernardino County",
                               "SAN DIEGO" = "San Diego County",
                               "SAN FRANCISCO" = "San Francisco County",
                               "SAN JOAQUIN" = "San Joaquin County",
                               "SAN LUIS OBISPO" = "San Luis Obispo County",
                               "SAN MATEO" = "San Mateo County",
                               "SANTA BARBARA" = "Santa Barbara County",
                               "SANTA CLARA" = "Santa Clara County",
                               "SANTA CRUZ" = "Santa Cruz County",
                               "SHASTA" = "Shasta County",
                               "SISKIYOU" = "Siskiyou County",
                               "SOLANO" = "Solano County",
                               "SONOMA" = "Sonoma County",
                               "STANISLAUS" = "Stanislaus County",
                               "SUTTER" = "Sutter County",
                               "TEHAMA" = "Tehama County",
                               "TRINITY" = "Trinity County",
                               "TULARE" = "Tulare County",
                               "TUOLUMNE" = "Tuolumne County",
                               "VENTURA" = "Ventura County",
                               "YOLO" = "Yolo County",
                               "YUBA" = "Yuba County"))

#Tabulate and plot segmented data
Incident_data %>%
        group_by(year, COUNTY) %>%
        summarize(n = n()) %>%
        spread(year, n) %>%
        kable()

ggplot(Incident_data) +
        geom_bar(mapping = aes(x = year)) +
        facet_wrap(~ COUNTY)

#Sort and select top 15 counties by number of incidents
county_sort <- Incident_data %>%
        count(COUNTY, sort = TRUE) %>%
        head(15)

county_sort <- Incident_data %>%
        filter(COUNTY %in% county_sort$COUNTY)

#Plot top 15 counties
ggplot(county_sort) +
        geom_bar(mapping = aes(x = year)) +
        facet_wrap(~ COUNTY)

##################
##################

#Mapping Use of Force Cases

##################
##################

#Generate map_data and subset for California counties
counties <- map_data("county")

ca_counties <- counties %>%
        filter(region == "california")

ca_base <- ggplot(data = ca_counties, mapping = aes(x = long, y = lat, group = group)) +
        coord_quickmap() +
        geom_polygon(color = "black", fill = "grey")

ca_base <- ca_base + theme_void() +
        geom_polygon(data = ca_counties, fill = NA, color = "white") +
        geom_polygon(color = "white", fill = NA)

forcebycounty <- Incident_data %>%
        count(COUNTY)

#Clean up variable names and values
forcebycounty <- forcebycounty %>%
        mutate(COUNTY = recode(COUNTY, 
                               "Alameda County" = "alameda",
                               "Butte County" = "butte",
                               "Calaveras County" = "calaveras",
                               "Contra Costa County" = "contra costa",
                               "Del Norte County" = "del norte",
                               "El Dorado County" = "el dorado",
                               "Fresno County" = "fresno",
                               "Glenn County" = "glenn",
                               "Humboldt County" = "humboldt",
                               "Imperial County" = "imperial",
                               "Kern County" = "kern",
                               "Kings County" = "kings",
                               "Lake County" = "lake",
                               "Lassen County" = "lassen",
                               "Los Angeles County" = "los angeles",
                               "Madera County" = "madera",
                               "Marin County" = "marin",
                               "Mariposa County" = "mariposa",
                               "Mendocino County" = "mendocino",
                               "Merced County" = "merced",
                               "Monterey County" = "monterey",
                               "Napa County" = "napa",
                               "Nevada County" = "nevada",
                               "Orange County" = "orange",
                               "Placer County" = "placer",
                               "Plumas County" = "plumas",
                               "Riverside County" = "riverside",
                               "Sacramento County" = "sacramento",
                               "San Bernardino County" = "san bernardino",
                               "San Diego County" = "san diego",
                               "San Francisco County" = "san francisco",
                               "San Joaquin County" = "san joaquin",
                               "San Luis Obispo County" = "san luis obispo",
                               "San Mateo County" = "san mateo",
                               "Santa Barbara County" = "santa barbara",
                               "Santa Clara County" = "santa clara",
                               "Santa Cruz County" = "santa cruz",
                               "Shasta County" = "shasta",
                               "Siskiyou County" = "siskiyou",
                               "Solano County" = "solano",
                               "Sonoma County" = "sonoma",
                               "Stanislaus County" = "stanislaus",
                               "Sutter County" = "sutter",
                               "Tehama County" = "tehama",
                               "Trinity County" = "trinity",
                               "Tulare County" = "tulare",
                               "Tuolumne County" = "tuolumne",
                               "Ventura County" = "ventura",
                               "Yolo County" = "yolo",
                               "Yuba County" = "yuba"))

forcebycounty <- mutate_each(forcebycounty, funs(tolower))

#Merge UoF data with map data
caforce <- left_join(ca_counties, forcebycounty, by = c("subregion" = "COUNTY"))

caforce <- tibble(caforce)

#Convert UoF data to numeric values to plot data
caforce <- caforce %>%
        mutate(n = as.numeric(n))

#Generate map
hotforce <- ca_base +
        geom_polygon(data = caforce, aes(fill = n), color = "white") + 
        geom_polygon(color = "black", fill = NA) +
        theme_void()

#Make map visually accessible
hotforce <- hotforce +
        scale_fill_viridis(breaks = c(100, 200, 300, 400, 500))

#Make map interactive
ggplotly(hotforce)








