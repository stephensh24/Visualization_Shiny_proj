library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(googleVis)
library(DT)

tanz = read.csv("Pipe_ind.csv")
Pipe_labels = read.csv("Pipe_labels.csv")
tanz = inner_join(tanz,Pipe_labels, by = "id")


#dataset for gps_height histo graph. excludes all 0 values
tanz_histo = tanz %>% filter(gps_height != 0)
nrow(tanz_histo)


# Converting Longitude into Regional mean (There were 1812 total. 807 in Mwanza, 1005 in Shinyanga)
tanz = tanz %>% mutate(longitude = ifelse(longitude == 0 & region == "Mwanza",33.09156, longitude))
tanz = tanz %>% mutate(longitude = ifelse(longitude == 0 & region == "Shinyanga",33.24012, longitude))


# Convertin gps_height into regional mean
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Mwanza",1199.643, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Shinyanga",1350.982, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Tabora",1018.861, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Dodoma",1018.861, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Mbeya",1018.861, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Tanga",667.9148, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Kagera",1018.861, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Morogoro",405.2507, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Mtwara",258.4103, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Pwani",667.9148, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Dar es Salaam",31.22195, gps_height))
tanz = tanz %>% mutate(gps_height = ifelse(gps_height == 0 & region == "Lindi",213.4183, gps_height))



# Converting Latitude into Regional mean
tanz = tanz %>% mutate(latitude = ifelse(latitude  > -1 & region == "Mwanza",-2.620502, latitude))
tanz = tanz %>% mutate(latitude = ifelse(latitude > -1 & region == "Shinyanga",-3.495696, latitude))
tanz = tanz %>% mutate(latitude = ifelse(latitude  > -1 & region == "Kagera",-1.963501, latitude))



#Remove duplicate rows and rename ----------------------------------
tanz = tanz %>% select(-payment_type, -waterpoint_type_group, -extraction_type_group, -source, -quality_group, 
                -quantity_group, -management_group, -extraction_type, -num_private, -scheme_name) %>%
                rename(source = source_type, extraction_type = extraction_type_class)

# col names for selection dropdown --------------------
tanz_cols = tanz %>% select(region, basin, extraction_type, water_quality, waterpoint_type, payment, gps_height)
choice <- colnames(tanz_cols)

#selected rows --------------------------------
tanz_sel = tanz %>% select(region, status_group, gps_height, basin, extraction_type, payment,
                           water_quality, quantity, waterpoint_type, construction_year, longitude, latitude)


