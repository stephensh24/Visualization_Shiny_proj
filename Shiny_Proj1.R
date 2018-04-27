tanz = read.csv("Pipe_ind.csv")
Pipe_labels = read.csv("Pipe_labels.csv")
tanz = inner_join(tanz,Pipe_labels, by = "id")
#setwd("C:/Users/Stephen/Desktop/git_proj/shiny_proj")
head(tanz)
library(dplyr)
library(ggplot2)


#Data Clean
#-------------------------------------------------------------------------------------------
#longitude

mean(tanz$longitude)

#tanz[tanz$longitude == 0,]$longitude = mean(tanz$longitude)

tanz %>% filter(amount_tsh != 0, region == "Mwanza") %>% summarise(avg = mean(amount_tsh))


# Converting Longitude into Regional mean (There were 1812 total. 807 in Mwanza, 1005 in Shinyanga)
tanz = tanz %>% mutate(longitude = ifelse(longitude == 0 & region == "Mwanza",33.09156, longitude))
tanz = tanz %>% mutate(longitude = ifelse(longitude == 0 & region == "Shinyanga",33.24012, longitude))



#GPS Height

#Morogoro
#Mtwara
#Pwani
#Dar es Salaam

#tanz %>% filter(gps_height != 0, region == "Lindi") %>% summarise(avg = mean(gps_height))

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



#latitude---------------------------------------------------

tanz %>% filter(latitude > -1, region == "Kagera") %>% summarise(avg = mean(amount_tsh))

tanz %>% filter(latitude <= -1, region == "Kagera") %>% summarise(avg = mean(latitude))


# Converting Longitude into Regional mean (There were 1812 total. 807 in Mwanza, 1005 in Shinyanga)
tanz = tanz %>% mutate(latitude = ifelse(latitude  > -1 & region == "Mwanza",-2.620502, latitude))
tanz = tanz %>% mutate(latitude = ifelse(latitude > -1 & region == "Shinyanga",-3.495696, latitude))
tanz = tanz %>% mutate(latitude = ifelse(latitude  > -1 & region == "Kagera",-1.963501, latitude))




#Remove duplicate rows ----------------------------------
tanz$payment_type = NULL
tanz$waterpoint_type_group = NULL
tanz$extraction_type_group = NULL
tanz$source = NULL
tanz = tanz %>% rename(source = source_type)
tanz$quality_group = NULL
tanz$quantity_group = NULL
tanz$management_group = NULL
tanz$extraction_type = NULL
tanz = tanz %>% rename(extraction_type = extraction_type_class)
tanz$num_private = NULL


colnames(tanz)

#-------------------------------------------------------

head(tanz)
ggplot(tanz, aes(x = construction_year))+
  geom_freqpoly(aes(color=status_group),bins=200)+coord_cartesian(xlim = c(1940,2013))


c_basin <- arrange(summarise(group_by(tanz, basin), 
                             TotalWells = length(unique(id)) ), desc(TotalWells) )
head(c_basin, n = 10)

length(tanz$amount_tsh[tanz$amount_tsh == 0])

sum(tanz$construction_year == 0)

summary(tanz$region)

ggplot(tanz, aes(x = basin))+ geom_bar(aes(fill=status_group),
                                       position = "dodge")


ggplot(tanz, aes(x = region ,y = population))+ geom_bar(stat = "identity")


ggplot(tanz, aes(x = waterpoint_type))+ geom_bar(aes(fill=status_group),
                                        position = "dodge")

colnames(tanz)


atsh_10k = tanz$amount_tsh[tanz$amount_tsh > 1000 & tanz$amount_tsh <= 10000]
length(atsh_10k)


tanz2 = group_by(knicks, season) %>%
  summarise(ratio=sum(win=="W")/n())


ggplot() +hist(tanz$gps_height, col = "yellow")
length(tanz$gps_height[tanz$gps_height < 0])        