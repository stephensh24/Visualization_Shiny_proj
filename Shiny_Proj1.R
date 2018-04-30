tanz = read.csv("Pipe_ind.csv")
Pipe_labels = read.csv("Pipe_labels.csv")
tanz = inner_join(tanz,Pipe_labels, by = "id")
#setwd("C:/Users/Stephen/Desktop/git_proj/shiny_proj")
head(tanz)
library(dplyr)
library(ggplot2)
library(plotly)


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

tanz_sel = tanz %>% select(status_group, id, gps_height, longitude, latitude, basin, region, subvillage, construction_year,
                extraction_type, payment, water_quality, quantity, waterpoint_type)
colnames(tanz)

#-------------------------------------------------------

head(tanz)
ggplot(tanz, aes(x = construction_year))+
  geom_freqpoly(aes(color=status_group),bins=200)+coord_cartesian(xlim = c(1940,2013))


c_basin <- arrange(summarise(group_by(tanz, basin), 
                             TotalWells = length(unique(id)) ), desc(TotalWells) )
head(c_basin, n = 10)
# 
# length(tanz$amount_tsh[tanz$amount_tsh == 0])
# 
# sum(tanz$construction_year == 0)
# 
# summary(tanz$region)

# ggplot(tanz, aes(x = basin))+ geom_bar(aes(fill=status_group),
#                                        position = "dodge")
# 
# 
# ggplot(tanz, aes(x = region ,y = population))+ geom_bar(stat = "identity")
# 
# 
# ggplot(tanz, aes(x = waterpoint_type))+ geom_bar(aes(fill=status_group),
#                                         position = "dodge")

colnames(tanz)


atsh_10k = tanz$amount_tsh[tanz$amount_tsh > 1000 & tanz$amount_tsh <= 10000]
length(atsh_10k)



ggplot() +geom_density(aes(x = tanz$status_group), fill = "yellow")
View(tanz)


## Making a bar chart of perc shared between function, non-function, etc.

tanz2 <- tanz %>% group_by(status_group) %>%
  summarise(count = n())
tanz2

status_group_sum <- sum(tanz2$count)
status_group_perc_graph <- tanz2 %>% mutate(perc = (count/status_group_sum))
status_group_perc_graph %>% ggplot(., aes(x= status_group, y = perc, fill = status_group)) + 
  geom_bar(stat = "identity")

#By basin
tanzb = tanz %>% group_by(basin,status_group) %>% summarise(count = n())
tanzb_perc_graph = tanzb %>% group_by(basin) %>% mutate(perc = count/sum(count))
tanzf = tanzb_perc_graph[tanzb$status_group == "non functional",]
tanzb_perc_graph[tanzb_perc_graph$status_group == "non functional",]


#### MEAN LINE
tanzf %>% ggplot(aes(x = basin, y = perc, fill = status_group)) +
  geom_bar(stat="identity","position" = "dodge") + geom_hline(aes(yintercept = mean(perc)))

tanzb_perc_graph %>% ggplot(aes(x = basin, y = perc, fill = status_group)) +
  geom_bar(stat="identity","position" = "dodge")


#Extraction type class
tanze = tanz %>% group_by(extraction_type,status_group) %>% summarise(count = n())
tanze_perc_graph = tanze %>% group_by(extraction_type) %>% mutate(perc = count/sum(count))
tanzg = tanze_perc_graph[tanze$status_group == "non functional",]
tanze_perc_graph[tanze_perc_graph$status_group == "non functional",]

tanzg %>% ggplot(aes(x = extraction_type, y = perc, fill = status_group)) +
  geom_bar(stat="identity","position" = "dodge") +  geom_hline(aes(yintercept = mean(perc)))

tanze_perc_graph %>% ggplot(aes(x = extraction_type, y = perc, fill = status_group)) +
  geom_bar(stat="identity","position" = "dodge")




#Density of functional pumps based on gps_height
tanz %>% ggplot(aes(x= gps_height, fill = status_group)) + geom_histogram(bins = 30)



#Region-----------------------------------
tanzR = tanz %>% group_by(region,status_group) %>% summarise(count = n()) %>%
  group_by(region) %>% mutate(perc = count/sum(count))
tanzR_perc_graph = tanzR %>% group_by(region) %>% mutate(perc = count/sum(count))
tanzG = tanzR_perc_graph[tanze$status_group == "non functional",]
tanzR_perc_graph[tanzR_perc_graph$status_group == "non functional",]

tanzG %>% ggplot(aes(x = region, y = perc, fill = status_group)) +
  geom_bar(stat="identity","position" = "dodge")

tanzR %>% ggplot(aes(x = region, y = perc, fill = status_group)) +
  geom_bar(stat="identity","position" = "dodge")



#Water quality------------------------------

tanzW = tanz %>% group_by(water_quality,status_group) %>% summarise(count = n()) %>%
  group_by(water_quality) %>% mutate(perc = count/sum(count))

tanzW %>% ggplot(aes(x = water_quality, y = perc, fill = status_group)) +
  geom_bar(stat="identity","position" = "dodge")


tanzW = tanz %>% group_by(payment,status_group) %>% summarise(count = n()) %>%
  group_by(payment) %>% mutate(perc = count/sum(count)) %>% 
  plot_ly(x = ~payment, y = ~status_group, type = 'bar', name = 'SF Zoo') %>%
  add_trace(y = ~status_group, name = 'LA Zoo') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')

tanzW

#-----------


#By waterpoint_type
tanzWP = tanz %>% group_by(waterpoint_type,status_group) %>% summarise(count = n())
tanzWP_perc_graph = tanzWP %>% group_by(waterpoint_type) %>% mutate(perc = count/sum(count))
tanzP = tanzWP_perc_graph[tanzb$status_group == "non functional",]
tanzWP_perc_graph[tanzb_perc_graph$status_group == "non functional",]

tanzP %>% ggplot(aes(x = waterpoint_type, y = perc, fill = status_group)) +
  geom_bar(stat="identity","position" = "dodge")

tanzWP_perc_graph %>% ggplot(aes(x = waterpoint_type, y = perc, fill = status_group)) +
  geom_bar(stat="identity","position" = "dodge")


#-------------------------

tanzfunc %>% ggplot(aes(x= basin, fill = status_group)) + geom_histogram(bins = 50, stat = "count")

tanzfunc = tanz %>% filter(status_group == "non functional")


str(tanz$status_group)

colnames(tanz)

tanz$source_class

tanz %>% ggplot(aes(x= water_quality, fill= status_group)) + geom_bar(position = "dodge")


#----------------

summary(tanz$region)


menuItem("Region Pump Funcionality", tabName = "data", icon = icon("bar-chart-o"))


#MAPING --------------------------------
install.packages("ggmap")
library(ggmap)
library(rworldmap)

newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(29, 41), ylim = c(-12, -1), asp = 1)
points(tanz$longitude, tanz$latitude, col = "blue", cex = .6)




summary(tanz$latitude)

?plot_ly()
