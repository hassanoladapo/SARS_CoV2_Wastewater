# Libraries
library(ggplot2)
library(hrbrthemes)
#library('dplyr')
library(anytime)
library(stringr)
library(fuzzyjoin)
library(lubridate)
library(spatialEco)
library(shiny)
library(dplyr)
library(ggplot2)


#'RETURNS TEMPORAL TRENDS IN SARS-CoV-2 WASTEWATER DATA AND SOME EPIDEMIOLOGICAL INDICATORS AT NATIONA SCALE

setwd("C:/Users/Oladapo Hassan/Desktop/GitHub/data_analysis") #çhange to your folder

# Read the datasets
cases <- read.csv("./data_input/COVID-19_cases_per_day_new.csv")
wastewater <- read.csv("./data_input/COVID-19_rioolwaterdata_new.csv")
icu <- read.csv("./data_input/COVID-19_icu_admissions.csv")
nursing_home <- read.csv("./data_input/COVID-19_nursing_home.csv")
disabled_home <- read.csv("./data_input/COVID-19_disabled_home.csv")
above70 <- read.csv("./data_input/COVID-19_thuiswonend_70plus.csv")


############################################
#Wastewater Data cleaning and organisation
############################################

wastewater <- wastewater[!is.na(wastewater$RNA_flow_per_100000),]

wastewater$date <- as.Date(wastewater$Date_measurement, format = "%m/%d/%Y")

wastewater$week <- strftime(wastewater$date, format = "%V")



#Use the weeknumber to obtain the first day of the week
wastewater <- data.frame(wastewater, wastewater$date,
                         newDate = cut(wastewater$date, "week"),
                         stringsAsFactors = FALSE)

wastewater_new <- wastewater %>% select(Date_measurement, RWZI_AWZI_code, RWZI_AWZI_name, 
                                        RNA_flow_per_100000, date, week, newDate)


wastewater_weekly <- wastewater_new %>% 
  group_by(newDate, week)%>% 
  summarise(weekly_RNA = sum(RNA_flow_per_100000))


#################################################################
#Other epidemiological indicators Data cleaning and organisation
################################################################

#remove cases with no values for the epdemiological indicators
cases <- cases %>% 
  filter(Total_reported != "") %>% 
  filter(Hospital_admission != "") %>% 
  filter(Deceased != "")
#View(cases)

# Check unique municipality
muni_unique <- cases %>% 
  select(Municipality_name, Municipality_code) %>% 
  group_by(Municipality_name) %>% 
  distinct()



# Changing from factor to date format for the date column in the data
cases$Date_of_publication <- as.Date(cases$Date_of_publication, format = "%m/%d/%Y")
icu$Date_of_statistics <- as.Date(icu$Date_of_statistics, format = "%m/%d/%Y")
nursing_home$Date_of_statistic_reported <- as.Date(nursing_home$Date_of_statistic_reported, 
                                                   format = "%m/%d/%Y")
disabled_home$Date_of_statistic_reported <- as.Date(disabled_home$Date_of_statistic_reported, 
                                                   format = "%m/%d/%Y")
above70$Date_of_statistic_reported <- as.Date(above70$Date_of_statistic_reported, 
                                                    format = "%m/%d/%Y")


#use first day in  the week as the newDate for analysis
cases_new<- data.frame(
  cases,cases$Date_of_publication,
  PublicationDate = cut(cases$Date_of_publication, "week"),
  stringsAsFactors = FALSE)
#View(cases_new)

icu_new<- data.frame(
  icu,icu$Date_of_statistics,
  PublicationDate = cut(icu$Date_of_statistics, "week"),
  stringsAsFactors = FALSE)
#View(icu_new)

nursing_home_new<- data.frame(
  nursing_home, nursing_home$Date_of_statistic_reported,
  PublicationDate = cut(nursing_home$Date_of_statistic_reported, "week"),
  stringsAsFactors = FALSE)
#View(nursing_home_new)

disabled_home_new<- data.frame(
  disabled_home, disabled_home$Date_of_statistic_reported,
  PublicationDate = cut(disabled_home$Date_of_statistic_reported, "week"),
  stringsAsFactors = FALSE)
#View(disabled_home_new)

above70_new<- data.frame(
  above70, above70$Date_of_statistic_reported,
  PublicationDate = cut(above70$Date_of_statistic_reported, "week"),
  stringsAsFactors = FALSE)
#View(above70_new)

######################################################################
#aggregate to week and include the total population of the Netherlands
cases_weekly <- cases_new %>% 
  # filter(is.na(Total_reported)) %>% 
  group_by(PublicationDate)%>% 
  summarise(weekly_positive_cases = sum(Total_reported),weekly_hospital_admissions= sum(Hospital_admission),
            weekly_deceased = sum(Deceased)) %>% 
  mutate(population = 17280000)

icu_weekly <- icu_new %>% 
  group_by(PublicationDate)%>% 
  summarise(weekly_icu_cases = sum(IC_admission))%>% 
  mutate(population = 17280000)
#View(icu_weekly)

nursing_weekly <- nursing_home_new %>% 
  group_by(PublicationDate)%>% 
  summarise(weekly_nursing_cases = sum(Total_cases_reported))%>% 
  mutate(population = 17280000)
#View(nursing_weekly)

disabled_weekly <- disabled_home_new %>% 
  group_by(PublicationDate)%>% 
  summarise(weekly_disabled_cases = sum(Total_cases_reported))%>% 
  mutate(population = 17280000)
#View(disabled_weekly)

above70_weekly <- above70_new %>% 
  group_by(PublicationDate)%>% 
  summarise(weekly_above70_cases = sum(Total_cases_reported))%>% 
  mutate(population = 17280000)
View(above70_weekly)

############################################################################################
#normalise the epidemiological indicators values using the population per 100000 inhabitants
############################################################################################

cases_pop_normalised <- cases_weekly %>%
  mutate(reported_norm = ((weekly_positive_cases/population)*100000))%>% 
  mutate(hospital_norm = ((weekly_hospital_admissions/population)*100000)) %>% 
  mutate(deceased_norm = ((weekly_deceased/population)*100000))

icu_pop_normalised <- icu_weekly %>% 
  mutate(icu_norm = (weekly_icu_cases/population)*100000)

nursing_pop_normalised <- nursing_weekly %>% 
  mutate(nursing_norm = (weekly_nursing_cases/population)*100000)

disabled_pop_normalised <- disabled_weekly %>% 
  mutate(disabled_norm = (weekly_disabled_cases/population)*100000)

above70_pop_normalised <- above70_weekly %>% 
  mutate(above70_norm = (weekly_above70_cases/population)*100000)
View(above70_pop_normalised)

nursing_icu_national = merge(nursing_pop_normalised, icu_pop_normalised, by.x =c("PublicationDate"),
                             by.y=c("PublicationDate") )


nursing_icu_national = merge(nursing_icu_national, nursing_icu_national, by.x =c("PublicationDate"),
                             by.y=c("PublicationDate") )


nursing_icu_national = merge(nursing_icu_national, above70_pop_normalised, by.x =c("PublicationDate"),
                             by.y=c("PublicationDate") )


#Table for the wastewater data and the other epidemiological indicators 
wastewater_cases_national = merge(wastewater_weekly, cases_pop_normalised, by.x =c("newDate"),by.y=c("PublicationDate") )
#View(wastewater_cases_national) 

#Join the table for icu data to the other epidemiological indicators
wastewater_epi_national <- left_join(wastewater_cases_national, nursing_icu_national, by = c("newDate" = "PublicationDate"))
#View(wastewater_cases_national) 

wastewater_epi_national$newDate <-as.Date(wastewater_cases_national$newDate)


#write.csv(wastewater_cases_national, file = "Final_data_national.csv", row.names = FALSE)

wastewater_epi_new<- wastewater_epi_national %>% 
  filter(newDate >= "2020-09-07" & newDate <= "2021-06-14")

View(wastewater_epi_new)



###################################
#Positive cases and wastewater RNA
##################################
coeff <- 45000000000000
totalreportedColor <- "#2ca25f"
wastewaterColor <- "red"

ggplot(wastewater_epi_new, aes(x=newDate)) +
  geom_line(aes(y=reported_norm), size=1, color=totalreportedColor) +
  geom_line(aes(y=weekly_RNA/coeff), size=1, color=wastewaterColor) +
  scale_y_continuous(
    name = "Reported positive cases/100.000 imhabitants",
    sec.axis = sec_axis(~.*coeff, name = "Reported wastewater RNA/100.000")    
  ) +
  scale_x_date(date_breaks = "2 week") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(color = totalreportedColor, size=10),
        axis.title.y.right = element_text(color = wastewaterColor, size=10)) +
  labs(x="Date")
#  ggtitle("Trends in COVID-19 reported positive cases and wastewater RNA in the Netherlands") 



#ggsave("positivecases_wastewater.png", width = 8, height = 7, units = c("cm"), dpi = 200)

########################################
#Hospital admissions and wastewater RNA
#######################################
coeff <- 5000000000000000
hospitalColor <- rgb(0.2, 0.6, 0.9, 1)
wastewaterColor <- "red"

ggplot(wastewater_epi_new, aes(x=newDate)) +
  geom_line(aes(y=hospital_norm), size=1, color=hospitalColor) +
  geom_line(aes(y=weekly_RNA/coeff), size=1, color=wastewaterColor) +
  scale_y_continuous(
    name = "Hospital admissions/100.000 imhabitants",
    sec.axis = sec_axis(~.*coeff, name = "Reported wastewater RNA/100.000 inhabitants")    
  ) +
  scale_x_date(date_breaks = "2 week") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(color = hospitalColor, size=10),
        axis.title.y.right = element_text(color = wastewaterColor, size=10)) +
  labs(x="Date")
  #ggtitle("Trends in COVID-19 reported positive cases and wastewater RNA in the Netherlands") 



########################################
# Deceased and wastewater RNA
#######################################
coeff <- 5000000000000000
MortalityColor <- "black"
wastewaterColor <- "red"

ggplot(wastewater_epi_new, aes(x=newDate)) +
  geom_line(aes(y=deceased_norm), size=1, color=MortalityColor) +
  geom_line(aes(y=weekly_RNA/coeff), size=1, color=wastewaterColor) +
  scale_y_continuous(
    name = "Reported deceased/100.000 imhabitants",
    sec.axis = sec_axis(~.*coeff, name = "Reported wastewater RNA/100.000 inhabitants")    
  ) +
  scale_x_date(date_breaks = "2 week") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(color = MortalityColor, size=10),
        axis.title.y.right = element_text(color = wastewaterColor, size=10)) +
  labs(x="Date")
#ggtitle("Trends in COVID-19 reported positive cases and wastewater RNA in the Netherlands") 




########################################
# ICU admissions and wastewater RNA
#######################################
coeff <- 5000000000000000
icuColor <- "#756bb1"
wastewaterColor <- "red"

ggplot(wastewater_epi_new, aes(x=newDate)) +
  geom_line(aes(y=icu_norm), size=1, color=icuColor) +
  geom_line(aes(y=weekly_RNA/coeff), size=1, color=wastewaterColor) +
  scale_y_continuous(
    name = "ICU admissions/100.000 imhabitants",
    sec.axis = sec_axis(~.*coeff, name = "Reported wastewater RNA/100.000 inhabitants")    
  ) +
  scale_x_date(date_breaks = "2 week") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(color = icuColor, size=10),
        axis.title.y.right = element_text(color = wastewaterColor, size=10)) +
  labs(x="Date")
#ggtitle("Trends in COVID-19 reported positive cases and wastewater RNA in the Netherlands") 




################################################
#Nursing home positive cases and wastewater RNA
###############################################
coeff <- 1000000000000000
nursingColor <- "#E7B800"
wastewaterColor <- "red"

ggplot(wastewater_epi_new, aes(x=newDate)) +
  geom_line(aes(y=nursing_norm), size=1, color=nursingColor) +
  geom_line(aes(y=weekly_RNA/coeff), size=1, color=wastewaterColor) +
  scale_y_continuous(
    name = "Nursing home cases/100.000 imhabitants",
    sec.axis = sec_axis(~.*coeff, name = "Reported wastewater RNA/100.000 inhabitants")    
  ) +
  scale_x_date(date_breaks = "2 week") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(color = nursingColor, size=10),
        axis.title.y.right = element_text(color = wastewaterColor, size=10)) +
  labs(x="Date")
#ggtitle("Trends in COVID-19 reported positive cases and wastewater RNA in the Netherlands") 


######################################################
#Disabled care home positive cases and wastewater RNA
######################################################
coeff <- 5000000000000000
disabledColor <- "#feb24c"
wastewaterColor <- "red"

ggplot(wastewater_epi_new, aes(x=newDate)) +
  geom_line(aes(y=disabled_norm), size=1, color=disabledColor) +
  geom_line(aes(y=weekly_RNA/coeff), size=1, color=wastewaterColor) +
  scale_y_continuous(
    name = "Disabled care home cases/100.000 imhabitants",
    sec.axis = sec_axis(~.*coeff, name = "Reported wastewater RNA/100.000 inhabitants")    
  ) +
  scale_x_date(date_breaks = "2 week") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(color = disabledColor, size=10),
        axis.title.y.right = element_text(color = wastewaterColor, size=10)) +
  labs(x="Date")
#ggtitle("Trends in COVID-19 reported positive cases and wastewater RNA in the Netherlands") 


######################################################
#Above 70 positive cases and wastewater RNA
######################################################
coeff <- 500000000000000
above70Color <- "#3182bd"
wastewaterColor <- "red"

ggplot(wastewater_epi_new, aes(x=newDate)) +
  geom_line(aes(y=above70_norm), size=1, color=above70Color) +
  geom_line(aes(y=weekly_RNA/coeff), size=1, color=wastewaterColor) +
  scale_y_continuous(
    name = "People above  70 cases/100.000 imhabitants",
    sec.axis = sec_axis(~.*coeff, name = "Reported wastewater RNA/100.000 inhabitants")    
  ) +
  scale_x_date(date_breaks = "2 week") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(color = above70Color, size=10),
        axis.title.y.right = element_text(color = wastewaterColor, size=10)) +
  labs(x="Date")
#ggtitle("Trends in COVID-19 reported positive cases and wastewater RNA in the Netherlands") 
