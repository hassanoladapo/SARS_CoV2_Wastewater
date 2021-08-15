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


#'RETURNS FINAL ORGANISED DATA OF THE EPIDEMIOLOGICAL INDICATORS AGGREGATED TO THE SAFETY REGION LEVEL


setwd("C:/Users/Oladapo Hassan/Desktop/GitHub/data_analysis") #ÇHANGE TO YOUR WORKING FOLDER

# Read the datasets
cases <- read.csv("./data_input/COVID-19_cases_per_day_new.csv")
wastewater <- read.csv("./data_input/COVID-19_rioolwaterdata_new.csv")
population <- read.csv("./data_input/muni_pop_new.csv") 
wwtp_catchments <-read.csv("./data_input/wwtp_catchments.csv")



###########################################
#Wastewater Data cleaning and organisation
##########################################3


wastewater <- wastewater[!is.na(wastewater$RNA_flow_per_100000),]

wastewater$date <- as.Date(wastewater$Date_measurement, format = "%m/%d/%Y")

wastewater$week <- strftime(wastewater$date, format = "%V")



#use first in a the week as the newDate file for analysis
wastewater <- data.frame(wastewater, wastewater$date,
                newDate = cut(wastewater$date, "week"),
                stringsAsFactors = FALSE)

colna <- colnames(wwtp_catchments)
colna <- colna[4:length(colna)]

hh <- data.frame(unique(wastewater$week))

for (j in colna){
  #print(j)
  g <- wwtp_catchments[!is.na(wwtp_catchments[,j]), c("Code_WWTP", j)]
  cc <- merge(x=wastewater[wastewater$RWZI_AWZI_code %in% g$Code_WWTP,], y=g, by.x="RWZI_AWZI_code", by.y= "Code_WWTP")
  kk <- data.frame(unique(cc$week), unique(cc$newDate))
  me <- c()
  for (i in unique(cc$week)) {
    data <- cc[cc$week==i,]
    me <- c(me, sum(data$RNA_flow_per_100000 * (data[,j]/100)) / length(data$RWZI_AWZI_code))
  }
  kk[,j] <- me
  hh <- merge(x=hh, y=kk, by.x="unique.wastewater.week.", by.y="unique.cc.week.",all.x=TRUE)
  hh <- hh[, !duplicated(colnames(hh))]
  
}
hh <- hh %>% dplyr::rename(date = unique.cc.newDate..x) %>% 
  select(-unique.cc.newDate..y, -unique.cc.newDate.)%>%
  arrange(date)

#write.csv(hh, file = "WWTP_test.csv", row.names = FALSE)


Transformedwastewater <- cbind(hh[1:2],stack(hh[-1])) #convert VR and GM to rows

Transformedwastewater <- Transformedwastewater %>%  dplyr::rename(region_municipality_code = ind) %>%  dplyr::rename(RNA_value = values)%>%  dplyr::rename(weeknumber = unique.wastewater.week.) %>% 
                          mutate(date = as.Date(date))

  
# Import the data containing municipality names and the GM codes of the municipalities
municipality_names <-read.csv("./data_input/municipality_GM_join.csv") #return municipality name with relevant GM code


wastewater_areas <- left_join(Transformedwastewater, municipality_names, 
                                     by = c("region_municipality_code"= "GM_name"))  #join municipality name to catchment


#######################################################
# Other epidemiological data cleaning and organisation 
#######################################################

#remove cases with no municipality name)
cases <- cases %>% 
   filter(Municipality_name != "")



# Test code for the cummulative number of reported cases
cases %>% 
  count(Total_reported) %>%  
  mutate(cumsum = cumsum(Total_reported))

# Changing from factor to date format for the date column in the data
cases$Date_of_publication <- as.Date(cases$Date_of_publication, format = "%m/%d/%Y")


# To add population to the cases data
cases_pop <- left_join(cases, population, by = c("Municipality_code" = "GM_code"))


#Generate the population per safety region
cases_pop_region <- cases_pop %>% group_by(Security_region_code) %>% 
  summarise(sum_population = sum(population)) %>% 
  filter(Security_region_code != "")

cases_pop_region <- left_join (cases_pop, cases_pop_region, by = "Security_region_code")%>%  #'join back to old dataframe to include summed population value
  filter(Security_region_code != "")

# Investigate if the RWZI name is equal to the municipality names in the wastewater & cases data respectively
#Obtaining the number of municipalities in the datasets
wastewater_municipalities <- wastewater %>% 
  select(RWZI_AWZI_name) %>% 
  group_by(RWZI_AWZI_name) %>% 
  distinct()

cases_municipalities <- cases_pop_region %>% 
  group_by(Municipality_name) %>% 
  select(Municipality_name) %>% 
  distinct()



# To identify the rows that exist in wastewater data and but not in cases, the strategies below can be used:
municipality_exist <- cases_municipalities[is.na(match(cases_municipalities$Municipality_name,
                                                       wastewater_municipalities$RWZI_AWZI_name)),]

municipality_exist <- anti_join(wastewater_municipalities, 
                                cases_municipalities, 
                                by = c('RWZI_AWZI_name'='Municipality_name'))


# Other epidemiological indicators population normalisation

cases_pop_normalised <- cases_pop_region %>%
  filter(!is.na(Total_reported)) %>%
  mutate(reported_norm = ((Total_reported/sum_population)*100000))%>% 
  mutate(hospital_norm = ((Hospital_admission/sum_population)*100000)) %>% 
  mutate(deceased_norm = ((Deceased/sum_population)*100000))


#use first day in  the week as the newDate for analysis
cases_pop_normalised <- data.frame(
  cases_pop_normalised,
  cases_pop_normalised$Date_of_publication,
  PublicationDate = cut(cases_pop_normalised$Date_of_publication, "week"),
  stringsAsFactors = FALSE)

#aggregate to week
cases_pop_normalised <- cases_pop_normalised %>% 
  mutate(PublicationDate = as.Date(PublicationDate)) %>% 
  filter(!is.na(sum_population)) %>% 
  group_by(PublicationDate, Security_region_name, Security_region_code, sum_population)%>% 
  summarise_if(is.numeric, sum)


#########################################
####LARGE TABLE FOR Safety region ########
######################################

#Table showing the organised indicators at different safety region
FINAL_REGION_TABLE = merge(wastewater_areas, cases_pop_normalised, by.x =c("date","region_municipality_code"),
                           by.y=c("PublicationDate","Security_region_code") )
  

#write.csv(FINAL_REGION_TABLE, file = "Final_data_region.csv", row.names = FALSE)



##APP RUN VARIABLE
region_distinct<- left_join(cases_pop_normalised, wastewater_areas, by = c("Security_region_code"= "region_municipality_code") ) %>% 
  select(Security_region_name, PublicationDate) %>%
    group_by(Security_region_name, PublicationDate) %>% 
  distinct()


region_distinct<- left_join(cases_pop_normalised, wastewater_areas, by = c("Security_region_code"= "region_municipality_code") ) %>% 
  select(Security_region_code, Security_region_name) %>%
  
  group_by(Security_region_code, Security_region_name) %>% 
  distinct()
View(region_distinct)


ui <- fluidPage(
  titlePanel(h2("Temporal Exploration of SARS-CoV-2 Wastewater concentration and other epidemiological indicators per region", align = "center")),
  sidebarLayout(    
    sidebarPanel(
      
      selectInput(inputId = "dataset",
                  label = "Choose a region:",
                choices = unique(region_distinct$Security_region_name),
                  selected = "Amsterdam-Amstelland"),
    dateRangeInput(inputId = "date",
                   strong("Date Range"),
                 format = "yyyy-mm-dd", start=min(region_distinct$PublicationDate), end=max(region_distinct$PublicationDate))),
                 
                 
      mainPanel(
      plotOutput("cases_plot"),
      plotOutput("hospitaladmissions_plot"),
      plotOutput("deceased_plot"),
      verbatimTextOutput("summary"))))

server <- shinyServer(
  
  function(input,output){
    
    datasetInput <- reactive({
    
 
     xx = merge(wastewater_areas, cases_pop_normalised, by.x =c("date","region_municipality_code"),by.y=c("PublicationDate","Security_region_code") ) %>% 
     group_by(Security_region_name) %>% 
       mutate(RNA_value = RNA_value/10000000000000) %>% 
       mutate(reported_norm = reported_norm*100) %>% 
       mutate(hospital_norm = hospital_norm*1000) %>% 
       mutate(deceased_norm = deceased_norm*10000) %>% 
       
       filter(municipality_name == input$dataset)%>% 
       distinct()
     xx<-xx %>% 
       filter(date >= input$date[1] & date <= input$date[2])
     
    })
    
    output$cases_plot <- renderPlot({
      ReportedColor <- "#3c5488ff"
      wastewaterColor <- "red"
      dataset <- datasetInput()

      dataset$newDate <- as.Date(dataset$date)
      datasetdate <- as.Date(dataset$date)

        
        ggplot(data = dataset, aes(x=dataset$date,y=dataset$reported_norm )) +
        geom_point(color=ReportedColor, size = 3 ) +
        geom_line(size=1, color=ReportedColor) +
        geom_line(aes(x=dataset$newDate, y=dataset$RNA_value*10), size=1, color=wastewaterColor) +
        geom_point(aes(x=dataset$newDate, y=dataset$RNA_value*10), size=3, color=wastewaterColor) +
          
        scale_y_continuous(
          name = "Positive confirmed cases/100.000",
          sec.axis = sec_axis(~./10, name = "Reported RNA/100.000")
        ) +
        
      
        scale_x_date(date_breaks = "1 week"
                     ) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
              axis.title.y = element_text(color = ReportedColor, size=10),
              axis.title.y.right = element_text(color = wastewaterColor, size=10),
              axis.line = element_line(colour = "gray")
            ) + 
        labs(x="Date")+
        ggtitle("Reported Confirmed Cases and Wastewater RNA")
      
    })
 



output$hospitaladmissions_plot<- renderPlot({
  HospitalColor <- "blue"
  wastewaterColor <- "red"
  dataset <- datasetInput()
  
  dataset$newDate<- as.Date(dataset$date)
  dataset$date <- as.Date(dataset$date)

  
  ggplot(data = dataset, aes(x=dataset$date,y=dataset$hospital_norm )) +
    geom_point(color=HospitalColor, size = 3 ) +

    geom_line(size=1, color=HospitalColor) +
    
    geom_line(aes(x=dataset$newDate, y=dataset$RNA_value*4), size=1, color=wastewaterColor) +
    geom_point(aes(x=dataset$newDate, y=dataset$RNA_value*4),color=wastewaterColor, size = 3 ) +
    
    scale_y_continuous(
      name = "Hospital admissions/100.000",
      sec.axis = sec_axis(~./4, name = "Reported RNA/100.000")
    ) +
    
    
    scale_x_date(date_breaks = "1 week"
    ) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
          axis.title.y = element_text(color = HospitalColor, size=10),
          axis.title.y.right = element_text(color = wastewaterColor, size=10),
          axis.line = element_line(colour = "gray")
          
    ) + 
    labs(x="Date")+
    ggtitle("Hospital Admissions and Wastewater RNA")
  
})




output$deceased_plot<- renderPlot({
  MortalityColor <- "black"
  wastewaterColor <- "red"
  dataset <- datasetInput()
  
  dataset$newDate<- as.Date(dataset$date)
  dataset$date <- as.Date(dataset$date)
  

    ggplot(data = dataset, aes(x=dataset$date,y=dataset$deceased_norm )) +
    geom_point(color=MortalityColor, size = 3 ) +
    geom_line(size=1, color=MortalityColor) +
    geom_line(aes(x=dataset$newDate, y=dataset$RNA_value*10), size=1, color=wastewaterColor) +
    geom_point(aes(x=dataset$newDate, y=dataset$RNA_value*10), size=3, color=wastewaterColor) +
    scale_y_continuous(
      name = "Mortality/100.000",
      
      sec.axis = sec_axis(~./4, name = "Reported RNA/100.000")
    ) +
    
    
    scale_x_date(date_breaks = "1 week"
    ) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
          axis.title.x = element_text(angle = 25, color="black", size=10, face=2),
          axis.title.y = element_text(color = MortalityColor, size=10),
          axis.title.y.right = element_text(color = wastewaterColor, size=10),
          axis.line = element_line(colour = "gray")
          
    ) + 
    labs(x="Date")+
    ggtitle("Mortality and Wastewater RNA")
  
})
})


app<-shinyApp(ui = ui, server = server)

