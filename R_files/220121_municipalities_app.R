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

#'RETURNS FINAL ORGANISED DATA OF THE EPIDEMIOLOGICAL INDICATORS AGGREGATED TO THE MUNICIPALITY LEVEL

setwd("C:/Users/Oladapo Hassan/Desktop/GitHub/data_analysis") #çhange to your folder


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
#View(cases)


# Test code for the cummulative number of reported cases
cases %>% 
  count(Total_reported) %>%  
  mutate(cumsum = cumsum(Total_reported))

# Changing from factor to date format for the date column in the data
cases$Date_of_publication <- as.Date(cases$Date_of_publication, format = "%m/%d/%Y")


# To add population to the cases data
#cases_pop <- left_join(cases, population, by = c("Municipality_name" = "municipality"))
# To add population to the cases data
cases_pop <- left_join(cases, population, by = c("Municipality_code" = "GM_code"))


# Investigate if the RWZI name is equal to the municipality names in the wastewater & cases data respectively
#Obtaining the number of municipalities in the datasets
wastewater_municipalities <- wastewater %>% 
  select(RWZI_AWZI_name) %>% 
  group_by(RWZI_AWZI_name) %>% 
  distinct()

cases_municipalities <- cases %>% 
  group_by(Municipality_name) %>% 
  select(Municipality_name) %>% 
  distinct()


# Other epidemiological indicators population normalisation

cases_pop_normalised <- cases_pop %>%
  filter(!is.na(Total_reported)) %>%
  mutate(reported_norm = ((Total_reported/population)*100000))%>% 
  mutate(hospital_norm = ((Hospital_admission/population)*100000)) %>% 
  mutate(deceased_norm = ((Deceased/population)*100000))


#use first day in  the week as the newDate for analysis
cases_pop_normalised <- data.frame(
  cases_pop_normalised,
  cases_pop_normalised$Date_of_publication,
  PublicationDate = cut(cases_pop_normalised$Date_of_publication, "week"),
  stringsAsFactors = FALSE)

#aggregate to week
cases_pop_normalised <- cases_pop_normalised %>% 
  mutate(PublicationDate = as.Date(PublicationDate)) %>% 
  filter(!is.na(population)) %>% 
  group_by(PublicationDate, Municipality_code, population)%>% 
  summarise_if(is.numeric, sum)



#########################################
####LARGE TABLE FOR MUNICIPALITY ########
######################################

# Create table with the organised readings for all the municipalities
FINAL_MUNICIPALITY_TABLE = merge(wastewater_areas, cases_pop_normalised, by.x =c("date","region_municipality_code"),
                                 by.y=c("PublicationDate","Municipality_code") )


#write.csv(FINAL_MUNICIPALITY_TABLE, file = "Final_data_municipality.csv", row.names = FALSE)


##APP RUN VARIABLE
municpality_distinct<- left_join(cases_pop_normalised, wastewater_areas, by = c("Municipality_code"= "region_municipality_code") ) %>% 
  select(municipality_name, PublicationDate) %>%
    group_by(municipality_name, PublicationDate) %>% 
  distinct()


ui <- fluidPage(
  titlePanel(h2("Temporal Exploration of SARS-CoV-2 Wastewater concentration and other epidemiological indicators per municipality", align = "center")),
  sidebarLayout(    
    sidebarPanel(
      
      selectInput(inputId = "dataset",
                  label = "Choose a municipality:",
                 # choices = c("Potasyum", "Protein")
                 # choices = sort(x=names(table(unique(municpality_100ww$Municipality_name)))),
                choices = unique(municpality_distinct$municipality_name),
                  selected = "Losser"),
    dateRangeInput(inputId = "date",
                   strong("Date Range"),
                  # start = "2020-01-04", end = "2021-02-30",
                 #  min = "2020-02-04", max ="2021-02-27" ),
                 format = "yyyy-mm-dd", start=min(municpality_distinct$PublicationDate), end=max(municpality_distinct$PublicationDate))),
                 
                 
      mainPanel(
      plotOutput("cases_plot"),
      plotOutput("hospitaladmissions_plot"),
      plotOutput("deceased_plot"),
      verbatimTextOutput("summary"))))

server <- shinyServer(
  
  function(input,output){
    
    datasetInput <- reactive({
      # xx <- left_join(cases_pop_normalised, wastewater_areas, by = c("Municipality_name" = "municipality_name") )%>%
      # 
      #   group_by(Municipality_name) %>%
      #   mutate(RNA_value = RNA_value/10000000000000) %>%
      #   mutate(reported_norm = reported_norm/10) %>%
      #   filter(Municipality_name == input$dataset)%>%
      #   distinct()
      # xx<-xx %>%
      #   filter( PublicationDate >= input$date[1] & PublicationDate <= input$date[2])
 
     xx = merge(wastewater_areas, cases_pop_normalised, by.x =c("date","region_municipality_code"),by.y=c("PublicationDate","Municipality_code") ) %>% 
     group_by(municipality_name) %>% 
       mutate(RNA_value = RNA_value/10000000000000) %>% 
       mutate(reported_norm = reported_norm/10) %>% 
       filter(municipality_name == input$dataset)%>% 
       distinct()
     xx<-xx %>% 
       filter(date >= input$date[1] & date <= input$date[2])
     

    })
    

    # plot time series
    output$cases_plot <- renderPlot({
     # coeff <- 100
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
          
          #breaks = seq(0,10, by =1)
          sec.axis = sec_axis(~./10, name = "Reported RNA/100.000")
        ) +
        
      
        scale_x_date(date_breaks = "1 week"
                     #,
                     #limits = as.Date(c(min(dataset$newDate), max(dataset$newDate))  )
                     ) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
              axis.title.y = element_text(color = ReportedColor, size=10),
              axis.title.y.right = element_text(color = wastewaterColor, size=10),
              axis.line = element_line(colour = "gray")
           # ,
           # panel.grid.major = element_blank(),
          #  panel.grid.minor = element_blank(),
          #  panel.background = element_blank()
            ) + 
        labs(x="Date")+
        ggtitle("Reported Confirmed Cases and Wastewater RNA")
      
    })
 



output$hospitaladmissions_plot<- renderPlot({
  # coeff <- 100
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
      
      #breaks = seq(0,10, by =1)
      sec.axis = sec_axis(~./4, name = "Reported RNA/100.000")
    ) +
    
    
    scale_x_date(date_breaks = "1 week"
                 #,
                 #limits = as.Date(c('2020-08-24', '2021-02-02')  )
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
  # coeff <- 100
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
      
      #breaks = seq(0,10, by =1)
      sec.axis = sec_axis(~./4, name = "Reported RNA/100.000")
    ) +
    
    
    scale_x_date(date_breaks = "1 week"
                 #,
                 #limits = as.Date(c('2020-08-24', '2021-02-02')  )
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

