library(dplyr)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(DT)


ui<- dashboardPage(
  dashboardHeader(title = "Crime Analytics"),
  dashboardSidebar(
    selectInput(inputId ="classes_input", label="Select Class:", choices=c("CRIMES_AGAINST_THE _PERSON", "CONTACT_RELATED_CRIMES", "PROPERTY_RELATED_CRIMES", "OTHER_SERIOUS_CRIMES", "CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION", "SUBCATEGORIES_OF_AGGRAVATED_ROBBERY")), 
    
    sliderInput("TotalCrimes", "Select minimum Cases:", 100,500,100)
  ),
  dashboardBody(
    column(width = 7,
           plotOutput("CrimesPerClass", height = 400)
    ),
    column(width = 5,
           plotOutput("PlotTCrimes", height = 400)
    ),
    DT::dataTableOutput("CrimeTable", width = "100%", height = "auto")
  )
  
)
server <- function(input, output){
  
  wdf<-read.csv("crime_data.csv", sep=",")  #storing the data into a wide data frame (wdf)
  cl <- wdf$classes
  wdf2<-wdf
  wdf2$X=NULL
  
  
  ldf<-melt(wdf2,id.vars=c("classes","CRIME_CATEGORY"), measure.vars = c("April_2006_to_March_2007","April_2007_to_March_2008","April_2008_to_March_2009","April_2009_to_March_2010","April_2010_to_March_2011","April_2011_to_March_2012","April_2012_to_March_2013","April_2013_to_March_2014","April_2014_to_March_2015","April_2015_to_March_2016"),
            variable.name="Years") #converting the wdf2 into a long data frame (ldf) to be able to plot the graphs
  
  
  ldf$value[which(is.na(ldf$value))]<-0  #changing NA values to 0
  
  
  ldf_agg<- aggregate(value~Years+classes,ldf,sum) #grouping the values by years and classes
  ldf_agg2<- aggregate(value~Years,ldf,sum) #because we need total values per year for the second graph,
  #we no more need it to be for both a specific year and class hence we're only calculating the values by year
  
  data_classes<-reactive({
    df_1<-ldf_agg %>% filter(classes==input$classes_input)
    
    return(df_1)
  })
  
  totalCrimes <- reactive({
    df_2<-ldf_agg2 %>% filter(value>input$TotalCrimes)
    
    return(df_2)
  })
  
  crimetable <- reactive({
    df_3 <- wdf2 %>% filter(classes==input$classes_input)
    
    return(df_3)
  })
  
  output$CrimesPerClass<-renderPlot({
    data_classes() %>%
      ggplot(aes(x=reorder(Years,value), y=value, fill=reorder(Years,value)))+
      geom_col()+
      labs(x=element_blank(),y="Total Crimes",title = "Crimes Statistics per Class")+
      guides(fill=FALSE)+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))+
      coord_flip()
    
  })
  
  output$PlotTCrimes <- renderPlot({
    totalCrimes() %>% 
      ggplot(aes(x=reorder(Years,value), y=value, fill=reorder(Years,value)))+
      geom_col()+
      labs(x=element_blank(), y="Number of cases per year", title = "Total Crime Cases")+
      guides(fill= FALSE)+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))+
      coord_flip()
  })
  output$CrimeTable = DT::renderDataTable({
    crimetable()
  })
}

shinyApp(ui, server)

