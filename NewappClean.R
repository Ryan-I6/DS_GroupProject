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
        selectInput(inputId ="classes_input", label="Select:", choices=c("CRIMES_AGAINST_THE _PERSON", "CONTACT_RELATED_CRIMES", "PROPERTY_RELATED_CRIMES", "OTHER_SERIOUS_CRIMES", "CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION", "SUBCATEGORIES_OF_AGGRAVATED_ROBBERY")), 
        
        sliderInput("TotalCrimes", "Number of Crimes per year", 100,500,0)
    ),
    dashboardBody(
        
        column(1, align="left",
               plotOutput("crimesperclass", width = "500px",height = "400px")
        ),
        column(10,align="right",
               plotOutput("PlotTCrimes",width = "400px",height = "400px")
        )
    )
    
)
server <- function(input, output){

    wdf<-read.csv("crime_data.csv", sep=",")
    cl <- wdf$classes
        
    
    aldf<-melt(wdf,id.vars=c("classes","CRIME_CATEGORY"), measure.vars = c("April_2006_to_March_2007","April_2007_to_March_2008","April_2008_to_March_2009","April_2009_to_March_2010","April_2010_to_March_2011","April_2011_to_March_2012","April_2012_to_March_2013","April_2013_to_March_2014","April_2014_to_March_2015","April_2015_to_March_2016"),
               variable.name="Years")
  
    
    aldf$value[which(is.na(aldf$value))]<-0
    
  
    aldf.agg<- aggregate(value~Years+classes,aldf,sum)

    

    
    data_classes<-reactive({
        df_1<-aldf.agg %>% filter(classes==input$classes_input) #%>% 
            
        
        return(df_1)
    })
    
    totalCrimes <- reactive({
        df_2<-aldf.agg %>% filter(value>input$TotalCrimes)
        
        return(df_2)
    })
   
    
    output$crimesperclass<-renderPlot({
        data_classes() %>%
            ggplot(aes(x=reorder(Years,value),y=value,fill=Years))+
            geom_col()+labs(x=element_blank(),y="Total Crimes",
                            title = "Crimes Statistics per Class")+
            guides(fill=FALSE)+
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5))+
            coord_flip()
        
    }, width = 700, height = 400)
    
    output$PlotTCrimes <- renderPlot({
        totalCrimes() %>% 
            ggplot(aes(x= reorder(Years, value),y=value, fill = Years))+
            geom_col()+
            labs(x=element_blank(), y="Number of cases per year", title = "Total Crime Cases")+
            guides(fill= FALSE)+
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5))+
            coord_flip()
    },width = 500, height = 400)
}

shinyApp(ui, server)
