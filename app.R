library(dplyr)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(DT)


ui<- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        selectInput(inputId ="classes_input", label="Select Classes", choices=c("CRIMES_AGAINST_THE _PERSON", "CONTACT_RELATED_CRIMES","PROPERTY_RELATED_CRIMES","OTHER_SERIOUS_CRIMES","CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION","SUBCATEGORIES_OF_AGGRAVATED_ROBBERY"))
    ),
    dashboardBody(
        
        column(width = 10,
               plotOutput("crimesperclass")
        )
    )
    
)
server <- function(input, output){

    wdf<-read.csv("crime_data.csv", sep = ",")

    #ldf<-tibble(melt(wdf,id.vars=c("classes","CRIME_CATEGORY"),variable.name="Years"))
   # aldf<-tibble(melt(wdf,id.vars=c("classes"),variable.name="cl"))
    aldf<-melt(wdf,id.vars=c("classes","CRIME_CATEGORY"), measure.vars = c("April_2006_to_March_2007","April_2007_to_March_2008","April_2008_to_March_2009","April_2009_to_March_2010","April_2010_to_March_2011","April_2011_to_March_2012","April_2012_to_March_2013","April_2013_to_March_2014","April_2014_to_March_2015","April_2015_to_March_2016"),
               variable.name="Years")
    
    aldf$value[which(is.na(aldf$value))]<-0
    
    data_classes<-reactive({
        df_1<-aldf %>% filter(classes==input$classes_input) %>% 
            mutate(value=as.factor(value))# Pclass=as.factor(Pclass))
        #df_2<- na.omit(df_1)
        
        return(df_1)
    })
    
    output$crimesperclass<-renderPlot({
        data_classes() %>% group_by(classes,CRIME_CATEGORY, Years) %>% count() %>% 
            mutate(n=as.factor(n)) %>% 
            ggplot(aes(x=Years,y=n))+
            geom_col()+labs(y="Total Crimes",
                            title = "Crimes Statistics per Class")+
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5))+
            coord_flip()
        
    })
}

shinyApp(ui = ui, server=server)