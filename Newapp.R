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
        selectInput(inputId ="classes_input", label="Select Class", choices=c("CRIMES_AGAINST_THE _PERSON", "CONTACT_RELATED_CRIMES","PROPERTY_RELATED_CRIMES","OTHER_SERIOUS_CRIMES","CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION","SUBCATEGORIES_OF_AGGRAVATED_ROBBERY"))
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
    #aldf2<-melt(wdf,id.vars=c("classes"), measure.vars = c("CRIME_CATEGORY","April_2006_to_March_2007","April_2007_to_March_2008","April_2008_to_March_2009","April_2009_to_March_2010","April_2010_to_March_2011","April_2011_to_March_2012","April_2012_to_March_2013","April_2013_to_March_2014","April_2014_to_March_2015","April_2015_to_March_2016"),
          #     variable.name="Years")
    
    aldf$value[which(is.na(aldf$value))]<-0
    
    
   # trial<- aldf %>%  group_by(classes,Years,value)
    #triial<-summarise(trial)
    aldf.agg<- aggregate(value~Years+classes,aldf,sum)
    #aldf.agg2<-aldf.agg %>% mutate(value=as.factor(value))
    
    #aldf.agg2$Years<-factor(aldf.agg2$Years,levels = sort(unique(aldf.agg2$value)))
    
 #   data_classes<-reactive({
 #       df_1<-aldf %>% filter(classes==input$classes_input) %>% 
 #           mutate(value=as.factor(value))
 #       #df_2<- na.omit(df_1)
 #       
 #       return(df_1)
 #   })
    
    data_classes<-reactive({
        df_1<-aldf.agg %>% filter(classes==input$classes_input) #%>% 
            #mutate(value=as.factor(value))
        #df_2<- na.omit(df_1)
        
        return(df_1)
    })
   # mutate(Years= fct_reorder(value, Years)) %>%
    
    output$crimesperclass<-renderPlot({
        data_classes() %>%
            ggplot(aes(x=reorder(Years,value),y=value,fill=Years))+
            geom_col()+labs(x=element_blank(),y="Total Crimes",
                            title = "Crimes Statistics per Class")+
            guides(fill=FALSE)+
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5))+
            coord_flip()
        
    })
}

shinyApp(ui = ui, server=server)