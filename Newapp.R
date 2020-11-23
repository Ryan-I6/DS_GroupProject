library(dplyr)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(DT)


ui <- dashboardPage(
    dashboardHeader(title = "Crime Analytics"),
    dashboardSidebar(
        selectInput(inputId ="classes_input", label="Select Class", choices=cl), 
        
        sliderInput("TotalCrimes", "Number of Crimes per year", 250,450,150)
    ),
    dashboardBody(
        
        column(1, align="left",
               plotOutput("crimesperclass", width = "500px",height = "400px")
        ),
        column(10,align="right",
               plotOutput("PlotTCrimes",width = "500px",height = "400px")
        )
    )
    
)
server <- function(input, output){

    wdf<-read.csv("crime_data.csv", sep = ",")
    cl <- wdf$classes
    
    noNA<-na.omit(wdf)
    
    
    
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
    
    totalCrimes <- reactive({
        
        total_06_07 <- sum(as.integer(noNA$April_2006_to_March_2007))
        total_07_08 <- sum(as.integer(noNA$April_2007_to_March_2008))
        total_08_09 <- sum(as.integer(noNA$April_2008_to_March_2009))
        total_09_10 <- sum(as.integer(noNA$April_2009_to_March_2010))
        total_10_11 <- sum(as.integer(noNA$April_2010_to_March_2011))
        total_11_12 <- sum(as.integer(noNA$April_2011_to_March_2012))
        total_12_13 <- sum(as.integer(noNA$April_2012_to_March_2013))
        total_13_14 <- sum(as.integer(noNA$April_2013_to_March_2014))
        total_14_15 <- sum(as.integer(noNA$April_2014_to_March_2015))
        total_15_16 <- sum(as.integer(noNA$April_2015_to_March_2016))
        
        YEARtotals<-c(total_06_07, total_07_08, total_08_09, total_09_10, total_10_11, total_11_12, total_12_13,total_13_14,total_14_15, total_15_16)
        
        df_2 <- data.frame(years=c("April_2006_to_March_2007","April_2007_to_March_2008","April_2008_to_March_2009","April_2009_to_March_2010","April_2010_to_March_2011","April_2011_to_March_2012","April_2012_to_March_2013","April_2013_to_March_2014","April_2014_to_March_2015","April_2015_to_March_2016"),
                           totals = YEARtotals
                           )
        df_3 <- df_2 %>% filter(totals>input$TotalCrimes)
        
        colours <- c(rep("pink"), rep("green"), rep("orange"), rep("red"), rep("blue"), rep("yellow"), rep("dark grey"), rep("light grey"), rep("light blue"), rep("dark green"))
        
        return(df_3)
        
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
    
    colours <- c(rep("pink",1 ), rep("green", 1), rep("orange",1), rep("red",1), rep("blue", 1), rep("yellow",1), rep("dark grey", 1), rep("light grey", 1), rep("light blue",1), rep("dark green",1))
    
    output$PlotTCrimes <- renderPlot({
    totalCrimes() %>% 
            ggplot(aes(x=reorder(years, totals),y= totals, fill= colours ))+
            geom_col()+
            labs(x=element_blank(), y="Number of cases per year", title = "Total Crime Cases")+
            guides(fill= FALSE)+
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5))+
            coord_flip()
    })
}

shinyApp(ui = ui, server=server)




