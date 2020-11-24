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
        selectInput(inputId ="classes_input", label="Select:", choices=c("CRIMES_AGAINST_THE _PERSON", "CONTACT_RELATED_CRIMES", "PROPERTY_RELATED_CRIMES", "OTHER_SERIOUS_CRIMES", "CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION", "SUBCATEGORIES_OF_AGGRAVATED_ROBBERY")), 
        
<<<<<<< HEAD
        sliderInput("TotalCrimes", "Number of Crimes per year", 100,500,100)
=======
<<<<<<< HEAD
        sliderInput("TotalCrimes", "Number of Crimes per year", 100,500,0)
=======
        sliderInput("TotalCrimes", "Number of Crimes per year", 250,450,150)
>>>>>>> b6cc4b77b8bb199b9ae8611ad196be0b594e6538
>>>>>>> 524735bdc762f6748d362bc2ed0ef0a720450c37
    ),
    
    dashboardBody(
        column(width = 7,
               plotOutput("crimesperclass", height = 400)
        ),
<<<<<<< HEAD
        column(width = 5,
               plotOutput("PlotTCrimes", height = 400)
=======
        column(10,align="right",
               plotOutput("PlotTCrimes",width = "400px",height = "400px")
>>>>>>> 524735bdc762f6748d362bc2ed0ef0a720450c37
        )
    )
    
    
    #dashboardBody(
     #   fluidRow(box(plotOutput("crimesperclass",width = 600 ,height = 450)), box(plotOutput("PlotTCrimes",width = 400,height = 450))) 
    #)
    
#    dashboardBody(
 #       plotOutput("crimesperclass",width = 600 ,height = 450), plotOutput("PlotTCrimes",width = 350,height = 450)#,width = 600 ,height = 450),plotOutput("PlotTCrimes")#,width = 400,height = 450)
  #  )




    
    
     #dashboardBody(
        
      #  column(1, align="left",
       #        plotOutput("crimesperclass", width = 600,height = 400)
        #),
        #column(11,align="right",
        #       plotOutput("PlotTCrimes",width = 450,height = 400)
        #)
    #)
    
)
server <- function(input, output){

    wdf<-read.csv("crime_data.csv", sep=",")
    cl <- wdf$classes
    
<<<<<<< HEAD
    
    aldf<-melt(wdf,id.vars=c("classes","CRIME_CATEGORY"), measure.vars = c("April_2006_to_March_2007","April_2007_to_March_2008","April_2008_to_March_2009","April_2009_to_March_2010","April_2010_to_March_2011","April_2011_to_March_2012","April_2012_to_March_2013","April_2013_to_March_2014","April_2014_to_March_2015","April_2015_to_March_2016"),
               variable.name="Years")
    
    aldf$value[which(is.na(aldf$value))]<-0
    
    aldf.agg<- aggregate(value~Years+classes,aldf,sum)
    aldf.agg2<- aggregate(value~Years,aldf,sum)
    #Totals1<- aldf.agg%>% mutate(value=as.factor(value))
    #Totals2<- aldf.agg2%>% mutate(value=as.factor(value))
    
    data_classes<-reactive({
        df_1<-aldf.agg %>% filter(classes==input$classes_input)
=======
    noNA<-na.omit(wdf)
    
    
    
    aldf<-melt(wdf,id.vars=c("classes","CRIME_CATEGORY"), measure.vars = c("April_2006_to_March_2007","April_2007_to_March_2008","April_2008_to_March_2009","April_2009_to_March_2010","April_2010_to_March_2011","April_2011_to_March_2012","April_2012_to_March_2013","April_2013_to_March_2014","April_2014_to_March_2015","April_2015_to_March_2016"),
               variable.name="Years")
  
    
    aldf$value[which(is.na(aldf$value))]<-0
    
  
    aldf.agg<- aggregate(value~Years+classes,aldf,sum)

    

    
    data_classes<-reactive({
        df_1<-aldf.agg %>% filter(classes==input$classes_input) #%>% 
            
>>>>>>> 524735bdc762f6748d362bc2ed0ef0a720450c37
        
        return(df_1)
    })
    
    totalCrimes <- reactive({
<<<<<<< HEAD
        df_2<-aldf.agg2 %>% filter(value>input$TotalCrimes)#%>%  mutate(value=as.factor(value))
=======
>>>>>>> 524735bdc762f6748d362bc2ed0ef0a720450c37
        
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
        
        return(df_3)
        
    })
<<<<<<< HEAD
=======
   
>>>>>>> 524735bdc762f6748d362bc2ed0ef0a720450c37
    
    output$crimesperclass<-renderPlot({
        data_classes() %>%
            ggplot(aes(x=reorder(Years,value),y=value,fill=Years))+
            geom_col()+labs(x=element_blank(),y="Total Crimes",
                            title = "Crimes Statistics per Class")+
            guides(fill=FALSE)+
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5))+
            coord_flip()
        
<<<<<<< HEAD
    })#, width = 850, height = 550)
    
    output$PlotTCrimes <- renderPlot({
        totalCrimes() %>%
            ggplot(aes(x=reorder(Years,value),y=value, fill = Years))+
=======
<<<<<<< HEAD
    }, width = 700, height = 400)
=======
    })
>>>>>>> b6cc4b77b8bb199b9ae8611ad196be0b594e6538
    
    output$PlotTCrimes <- renderPlot({
    totalCrimes() %>% 
            ggplot(aes(x=reorder(years, totals),y= totals, fill= years ))+
>>>>>>> 524735bdc762f6748d362bc2ed0ef0a720450c37
            geom_col()+
            labs(x=element_blank(), y="Number of cases per year", title = "Total Crime Cases")+
            guides(fill= FALSE)+
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5))+
            coord_flip()
<<<<<<< HEAD
    })#,width = 650, height = 550)
=======
<<<<<<< HEAD
    },width = 500, height = 400)
}

shinyApp(ui, server)
=======
    })
>>>>>>> 524735bdc762f6748d362bc2ed0ef0a720450c37
}

shinyApp(ui = ui, server=server)




>>>>>>> b6cc4b77b8bb199b9ae8611ad196be0b594e6538
