#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)
library(gridExtra)
library(dplyr)
source('plot.r')

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Packer WRs"),
    
    
    
    plotOutput("p1"),
    plotOutput("p2"),
    plotOutput("p3"),
    plotOutput("p4"),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    gnbWRData<-readRDS('wrData.rds')
    output$p1 <- renderPlot({
        #most yards
        top_yards<-gnbWRData %>%
            select(name, receiving_yards) %>%
            group_by(name) %>%
            summarise(value = sum(receiving_yards)) %>%
            arrange(desc(value))
        top_yards<-top_yards[1:6,]
        g1<-jplot(top_yards$name, top_yards$value, type="bar", 
                  x_label = "WR Name",y_label = "Receiving Yards",title="Most Receiving Yards")
        g1
    })
    output$p2 <- renderPlot({
        #most recieving TDs
        top_tds<-gnbWRData %>%
            select(name, receiving_touchdowns) %>%
            group_by(name) %>%
            summarise(value = sum(receiving_touchdowns)) %>%
            arrange(desc(value))
        top_tds<-top_tds[1:6,]
        g2<-jplot(top_tds$name, top_tds$value, type="bar",
                  x_label = "WR Name",y_label = "Receiving Tiddy's",title="Most Receiving Tiddy's")
        g2
    })
    output$p3 <- renderPlot({
        #top_years
        top_years_yards<-gnbWRData %>%
            select(name,year, receiving_yards) %>%
            group_by(name, year) %>%
            summarise(value = sum(receiving_yards)) %>%
            arrange(desc(value))
        
        top_years_yards<-top_years_yards[1:10,]
        
        g2<-jplot(paste(top_years_yards$name, " ",top_years_yards$year), top_years_yards$value, type="bar",
                  x_label = "Best years receiving yards",y_label = "Receiving yards",title="Best Single Year Receiving Yards")
        g2
    })
    output$p4 <- renderPlot({
        top_years_tds<-gnbWRData %>%
            select(name,year, receiving_touchdowns) %>%
            group_by(name, year) %>%
            summarise(value = sum(receiving_touchdowns)) %>%
            arrange(desc(value))
        
        top_years_tds<-top_years_tds[1:10,]
        
        g2<-jplot(paste(top_years_tds$name, " ",top_years_tds$year), top_years_tds$value, type="bar",
                  x_label = "Best years TIDDY's!!",y_label = "Receiving TITS!",title="Best Single Year MOST FAT ASS TITS")
        g2
    })
}

# Run the application 
shinyApp(ui = ui, server = server)