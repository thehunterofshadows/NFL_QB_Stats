#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#test
library(shiny)
library(shinydashboard)
library(leaflet)

input_values<-read.csv("./data/input_values.csv")
#input_values<-input_values[order(input_values$Order),]
input_names<-as.character(input_values[,4])
names(input_names)<-input_values[,2]

# Define UI for application that draws a histogram
#shinyUI(fluidPage(

shinyUI(dashboardPage(
  dashboardHeader(title="QB Stats"),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    tags$head(tags$script('
        // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 30;

        $("#plot").height(boxHeight);
        $("#distPlot").height(boxHeight - 20);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
      ')),
    fluidRow(
      box(id="plot",
          plotOutput("distPlot")),
      box(
        title="Controls",
        
        radioButtons("chart_type", 
                     "Chart Type: ",
                     c("Career Averages" = "avg",
                       "Top 5 Season Averages" = "top_five",
                       "Career Totals" = "total"
                       )),
        radioButtons("season_vs_playoffs", 
                     "Which games to include?: ",
                     c("Both" = "both",
                       "Regular Season" = "reg",
                       "Playoffs" = "playoffs"
                       )),
        
        
        selectInput("chart_option",
                    "Chart Type:",
                    input_names,
                    selected = 2
        ),
      )
    )
  )
))