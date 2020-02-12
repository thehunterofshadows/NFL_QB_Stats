#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)

# Define UI for application that draws a histogram
#shinyUI(fluidPage(

shinyUI(dashboardPage(
    dashboardHeader(title="QB Stats"),
    dashboardSidebar(),
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
                title="Contorls",
                selectInput("chartType",
                            "Chart Type:",
                            c("Touchdowns to INTs" = "TDtoINT",
                              "Total Touchdowns" = "TotalTD")
                            
                )    
            )
        )

    # Application title
    #titlePanel("QB Stats"),

    # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         # sliderInput("bins",
    #         #             "Number of bins:",
    #         #             min = 1,
    #         #             max = 50,
    #         #             value = 30),
    #         # sliderInput("test",
    #         #             "testing thisNumber of bins:",
    #         #             min = 1,
    #         #             max = 50,
    #         #             value = 30),
    #         selectInput("chartType",
    #                     "Chart Type:",
    #                     c("Touchdowns to INTs" = "TDtoINT",
    #                       "Total Touchdowns" = "TotalTD")
    #             
    #         )
    #         
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         plotOutput("distPlot")
        )
    )
)
