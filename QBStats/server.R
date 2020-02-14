#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)

# Define server logic required to draw a histogram


pullPasserRating<-function(qbData){
    #Pull yards per game data
    yards<-qbData %>%
        select(name, pri_color, sec_color, passing_rating) %>%
        group_by(name,pri_color, sec_color) %>%
        summarise(y=average(passing_rating),games=n())%>%
        filter(games>36)
        
    yards<-yards[order(-yards$y),]
    graphTitle<<-"Avg Passer Rating"
    yards
}

pullCompPct<-function(qbData){
    #Pull yards per game data
    yards<-qbData %>%
        select(name, pri_color, sec_color, fixed_passing_attempts,fixed_passing_completions) %>%
        group_by(name,pri_color, sec_color) %>%
        summarise(passing_attempts=sum(fixed_passing_attempts),passing_completions=sum(fixed_passing_completions)) %>%
        filter(passing_attempts>1500)
    yards$y=yards$passing_completions/yards$passing_attempts
    yards<-yards[order(-yards$y),]
    graphTitle<<-"Passing %"
    yards
}

pullGamesWon<-function(qbData){
    #Pull yards per game data
    yards<-qbData %>%
        select(name, pri_color, sec_color, game_won) %>%
        filter(game_won=="True") %>%
        group_by(name,pri_color, sec_color) %>%
        summarise(y=n())
    
    yards<-yards[order(-yards$y),]
    graphTitle<<-"Games Won"
    yards
}

pullPassingComplete<-function(qbData){
    #Pull yards per game data
    yards<-qbData %>%
        select(name, pri_color, sec_color, passing_completions) %>%
        group_by(name,pri_color, sec_color) %>%
        summarise(y=sum(passing_completions))
    
    yards<-yards[order(-yards$y),]
    graphTitle<<-"Passing Completions"
    yards
}

pullYardsPerGame<-function(qbData){
    #Pull yards per game data
    yards<-qbData %>%
        select(name, pri_color, sec_color, passing_yards) %>%
        group_by(name,pri_color, sec_color) %>%
        summarise(passing_yards=sum(passing_yards), num_games=n()) %>%
        filter(num_games >36)
    
    yards$y<-yards$passing_yards/yards$num_games
    
    yards<-yards[order(-yards$y),]
    graphTitle<<-"Yards per Game"
    yards
}

pullTotalYards<-function(qbData){
    #Pull total yards data
    yards<-qbData %>%
        select(name, pri_color, sec_color, passing_yards) %>%
        group_by(name,pri_color, sec_color) %>%
        summarise(passing_yards=sum(passing_yards), num_games=n()) %>%
        filter(num_games >36)
    
    yards$y<-yards$passing_yards
    
    yards<-yards[order(-yards$y),]
    graphTitle<<-"Total Yards"
    yards
}

pullTouchDowns<-function(qbData){
    #Pull total yards data
    yards<-qbData %>%
        select(name, pri_color, sec_color, passing_touchdowns) %>%
        group_by(name, pri_color, sec_color) %>%
        summarise(y=sum(passing_touchdowns), num_games=n()) %>%
        filter(num_games >36)
    
    yards<-yards[order(-yards$y),]
    graphTitle<<-"Touchdowns"
    yards
}

pullTouchDownsToInts<-function(qbData){
    #Pull total yards data
    yards<-qbData %>%
        select(name, pri_color, sec_color, passing_touchdowns,passing_interceptions) %>%
        group_by(name, pri_color, sec_color) %>%
        summarise(passing_touchdowns=sum(passing_touchdowns),passing_interceptions=sum(passing_interceptions), num_games=n()) %>%
        filter(num_games >36)
    yards$y<-yards$passing_touchdowns/yards$passing_interceptions
    
    yards<-yards[order(-yards$y),]
    graphTitle<<-"Touchdowns to Ints"
    yards
}

    
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
    #output$distPlot <- renderLeaflet({
        
        qbData<-readRDS("./data/qbData.rds")
        if (input$chartType=="TDtoINT") {
            yards<-pullTouchDownsToInts(qbData)
        } else if (input$chartType=="TotalTD"){
            yards<-pullTouchDowns(qbData)
        } else if (input$chartType=="YardsperGame"){
            yards<-pullYardsPerGame(qbData)
        } else if (input$chartType=="TotalYards"){
            yards<-pullTotalYards(qbData)
        } else if (input$chartType=="PassingComp"){
            yards<-pullPassingComplete(qbData)
        } else if (input$chartType=="GamesWon"){
            yards<-pullGamesWon(qbData)
        } else if (input$chartType=="CompPct"){
            yards<-pullCompPct(qbData)
        } else if (input$chartType=="PasserRating"){
            yards<-pullPasserRating(qbData)
        }
        
        g<-ggplot(
            data = yards[1:10,],
            mapping = aes(x=reorder(name,-y), 
                          y=y 
            )
        )+
            geom_bar(
                stat="identity",
                fill=yards$pri_color[1:10]
            )+
            theme(
                axis.title.x=element_text(size=14, color="#993333", face="bold"),
                axis.title.y=element_text(size=14, color="#993333", face="bold"),
                plot.title=element_text(size=14, color="red", face="bold",hjust=.5),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank()
            )+
            labs(
                title=graphTitle,x="QB Names",y=graphTitle
            )+
            geom_text(
                mapping=aes(label=name,hjust=1.1,vjust=.5),
                angle=90,size=8,colour=yards$sec_color[1:10]
            )+
            scale_colour_manual(
                values="white"
            )+
            coord_cartesian(
                ylim=c(
                    min(yards$y[1:30]),
                    max(yards$y[1:10]))
            )
        g


    })

})
