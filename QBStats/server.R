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


form_data<-function(x, type, option){
    #constants
    min_games = 36
    top_years = 5
    stat_value = option
    if (type = "top_5") {
        top_yearsTF = TRUE
        stat_select = list(stat_value,"year","pri_color","sec_color")
        stat_group = list("year","pri_color","sec_color")
        noYear_stat_select = list(stat_value,"pri_color","sec_color")
        noYear_stat_group = list("pri_color","sec_color")
        summ <- paste0("mean(", stat_value, ')')  # construct summary method, e.g. mean(mpg)
        summ_name <- paste0('mean_', stat_value)
    } else if (type = "avg") {
        top_yearsTF = FALSE
        stat_select = list(stat_value,"year","pri_color","sec_color")
        stat_group = list("year","pri_color","sec_color")
        noYear_stat_select = list(stat_value,"pri_color","sec_color")
        noYear_stat_group = list("pri_color","sec_color")
        summ <- paste0("mean(", stat_value, ')')  # construct summary method, e.g. mean(mpg)
        summ_name <- paste0('mean_', stat_value)
    } else if (type = "total"){
        top_yearsTF = FALSE
        stat_select = list(stat_value,"year","pri_color","sec_color")
        stat_group = list("year","pri_color","sec_color")
        noYear_stat_select = list(stat_value,"pri_color","sec_color")
        noYear_stat_group = list("pri_color","sec_color")
        summ <- paste0("sum(", stat_value, ')')  # construct summary method, e.g. mean(mpg)
        summ_name <- paste0('sum_', stat_value)
    }
    min_top_games = 8
    
    
    
    #still trying to figure out how i'm going to do this
    #through it to iterate through each name and for each state pull up the data
    #then filter for top 5, then re-average or retotal
    #will probably need to create some of the other fields in the orginal data
    #such as compleation %.  the per game stuff, not sure how to handle that yet
    results<-data.table(
        name = character(),
        passer_rating = numeric(),
        years = character()
    )
    namesList<-x %>%
        select(name) %>%
        group_by(name) %>%
        summarise(games=n()) %>%
        filter(games>min_games)
    names<-namesList$name
    
    for (myName in names){
        #reset number of good years
        number_of_years<-top_years
        
        if (top_yearsTF){
            yards<- x %>%
                filter(myName==name) %>%
                filter(passing_attempts>11) %>%
                select_(.dots = stat_select) %>%
                group_by_(.dots = stat_group) %>%
                summarise_(y=(.dots = setNames(summ, summ_name)), games = (.dots="n()")) %>%
                filter(games>min_top_games)
            number_of_years<-nrow(yards)
            
            if (number_of_years>=top_years){
                yards<-yards[order(-yards$y),]
                myYears<-yards$year[1:top_years]
                
                yards<- x %>%
                    filter(myName==name, year %in% myYears) %>%
                    select_(.dots=noYear_stat_select) %>%
                    group_by_(.dots=noYear_stat_group) %>%
                    summarise_(y=(.dots = setNames(summ, summ_name)))
            }
        } else if (!top_yearsTF) {
            yards<- x %>%
                filter(myName==name) %>%
                select_(.dots=noYear_stat_select) %>%
                group_by_(.dots=noYear_stat_group) %>%
                summarise_(y=(.dots = setNames(summ, summ_name)))
            myYears<-"All"
        }
        if (number_of_years>=top_years){
            passer_rating<-yards$y
            myName<-as.character(myName)
            myYear<-paste(myYears,collapse = " ")
            number_of_years<-nrow(yards)
            
            results<-rbind(results, data.table(name=myName, passer_rating=passer_rating, years=myYear))
        }
    }
    results<-results[order(results$passer_rating,decreasing = TRUE),]
    results
}


shinyServer(function(input, output) {
    
    

    output$distPlot <- renderPlot({
        #output$distPlot <- renderLeaflet({
        
        all_qb_data<-readRDS("./data/qbData.rds")
        
        qbData<-form_data(all_qb_data,input$chart_type, input$chart_option)
        
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
