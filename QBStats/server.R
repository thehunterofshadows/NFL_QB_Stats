myDebug<-TRUE

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


form_data<-function(x, chart_type, chart_option){
    #test
    
    #constants
    min_games = 36
    top_years = 5
    stat_value = as.character(chart_option)
    top_yearsTF = FALSE
    min_top_games = 8
    
    if (chart_type == "top_five") {
        top_yearsTF = TRUE
        stat_select = list(stat_value,"year","pri_color","sec_color")
        stat_group = list("year","pri_color","sec_color")
        noYear_stat_select = list(stat_value,"pri_color","sec_color")
        noYear_stat_group = list("pri_color","sec_color")
        summ <- paste0("mean(", stat_value, ')') 
        summ_name <- paste0('mean_', stat_value)
    } else if (chart_type == "avg") {
        top_yearsTF = FALSE
        stat_select = list(stat_value,"year","pri_color","sec_color")
        stat_group = list("year","pri_color","sec_color")
        noYear_stat_select = list(stat_value,"pri_color","sec_color")
        noYear_stat_group = list("pri_color","sec_color")
        summ <- paste0("mean(", stat_value, ')') 
        summ_name <- paste0('mean_', stat_value)
    } else if (chart_type == "total"){
        top_yearsTF = FALSE
        stat_select = list(stat_value,"year","pri_color","sec_color")
        stat_group = list("year","pri_color","sec_color")
        noYear_stat_select = list(stat_value,"pri_color","sec_color")
        noYear_stat_group = list("pri_color","sec_color")
        summ <- paste0("sum(", stat_value, ')')  
        summ_name <- paste0('sum_', stat_value)
    }
    
    
    results<-data.table(
        name = character(),
        y = numeric(),
        years = character(),
        pri_color = character(),
        sec_color = character()
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
            myName<-as.character(myName)
            value<-yards$y
            myYear<-paste(myYears,collapse = " ")
            results<-rbind(results, 
                           data.table(name=myName, 
                                      y=value, 
                                      years=myYear,
                                      pri_color = yards$pri_color,
                                      sec_color = yards$sec_color))
        }
    }
    
    results<-results[order(results$y,decreasing = TRUE),]
    results
}


shinyServer(function(input, output) {
    
    top_yearsTF=FALSE
    output$distPlot <- renderPlot({
        #output$distPlot <- renderLeaflet({
        
        all_qb_data<-readRDS("./data/qbData.rds")
        
        graph_title<-"Temp Title"
        
        yards<-form_data(all_qb_data,input$chart_type, input$chart_option)
        
        
        if (input$chart_type == "top_five") {top_yearsTF = TRUE} else {top_yearsTF = FALSE}
        
        #pull out the years as we aren't using that right now
        #also only keeping top 5
        chart_data<-yards[1:10,1:2]
        
        g<-ggplot(
            data = chart_data,
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
                title=graph_title,x="QB Names",y=graph_title
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
