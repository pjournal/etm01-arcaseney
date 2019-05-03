library(shiny)
library(shinydashboard)
library(rhandsontable)
library(data.table)
library(lubridate)
library(plotly)

matches=readRDS("C:\\Users\\a-adsene\\Desktop\\ETM58D\\df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
matches[,type:=NULL]
matches$leagueId=NULL
matches[,c('score_home','score_away'):=tstrsplit(score,':')]
matches=matches[complete.cases(matches)]
matches[,score_home:=as.numeric(score_home)]
matches[,score_away:=as.numeric(score_away)]

#check naming errors to standartize
sort(unique(matches$home))
sort(unique(matches$away))

# fix data error
matches[home=='crystal-palace',home:='crystal palace']
matches[home=='manchester-city',home:='manchester city']
matches[home=='manchester-utd',home:='manchester united']
matches[home=='manchester-united',home:='manchester united']
matches[home=='newcastle utd',home:='newcastle']
matches[home=='stoke',home:='stoke city']
matches[home=='west-ham',home:='west ham']
matches[away=='crystal-palace',away:='crystal palace']
matches[away=='manchester-city',away:='manchester city']
matches[away=='manchester-utd',away:='manchester united']
matches[away=='manchester-united',away:='manchester united']
matches[away=='newcastle utd',away:='newcastle']
matches[away=='stoke',away:='stoke city']
matches[away=='west-ham',away:='west ham']

#update time format
matches[,timestamp:=as_datetime(date,tz='Turkey')]
matches[,date:=NULL]

# define seasons
matches[
  ,season:=ifelse(((month(matches$timestamp,label=FALSE)>=7)&(year(matches$timestamp)==2010)) | ((month(matches$timestamp,label=FALSE)<=6)&(year(matches$timestamp)==2011)),"2010-2011"
                  ,ifelse(((month(matches$timestamp,label=FALSE)>=7)&(year(matches$timestamp)==2011)) | ((month(matches$timestamp,label=FALSE)<=6)&(year(matches$timestamp)==2012)),"2011-2012"
                          ,ifelse(((month(matches$timestamp,label=FALSE)>=7)&(year(matches$timestamp)==2012)) | ((month(matches$timestamp,label=FALSE)<=6)&(year(matches$timestamp)==2013)),"2012-2013"
                                  ,ifelse(((month(matches$timestamp,label=FALSE)>=7)&(year(matches$timestamp)==2013)) | ((month(matches$timestamp,label=FALSE)<=6)&(year(matches$timestamp)==2014)),"2013-2014"
                                          ,ifelse(((month(matches$timestamp,label=FALSE)>=7)&(year(matches$timestamp)==2014)) | ((month(matches$timestamp,label=FALSE)<=6)&(year(matches$timestamp)==2015)),"2014-2015"
                                                  ,ifelse(((month(matches$timestamp,label=FALSE)>=7)&(year(matches$timestamp)==2015)) | ((month(matches$timestamp,label=FALSE)<=6)&(year(matches$timestamp)==2016)),"2015-2016"
                                                          ,ifelse(((month(matches$timestamp,label=FALSE)>=7)&(year(matches$timestamp)==2016)) | ((month(matches$timestamp,label=FALSE)<=6)&(year(matches$timestamp)==2017)),"2016-2017"
                                                                  ,ifelse(((month(matches$timestamp,label=FALSE)>=7)&(year(matches$timestamp)==2017)) | ((month(matches$timestamp,label=FALSE)<=6)&(year(matches$timestamp)==2018)),"2017-2018"
                                                                          ,ifelse(((month(matches$timestamp,label=FALSE)>=7)&(year(matches$timestamp)==2018)) | ((month(matches$timestamp,label=FALSE)<=6)&(year(matches$timestamp)==2019)),"2018-2019","NA")))))))))
  ]

setDT(matches)

#shiny 

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = "Premier League Stats"),
  dashboardSidebar(sidebarMenu(
    menuItem("Seasons / Teams",tabName = "seasons",icon = icon("calendar-alt"))
  )),
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "seasons",
            fluidRow(
              column(
                width = 5,
                textOutput("welcome"),
                selectInput(
                  inputId = "season_choice",
                  label = "Select a season:",
                  choices = unique(matches$season),
                  selected = NULL
                ),
                selectInput(
                  inputId = "team_choice",
                  label = "Select a team:",
                  choices = sort(unique(matches$home)),
                  selected = NULL
                ),
                actionButton(inputId = "random_season", "Select random season!"),
                actionButton(inputId = "random_team", "Select random team!"),
                dataTableOutput("season_table")
              ),
              column(
                width = 7,
                plotlyOutput("season_plot")
              )
            ))
)))


server <- function(input, output, session) {

  observeEvent({
    input$random_team
  },
  updateSelectInput(
    session = session,
    inputId = "team_choice",
    label = "Select a team:",
    choices = sort(unique(matches$home)),
    selected = sample(unique(matches$home), 1)
  ))
  
  observeEvent({
    input$random_season
  },
  updateSelectInput(
    session = session,
    inputId = "season_choice",
    label = "Select a season:",
    choices = sort(unique(matches$season)),
    selected = sample(unique(matches$season), 1)
  ))
  
  output$season_table <- renderDataTable({
    table_to_show = matches[season == input$season_choice & (home == input$team_choice | away == input$team_choice),c(2,3,4,7)][order(timestamp)]
    table_to_show
  }
  )
  
  output$season_plot <- renderPlotly({
    plot_ly(data=matches[season == input$season_choice & (home == input$team_choice | away == input$team_choice)],
     x=c("Home","Away"),
     y=c(sum(matches[season == input$season_choice & (home == input$team_choice)]$score_home)/length(matches[season == input$season_choice & (home == input$team_choice)]$matchId),
         sum(matches[season == input$season_choice & (away == input$team_choice)]$score_away)/length(matches[season == input$season_choice & (away == input$team_choice)]$matchId)),
     type="bar", 
     name="Scored") %>%
      add_trace(y = c(sum(matches[season == input$season_choice & (home == input$team_choice)]$score_away)/length(matches[season == input$season_choice & (home == input$team_choice)]$matchId),
                      sum(matches[season == input$season_choice & (away == input$team_choice)]$score_home)/length(matches[season == input$season_choice & (away == input$team_choice)]$matchId)),
                name = 'Conceded') %>%
      layout(yaxis = list(title = 'Average Goals'), barmode = 'group')

  })
  
  output$teams_table <- renderRHandsontable({
    table_to_show = matches[home == input$team_choice][order(-matches$home_score)]
    table_to_show
  })
  
  output$welcome <- renderText({ 
    paste("Welcome to Premier League Stats provided by Adil Arca Seney and Begum Urgenc")
  })
}

shinyApp(ui, server)
