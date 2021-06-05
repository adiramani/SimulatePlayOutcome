library(xgboost)
library(dplyr)
library(shiny)
source("helper.R")

ui <- fluidPage(
  titlePanel("Simulate NFL Plays"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a play to simulate."),
      uiOutput("seasonSelect"),
      uiOutput("weekSelect"),
      uiOutput("gameSelect"),
      uiOutput("driveSelect"),
      uiOutput("playSelect"),
      sliderInput("numSims", 
                  label = "Number of simulations",
                  min = 0, max = 20, value = 10)
    ),
    mainPanel(
      htmlOutput("simResult"),
      htmlOutput("actualResult")
    )
  )
)

server <- function(input, output) {
  
  output$seasonSelect <- renderUI({
    seasons <- pbp_data %>% select(season) %>% unique() %>% .$season
    selectInput("seasonVal", h3("Select Season"), seasons, selected=2020)
  })
  
  output$weekSelect <- renderUI({
    req(input$seasonVal)
    weeks <- pbp_data %>% filter(season == input$seasonVal) %>% select(week) %>% unique() %>% .$week
    selectInput("weekVal", h3("Select Week"), weeks, selected=1)
  })
  
  output$gameSelect <- renderUI({
    req(input$weekVal)
    games <- pbp_data %>% filter(season == input$seasonVal & week == input$weekVal) %>% 
                              mutate(game_name = paste(away_team, " at ", home_team)) %>%
                              select(game_name, game_id) %>% unique()
    gamesList <- games$game_id
    names(gamesList) <- games$game_name
    selectInput("gameId", h3("Select Game"), gamesList)
  })
  
  output$driveSelect <- renderUI({
    req(input$gameId)
    drives <- pbp_data %>% filter(season == input$seasonVal & week == input$weekVal 
                                   & game_id == input$gameId & !is.na(drive) & !is.na(posteam)) %>%
                          group_by(drive) %>% mutate(drive_name = paste(drive, ': ', posteam)) %>%
                          select(drive, drive_name) %>% unique() #, home_team, awayTeam) %>% unique()
    
    'homeDriveNum <- 0
    awayDriveNum <- 0
    namesList <- c()
    for (currDriveNum in driveList) {
      driveTeamName <- pbp_data %>% filter(season == input$seasonVal & week == input$weekVal 
                                       & game_id == input$gameId & !is.na(drive) &
                                         drive == currDriveNum & !is.na(posteam)) %>% 
                                select(posteam) %>% unique() %>% .$posteam
      if(driveTeamName == homeTeam) {
        homeDriveNum <- homeDriveNum + 1
        namesList <- append(namesList, paste(homeTeam, ": ", homeDriveNum))
      } else {
        awayDriveNum <- awayDriveNum + 1
        namesList <- append(namesList, paste(awayTeam, ": ", awayDriveNum))
      }
    }'
    driveList <- drives$drive
    names(driveList) <- drives$drive_name
    selectInput("driveNum", h3("Select Drive"), driveList, selected = 1)
  })
  
  output$playSelect <- renderUI({
    req(input$driveNum)
    selectInput("playId", h3("Select Play"), pbp_data %>% filter(season == input$seasonVal & 
                                                                   week == input$weekVal & game_id == input$gameId 
                                                                 & drive == input$driveNum) %>%
                                                          filter(play_type %in% c("run", "pass")) %>% 
                                                          unique() %>% .$play_id)
  })
  
  getPlayData <- reactive({
    req(input$playId)
    generalMutateData(pbp_data %>% filter(play_id == input$playId & game_id == input$gameId))
  })
  
  output$simResult <- renderUI({
    req(input$playId)
    playData <- getPlayData()
    
    # passing vars
    numPass <- 0
    passYards <- 0
    completePassYards <- 0
    cumAirYards <- 0
    cumYAC <- 0
    numCompletePasses <- 0
    
    # rushing vars
    numRun <- 0
    runYards <- 0
    
    xPassPct <- playData$xpass
    # simulate play
    for (sim in 1:input$numSims) {
      dataToPassIn <- playData
      
      # randomly determine if simulated play is a pass or run based on xpass%
      if(isPass(xPassPct)) {
        numPass <- numPass + 1
        
        # randomly select pass side
        dataToPassIn$pass_side <- sample(1:2, 1)
        
        # simulated air yards
        airYards <- getExpectedAirYards(dataToPassIn)
        cumAirYards <- cumAirYards + airYards
        dataToPassIn$air_yards <- airYards
        
        # simulate if pass is complete
        compPct <- getComplectionPct(dataToPassIn)
        if(isComplete(compPct)) {
          numCompletePasses <- numCompletePasses + 1
          
          # simulated yac
          yac <- getExpectedYAC(dataToPassIn)
          cumYAC <- cumYAC + yac
          
          # track yards
          passYards <- passYards + airYards + yac
          completePassYards <- completePassYards + airYards + yac
        }
        
      } else {
        numRun <- numRun + 1
        
        # randomly select run gap and side
        if(is.na(dataToPassIn$run_gap)) {
          dataToPassIn$run_gap <- sample(1:3, 1)
        }
        if(is.na(dataToPassIn$run_side)) {
          dataToPassIn$run_side <- sample(1:2, 1)
        }
        
        #total yards
        runYards <- runYards + getExpectedRushYards(dataToPassIn)
      }
    }
    
    # prettify output data
    avgPassYds <- round(passYards / numPass, digits=2)
    avgAirYards <- round(cumAirYards / numPass, digits=2)
    avgYAC <- round(cumYAC / numPass, digits=2)
    completionPct <- round(numCompletePasses / numPass, digits=4) * 100
    if(numPass == 0) {
      avgPassYds <- 0
      avgAirYards <- 0
      avgYAC <- 0
      completionPct <- 0
    }
    
    avgCompletePassYds <- round(completePassYards / numCompletePasses, digits=2)
    if(numCompletePasses == 0) {
      avgCompletePassYds <- 0
    }
    
    avgRushYds <- round(runYards / numRun, digits=2)
    if(numRun == 0) {
      avgRushYds <- 0
    }
    
    totalAvg <- round((passYards + runYards) / (numPass + numRun), digits=2)
    
    # print output
    startStr <- paste("<h3>Over ", input$numSims, "simulations, this play was...</h3>")
    passStr <- paste("<strong>a pass </strong>", numPass, "times, gaining an average of ", avgPassYds, "yards (", avgCompletePassYds, "on complete passes).", " AIr Yards: ", avgAirYards, "YAC: ", avgYAC, "Cmp%: ", completionPct, "%")
    rushStr <- paste("<strong>a run </strong>", numRun, "times, gaining an average of ", avgRushYds, "yards.")
    overallStr <- paste("<h5><strong>On average</strong>, this play gained", totalAvg, " yards over ", input$numSims, "simulations</h5>")
    HTML(paste(startStr, passStr, rushStr, overallStr, sep="<br/>"))
  })
  
  # print actual result of the play
  output$actualResult <- renderUI({
    req(input$playId)
    playData <- getPlayData()
    outcome <- paste("gained ", playData$yards_gained)
    if(playData$yards_gained < 0) {
      outcome <- paste("lost ", playData$yards_gained)
    }
    HTML(paste("<h4>The actual result of the play was a ", playData$play_type, " and it ", outcome, " yards.</h4>"))
  })
  
}

# Run app ----
shinyApp(ui, server)
