library(shinydashboard)
library(shiny)
library(kableExtra)
library(tidyverse)
library(plotly)
library(RColorBrewer)


function(input, output, session) {
  
  #Static data for ease
  users <- read.csv2("data/users.csv", header = T, sep = ";", fileEncoding="UTF-8-BOM")
  matches <- read.csv2("data/matches.csv", header = T, sep = ";", fileEncoding="UTF-8-BOM")
  players <- read.csv2("data/players.csv", header = T, sep = ";", fileEncoding="UTF-8-BOM")
  leagues <- read.csv2("data/leagues.csv", header = T, sep = ";", fileEncoding="UTF-8-BOM") %>% 
    mutate(Active = ifelse(Sys.Date() > startDate & Sys.Date() < endDate, T, F)) 
  
  #Login
  login.page = paste(
    isolate(session$clientData$url_protocol),
    "//",
    isolate(session$clientData$url_hostname),
    ":",
    isolate(session$clientData$url_port),
    sep = ""
  )
  
  #https://stackoverflow.com/questions/44708473/shiny-dashboard-user-authentication
  
  USER <- reactiveValues(Logged = F, Id = NULL, Name = NULL)
  
  observe({
    if (USER$Logged == F) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$username)
          Password <- isolate(input$passwd)
          Id.username <- which(users$username %in% Username)
          Id.password <- which(users$password %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0){
            if (Id.username == Id.password) {
              
              USER$Logged <- T
              USER$Id <- users %>% 
                filter(userId == Id.username) %>% 
                pull(playerId)
              
              USER$Admin <- users %>% 
                filter(userId == Id.username) %>% 
                pull(admin)
              
              USER$Name <- players %>% 
                filter(playerId == isolate(USER$Id)) %>% 
                pull(name)
              
            }
          }
        }
      }
    }
  })
  
  #Set Reactive Data
  
  data <- reactiveValues(players = NULL,
                         uniqueIds = NULL,
                         leagues = NULL,
                         activeLeagues = NULL,
                         matches = NULL,
                         users = NULL)
  
  data$matches <- matches
  
  data$players <- players
  
  data$uniqueIds <- players %>% 
    pull(playerId)
  
  data$leagues <- leagues
  
  data$users <- users
  
  activeLeagues <- leagues %>% 
    filter(Active) %>% 
    pull(league)
  
  #For backup (reactive for all?)
  backupMatches <- isolate(data$matches)
  
  #Leagues
  
  output$league = renderUI(
    selectInput("league", "League", activeLeagues)
  )
  
  #Standings
  
  output$standings <- renderText({
    
    temp <- data$matches %>% 
      filter(league == input$league & correct) %>% 
      mutate(playerId = as.numeric(playerId))
    
    if(nrow(temp) > 0){
      
      temp %>% 
        filter(league == input$league) %>% 
        select(-league) %>% 
        group_by(matchId) %>% 
        #Point system here!
        mutate(point = ifelse(score > mean(score), 3,
                              ifelse(score == mean(score), 1, 0))) %>% 
        ungroup() %>% 
        group_by(playerId) %>% 
        summarise(points = sum(point),
                  wins = sum(point == 3),
                  ties = sum(point == 1),
                  losses = sum(point == 0),
                  gamesWon = sum(score)) %>% 
        inner_join(data$players %>% 
                     mutate(playerId = as.numeric(playerId)), by = "playerId") %>% 
        arrange(desc(points)) %>% 
        mutate(rank = row_number()) %>% 
        select(rank, name, wins, ties, losses, gamesWon, points) %>% 
        setNames(c("Rank", "Name", "Match Wins", "Match Ties", "Match Losses", "Games Won", "Total Points")) %>% 
        kable(caption = paste("Standings in", input$league)) %>%
        kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "condensed"))
      
    } else {
      
      data.frame(matrix(ncol = 7, nrow = 0)) %>% 
        setNames(c("Rank", "Name", "Match Wins", "Match Ties", "Match Losses", "Games Won", "Total Points")) %>% 
        kable(caption = paste("Standings in", input$league)) %>%
        kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "condensed"))
      
    }
    
    
  })
  
  #Recent Matches
  #TODO should be table, scrollable etc
  output$recentMatches <- renderText({
    
    temp <- data$matches %>% 
      filter(league == input$league & correct) %>% 
      mutate(playerId = as.numeric(playerId))
    
    if(nrow(temp) > 0){
      
      temp %>%  
        inner_join(data$players %>% 
                     mutate(as.numeric(playerId)), by = "playerId") %>% 
        group_by(matchId) %>% 
        mutate(totScore = paste(score, collapse = "-"),
                  names = paste(name, collapse = ", "),
                  date = date) %>% 
        ungroup() %>% 
        distinct(matchId, .keep_all = TRUE) %>% 
        arrange(desc(date)) %>%   
        separate(names, c("name_1", "name_2"), sep = ", ") %>% 
        slice(1:20) %>% 
        select(date, name_1, totScore, name_2) %>% 
        setNames(c("Date", "Player 1", "Score", "Player 2")) %>% 
        kable(caption = "Recent Matches", row.names = NA) %>%
        kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "condensed")) 
      
    } else {
      
      data.frame(matrix(ncol = 4, nrow = 0)) %>% 
        setNames(c("Date", "Player 1", "Score", "Player 2")) %>% 
        kable(caption = "Recent Matches") %>%
        kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "condensed")) 
      
    }
  })
  
  #Add Match
  
  output$opponent = renderUI(
    selectInput("opponent", "Opponent", isolate(data$uniqueIds)[isolate(data$uniqueIds) != USER$Id])
  )
  output$scorePlayer = renderUI(
    numericInput("scorePlayer", "Your Score", value = 0, min = 0)
  )
  output$scoreOpponent = renderUI(
    numericInput("scoreOpponent", "Opponents Score", value = 0, min = 0)
  )
  
  #Match to Add
  
  output$matchToAdd <- renderText({
    
    req(input$opponent)
    req(input$scorePlayer)
    req(input$scoreOpponent)
    
    data.frame("playerId" = c(USER$Id, input$opponent), 
               "score" = c(input$scorePlayer, input$scoreOpponent)) %>% 
      mutate(playerId = as.numeric(playerId)) %>% 
      inner_join(data$players, by = "playerId") %>% 
      mutate(totScore = paste(score, collapse = "-"),
             names = paste(name, collapse = ", "),
             date = input$matchDate) %>% 
      separate(names, sep = ", ",into = c("name_1", "name_2")) %>% 
      distinct(totScore, .keep_all = T) %>% 
      select(date, name_1, totScore, name_2) %>% 
      setNames(c("Date", "Player 1", "Score", "Player 2")) %>% 
      kable(caption = "Match to Add") %>%
      kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "condensed")) 
    
    
  })
  
  #TODO Error handling here
  observeEvent(input$addMatch, {
    
    req(input$opponent)
    req(input$scorePlayer)
    req(input$scoreOpponent)
    
    curMatchesInLeague <- data$matches %>% 
      filter(league == input$league & correct) %>% 
      arrange(desc(matchId))
    
    newId <- ifelse(nrow(curMatchesInLeague) != 0, max(curMatchesInLeague$matchId) + 1, 1)
    
    temp <- data.frame("playerId" = c(USER$Id, input$opponent), 
                       "score" = c(input$scorePlayer, input$scoreOpponent)) %>% 
      mutate(date = input$matchDate,
             matchId = newId,
             loggedByPlayer = USER$Id, 
             league = input$league,
             correct = T) %>% 
      select(league, matchId, playerId, date, score, loggedByPlayer, correct)
    
    write.table(temp, "data/matches.csv", sep = ";", append = T, col.names = F, row.names = F, fileEncoding="UTF-8")
    
    #Update rval
    data$matches = data$matches %>% 
      rbind(temp)
    
    cat("Adding \n")
    print(temp)
    
    showModal(modalDialog(
      title = "Match Added Successfully",
      "If you fucked up, contact Admin"
    ))
    
  })
  
  #Player Mappings
  output$pMaps <- renderText({
    
    data$players %>% 
      select(playerId, name) %>% 
      setNames(c("Player ID", "Player Name")) %>% 
      kable(caption = "Player Ids") %>% 
      kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "condensed")) 
    
  })
  
  #Player Analysis
  
  #TODO Tabs for each type of plot, clean up, nicer error handling etc
  observeEvent(input$Login,{  
    
    raw <- data$matches
    
    if(nrow(raw) > 0){
    
      clean <- raw %>% 
        group_by(matchId) %>% 
        mutate(opponentId = ifelse(playerId == max(playerId), min(playerId), max(playerId)),
               outcome = factor(ifelse(score > mean(score), "Win",
                                       ifelse(score == mean(score), "Tie", "Loss"))),
               opponentScore = ifelse(score > mean(score), min(score),
                                      ifelse(score == mean(score), score, max(score))),
               date = as.Date(date),
               month = format(date, "%Y-%m")) %>% 
        ungroup() %>% 
        filter(playerId == USER$Id)
      
      if(nrow(clean) > 0){
          
        #Historical Win/Tie/Loss
        output$wtlPlot <- renderPlotly({
          
            ggplotly(clean %>% 
                      group_by(playerId, month, opponentId) %>% 
                      summarise(games = n(),
                                Wins = sum(outcome == "Win"),
                                Ties = sum(outcome == "Tie"),
                                Losses = sum(outcome == "Loss")) %>% 
                      gather(key = type, value = count,  -playerId, -month, -games, -opponentId) %>% 
                      inner_join(data$players, by = c("opponentId" = "playerId")) %>% 
                      mutate(type = factor(type, levels = c("Wins", "Ties", "Losses"))) %>% 
                      ggplot() +
                      geom_bar(aes(x = month, y = count, fill = type), position="stack", stat="identity") +
                      xlab("Month") +
                      ylab("Count") +
                      ggtitle("Historical Results") +
                      facet_wrap(~name, scales = "free_y") +
                      theme_bw(),
                    height = 400, 
                    width = 800,
                    margin = list(b = 100, l = 100))
          
        })
        
        output$wcPlot <- renderPlotly({
          
          ggplotly(clean %>% 
                     group_by(playerId, month) %>% 
                     summarise(propWon = sum(score)/sum(score + opponentScore)) %>% 
                     ggplot(aes(x = month, y = propWon), color = "blue") +
                     geom_point() +
                     geom_line() +
                     xlab("Month") +
                     ylab("Fraction of Games (Not Matches) Won Over Time") +
                     theme_bw(),
                   height = 400, 
                   width = 800, 
                   maring = list(b = 100, l = 100))
          
        })
      }
    }
  })
  

    
  #Alter Matches
  output$rmId <- renderUI(
    if(USER$Admin){
      numericInput("idToAlter", "Match ID to Alter", value = 0, min = 0)
    } else {
      HTML(paste("You need admin privileges to alter match data."))
    }
  )
  output$rmButton <- renderUI(
    if(USER$Admin){
      actionButton("alterMatch", "Alter Match")
    }
  )
  
  output$leagueMatches <- renderText({
    
    data$matches %>% 
      filter(league == input$league) %>% 
      arrange(desc(matchId)) %>% 
      kable(caption = "All League Matches") %>% 
      kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "condensed")) 
    
  })
  
  #Iffy as we essentialy rewrite entire "database"
  observeEvent(input$alterMatch, {
    
    matches <- data$matches
    
    if(nrow(matches[matches$league == input$league & matches$matchId == input$idToAlter,]) != 0){
      
      temp <- matches %>% 
        filter(!(league == input$league & matchId == input$idToAlter))
      
      write.table(temp, "data/matches.csv", sep = ";", append = F, col.names = T, row.names = F, fileEncoding="UTF-8")
      
      data$matches = temp
      
      showModal(modalDialog(
        title = "Match Removed",
        paste("MatchId", input$idToAlter, "in league", input$league, "has been removed")
      ))
      
    } else {
      
      showModal(modalDialog(
        title = "MatchId does not exist",
        paste("MatchId", input$idToAlter, "in league", input$league, "does not exist")
      ))
      
    }
    
  })
  
  #Add Users
  
  observeEvent(input$addUsers, {
    
    newId <- as.numeric(max(max(data$users$userId), max(data$players$playerId))) + 1 #just so that we can chill with user=player id all the time
    
    newUser <- matrix(c(newId, input$usern, input$pass, newId, input$adm, USER$Id), ncol = 6) %>% 
      data.frame() %>% 
      setNames(c("userId", "username", "password", "playerId", "admin", "addedByUser"))
    
    write.table(newUser, "data/users.csv", sep = ";", append = T, col.names = F, row.names = F, fileEncoding="UTF-8")
    
    data$users <- data$users %>% 
      rbind(newUser)
    
    showModal(modalDialog(
      title = "New User Added Successfully",
      paste(input$usern, "has been created")
    ))
    
  })
  
  #Remove User
  
  observeEvent(input$removeUsers, {
    
    users <- data$users
  
    if(nrow(users[users$userId == input$userToRemove,]) != 0){
      
      temp <- users %>% 
        filter(userId != input$userToRemove)
      
      write.table(temp, "data/users.csv", sep = ";", append = F, col.names = T, row.names = F, fileEncoding="UTF-8")
      
      data$users <- temp
      
      showModal(modalDialog(
        title = "User Remove Successfully",
        paste("User with id", input$userToRemove, "has been removed")
      ))
      
    } else {
      
      showModal(modalDialog(
        title = "UserId does not exist",
        paste("UserId", input$userToRemove, "does not exist")
      ))
      
    }
    
  })
  
  #All Users
  
  output$users <- renderText({
    
    data$users %>% 
      select(-password) %>% #lol
      arrange(userId) %>% 
      kable(caption = "All Existing Users") %>% 
      kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "condensed")) 
    
  })
  
  #Change User Info
  
  output$userInfo <- renderText({
    
    data$users %>% 
      mutate(playerId = as.numeric(playerId)) %>% 
      filter(userId == USER$Id) %>% 
      inner_join(data$players %>% mutate(playerId = as.numeric(playerId)), by = "playerId") %>% 
      mutate(password = paste(rep("*", nchar(password)), collapse = "")) %>% 
      kable(caption = "Your Info") %>% 
      kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "condensed")) 
    
  })
  
  #Username
  observeEvent(input$changeUsername, {
    
    userData <- read.csv2("data/users.csv", header = T, sep = ";", fileEncoding="UTF-8-BOM")
    
    temp <- userData %>% 
      mutate(username = ifelse(userId == USER$Id, input$newUsername, username))
    
    write.table(temp, "data/users.csv", sep = ";", append = F, col.names = T, row.names = F, fileEncoding="UTF-8")
    
    data$users <- temp
    
    showModal(modalDialog(
      title = "You username has been updated",
      paste("Your new username is", input$newUsername)
    ))
    
  })
  
  #Name
  observeEvent(input$changeName, {
    
    playerData <- read.csv2("data/players.csv", header = T, sep = ";", fileEncoding="UTF-8-BOM")
    
    temp <- playerData %>% 
      mutate(name = ifelse(playerId == USER$Id, input$newName, name))
    
    write.table(temp, "data/players.csv", sep = ";", append = F, col.names = T, row.names = F, fileEncoding="UTF-8")
    
    data$players <- temp
    
    showModal(modalDialog(
      title = "You name has been updated",
      paste("Your new name is", input$newName)
    ))
    
  })
  
  #Pass
  observeEvent(input$changePassword, {
    
    userData <- read.csv2("data/users.csv", header = T, sep = ";", fileEncoding="UTF-8-BOM")
    
    currentPass <- userData %>% 
      filter(userId == USER$Id) %>% 
      pull(password)
    
    if(isolate(input$oldPassword) == currentPass){
      if(isolate(input$newPassword1) == isolate(input$newPassword2)){
      
        temp <- userData %>% 
          mutate(password = ifelse(userId == isolate(USER$Id), isolate(input$newPassword1), password))
        
        write.table(temp, "data/users.csv", sep = ";", append = F, col.names = T, row.names = F, fileEncoding="UTF-8")
        
        data$users <- temp
        
        showModal(modalDialog(
          title = "You password has been updated",
          paste("")
        ))
        
      } else {
        
        showModal(modalDialog(
          title = "The password could not be changed",
          paste("The new passwords do no match")
        ))
        
      } 
      
    } else {
      
      showModal(modalDialog(
        title = "The password could not be changed",
        paste("The old password does not match")
      ))
      
    }
    
  })
  
  
  #Create Profile
  
  observeEvent(input$createProfile, {
    
    if(nchar(input$playerName) > 4){
      
      newPlayer <- matrix(c(USER$Id, input$playerName, format(Sys.Date(), "%Y-%m-%d"), "Active"), ncol = 4) %>% 
        data.frame() %>% 
        setNames(c("playerId", "name", "lastactive", "status"))
      
      write.table(newPlayer, "data/players.csv", sep = ";", append = T, col.names = F, row.names = F, fileEncoding="UTF-8")
      
      data$players <- data$players %>% 
        rbind(newPlayer) %>% 
        mutate(playerId = as.numeric(playerId))
      
      data$uniqueIds <- data$players$playerId %>% unique()
      
      showModal(modalDialog(
        title = "Player Profile Created",
        paste("Welcome", newPlayer$name)
      ))
    
    } else {
      
      showModal(modalDialog(
        title = "Name to short",
        paste("The name you entered is too short (less than four characters). Please include your surname as well")
      ))
      
    }
    
  })
  
  #UI after Log
  
  #Body
  output$body <- renderUI({
    if(USER$Logged == T){
      
      if(!(USER$Id %in% data$uniqueIds)){
        
        box(
          title = "Create Player Profile",
          textInput("playerName", "Full Name"),
          br(),
          HTML("Unlike the username the name you enter here is showed in rankings, analysis etc."),
          br(),
          actionButton("createProfile", "Create Profile")
        )
        
      } else {
      
        tabItems(
          # Standings
          tabItem(tabName = "standings",
                  h2("Standings"),
                  uiOutput("standings")
                  
          ),
          # Add Match
          tabItem(tabName = "addMatch",
                  h2("Add Matches"),
                  sidebarLayout(
                    sidebarPanel(
                      #Player 1
                      uiOutput("scorePlayer"),
                      #Player 2
                      uiOutput("opponent"),
                      uiOutput("scoreOpponent"),
                      #Date
                      dateInput(
                        "matchDate",
                        "Match Date",
                        format = "yyyy-mm-dd"
                      )
                    ),
                    mainPanel(
                      htmlOutput("matchToAdd"),
                      actionButton("addMatch", "Add Match")
                    )
                  )
                  
          ),
          #Recent Matches
          tabItem(tabName = "recentMatch",
                  h2("Recent Matches"),
                  htmlOutput("recentMatches") 
                  
          ),
          #Player Ids
          tabItem(tabName = "pMaps",
                  h2("Player Mappings"),
                  uiOutput("pMaps") 
                  
          ),
          #Remove/Alter Matches
          tabItem(tabName = "alterMatches",
                  h2("Alter Matches"),
                  uiOutput("leagueMatches"),
                  uiOutput("rmId"),
                  uiOutput("rmButton")
                  
          ),
          #Analysis
          tabItem(tabName = "playerAnalysis",
                  h2("Player Analysis"),
                  uiOutput("p2w"),
                  HTML("No data to analyze yet :/")
                  #plotlyOutput("wtlPlot"),
                  #plotlyOutput("wcPlot")
          ),
          #Alter Users
          tabItem(tabName = "alterUsers",
                  h2("Alter users"),
                  uiOutput("users"),
                  fluidRow(
                    column(6,
                           h3("Add a new user"),
                           textInput("usern", "Username"),
                           textInput("pass", "Password"),
                           checkboxInput("adm", "Admin"),
                           actionButton("addUsers", "Add New User")
                           ),
                    column(6,
                           h3("Remove a user"),
                           numericInput("userToRemove", "User ID of User to Remov", min = 0, value = 0),
                           actionButton("removeUsers", "Remove a User")
                           )
                  )
          ),
          #Change Info
          tabItem(tabName = "changeInfo",
                  h2("Change User Info"),
                  uiOutput("userInfo"),
                  fluidRow(
                    column(4,
                           h5("Change username (login)"),
                           textInput("newUsername", "New Username"),
                           actionButton("changeUsername", "Change Username")
  
                    ),
                    column(4,
                           h5("Change password"),
                           passwordInput("oldPassword", "Old Password"),
                           passwordInput("newPassword1", "New Paswword"),
                           passwordInput("newPassword2", "New Paswword (Again)"),
                           actionButton("changePassword", "Change Password")

                    ),
                    column(4,
                           h5("Change name (display)"),
                           textInput("newName", "New Name"),
                           actionButton("changeName", "Change Name")
                           
                    )
                  )
          )
        )
      }
    } else {
      
      box(
        title = "Squash Dashboard Login",
        textInput("username", "Username"),
        passwordInput("passwd", "Password"),
        br(),
        actionButton("Login", "Log in")
      )
      
    }
  })
  
  #Sidebar
  output$sidebar <- renderUI({
    
    if(USER$Logged == T){
      if(USER$Admin == T){
        div(
          sidebarUserPanel(
            paste("Welcome", USER$Name),
            subtitle = a(icon("usr"), "Logout", href = login.page)
          ),
          sidebarMenu(
            menuItem("Standings", tabName = "standings", icon = icon("list-ol")),
            menuItem("Add Match", tabName = "addMatch", icon = icon("plus")),
            menuItem("Player Analysis", tabName = "playerAnalysis", icon = icon("bar-chart-o")),
            menuItem("Recent Matches", tabName = "recentMatch", icon = icon("table")),
            menuItem("Player Mappings", tabName = "pMaps", icon = icon("map")),
            menuItem("Alter Match", tabName = "alterMatches", icon = icon("database")),
            menuItem("Alter Users", tabName = "alterUsers", icon = icon("user-plus")),
            menuItem("Change Info", tabName = "changeInfo", icon = icon("th"))
          ),
          uiOutput("league")
        )
      } else {
        
        div(
          sidebarUserPanel(
            paste("Welcome", USER$Name),
            subtitle = a(icon("usr"), "Logout", href = login.page)
          ),
          sidebarMenu(
            menuItem("Standings", tabName = "standings", icon = icon("list-ol")),
            menuItem("Add Match", tabName = "addMatch", icon = icon("plus")),
            menuItem("Player Analysis", tabName = "playerAnalysis", icon = icon("poll")),
            menuItem("Recent Matches", tabName = "recentMatch", icon = icon("table")),
            menuItem("Player Mappings", tabName = "pMaps", icon = icon("map")),
            menuItem("Change Info", tabName = "changeInfo", icon = icon("th"))
          ),
          uiOutput("league")
        )
        
      }
    }
  })
  
}

  