options(shiny.maxRequestSize=200*1024^2)
library(shiny)
library(shinydashboard)
library(shinycssloaders)

#SERVER
library(tidyverse)
library(openxlsx)
library(lubridate)
library(magrittr)
library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)
library(httr)
library(sodium)
library(shinyjs)
library(DT)
library(rpivotTable)
library(rjson)
source('global.R')

#setwd('/home/pierre/Documents/DEV/bhbc-indicator-generator')

#install.packages('rsconnect')
#library('rsconnect')
#deployApp()

loginpage <- div(
      style = "margin-left : 30%;margin-top:10%",
      box(width = 5, "",status = "warning",
            tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
            textInput("userName", placeholder="Login", label = tagList(icon("user"), "Login")),
            passwordInput("passwd", placeholder="Mot de passe", label = tagList(icon("unlock-alt"), "Mot de passe")),
            br(),
            div(
                 style = "text-align: center;",
                 actionButton("login", "LOG IN", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                 shinyjs::hidden(
                    div(id = "nomatch",
                           tags$p("Login ou mot de passe incorrect!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br()
             )
    )
)

dashboard <- tabItems(
  
  tabItem(tabName = "newlydiagpermonth",
          # h3("Dashboard tab content"),

          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("monthlyDiag_startDate", "Date début:",language = "fr"),
                       
                       dateInput("monthlyDiag_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_newlydiag", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("monthlyDiag_contents") ,
                       downloadButton('download_file', 'Téléchargez')
                   )
            )
          )
  ),
  
  tabItem(tabName = "monthlytreatrate",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("monthlytreat_startDate", "Date début:",language = "fr"),
                       
                       dateInput("monthlytreat_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_monthlytreat", "Générer")
                   )
            ),
            # column(12,
            #        dataTableOutput('monthlyTreat_contents')
            # )
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("monthlyTreat_contents")
                       #downloadButton('download_file', 'Téléchargez')
                   )
            )
          )
  ),
  tabItem(tabName = "monthlycontrolrate",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("monthlycont_startDate", "Date début:",language = "fr"),
                       
                       dateInput("monthlytcont_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_monthlycont", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("monthlyCont_contents") 
                       #downloadButton('download_file', 'Téléchargez')
                   )
            )
          )
  ),
  tabItem(tabName = "quarterlynewlydiag",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("quartelynewly_startDate", "Date début:",language = "fr"),
                       
                       dateInput("quartelynewly_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_quartelynewly", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("quartelynewly_contents") 
                       #downloadButton('download_file', 'Téléchargez')
                   )
            )
          )
  ),
  tabItem(tabName = "quarterlytreatrate",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("quartelytreat_startDate", "Date début:",language = "fr"),
                       
                       dateInput("quartelytreat_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_quartelytreat", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("quartelytreat_contents") 
                       #downloadButton('download_file', 'Téléchargez')
                   )
            )
          )
  ),
  tabItem(tabName = "quartelycontrolrate",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("quartelycont_startDate", "Date début:",language = "fr"),
                       
                       dateInput("quartelycont_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_quartelycont", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("quartelycont_contents") 
                       #downloadButton('download_file', 'Téléchargez')
                   )
            )
          )
  ),
  tabItem(tabName = "sixmonthlycontrolrate",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("sixmonthlycont_startDate", "Date début:",language = "fr"),
                       
                       dateInput("sixmonthlycont_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_sixmonthlycont", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("sixmonthlycont_contents") 
                       #downloadButton('download_file', 'Téléchargez')
                   )
            )
          )
  ),
  tabItem(tabName = "netsystolic",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("netsystolic_startDate", "Date début:",language = "fr"),
                       
                       dateInput("netsystolic_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_netsystolic", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("netsystolic_contents") 
                       #downloadButton('download_file', 'Téléchargez')
                   )
            )
          )
  ),
  tabItem(tabName = "totaldiag",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("totaldiag_startDate", "Date début:",language = "fr"),
                       
                       dateInput("totaldiag_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_totaldiag", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("totaldiag_contents")
                   )
            )
          )
  ),
  tabItem(tabName = "totaltreat",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("totaltreat_startDate", "Date début:",language = "fr"),
                       
                       dateInput("totaltreat_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_totaltreat", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("totaltreat_contents") 
                       #downloadButton('download_file', 'Téléchargez')
                   )
            )
          )
  ),
  tabItem(tabName = "totalcontrol",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("totalcontrol_startDate", "Date début:",language = "fr"),
                       
                       dateInput("totalcontrol_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_totalcontrol", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("totalcontrol_contents") 
                   )
            )
          )
  ),
  tabItem(tabName = "totalscreened",
          fluidRow(
            
            column(width = 2,
                   
                   box(width = NULL, status = "warning",
                       
                       dateInput("totalscreened_startDate", "Date début:",language = "fr"),
                       
                       dateInput("totalscreened_endDate", "Date fin:", language = "fr"),
                       
                       actionButton("btn_totalscreened", "Générer")
                   )
            ),
            column(width = 12,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       rpivotTableOutput("totalscreened_contents") 
                   )
            )
          )
  )
)

sidebar <-  sidebarMenu(
  menuItem("Monthly Diagnosed Rate", icon = icon("file"), tabName = "newlydiagpermonth"),
  menuItem("Monthly Treatment Rate", icon = icon("th"), tabName = "monthlytreatrate"),
  menuItem("Monthly Control Rate", icon = icon("th"), tabName = "monthlycontrolrate"),
  menuItem("Quarterly Diagnosed Rate", icon = icon("th"), tabName = "quarterlynewlydiag"),
  menuItem("Quarterly Treatment Rate", icon = icon("th"), tabName = "quarterlytreatrate"),
  
  menuItem("Quarterly Control Rate", icon = icon("th"), tabName = "quartelycontrolrate"),
  menuItem("Six monthly Control Rate", icon = icon("th"), tabName = "sixmonthlycontrolrate"),
  menuItem("Net systolic", icon = icon("th"), tabName = "netsystolic"),
  
  menuItem("Total diagnosed", icon = icon("th"), tabName = "totaldiag"),
  menuItem("Total treated", icon = icon("th"), tabName = "totaltreat"),
  menuItem("Total control", icon = icon("th"), tabName = "totalcontrol"),
  menuItem("Total screened", icon = icon("th"), tabName = "totalscreened")
)

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(title="HTA Indicators",uiOutput("logoutbtn")),
  
  dashboardSidebar(
    
    #sidebard
    uiOutput("sidebarpanel")
  ),
  
  dashboardBody(
    #dashboard
    shinyjs::useShinyjs(),
    uiOutput("body")
  )
)
getOrgUnits <- function(username, password){
  print(paste0(BASE.URL,orgUnitsURL))
  r <- httr::GET(paste0(BASE.URL,orgUnitsURL), httr::authenticate(username,password),
                 httr::timeout(60))
  r <- httr::content(r, "text")
  # Convert the JSON to an R data structure
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  
  ou <- d$organisationUnits 
  
  print(ou)
  
  orgUnits <-  ou %>% 
    select(id,parent.name, displayName) %>%
    dplyr::rename(orgUnitID = id, parent = parent.name)
    return(orgUnits)
}

getTreatmentRateQuartely <- function(startDate, endDate){
  
  #Get org units
  ou <- getOrgUnits(username,password)
  
  #####---------------GET NUMERATOR
  #Suivi
  url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&dimension=wDk1IkO7kXQ&skipMeta=true&paging=false")
  
  print(url)
  
  r <- httr::GET(url, httr::authenticate(username,password),
                 httr::timeout(60))
  r <- httr::content(r, "text")
  # Convert the JSON to an R data structure
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  
  # Get the base data
  data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
  
  # Use the original response to get the names of the columns
  names(data) <- d$headers$column
  
  
  data <- data %>%
    distinct(Uuid, .keep_all = TRUE) %>%
    dplyr::rename(orgUnitID = `Organisation unit`) %>%
    filter(Traitement != "")

  
  data <- left_join(data, ou, by = "orgUnitID") %>%
    dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
    select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid)
  
  
  numerator <-  data %>% 
    group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
    summarise(n=n(), .groups = 'keep') %>%
    dplyr::rename(numerator = n)
  
  #print(numerator)
  
  
  #####---------------GET DENOMINATOR
  #Suivi patient
  url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&skipMeta=true&paging=false")
  
  r <- httr::GET(url, httr::authenticate(username,password),
                 httr::timeout(60))
  r <- httr::content(r, "text")
  # Convert the JSON to an R data structure
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  
  # Get the base data
  data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
  
  # Use the original response to get the names of the columns
  names(data) <- d$headers$column
  
  data <- data %>%
    distinct(Uuid, .keep_all = TRUE) %>%
    dplyr::rename(orgUnitID = `Organisation unit`)
  
  data <- left_join(data, ou, by = "orgUnitID") %>%
    dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
    select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid)
  
  denominator <-  data %>% 
    group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
    summarise(n=n(), .groups = 'keep') %>%
    dplyr::rename(denominator = n)
  
  #print(denominator)
  
  indicator <- left_join(numerator, denominator, by = c("district" = "district","structure" = "structure","Sexe"="Sexe", "groupe_age"="groupe_age")) %>%
    group_by(district,structure,Sexe, groupe_age)  %>%
    summarise(indicateur = sum(numerator/denominator), .groups = 'keep')
  
  return(indicator)
}

getTreatmentRateMonthly <- function(startDate, endDate){
  
  #Get org units
  ou <- getOrgUnits(username,password)
  
  #####---------------GET NUMERATOR
  #Suivi
  url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&dimension=wDk1IkO7kXQ&skipMeta=true&paging=false")
  
  print(url)
  
  r <- httr::GET(url, httr::authenticate(username,password),
                 httr::timeout(60))
  r <- httr::content(r, "text")
  # Convert the JSON to an R data structure
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  
  # Get the base data
  data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
  
  # Use the original response to get the names of the columns
  names(data) <- d$headers$column
  
  data <- data %>%
    distinct(Uuid, .keep_all = TRUE) %>%
    dplyr::rename(orgUnitID = `Organisation unit`) %>%
    filter(Traitement != "")
  
  #print(data)
  data <- left_join(data, ou, by = "orgUnitID") %>%
    dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name")
    #summarise(date_event = min("Event date"),n=n(), .groups = 'keep') %>%
  
  numerator <-  data %>% 
    select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid) %>%
    group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
    dplyr::summarise(date_event = min("Event date"),n=n(), .groups = 'keep') %>%
    dplyr::rename(numerator = n)
  
  print("Numerator")
  print(numerator)
  
  
  #####---------------GET DENOMINATOR
  #Suivi patient
  url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&skipMeta=true&paging=false")
  
  r <- httr::GET(url, httr::authenticate(username,password),
                 httr::timeout(60))
  r <- httr::content(r, "text")
  # Convert the JSON to an R data structure
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  
  # Get the base data
  data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
  
  # Use the original response to get the names of the columns
  names(data) <- d$headers$column
  
  data <- data %>%
    distinct(Uuid, .keep_all = TRUE) %>%
    dplyr::rename(orgUnitID = `Organisation unit`)
  
  data <- left_join(data, ou, by = "orgUnitID") %>%
    dplyr::rename(district = "parent", groupe_age = "Groupe d’age") %>%
    select(district, orgUnitID,`Sexe`,groupe_age, Uuid)
  
  
  
  denominator <-  data %>% 
    group_by(district, orgUnitID,`Sexe`,groupe_age) %>%
    dplyr::summarise(n=n(), .groups = 'keep') %>%
    dplyr::rename(denominator = n)
  
  print("Denominator")
  print(denominator)
  
  indicator <- left_join(numerator, denominator, by = c("district" = "district","orgUnitID"="orgUnitID", "Sexe"="Sexe", "groupe_age"="groupe_age")) %>%
    group_by(district,structure,Sexe, groupe_age)  %>%
    mutate(ratio = ((numerator)/(denominator))*100, .groups = 'keep') %>%
    select(district,structure,Sexe, groupe_age, indicateur)
  
  print(indicator)
  
  return(indicator)
}

getDiagRateMonthly <- function(startDate, endDate){
  
  print("Monthly Diagnosed Rate")
  
  #Get org units
  ou <- getOrgUnits(username,password)
  #####---------------GET NUMERATOR
  #Provenance
  url <- paste0(BASE.URL,BASE.REQ,"?stage=UD8R4GcuOsO&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&skipMeta=true&paging=false")
  
  r <- httr::GET(url, httr::authenticate(username,password),
                 httr::timeout(60))
  r <- httr::content(r, "text")
  # Convert the JSON to an R data structure
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  
  # Get the base data
  data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
  
  # Use the original response to get the names of the columns
  names(data) <- d$headers$column
  
  data <- data %>%
    dplyr::rename(orgUnitID = `Organisation unit`)
  
  #print(data)
  
  data <- left_join(data, ou, by = "orgUnitID") %>%
    dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
    select(district, orgUnitID,structure)
  
  numerator <-  data %>% 
    group_by(district, orgUnitID,structure) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::rename(num = n)
  
  print("Numerator")
  print(numerator)
  

  
  #####---------------GET DENOMINATOR
  #Suivi patient
  base <- paste0(BASE.URL,"analytics?")
  url <- paste0(base,"dimension=dx:jB7ixkYVKD4&dimension=ou:LEVEL-3&startDate=",startDate,"&endDate=",endDate)
  
  
  r <- httr::GET(url, httr::authenticate(username,password),
                 httr::timeout(60))
  r <- httr::content(r, "text")
  # Convert the JSON to an R data structure
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  
  
  # Get the base data
  data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
  
  
  # Use the original response to get the names of the columns
  names(data) <- d$headers$column
  
  denominator <- data %>%
    dplyr::rename(orgUnitID = `Organisation unit`, denom = Value) %>%
    select(orgUnitID, denom)
  
  
  #denominator <- left_join(data, ou, by = "orgUnitID") %>%
    #dplyr::rename(district = "parent", structure = "displayName")
  
  print("Denominator")
  print(denominator)
  #indicator <- left_join(numerator, denominator, by = c("orgUnitID" = "district","structure" = "structure")) %>%
  
  indicator <- left_join(numerator, denominator, by = "orgUnitID" ) %>%
    group_by(orgUnitID)  %>%
    mutate(ratio = (num/as.numeric(denom))*100, .groups = 'keep') %>% 
    select(district, structure, ratio)
  print("Indicator")
  print(indicator)
  
  return(indicator)
}


getDiagRateQuartely <- function(startDate, endDate){
  
  #Get org units
  ou <- getOrgUnits(username,password)
  #####---------------GET NUMERATOR
  #Provenance
  url <- paste0(BASE.URL,BASE.REQ,"?stage=UD8R4GcuOsO&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&skipMeta=true&paging=false")
  
  r <- httr::GET(url, httr::authenticate(username,password),
                 httr::timeout(60))
  r <- httr::content(r, "text")
  # Convert the JSON to an R data structure
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  
  # Get the base data
  data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
  
  # Use the original response to get the names of the columns
  names(data) <- d$headers$column
  
  data <- data %>%
    dplyr::rename(orgUnitID = `Organisation unit`)
  
  #print(data)
  
  data <- left_join(data, ou, by = "orgUnitID") %>%
    dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
    distinct(Uuid, .keep_all = TRUE) %>%
    select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid)
  
  numerator <-  data %>% 
    group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
    summarise(n=n()) %>%
    dplyr::rename(numerator = n)
  
  print(numerator)
  
  
  #####---------------GET DENOMINATOR
  #Suivi patient
  url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&skipMeta=true&paging=false")
  
  r <- httr::GET(url, httr::authenticate(username,password),
                 httr::timeout(60))
  r <- httr::content(r, "text")
  # Convert the JSON to an R data structure
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  
  # Get the base data
  data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
  
  # Use the original response to get the names of the columns
  names(data) <- d$headers$column
  
  data <- data %>%
    distinct(Uuid, .keep_all = TRUE) %>%
    dplyr::rename(orgUnitID = `Organisation unit`)
  
  #print(data)
  
  
  data <- left_join(data, ou, by = "orgUnitID") %>%
    dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
    select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid)
  
  denominator <-  data %>% 
    group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
    summarise(n=n(), .groups = 'keep') %>%
    dplyr::rename(denominator = n)
  
  print(denominator)
  
  indicator <- left_join(numerator, denominator, by = c("district" = "district","structure" = "structure","Sexe"="Sexe", "groupe_age"="groupe_age")) %>%
    group_by(district,structure,Sexe, groupe_age)  %>%
    summarise(indicateur = sum(numerator/denominator), .groups = 'keep')
  
  print(indicator)
  
  return(indicator)
}

server <- function(input, output){
  
  login = F
  
  USER <- reactiveValues(login = login)
  
  
  #--------------------------------------------------------------
  #-----------------Monthly Diagnosed Rate ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_newlydiag, {
    
    startDate = input$monthlyDiag_startDate
    
    endDate = input$monthlyDiag_endDate
    
    indicator <- getDiagRateMonthly(startDate, endDate)
    
    output$monthlyDiag_contents = renderRpivotTable({
      rpivotTable(
        indicator,
        rows = c("district","structure","ratio"),
        cols = c("Sexe", "groupe_age"),
        aggregatorName = "Sum",
        vals = "indicateur",
        width = "100%",
        height = "500px"
      )
    })
    
  })
  
  #--------------------------------------------------------------
  #-----------------Total Diagnosed ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_totaldiag, {
  
    startDate = input$totaldiag_startDate
    
    endDate = input$totaldiag_endDate

    #Get org units
    ou <- getOrgUnits(username,password)
    
    #####---------------GET NUMERATOR
    #Suivi
    url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&dimension=wDk1IkO7kXQ&skipMeta=true&paging=false")
    
    print(url)
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
    
    data <- data %>%
      #distinct(Uuid, .keep_all = TRUE) %>%
      dplyr::rename(orgUnitID = `Organisation unit`) %>%
      filter(Traitement != "")

    data <- left_join(data, ou, by = "orgUnitID") %>%
      dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid)
    
    data <-  data %>% 
      group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
      summarise(n=n()) #%>%
      #dplyr::rename(numerator = n)
    
    print(data)
    
    output$totaldiag_contents = renderRpivotTable({
      rpivotTable(
        data,
        rows = c("district","structure"),
        cols = c("Sexe", "groupe_age"),
        aggregatorName = "Count",
        vals = "n",
        width = "100%",
        height = "500px"
      )
    })
    
  })
  
  #--------------------------------------------------------------
  #-----------------Net Systolic ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_netsystolic, {
    
    startDate = input$netsystolic_startDate
    
    endDate = input$netsystolic_endDate
    
    #Get org units
    ou <- getOrgUnits(username,password)
    
    #####---------------GET NUMERATOR
    #Suivi
    url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&dimension=iU1pq8kluwL&skipMeta=true&paging=false")
    
    print(url)
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
    
    data <- data %>%
      #distinct(Uuid, .keep_all = TRUE) %>%
      dplyr::rename(orgUnitID = `Organisation unit`) #%>%
      #filter(Traitement != "")
    
    
    data <- left_join(data, ou, by = "orgUnitID") %>%
      dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name",moy_systo = "MOY SYSTO") %>%
      group_by(district, orgUnitID,structure) %>%
      summarise(moy = mean(moy_systo)) %>%
      select(district, orgUnitID,structure, moy)
    
    print(data)
    return()
    
    data <-  data %>% 
      group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
      summarise(n=n()) #%>%
    #dplyr::rename(numerator = n)
    
    print(data)
    
    output$netsystolic_contents = renderRpivotTable({
      rpivotTable(
        data,
        rows = c("district","structure"),
        cols = c("Sexe", "groupe_age"),
        aggregatorName = "Count",
        vals = "n",
        width = "100%",
        height = "500px"
      )
    })
    
  })
  
  #--------------------------------------------------------------
  #-----------------Total Treated ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_totaltreat, {
    
    print("Total treated")
    
    startDate = input$totaltreat_startDate
    
    endDate = input$totaltreat_endDate

    
    #Get org units
    ou <- getOrgUnits(username,password)
    #####---------------GET NUMERATOR
    #Provenance
    url <- paste0(BASE.URL,BASE.REQ,"?stage=UD8R4GcuOsO&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&skipMeta=true&paging=false")
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
    
    data <- data %>%
      dplyr::rename(orgUnitID = `Organisation unit`)
    
    data <- left_join(data, ou, by = "orgUnitID") %>%
      dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid)
    
    data <-  data %>% 
      group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
      summarise(n=n()) #%>%
      #dplyr::rename(numerator = n)
    
    print(data)
    
    output$totaltreat_contents = renderRpivotTable({
      rpivotTable(
        data,
        rows = c("district","structure"),
        cols = c("Sexe", "groupe_age"),
        aggregatorName = "Count",
        vals = "n",
        width = "100%",
        height = "500px"
      )
    })
    
  })
  
  #--------------------------------------------------------------
  #-----------------Total Control ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_totalcontrol, {
    
    print("Total control")
    
    startDate = input$totalcontrol_startDate
    
    endDate = input$totalcontrol_endDate
    
    
    #Get org units
    ou <- getOrgUnits(username,password)
    #####---------------GET NUMERATOR
    
    url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&dimension=rrKGcWkFGv5&skipMeta=true&paging=false")
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
    
    data <- data %>%
      dplyr::rename(orgUnitID = `Organisation unit`)
    
    data <- left_join(data, ou, by = "orgUnitID") %>%
      dplyr::rename(event_date =  "Event date",district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name", pa_controle="PA Controlé") %>%
      select(event_date, district, orgUnitID,structure,`Sexe`,groupe_age, Uuid, pa_controle) %>%
      dplyr::filter(pa_controle == "1")
    
    data <-  data %>% 
      group_by(Uuid,district, orgUnitID,structure) %>%
      summarise(date_event = max(event_date),nb = n()) #%>%
    #dplyr::rename(numerator = n)
    
    
    output$totalcontrol_contents = renderRpivotTable({
      rpivotTable(
        data,
        rows = c("district","structure"),
        width = "100%",
        height = "500px"
      )
    })
    
  })
  
  
  #--------------------------------------------------------------
  #-----------------Total Screened ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_totalscreened, {
    
    print("Total screened")
    
    startDate = input$totalscreened_startDate
    
    endDate = input$totalscreened_endDate
    
    
    #Get org units
    ou <- getOrgUnits(username,password)
    #####---------------GET NUMERATOR
    
   
    base <- paste0(BASE.URL, "analytics?")
    url <- paste0(base,"dimension=dx:jB7ixkYVKD4&dimension=ou:LEVEL-3&startDate=",startDate,"&endDate=",endDate)
    
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
    
    total <- data %>%
      dplyr::rename(orgUnitID = `Organisation unit`, total = Value) %>%
      select(orgUnitID, total)
    #dplyr::rename(numerator = n)
    
    data <- left_join(total, ou, by = "orgUnitID") %>%
      dplyr::rename(district = "parent", structure = "displayName") %>%
      select(district,structure,total)
    
    print(data)
    
    output$totalscreened_contents = renderRpivotTable({
      rpivotTable(
        data,
        rows = c("district","structure","total"),
        aggregatorName = "Sum",
        vals = "total",
        width = "100%",
        height = "500px"
      )
    })
    
  })
  #--------------------------------------------------------------
  #-----------------Monthly Treatment Rate ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_monthlytreat, {
    
    startDate = input$monthlytreat_startDate
    
    endDate = input$monthlytreat_endDate
    
    indicator = getTreatmentRateMonthly(startDate,endDate)    #print(indicator)
    
    #output$monthlyTreat_contents <- renderDataTable(indicator)
    
    output$monthlyTreat_contents = renderRpivotTable({
      rpivotTable(
        indicator,
        rows = c("district","structure","ratio"),
        columns = c("sexe","group_age"),
        width = "100%",
        height = "500px"
      )
    })
    
  })
  #--------------------------------------------------------------
  #-----------------Quarterly Treatment Rate ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_quartelytreat , {
    
    startDate = input$quartelytreat_startDate
    
    endDate = input$quartelytreat_endDate
    
    indicator = getTreatmentRateQuartely(startDate,endDate)    #print(indicator)
    
    output$quartelytreat_contents = renderRpivotTable({
      rpivotTable(
        indicator,
        rows = c("district","structure"),
        cols = c("Sexe", "groupe_age"),
        aggregatorName = "Sum",
        vals = "indicateur",
        width = "100%",
        height = "500px"
      )
    })
    
  })
  
  #--------------------------------------------------------------
  #-----------------Quarterly Newly Diag Rate ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_quartelynewly , {
    
    startDate = input$quartelynewly_startDate
    
    endDate = input$quartelynewly_endDate
    
    indicator = getDiagRateQuartely(startDate,endDate) 
    
    output$quartelynewly_contents = renderRpivotTable({
      rpivotTable(
        indicator,
        rows = c("district","structure"),
        cols = c("Sexe", "groupe_age"),
        aggregatorName = "Sum",
        vals = "indicateur",
        width = "100%",
        height = "500px"
      )
    })
    
  })
  
  #--------------------------------------------------------------
  #-----------------Monthly Control Rate ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_monthlycont, {
    
    print("Monthly Control Real")
    
    startDate = input$monthlycont_startDate
    
    endDate = input$monthlytcont_endDate
    
    #Get org units
    ou <- getOrgUnits(username,password)
    
    #####---------------GET NUMERATOR
    #Suivi
    url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&dimension=rrKGcWkFGv5&skipMeta=true&paging=false")
    
    print(url)
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
      
    data <- data %>%
      dplyr::rename(orgUnitID = `Organisation unit`, PA_controle = `PA Controlé`, event_date = `Event date`) %>%
      dplyr::filter(PA_controle == "1")
    
    print(data)
    
    data <- left_join(data, ou, by = "orgUnitID") %>%
      dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid, event_date)
      
    numerator <-  data %>% 
      group_by(district, orgUnitID,structure,`Sexe`,groupe_age,Uuid) %>%
      dplyr::summarise(date_event = max(event_date),n=n(), .groups = 'keep') %>%
      dplyr::rename(numerator = n) %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age,numerator)

    
    #####---------------GET DENOMINATOR
    #Suivi patient
    url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&skipMeta=true&paging=false")
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
    
    data <- data %>%
      distinct(Uuid, .keep_all = TRUE) %>%
      dplyr::rename(orgUnitID = `Organisation unit`)
    
    #print(data)
    
    
    data <- left_join(data, ou, by = "orgUnitID") %>%
      dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid)
    
    denominator <-  data %>% 
      group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
      dplyr::summarise(n=n(), .groups = 'keep') %>%
      dplyr::rename(denominator = n)
    
    print(denominator)
    
    indicator <- left_join(numerator, denominator, by = c("district" = "district","structure" = "structure","Sexe"="Sexe", "groupe_age"="groupe_age")) %>%
      group_by(district,structure,Sexe, groupe_age)  %>%
      mutate(ratio = ((numerator)/(denominator))*100, .groups = 'keep') %>%
      select(district, structure, Sexe, groupe_age, ratio)
    
    print(indicator)
    
    output$monthlyCont_contents = renderRpivotTable({
      rpivotTable(
        indicator,
        rows = c("district","structure", "ratio"),
        width = "100%",
        height = "500px"
      )
    })
    
  })
  
  #--------------------------------------------------------------
  #-----------------Quartely Control Rate ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_quartelycont, {
    
    print("Quartely Control Rate")
    
    startDate = input$quartelycont_startDate
    
    endDate = input$quartelycont_endDate
    
    #Get org units
    ou <- getOrgUnits(username,password)
    
    #####---------------GET NUMERATOR
    #Suivi
    url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&dimension=rrKGcWkFGv5&skipMeta=true&paging=false")
    
    print(url)
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
    
    data <- data %>%
      distinct(Uuid, .keep_all = TRUE) %>%
      dplyr::rename(orgUnitID = `Organisation unit`, PA_controle = `PA Controlé`, event_date = `Event date`) %>%
      dplyr::filter(PA_controle == "1")
    
    print(data)
    
    data <- left_join(data, ou, by = "orgUnitID") %>%
      dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid, event_date)
    
    numerator <-  data %>% 
      group_by(district, orgUnitID,structure,`Sexe`,groupe_age,Uuid) %>%
      summarise(date_event = max(event_date),n=n(), .groups = 'keep') %>%
      dplyr::rename(numerator = n) %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age,numerator)
    
    print("DATA")
    print(numerator)
    
    
    #####---------------GET DENOMINATOR
    #Suivi patient
    url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&skipMeta=true&paging=false")
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
    
    data <- data %>%
      distinct(Uuid, .keep_all = TRUE) %>%
      dplyr::rename(orgUnitID = `Organisation unit`)
    
    #print(data)
    
    
    data <- left_join(data, ou, by = "orgUnitID") %>%
      dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid)
    
    denominator <-  data %>% 
      group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
      summarise(n=n(), .groups = 'keep') %>%
      dplyr::rename(denominator = n)
    
    print(denominator)
    
    indicator <- left_join(numerator, denominator, by = c("district" = "district","structure" = "structure","Sexe"="Sexe", "groupe_age"="groupe_age")) %>%
      group_by(district,structure,Sexe, groupe_age)  %>%
      summarise(indicateur = sum(numerator/denominator), .groups = 'keep')
    
    print(indicator)
    
    output$quartelycont_contents = renderRpivotTable({
      rpivotTable(
        indicator,
        rows = c("district","structure"),
        cols = c("Sexe", "groupe_age"),
        aggregatorName = "Sum",
        vals = "indicateur",
        width = "100%",
        height = "500px"
      )
    })
    
  })
  #--------------------------------------------------------------
  #-----------------Six Monthly Control Rate ----------------------
  #--------------------------------------------------------------
  observeEvent(input$btn_sixmonthlycont, {
    
    print("Six Monthly Control Rate")
    
    startDate = input$sixmonthlycont_startDate
    
    endDate = input$sixmonthlycont_endDate
    
    #Get org units
    ou <- getOrgUnits(username,password)
    
    #####---------------GET NUMERATOR
    #Suivi
    url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&dimension=rrKGcWkFGv5&skipMeta=true&paging=false")
    
    print(url)
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
    
    data <- data %>%
      distinct(Uuid, .keep_all = TRUE) %>%
      dplyr::rename(orgUnitID = `Organisation unit`, PA_controle = `PA Controlé`, event_date = `Event date`) %>%
      dplyr::filter(PA_controle == "1")
    
    print(data)
    
    data <- left_join(data, ou, by = "orgUnitID") %>%
      dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid, event_date)
    
    numerator <-  data %>% 
      group_by(district, orgUnitID,structure,`Sexe`,groupe_age,Uuid) %>%
      summarise(date_event = max(event_date),n=n(), .groups = 'keep') %>%
      dplyr::rename(numerator = n) %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age,numerator)
    
    print("DATA")
    print(numerator)
    
    
    #####---------------GET DENOMINATOR
    #Suivi patient
    url <- paste0(BASE.URL,BASE.REQ,"?stage=xVMoiMMEaqj&startDate=",startDate,"&endDate=",endDate,"&dimension=ou:",BASE.DIM,"&dimension=vjNskFa2nwh&dimension=iYMDdwJ0Kzk&dimension=WfCKF3dicir&dimension=SMLeL7kXzf4&skipMeta=true&paging=false")
    
    r <- httr::GET(url, httr::authenticate(username,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    # Convert the JSON to an R data structure
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    # Get the base data
    data <- as.data.frame(d$rows,stringsAsFactors=FALSE)
    
    # Use the original response to get the names of the columns
    names(data) <- d$headers$column
    
    data <- data %>%
      distinct(Uuid, .keep_all = TRUE) %>%
      dplyr::rename(orgUnitID = `Organisation unit`)
    
    #print(data)
    
    
    data <- left_join(data, ou, by = "orgUnitID") %>%
      dplyr::rename(district = "parent", groupe_age = "Groupe d’age", structure = "Organisation unit name") %>%
      select(district, orgUnitID,structure,`Sexe`,groupe_age, Uuid)
    
    denominator <-  data %>% 
      group_by(district, orgUnitID,structure,`Sexe`,groupe_age) %>%
      summarise(n=n(), .groups = 'keep') %>%
      dplyr::rename(denominator = n)
    
    print(denominator)
    
    indicator <- left_join(numerator, denominator, by = c("district" = "district","structure" = "structure","Sexe"="Sexe", "groupe_age"="groupe_age")) %>%
      group_by(district,structure,Sexe, groupe_age)  %>%
      summarise(indicateur = sum(numerator/denominator), .groups = 'keep')
    
    print(indicator)
    
    
    output$sixmonthlycont_contents = renderRpivotTable({
      rpivotTable(
        indicator,
        rows = c("district","structure"),
        cols = c("Sexe", "groupe_age"),
        aggregatorName = "Sum",
        vals = "indicateur",
        width = "40%",
        height = "200px"
      )
    })
    
  })
  
  observe({ 
    
    #LOGIN
    if (USER$login == FALSE) {
      
      if (!is.null(input$login)) {
        
        if (input$login > 0) {
          
          user <- isolate(input$userName)
          
          pass<- isolate(input$passwd)
      
          
          if(nchar(username) > 0 && nchar(password) > 0) { 
            
            request <- paste0(BASE.URL,"me")
            
            res <- GET(request,authenticate(user,pass))
            
            
            if(res$status == 200L) {
              
              USER$login <- TRUE
              
            } else {
              
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              
            }
          } else {
            
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    
    req(USER$login)
    
    tags$li(a(icon("fa fa-sign-out"), "Déconnexion", 
              href="javascript:window.location.reload(true)"),
              class = "dropdown", 
              style = "font-color:#ffffff;background-color: #f39c12 !important; border: 1px solid #f39c12;
              font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebar
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      dashboard
    }
    else {
      loginpage
    }
  })
}

shinyApp(ui, server)