library(shinydashboard)
library(shiny)
library(kableExtra)
library(tidyverse)
library(plotly)
library(RColorBrewer)

header <- dashboardHeader(title = "Squash Dashboard")
body <- dashboardBody(uiOutput("body"))
siderbar <- dashboardSidebar(uiOutput("sidebar"))

dashboardPage(header, siderbar, body)