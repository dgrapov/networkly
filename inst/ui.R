library(shiny)
library(shinydashboard)
library(plotly)
library(jsonlite)
library(networkly)
library(dplyr)

shinyUI(dashboardPage(
  dashboardHeader(title="Plotly Network"),
  dashboardSidebar(uiOutput("controls")),
  dashboardBody(
    uiOutput("network_UI"),
    uiOutput("obj_UI")
  )
))
