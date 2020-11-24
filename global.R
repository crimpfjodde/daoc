library(shinydashboard)
library(dashboardthemes)
library(shiny)
library(plotly)
library(shinyjs)
library(shinyBS)
library(DT)
library(RCurl)
library(XML)
library(dplyr)
# library(waiter)
library(shinycssloaders)
library(shinyWidgets)
# source('./R/function_daoc.R')

load('data/daoc.Rdata')


options(spinner.type = 6,
        spinner.size = 1
        )# spinner.color = "#E87722"