#
#
# octavian
# Git
# /OneDrive/metis2/metic_select_retrieve/octavian
# - resegment_observe branch

# SERVER
#
#
# IMPORTANT NOTE - If plyr is loaded after dplyr (in tidyverse()), then group_by statement will fail
# with group factor not carried across
#
#


library(shiny)
library(shinydashboard)
Sys.setlocale('LC_ALL','C')
library(shiny)
library(pool)
library(tidyverse)
library(DBI)
library(zoo)
library(shinysky)
library(reshape2)
library(shinythemes)
library(shinyWidgets)
library(MASS)
library(plotly)
#library(ggpubr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
