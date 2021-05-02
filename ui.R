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
library(ggpubr)
library(ggbiplot)

dashboardPage(
    skin = "red",

    dashboardHeader(title = "Media Sentiment Analysis", titleWidth = 450),
    dashboardSidebar(
        dateRangeInput(inputId= 'dateRange',
                       label = "Date range",
                       start = Sys.Date() - 14,
                       format = "yyyy_mm_dd"),
        numericInput(inputId = "dateGrouping",
                     "Rolling average ",
                     value = 5,
                     min = 1,
                     max = 90),
        sidebarMenu(
            menuItem("Comparison", tabName = "comparison", icon = icon("chart-line")),
            menuItem("Individual", tabName = "individual", icon = icon("dashboard")),
            menuItem("Sentiment", tabName = "sentiment", icon = icon("dashboard")),
            menuItem("Correlation", tabname = "correlation", icon = icon("dashboard")),
            menuItem("Source", tabName = "source", icon = icon("dashboard"))
        )
    ),
    #    print("ui 1 - Start of page"),
    ######
     dashboardBody(

         ##### Dropdown selection items
         fluidRow(column(width = 2,
               dropdown(
 #                  tooltip = TRUE,
                   label = "Selection 1",
                   tags$h3("Selection 1"),
                   # selectizeInput("isource",
                   #                "Source 1",
                   #                choices = c("One", "Two", "Three")         #rssSources.names,
                   #                multiple = TRUE),

                   selectizeInput("isourcetype",
                                  "Source Type 1",
                                  choices = rss.SourceTypes,
                                  multiple = TRUE),
                   selectizeInput("icountry",
                                  "Country 1",
                                  choices = rss.Countries,
                                  multiple = TRUE),
                   selectizeInput("iregion",
                                  'Region 1', choices = rss.Regions,
                                  multiple = TRUE),
                   selectizeInput("iorientation",
                                  "Orientation 1",
                                  choices = rss.Orientation,
                                  multiple = TRUE),
                   selectizeInput("iSentimentFactor",
                                  "Sentiment factor 1",
                                  c("Syuzhet" = 'syuzhet', "Afinn" = "afinn","Bing" = "bing", "Anger - nrc" = "nrc_anger","Anticipation - nrc" = "nrc_anticipation","Disgust - nrc" = "nrc_disgust",
                                    "Fear - nrc" = "nrc_fear", "Joy - nrc" = "nrc_joy","Positive- nrc"= "nrc_positive","Negative - nrc" = "nrc_negative","Sadness - nrc" = "nrc_sadness",
                                    "Surprise - nrc" = "nrc_surprise", "Trust - nrc" = "nrc_trust","Constraining - Lo" = "loughran_constraining","Litigous - Lo" = "loughran_litigious",
                                    "Uncertain- Lo" = "loughran_uncertain", "Negative - Lo" = "loughran_negative","Positive - Lo" = "loughran_positive", "Ensemble +/-" = "ensemble_posneg") ,
                                  multiple = TRUE,
                                  selected = "ensemble_posneg"),
                   textInput("itextinput",
                             "Text selection 1",
                             value = " ")
               )
        ),

        column(width = 2,
               dropdown(
                   tooltip = TRUE,
                   label = "Selection 2",
                   tags$h3("Selection 2"),
                   # selectizeInput("isource2",
                   #                "Source 2",
                   #                choices = rssSources.names,
                   #                multiple = TRUE),
                   selectizeInput("isourcetype2",
                                  "Source Type 2",
                                  choices = rss.SourceTypes,
                                  multiple = TRUE),
                   selectizeInput("icountry2",
                                  "Country 2",
                                  choices = rss.Countries,
                                  multiple = TRUE),
                   selectizeInput("iregion2",
                                  'Region 2', choices = rss.Regions,
                                  multiple = TRUE),
                   selectizeInput("iorientation2",
                                  "Orientation 2",
                                  choices = rss.Orientation,
                                  multiple = TRUE),
                   selectizeInput("iSentimentFactor2",
                                  "Sentiment factor 2",
                                  c("Syuzhet" = 'syuzhet', "Afinn" = "afinn","Bing" = "bing", "Anger - nrc" = "nrc_anger","Anticipation - nrc" = "nrc_anticipation","Disgust - nrc" = "nrc_disgust",
                                    "Fear - nrc" = "nrc_fear", "Joy - nrc" = "nrc_joy","Positive- nrc"= "nrc_positive","Negative - nrc" = "nrc_negative","Sadness - nrc" = "nrc_sadness",
                                    "Surprise - nrc" = "nrc_surprise", "Trust - nrc" = "nrc_trust","Constraining - Lo" = "loughran_constraining","Litigous - Lo" = "loughran_litigious",
                                    "Uncertain- Lo" = "loughran_uncertain", "Negative - Lo" = "loughran_negative","Positive - Lo" = "loughran_positive", "Ensemble +/-" = "ensemble_posneg") ,
                                  multiple = TRUE,
                                  selected = "ensemble_posneg"),
                   textInput("itextinput2",
                             "Text selection 2",
                             value = " ")
               )
        ),
        column(width = 2,
               dropdown(
                   tootip = TRUE,
                   label = "Smooth/Corr",
                   tags$h3("Smoothing"),
                   radioButtons("ismooth", "Method",
                                c("None"= "", "loess" = "loess", "lm" = "lm","gam" = "gam", "glm" = "glm", "MASS:rlm" = "MASS:rlm" )),

                   numericInput("iconfidenceLevel", label = "Confidence value", value = 0.95, min = 0, max = 1, width = "30%" ),
                   checkboxInput("iconfidence", label = "On", FALSE),
                   tags$h3("Correlation"),
                   selectizeInput("icorrelate", label = "Method", c("pearson", "kendall", "spearman"), multiple = FALSE),
                   selectizeInput("icorr.alternate", label = "Alternative", c("two.sided", "greater", "less"))
               )),
        column(width = 2,
               dropdown(
                   tooltip = TRUE,
                   label = "Normalize",
                   fluidRow(
                       tags$h3("Normalise"),
                       checkboxInput("iPosNegNorm", "Pos/neg"),
                       checkboxInput("iLRCNorm", "Orientation"),
                       checkboxInput("iCountryNorm", "Countries"))
               )),
        column(width = 2,
               dropdown(
                   tooltip = TRUE,
                   label = "Format",
                   fluidRow(
                       tags$h3("Chart format"),
                       tags$h5("Time Series"),
                       checkboxInput("aColumn", "Column", FALSE),
                       checkboxInput("aLine", "Line", TRUE),
                       checkboxInput("aPoint", "Points", FALSE),
                       tags$h5("Correlation"),
                       checkboxInput("aStar", "Star", FALSE)
                   )

               ))),
#################### Chart section



    fluidRow(
            h4("Comparative content"),
            column(width = 12, plotlyOutput("SA_by_date_line_comp"))

        ),
              # End of tab 1

        h4("Selection 1"),
        fluidRow(
            column(width = 6, plotlyOutput("SA_by_date_line")),

            column(width = 6, plotlyOutput("SA_summary_by_period"))),
        h4("Selection 2"),
        fluidRow(
            column(width = 6, plotlyOutput("SA_by_date_line2")),
            column(width = 6, plotlyOutput("SA_summary_by_period2"))),
              # End of tab 2


       fluidRow(
           h4("Autocorrelation"),
           column(width = 6, plotOutput("ACF1_large")),
           column(width = 6, plotOutput("ACF2_large"))
       ),
        h4("Statistics"),
        fluidRow(
            column(width = 8, plotlyOutput("SA_correlation")),
            column(width = 4, DT::dataTableOutput("corrStats"))
        ),


        fluidRow(
            h4("Cluster"),
            h5("Principal Component Analysis"),
            column(width = 12, plotOutput("PCA"))
        ),


##################### Source

        h4("Sources"),
        DT::dataTableOutput("tbl")
)

#    )
)
