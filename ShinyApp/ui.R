## ui.R
#install.packages('tm')

library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
            sidebarMenu(
              menuItem(tabName = "genre", "By Movie Genre",icon = icon("film")),
              menuItem(tabName = "rate", "By Movie Ratings",icon = icon("film")))),

          dashboardBody(includeCSS("css/movies.css"),
            tabItems(            
                tabItem(
                          tabName = "genre",
                          
                          fluidRow(
                            textOutput("Select atleast 3 movie genre")
                          ),
                          fluidRow(
                            box(width = 12, title = "Select movie genre for recommendation", status = "info", solidHeader = TRUE, collapsible = TRUE,
                            div(selectInput("genrelist", "",
                                        c("Action","Adventure", "Animation", "Children", 
                                          "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                                          "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                                          "Sci-Fi", "Thriller", "War", "Western"),width = 400)))
                        ),
                        fluidRow(
                          useShinyjs(),
                          box(width = 12, title = "Movies we think you might like ", status = "info", solidHeader = TRUE,
                          #verbatimTextOutput("verb1"),
                          #verbatimTextOutput("verb2"),                              
                          withBusyIndicatorUI(                              
                          actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")),
                          br(),
                          tableOutput("results1"),
                          br()
                          )
                        )),
            tabItem(
                tabName = "rate",                        
                          
              fluidRow(
                  box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings')
                      )
                  )
                ),
              fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover movies you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results")
                  )
               )
            )
          )
          
    )
)
)