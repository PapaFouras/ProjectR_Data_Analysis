library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(leaflet.extras)

#peut etre mettre les install des lib ? 

# dataset <- trains


dashboardPage(
  dashboardHeader(title = "Projet"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("SNCF", tabName = "trains", icon = icon("train")),
      menuItem("Flights", tabName = "flights", icon = icon("plane"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "trains",
              fluidRow(    
                box(
                  selectInput(
                    inputId = "Annee",
                    label = "Year : ",
                    choices = c("ALL YEARS", as.character(sort(unique(ordered(agg_train_by_Année$Année))))),
                    selected = NULL)
                ),
                box(
                  selectInput(
                    inputId = "Gare.de.depart",
                    label = "Train Stations : ",
                    choices = c("ALL TRAIN STATIONS", as.character(sort(unique(agg_train_by_Gare_Départ$Gare.de.départ)))),
                    selected = NULL
                  ))
              ),
              tabsetPanel(
                tabPanel("Values", 
                         fluidRow(
                           valueBoxOutput("trains_carried_out",  width=3),
                           valueBoxOutput("trains_cancelled_percent", width=3),
                           valueBoxOutput("trains_cancelled", width=3)
                         ),
                         fluidRow(
                           valueBoxOutput("trains_delayed_dep", width=3),
                           valueBoxOutput("trains_delayed_arr", width=3)
                         ),
                         fluidRow(
                           valueBoxOutput("avg_dep_delay_time_all_trains", width=3),
                           valueBoxOutput("avg_arr_delay_time_all_trains", width=3),
                           valueBoxOutput("avg_dep_delay_time_delayed_trains", width=3),
                           valueBoxOutput("avg_arr_delay_time_delayed_trains", width=3)
                         ),
                         fluidRow(
                           valueBoxOutput("avg_trains_delayed_dep", width=3),
                           valueBoxOutput("avg_trains_delayed_arr", width=3)
                         )
                ),
                
                tabPanel("Distribution Delay Causes", 
                         fluidRow(
                           box(
                             plotOutput("proportion_of_causes"),
                             width = 12, status = "info", solidHeader = TRUE
                             # title = "Répartition des causes du retard"
                           )
                         )
                ),
                tabPanel("Evolution of cancelled Trains",
                         fluidRow(
                           box(
                             plotOutput("evolution_of_cancelled_trains"),
                             width = 12, status = "info", solidHeader = TRUE
                             # title = "Evolution des trains annulés entre 2015 et 2018"
                           )
                         )
                ),
                tabPanel("Evolution of delayed Trains",
                         fluidRow(
                           box(
                             plotOutput("evolution_of_delayed_trains"),
                             width = 12, status = "info", solidHeader = TRUE
                           )
                         )
                )
                ,
                tabPanel("Overview of trains",
                         fluidRow(
                           box(
                             plotOutput("overview_of_trains"),
                             width = 12, status = "info", solidHeader = TRUE
                           )
                         )
                )
                
              )
              
      )
      ,
      
      tabItem(tabName = "flights",
              fluidRow(
                box(
                  selectInput(inputId = "Airline", label = "Choose an airline : ", choices = c("ALL AIRLINES", as.character(sort(unique(airlines$AIRLINE)))),selected = NULL)
                ),
                box(
                  selectInput(inputId = "Airport", label = "Choose an airport for departure : ", choices = c("ALL AIRPORTS", as.character(sort(unique(airports$AIRPORT)))),selected = NULL
                  )) 
              ) ,
              tabsetPanel(
                tabPanel("Values",
                         fluidRow(
                           valueBoxOutput("total_nb_flights",  width=3),
                           valueBoxOutput("avg_flight_duration",  width=3),
                           valueBoxOutput("avg_flight_distance",  width=3),
                           valueBoxOutput("total_distance", width=3)
                           
                         ),
                         fluidRow(
                           valueBoxOutput("total_nb_delayed_flights",  width=3),
                           valueBoxOutput("avg_depature_delay", width=3),
                           valueBoxOutput("avg_arrival_delay", width=3)
                           
                         )
                         
                ),
                
                tabPanel("Map",
                         fluidRow(
                           box(
                             width = 12,
                             leafletOutput(outputId = "map_flights", height = 500)
                           )
                         )
                )
              )
      )
    )
  )
)