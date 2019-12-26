library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(shape)
library(leaflet)
library(leaflet.extras)
library(shinydashboard)
library(shinyWidgets)

#install.packages("utf8")

#install.packages(pkgs="dplyr",dependencies=TRUE)
#install.packages(pkgs="plotly",dependencies=TRUE)
#install.packages(pkgs="scales",dependencies=TRUE)
#install.packages(pkgs="shape",dependencies=TRUE)

aggregation_trains <- function(input, agg_val){
  #toutes les données
  if(input$Annee == 'ALL YEARS' & input$Gare.de.depart == 'ALL TRAIN STATIONS') {
    valeur <- agg_train %>%
      select(agg_val)
  } 
  #Données groupées par Gare pour toutes les années 
  else if (input$Annee == 'ALL YEARS') {
    valeur <- agg_train_by_Gare_Départ %>%
      filter(Gare.de.départ == input$Gare.de.depart) %>%
      select(agg_val)
  }
  #Données groupées par Année pour toutes les Gares
  else if (input$Gare.de.depart == 'ALL TRAIN STATIONS') {
    valeur <- agg_train_by_Année %>%
      filter(Année == input$Annee) %>%
      select(agg_val)
  } 
  #Données groupées par Année et par Gare 
  else {
    valeur <- agg_train_by_Année_Gare_Départ %>%
      filter(Année == input$Annee & Gare.de.départ == input$Gare.de.depart) %>%
      ungroup() %>%
      select(agg_val)
  }
  return(valeur)
}


aggregation_flights <- function(input, agg_val){
  #toutes les données
  if(input$Airline == 'ALL AIRLINES' & input$Airport == 'ALL AIRPORTS') {
    valeur <- agg_flights %>%
      select(agg_val)
  } 
  #Données groupées par airport selon tous les airlines 
  else if (input$Airline == 'ALL AIRLINES') {
    valeur <- agg_flights_by_airport %>%
      filter(AIRPORT == input$Airport) %>%
      select(agg_val)
  }
  #Données groupées par airline selon tous les airports
  else if (input$Airport == 'ALL AIRPORTS') {
    valeur <- agg_flights_by_airline %>%
      filter(AIRLINE.y == input$Airline) %>%
      select(agg_val)
  } 
  #Données groupées par airport et airline
  else {
    valeur <- agg_flights_by_airline_airport %>%
      filter(AIRLINE.y == input$Airline & AIRPORT == input$Airport) %>%
      ungroup() %>%
      select(agg_val)
  }
  return(valeur)
}


init_trains <- function(){
  
  agg_train <- read.csv(file = "regularite-mensuelle-tgv-aqst.csv", sep=";", encoding="UTF-8") %>%
    summarise(
      total_nb_trains_carried_out = sum(Nombre.de.circulations.prévues-Nombre.de.trains.annulés, na.rm=TRUE),
      total_nb_trains_delayed_departure = sum(Nombre.de.trains.en.retard.au.départ, na.rm=TRUE),
      total_nb_trains_delayed_arrival = sum(Nombre.de.trains.en.retard.à.l.arrivée,na.rm=TRUE),
      avg_nb_delayed_trains_departure = mean(Nombre.de.trains.en.retard.au.départ, na.rm=TRUE),
      avg_nb_delayed_trains_arrival = mean(Nombre.de.trains.en.retard.à.l.arrivée, na.rm=TRUE),
      total_avg_departure_delay_time_all_trains = mean(Retard.moyen.de.tous.les.trains.au.départ..min., na.rm=TRUE),
      total_avg_arrival_delay_time_all_trains = mean(Retard.moyen.de.tous.les.trains.à.l.arrivée..min., na.rm=TRUE),
      total_avg_departure_delay_time_delayed_trains = mean(Retard.moyen.des.trains.en.retard.au.départ..min.,na.rm=TRUE),
      total_avg_arrival_delay_time_delayed_trains = mean(Retard.moyen.des.trains.en.retard.à.l.arrivée..min.,na.rm=TRUE),
      total_nb_cancelled_trains = sum(Nombre.de.trains.annulés,na.rm=TRUE),
      percentage_cancelled_trains = mean((Nombre.de.trains.annulés/Nombre.de.circulations.prévues)*100, na.rm=TRUE),
      causes_externes = mean(Retard.pour.causes.externes, na.rm = TRUE),
      causes_infra_ferroviaire = mean(Retard.à.cause.infrastructure.ferroviaire, na.rm = TRUE),
      causes_gestion_trafic = mean(Retard.à.cause.gestion.trafic, na.rm = TRUE),
      causes_materiel_roulant = mean(Retard.à.cause.matériel.roulant, na.rm = TRUE),
      causes_gestion_gare = mean(Retard.à.cause.gestion.en.gare.et.réutilisation.de.matériel, na.rm = TRUE),
      causes_voyageur = mean(Retard.à.cause.prise.en.compte.voyageurs, na.rm = TRUE)
    )
  
  agg_train_by_Année <- read.csv(file = "regularite-mensuelle-tgv-aqst.csv", sep=";", encoding="UTF-8") %>%
    group_by(Année) %>%
    summarise(
      total_nb_trains_carried_out = sum(Nombre.de.circulations.prévues-Nombre.de.trains.annulés, na.rm=TRUE),
      total_nb_trains_delayed_departure = sum(Nombre.de.trains.en.retard.au.départ, na.rm=TRUE),
      total_nb_trains_delayed_arrival = sum(Nombre.de.trains.en.retard.à.l.arrivée,na.rm=TRUE),
      avg_nb_delayed_trains_departure = mean(Nombre.de.trains.en.retard.au.départ, na.rm=TRUE),
      avg_nb_delayed_trains_arrival = mean(Nombre.de.trains.en.retard.à.l.arrivée, na.rm=TRUE),
      total_avg_departure_delay_time_all_trains = mean(Retard.moyen.de.tous.les.trains.au.départ..min., na.rm=TRUE),
      total_avg_arrival_delay_time_all_trains = mean(Retard.moyen.de.tous.les.trains.à.l.arrivée..min., na.rm=TRUE),
      total_avg_departure_delay_time_delayed_trains = mean(Retard.moyen.des.trains.en.retard.au.départ..min.,na.rm=TRUE),
      total_avg_arrival_delay_time_delayed_trains = mean(Retard.moyen.des.trains.en.retard.à.l.arrivée..min.,na.rm=TRUE),
      total_nb_cancelled_trains = sum(Nombre.de.trains.annulés,na.rm=TRUE),
      percentage_cancelled_trains = mean((Nombre.de.trains.annulés/Nombre.de.circulations.prévues)*100, na.rm=TRUE),
      causes_externes = mean(Retard.pour.causes.externes, na.rm = TRUE),
      causes_infra_ferroviaire = mean(Retard.à.cause.infrastructure.ferroviaire, na.rm = TRUE),
      causes_gestion_trafic = mean(Retard.à.cause.gestion.trafic, na.rm = TRUE),
      causes_materiel_roulant = mean(Retard.à.cause.matériel.roulant, na.rm = TRUE),
      causes_gestion_gare = mean(Retard.à.cause.gestion.en.gare.et.réutilisation.de.matériel, na.rm = TRUE),
      causes_voyageur = mean(Retard.à.cause.prise.en.compte.voyageurs, na.rm = TRUE)
    )
  
  
  agg_train_by_Gare_Départ <- read.csv(file = "regularite-mensuelle-tgv-aqst.csv", sep=";", encoding="UTF-8") %>%
    group_by(Gare.de.départ) %>%
    summarise(
      total_nb_trains_carried_out = sum(Nombre.de.circulations.prévues-Nombre.de.trains.annulés, na.rm=TRUE),
      total_nb_trains_delayed_departure = sum(Nombre.de.trains.en.retard.au.départ, na.rm=TRUE),
      total_nb_trains_delayed_arrival = sum(Nombre.de.trains.en.retard.à.l.arrivée,na.rm=TRUE),
      avg_nb_delayed_trains_departure = mean(Nombre.de.trains.en.retard.au.départ, na.rm=TRUE),
      avg_nb_delayed_trains_arrival = mean(Nombre.de.trains.en.retard.à.l.arrivée, na.rm=TRUE),
      total_avg_departure_delay_time_all_trains = mean(Retard.moyen.de.tous.les.trains.au.départ..min., na.rm=TRUE),
      total_avg_arrival_delay_time_all_trains = mean(Retard.moyen.de.tous.les.trains.à.l.arrivée..min., na.rm=TRUE),
      total_avg_departure_delay_time_delayed_trains = mean(Retard.moyen.des.trains.en.retard.au.départ..min.,na.rm=TRUE),
      total_avg_arrival_delay_time_delayed_trains = mean(Retard.moyen.des.trains.en.retard.à.l.arrivée..min.,na.rm=TRUE),
      total_nb_cancelled_trains = sum(Nombre.de.trains.annulés,na.rm=TRUE),
      percentage_cancelled_trains = mean((Nombre.de.trains.annulés/Nombre.de.circulations.prévues)*100, na.rm=TRUE),
      causes_externes = mean(Retard.pour.causes.externes, na.rm = TRUE),
      causes_infra_ferroviaire = mean(Retard.à.cause.infrastructure.ferroviaire, na.rm = TRUE),
      causes_gestion_trafic = mean(Retard.à.cause.gestion.trafic, na.rm = TRUE),
      causes_materiel_roulant = mean(Retard.à.cause.matériel.roulant, na.rm = TRUE),
      causes_gestion_gare = mean(Retard.à.cause.gestion.en.gare.et.réutilisation.de.matériel, na.rm = TRUE),
      causes_voyageur = mean(Retard.à.cause.prise.en.compte.voyageurs, na.rm = TRUE)
    )
  
  
  agg_train_by_Année_Gare_Départ <- read.csv(file = "regularite-mensuelle-tgv-aqst.csv", sep=";", encoding="UTF-8") %>%
    group_by(Année,Gare.de.départ) %>%
    summarise(
      total_nb_trains_carried_out = sum(Nombre.de.circulations.prévues-Nombre.de.trains.annulés, na.rm=TRUE),
      total_nb_trains_delayed_departure = sum(Nombre.de.trains.en.retard.au.départ, na.rm=TRUE),
      total_nb_trains_delayed_arrival = sum(Nombre.de.trains.en.retard.à.l.arrivée,na.rm=TRUE),
      avg_nb_delayed_trains_departure = mean(Nombre.de.trains.en.retard.au.départ, na.rm=TRUE),
      avg_nb_delayed_trains_arrival = mean(Nombre.de.trains.en.retard.à.l.arrivée, na.rm=TRUE),
      total_avg_departure_delay_time_all_trains = mean(Retard.moyen.de.tous.les.trains.au.départ..min., na.rm=TRUE),
      total_avg_arrival_delay_time_all_trains = mean(Retard.moyen.de.tous.les.trains.à.l.arrivée..min., na.rm=TRUE),
      total_avg_departure_delay_time_delayed_trains = mean(Retard.moyen.des.trains.en.retard.au.départ..min.,na.rm=TRUE),
      total_avg_arrival_delay_time_delayed_trains = mean(Retard.moyen.des.trains.en.retard.à.l.arrivée..min.,na.rm=TRUE),
      total_nb_cancelled_trains = sum(Nombre.de.trains.annulés,na.rm=TRUE),
      percentage_cancelled_trains = mean((Nombre.de.trains.annulés/Nombre.de.circulations.prévues)*100, na.rm=TRUE),
      causes_externes = mean(Retard.pour.causes.externes, na.rm = TRUE),
      causes_infra_ferroviaire = mean(Retard.à.cause.infrastructure.ferroviaire, na.rm = TRUE),
      causes_gestion_trafic = mean(Retard.à.cause.gestion.trafic, na.rm = TRUE),
      causes_materiel_roulant = mean(Retard.à.cause.matériel.roulant, na.rm = TRUE),
      causes_gestion_gare = mean(Retard.à.cause.gestion.en.gare.et.réutilisation.de.matériel, na.rm = TRUE),
      causes_voyageur = mean(Retard.à.cause.prise.en.compte.voyageurs, na.rm = TRUE)
    )
}

init_flights <-function(){
  
  # aiports and airlines information
  
  airports <- read.csv(file = "airports.csv", sep=",", encoding="UTF-8")
  airlines <- read.csv(file = "airlines.csv", sep=",", encoding="UTF-8")
  
  
  # agg_flights_by_airline give us : 
  #   nb_flight, nb_delayed_flights, avg_duration, avg_distance, total_distance, avg_departure_delay, avg_arrival_delay
  # of all flights for each airline
  # and then we join it with airlines information dataset to enrich it 
  
  agg_flights_by_airline <- read.csv(file = "flights.csv", sep=",", encoding="UTF-8") %>%
    group_by(AIRLINE) %>%
    summarise(
      nb_flight = n(),
      nb_delayed_flights = sum(DEPARTURE_DELAY>0, na.rm=TRUE),
      avg_duration = mean(ARRIVAL_TIME - DEPARTURE_TIME, na.rm = TRUE),
      avg_distance = mean(DISTANCE, na.rm = TRUE),
      total_distance = sum(DISTANCE, na.rm = TRUE),
      avg_departure_delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
      avg_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)
    ) %>%
    merge(airlines, by.x=c("AIRLINE"), by.y=c("IATA_CODE"))
  
  
  
  # agg_flights_by_airport give us : 
  #   nb_flight, nb_delayed_flights, avg_duration, avg_distance, total_distance, avg_departure_delay, avg_arrival_delay
  # at the departure of each airport 
  # and then we join it with airports information dataset to enrich it 
  
  agg_flights_by_airport <- read.csv(file = "flights.csv", sep=",", encoding="UTF-8") %>%
    group_by(ORIGIN_AIRPORT) %>%
    summarise(
      nb_flight = n(),
      nb_delayed_flights = sum(DEPARTURE_DELAY>0, na.rm=TRUE),
      avg_duration = mean(ARRIVAL_TIME - DEPARTURE_TIME, na.rm = TRUE),
      avg_distance = mean(DISTANCE, na.rm = TRUE),
      total_distance = sum(DISTANCE, na.rm = TRUE),
      avg_departure_delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
      avg_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)
    ) %>%
    merge(airports, by.x=c("ORIGIN_AIRPORT"), by.y=c("IATA_CODE"))
  
  
  
  # agg_flights_by_airline_airport give us : 
  #   nb_flight, nb_delayed_flights, avg_duration, avg_distance, total_distance, avg_departure_delay, avg_arrival_delay
  # of each airline at the departure of each airport 
  # and then we join it with airlines and airports information datasets to enrich it 
  
  agg_flights_by_airline_airport <- read.csv(file = "flights.csv", sep=",", encoding="UTF-8") %>%
    group_by(AIRLINE,ORIGIN_AIRPORT) %>%
    summarise(
      nb_flight = n(),
      nb_delayed_flights = sum(DEPARTURE_DELAY>0, na.rm=TRUE),
      avg_duration = mean(ARRIVAL_TIME - DEPARTURE_TIME, na.rm = TRUE),
      avg_distance = mean(DISTANCE, na.rm = TRUE),
      total_distance = sum(DISTANCE, na.rm = TRUE),
      avg_departure_delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
      avg_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)
    ) %>%
    merge(airlines, by.x=c("AIRLINE"), by.y=c("IATA_CODE")) %>% 
    merge(airports, by.x=c("ORIGIN_AIRPORT"), by.y=c("IATA_CODE"))
  
  
  
  # agg_flights give us : 
  #   nb_flight, nb_delayed_flights, avg_duration, avg_distance, total_distance, avg_departure_delay, avg_arrival_delay
  # of all flights 
  
  agg_flights <- read.csv(file = "flights.csv", sep=",", encoding="UTF-8") %>%
    summarise(
      nb_flight = n(),
      nb_delayed_flights = sum(DEPARTURE_DELAY>0, na.rm=TRUE),
      avg_duration = mean(ARRIVAL_TIME - DEPARTURE_TIME, na.rm = TRUE),
      avg_distance = mean(DISTANCE, na.rm = TRUE),
      total_distance = sum(as.numeric(DISTANCE), na.rm = TRUE),
      avg_departure_delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
      avg_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)
    )
  
  
  
  ######## MAP ######## 
  
  
  # give us : 
  #   nb_flight, nb_delayed_flights, avg_duration, avg_distance, total_distance, avg_departure_delay, avg_arrival_delay
  # for each path (defined by an origin and a destinatination)
  # and then we join it with airports dataset twice to get information for both origin and destination 
  
  agg_flights_by_airport_map <- read.csv(file = "flights.csv", sep=",", encoding="UTF-8") %>%
    group_by(ORIGIN_AIRPORT, DESTINATION_AIRPORT) %>%
    summarise(
      nb_flight = n(),
      nb_delayed_flights = sum(DEPARTURE_DELAY>0, na.rm=TRUE),
      avg_duration = mean(ARRIVAL_TIME - DEPARTURE_TIME, na.rm = TRUE),
      avg_distance = mean(DISTANCE, na.rm = TRUE),
      total_distance = sum(DISTANCE, na.rm = TRUE),
      avg_departure_delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
      avg_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)
    ) %>%
    merge(airports,by.x=c("ORIGIN_AIRPORT"), by.y=c("IATA_CODE")) %>%
    merge(airports, by.x=c("DESTINATION_AIRPORT"), by.y=c("IATA_CODE"))
  
  
  # give us : 
  #   nb_flight, nb_delayed_flights, avg_duration, avg_distance, total_distance, avg_departure_delay, avg_arrival_delay
  # for each path (defined by an origin and a destinatination) of each airline
  # and we join it with airlines dataset to get information about the airline and 
  # also with airports dataset twice to get information for both origin and destination and once
  
  agg_flights_by_airline_map <- read.csv(file = "flights.csv", sep=",", encoding="UTF-8") %>%
    group_by(AIRLINE, ORIGIN_AIRPORT, DESTINATION_AIRPORT) %>%
    summarise(nb_flight = n(),
              nb_delayed_flights = sum(DEPARTURE_DELAY>0, na.rm=TRUE),
              avg_duration = mean(ARRIVAL_TIME - DEPARTURE_TIME, na.rm = TRUE),
              avg_distance = mean(DISTANCE, na.rm = TRUE),
              total_distance = sum(DISTANCE, na.rm = TRUE),
              avg_departure_delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
              avg_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)
    ) %>%
    merge(airlines,by.x=c("AIRLINE"), by.y=c("IATA_CODE")) %>%
    merge(airports,by.x=c("ORIGIN_AIRPORT"), by.y=c("IATA_CODE")) %>%
    merge(airports,by.x=c("DESTINATION_AIRPORT"), by.y=c("IATA_CODE"))
}


function(input, output) {
  
  
  ########### SNCF  ########### 
  
  output$trains_carried_out <- renderValueBox({
    
    if (nrow(aggregation_trains(input, 'total_nb_trains_carried_out'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- aggregation_trains(input, 'total_nb_trains_carried_out')
      color_ <- "green"
      icon_ = icon("train")
    }
    
    valueBox(
      subtitle = 'Carried out',
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$trains_delayed_dep <- renderValueBox({
    if (nrow(aggregation_trains(input, 'total_nb_trains_delayed_departure'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
      
    } else {
      value_ <- aggregation_trains(input, 'total_nb_trains_delayed_departure')
      color_ <- "orange"
      icon_ <- icon("clock")
      
    }
    valueBox(
      subtitle = 'Delayed at departure', 
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$trains_delayed_arr <- renderValueBox({
    if (nrow(aggregation_trains(input, 'total_nb_trains_delayed_arrival'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
      
    } else {
      value_ <- aggregation_trains(input, 'total_nb_trains_delayed_arrival')
      color_ <- "orange"
      icon_ <- icon("clock")
      
    }
    valueBox(
      subtitle = 'Delayed at arrival', 
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$avg_trains_delayed_dep <- renderValueBox({
    if (nrow(aggregation_trains(input, 'avg_nb_delayed_trains_departure'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- round(aggregation_trains(input, 'avg_nb_delayed_trains_departure'),2)
      color_ <- "orange"
      icon_ <- icon("user-clock")
      
    }
    valueBox(
      subtitle = 'Average nb delayed at Departure', 
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$avg_trains_delayed_arr <- renderValueBox({
    if (nrow(aggregation_trains(input, 'avg_nb_delayed_trains_arrival'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- round(aggregation_trains(input, 'avg_nb_delayed_trains_arrival'),2)
      color_ <- "orange"
      icon_ <- icon("user-clock")
      
    }
    valueBox(
      subtitle = 'Average nb delayed at Arrival', 
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$avg_dep_delay_time_all_trains <- renderValueBox({
    if (nrow(aggregation_trains(input, 'total_avg_departure_delay_time_all_trains'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- round(aggregation_trains(input, 'total_avg_departure_delay_time_all_trains'), 2)
      color_ <- "yellow"
      icon_ <- icon("clock")
      
    }
    valueBox(
      subtitle = 'Average delayed time at Departure (all trains)', 
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$avg_arr_delay_time_all_trains <- renderValueBox({
    if (nrow(aggregation_trains(input, 'total_avg_arrival_delay_time_all_trains'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
      
    } else {
      value_ <- round(aggregation_trains(input, 'total_avg_arrival_delay_time_all_trains'), 2)
      color_ <- "yellow"
      icon_ <- icon("clock")
      
    }
    valueBox(
      subtitle = 'Average delayed time at Arrival (all trains)', 
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$avg_dep_delay_time_delayed_trains <- renderValueBox({
    if (nrow(aggregation_trains(input, 'total_avg_departure_delay_time_delayed_trains'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- round(aggregation_trains(input, 'total_avg_departure_delay_time_delayed_trains'), 2)
      color_ <- "yellow"
      icon_ <- icon("clock")
    }
    valueBox(
      subtitle = 'Average delayed time of delayed trains at Departure', 
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$avg_arr_delay_time_delayed_trains <- renderValueBox({
    if (nrow(aggregation_trains(input, 'total_avg_arrival_delay_time_delayed_trains'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- round(aggregation_trains(input, 'total_avg_arrival_delay_time_delayed_trains'), 2)
      color_ <- "yellow"
      icon_ <- icon("clock")
    }
    valueBox(
      subtitle = 'Average delayed time of delayed trains at Arrival', 
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$trains_cancelled <- renderValueBox({
    if (nrow(aggregation_trains(input, 'total_nb_cancelled_trains'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- round(aggregation_trains(input, 'total_nb_cancelled_trains'), 2)
      color_ <- "red"
      icon_ <- icon("times-circle")
    }
    valueBox(
      subtitle = 'Average nb of cancelled', 
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$trains_cancelled_percent <- renderValueBox({
    if (nrow(aggregation_trains(input, 'percentage_cancelled_trains'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- paste(round(aggregation_trains(input, 'percentage_cancelled_trains'), 2),"%")
      color_ <- "red"
      icon_ <- icon("times-circle")
    }
    valueBox(
      subtitle = 'Percentage of cancelled', 
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$proportion_of_causes <- renderPlot({
    
    if(input$Annee == 'ALL YEARS' & input$Gare.de.depart == 'ALL TRAIN STATIONS'){
      valeur_proportion <- agg_train %>%
        select(causes_externes, causes_infra_ferroviaire, causes_gestion_trafic, causes_materiel_roulant, causes_gestion_gare, causes_voyageur)
    } 
    else if (input$Annee == 'ALL YEARS') {
      valeur_proportion <- agg_train_by_Gare_Départ %>%
        filter(Gare.de.départ == input$Gare.de.depart) %>%
        select(causes_externes,causes_infra_ferroviaire,causes_gestion_trafic,causes_materiel_roulant,causes_gestion_gare,causes_voyageur)
    } 
    else if (input$Gare.de.depart == 'ALL TRAIN STATIONS') {
      valeur_proportion <- agg_train_by_Année %>%
        filter(Année == input$Annee) %>%
        select(causes_externes,causes_infra_ferroviaire,causes_gestion_trafic,causes_materiel_roulant,causes_gestion_gare,causes_voyageur)
    } 
    else {
      valeur_proportion <- agg_train_by_Année_Gare_Départ %>%
        filter(Année == input$Annee & Gare.de.départ == input$Gare.de.depart) %>%
        ungroup() %>%
        select(causes_externes,causes_infra_ferroviaire,causes_gestion_trafic,causes_materiel_roulant,causes_gestion_gare,causes_voyageur)
    }
    
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        colour = "transparent"),
        axis.text.y = element_blank(),
        plot.title=element_text(size=14, face="bold"),
        plot.background = element_rect(fill = "transparent",colour = "transparent")
      )
    
    if (nrow(valeur_proportion)==0) {
      valeur_proportion <- tibble("val" = c(100), "Name" = c("UNKNOWN"))
      ggplot(data = valeur_proportion, aes(x=0, y=val), show.legend = FALSE) + geom_bar(width = 1, stat = "identity", aes(fill=Name), fill="#FF9169") + 
        coord_polar(theta = "y") +
        scale_fill_brewer("Causes") + blank_theme +
        theme(axis.text.x=element_blank(), legend.position = "none")+
        geom_label(aes(label = Name ,group = val),position = position_stack(vjust = 0))
      
    } else {
      valeur_proportion <- t(valeur_proportion)
      valeur_proportion <- as.data.frame(valeur_proportion)
      valeur_proportion$causes <- rownames(valeur_proportion)
      colnames(valeur_proportion) <-c("value","causes")
      
      ggplot(data = valeur_proportion, aes(x=0, y=value))+
        geom_bar(width = 1, stat = "identity", aes(fill=causes)) + 
        coord_polar(theta = "y") +
        scale_fill_brewer("Causes") + blank_theme +
        theme(axis.text.x=element_blank())+
        geom_label(aes(label = paste(round(value,2),"%"),group = causes),position = position_stack(vjust = 0.5) , show.legend = FALSE)
      
    }
  }, bg="transparent")
  
  output$evolution_of_cancelled_trains <- renderPlot({
    
    
    if(input$Gare.de.depart == 'ALL TRAIN STATIONS')
    {
      valeur <- agg_train_by_Année %>%
        select(Année,total_nb_cancelled_trains,percentage_cancelled_trains)
    }
    else 
    {
      valeur <- agg_train_by_Année_Gare_Départ %>%
        filter(Gare.de.départ == input$Gare.de.depart) %>%
        select(Année,total_nb_cancelled_trains,percentage_cancelled_trains)
    }
    
    
    ggplot(valeur, aes(x=Année, y=total_nb_cancelled_trains, colour = "Number of cancelled trains")) +
      geom_line()+
      geom_point()+
      geom_point(aes(x=Année, y=percentage_cancelled_trains))+
      geom_label(aes(x=Année,y=percentage_cancelled_trains,label = paste(round(percentage_cancelled_trains,2),"%"), colour = "Percentage of cancelled trains"), show.legend = FALSE)
  }, bg="transparent")
  
  output$evolution_of_delayed_trains <- renderPlot({
    
    if(input$Gare.de.depart == 'ALL TRAIN STATIONS')
    {
      valeur <- agg_train_by_Année %>%
        select(Année,total_nb_trains_delayed_departure,total_nb_trains_delayed_arrival)
    }
    else 
    {
      valeur <- agg_train_by_Année_Gare_Départ %>%
        filter(Gare.de.départ == input$Gare.de.depart) %>%
        select(Année,total_nb_trains_delayed_departure,total_nb_trains_delayed_arrival)
    }
    
    
    ggplot(valeur) +
      geom_line(aes(x=Année, y=total_nb_trains_delayed_departure,colour = "delayed departure"))+
      geom_point(aes(x=Année, y=total_nb_trains_delayed_departure,colour = "delayed departure"))+
      geom_line(aes(x=Année, y=total_nb_trains_delayed_arrival,colour = "delayed arrival"))+
      geom_point(aes(x=Année, y=total_nb_trains_delayed_arrival, colour = "delayed arrival"))
  }, bg="transparent")
  
  output$overview_of_trains <- renderPlot({
    
    if(input$Gare.de.depart == 'ALL TRAIN STATIONS')
    {
      valeur <- agg_train_by_Année %>%
        select(Année,total_nb_cancelled_trains,total_nb_trains_carried_out,total_nb_trains_delayed_arrival)
    }
    else
    {
      valeur <- agg_train_by_Année_Gare_Départ %>%
        filter(Gare.de.départ == input$Gare.de.depart) %>%
        select(Année,total_nb_cancelled_trains,total_nb_trains_carried_out,total_nb_trains_delayed_arrival)
    }
    
    
    valeurcancelled <- data.frame(Année=valeur["Année"],
                               etat=rep("Annulé"),
                               nombre=rep(valeur["total_nb_cancelled_trains"]))
    
    valeurcarriedout <- data.frame(Année=valeur["Année"],
                               etat=rep("Réalisé"),
                               nombre=rep(valeur["total_nb_trains_carried_out"]))
    
    valeurdelayed <- data.frame(Année=valeur["Année"],
                                   etat=rep("Retardé"),
                                   nombre=rep(valeur["total_nb_trains_delayed_arrival"]))
    
    valeurcarriedout <- data.frame(Année=valeur["Année"],
                                   etat=rep("Réalisé"),
                                   nombre=rep(valeur["total_nb_trains_carried_out"]-valeur["total_nb_trains_delayed_arrival"]))

    
    names(valeurcancelled)[names(valeurcancelled) == "total_nb_cancelled_trains"] <- "nombre"
    names(valeurcarriedout)[names(valeurcarriedout) == "total_nb_trains_carried_out"] <- "nombre"
    names(valeurdelayed)[names(valeurdelayed) == "total_nb_trains_delayed_arrival"] <- "nombre"

    
    valeur <- rbind(valeurcancelled,valeurcarriedout,valeurdelayed)

    
    ggplot(valeur, aes(x=Année, y=nombre, fill = etat)) +
      geom_bar(stat="identity")
    
   
  }, bg="transparent")
  
  
  
  
  ########### FLIGHTS  ########### 
  
  
  output$total_nb_flights <- renderValueBox({
    if (nrow(aggregation_flights(input, 'nb_flight'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
      
    } else {
      value_ <- aggregation_flights(input, 'nb_flight')
      color_ <- "green"
      icon_ <- icon("plane")
    }
    valueBox(
      subtitle = 'Number of flights',
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$total_nb_delayed_flights <- renderValueBox({
    if (nrow(aggregation_flights(input, 'nb_delayed_flights'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- aggregation_flights(input, 'nb_delayed_flights')
      color_ <- "orange"
      icon_ <- icon("clock")
    }
    valueBox(
      subtitle = 'Number of delayed flights',
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$avg_flight_duration <- renderValueBox({
    if (nrow(aggregation_flights(input, 'avg_duration'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- paste(round(aggregation_flights(input, 'avg_duration')),"minutes")
      color_ <- "green"
      icon_ <- icon("clock")
    }
    valueBox(
      subtitle = 'Average Flight Duration',
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$avg_flight_distance <- renderValueBox({
    if (nrow(aggregation_flights(input, 'avg_distance'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- paste(round(aggregation_flights(input, 'avg_distance'),2),"km")
      color_ <- "green"
      icon_ <- icon("globe-americas")
    }
    valueBox(
      subtitle = 'Average Flight Distance',
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$total_distance <- renderValueBox({
    if (nrow(aggregation_flights(input, 'total_distance'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      icon_ <- icon("question")
    } else {
      value_ <- round(aggregation_flights(input, 'total_distance'),2)
      color_ <- "green"
      icon_ <- icon("globe-americas")
    }
    
    valueBox(
      subtitle = 'Total Distance (km)',
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$avg_depature_delay <- renderValueBox({
    if (nrow(aggregation_flights(input, 'avg_departure_delay'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      subtitle_ <- 'Average minutes departure'
      icon_ <- icon("question")
    } else {
      value_ <- round(aggregation_flights(input, 'avg_departure_delay'))
      if (value_ < 0) {
        subtitle_ <- 'Average minutes departure ahead'
        value_ <- paste((-1)*value_,"minutes")
        color_ <- "green"
        icon_ <- icon("plane-departure")
      } else if (value_== 0){
        subtitle_ <- 'Average minutes departure'
        value_ <- 'On time'
        color_ <- "green"
        icon_ <- icon("plane-departure")
      } else {
        subtitle_ <- 'Average minutes departure delay'
        color_ <- "orange"
        value_ <- paste(value_,"minutes")
        icon_ <- icon("plane-departure")
      }
    }
    valueBox(
      subtitle = subtitle_,
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$avg_arrival_delay <- renderValueBox({
    if (nrow(aggregation_flights(input, 'avg_arrival_delay'))==0) {
      value_ <- 'Unknown'
      color_ <- 'teal'
      subtitle_ <- 'Average minutes arrival'
      icon_ <- icon("question")
    } else {
      value_ <- round(aggregation_flights(input, 'avg_arrival_delay'))
      if (value_ < 0) {
        subtitle_ <- 'Average minutes arrival ahead'
        value_ <- paste((-1)*value_,"minutes")
        color_ <- "green"
        icon_ <- icon("plane-arrival")
      } else if (value_== 0){
        subtitle_ <- 'Average minutes arrival'
        value_ <- 'On time'
        color_ <- "green"
        icon_ <- icon("plane-arrival")
      } else {
        subtitle_ <- 'Average minutes arrival delay'
        color_ <- "orange"
        value_ <- paste(value_,"minutes")
        icon_ <- icon("plane-arrival")
      }
    }
    
    valueBox(
      subtitle = subtitle_,
      value = value_,
      color = color_,
      icon = icon_
    )
  })
  
  output$map_flights <- renderLeaflet({
    
    #TOUT
    if(input$Airport=='ALL AIRPORTS' & input$Airline=='ALL AIRLINES'){
      content <- paste("<b>",agg_flights_by_airport$CITY,"(" ,agg_flights_by_airport$AIRPORT, ")</b> <br/>",agg_flights_by_airport$nb_flight , " flights <br/>", round(agg_flights_by_airport$avg_departure_delay,2), "avg departure delay")
      map <- leaflet(agg_flights_by_airport) %>% 
        setView(lng = -99, lat = 45, zoom = 3)  %>% 
        addTiles() %>% 
        addCircles(data = agg_flights_by_airport, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = ~ nb_flight, color = 'red', popup = content, fillOpacity = 0.5) 
      
      for(i in 1:nrow(agg_flights_by_airport_map)) 
        if(agg_flights_by_airport_map[i,]$nb_flight>3000){
          map <-map %>% 
            addPolylines(
              lat=c(agg_flights_by_airport_map[i,]$LATITUDE.x,agg_flights_by_airport_map[i,]$LATITUDE.y),
              lng=c(agg_flights_by_airport_map[i,]$LONGITUDE.x,agg_flights_by_airport_map[i,]$LONGITUDE.y),
              weight = 1
            )
        }
    } 
    #FILTRE PAR AIRLINE OK 
    else if(input$Airport=='ALL AIRPORTS') {
      temp <- agg_flights_by_airline_airport %>%
        filter(AIRLINE.y == input$Airline)
      
      content <- paste("<b>",temp$CITY,"(" ,temp$AIRPORT, ")</b> <br/>",temp$nb_flight , " flights <br/>", round(temp$avg_departure_delay,2), "avg departure delay")
      
      map <- leaflet(temp) %>% 
        setView(lng = -99, lat = 45, zoom = 3)  %>% 
        addTiles() %>% 
        addCircles(data = temp, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = ~ nb_flight*2, color = 'red', popup = content, fillOpacity = 0.5) 
      
      temp <- agg_flights_by_airline_map %>%
        filter(AIRLINE.y == input$Airline)
      
      for(i in 1:nrow(temp))
        map <-map %>%
        addPolylines(
          lat=c(temp[i,]$LATITUDE.x,temp[i,]$LATITUDE.y),
          lng=c(temp[i,]$LONGITUDE.x,temp[i,]$LONGITUDE.y),
          weight = 1
        )
    } 
    #FILTRE PAR AIRPORT
    else if(input$Airline=='ALL AIRLINES') {
      temp <- agg_flights_by_airport %>%
        filter(AIRPORT == input$Airport)
      
      temp_destination <- agg_flights_by_airport_map %>%
        filter(AIRPORT.x == input$Airport)
      
      content <- paste("<b>",temp$CITY,"(" ,temp$AIRPORT, ")</b> <br/>",temp$nb_flight , " flights <br/>", round(temp$avg_departure_delay,2), "avg departure delay")
      content2 <- paste("<b>", temp_destination$CITY.y , "</b> <br/>" , temp_destination$nb_flight ," flights<br/>" , round(temp_destination$avg_duration) , "minutes flight (avg) <br/>" , temp_destination$avg_distance, "km" )
      map <- leaflet(temp) %>%
        setView(lng = -99, lat = 45, zoom = 3)  %>%
        addTiles() %>%
        addCircles(data = temp, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = ~ nb_flight, color = 'red', popup = content, fillOpacity = 0.5) %>%
        addCircles(data = temp_destination, lat = ~ LATITUDE.y, lng = ~ LONGITUDE.y, weight = 1, radius = ~ nb_flight*5, color = 'blue',popup = content2, fillOpacity = 0.5)
      
      
      temp_lignes <- agg_flights_by_airport_map %>%
        filter(AIRPORT.x == input$Airport)
      
      for(i in 1:nrow(temp_lignes))
        map <-map %>%
        addPolylines(
          lat=c(temp_lignes[i,]$LATITUDE.x,temp_lignes[i,]$LATITUDE.y),
          lng=c(temp_lignes[i,]$LONGITUDE.x,temp_lignes[i,]$LONGITUDE.y),
          weight = 1
        )
    } 
    #FILTRE PAR TOUT
    else {
      temp <-agg_flights_by_airline_airport %>%
        filter(AIRPORT == input$Airport & AIRLINE.y == input$Airline)
      
      temp_destination <- agg_flights_by_airline_map %>%
        filter(AIRPORT.x == input$Airport & AIRLINE.y == input$Airline)
      
      content <- paste("<b>",temp$CITY,"(" ,temp$AIRPORT, ")</b> <br/>",temp$nb_flight , " flights <br/>", round(temp$avg_departure_delay,2), "avg departure delay")
      content2 <- paste("<b>", temp_destination$CITY.y , "</b> <br/>" , temp_destination$nb_flight ," flights<br/>" , round(temp_destination$avg_duration) , "minutes flight (avg) <br/>" , temp_destination$avg_distance, "km" )
      
      map <- leaflet(agg_flights_by_airport) %>%
        setView(lng = -99, lat = 45, zoom = 3)  %>%
        addTiles() %>%
        addCircles(data = temp, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = ~ nb_flight*5, color = 'red', popup = content, fillOpacity = 0.5) %>%
        addCircles(data = temp_destination, lat = ~ LATITUDE.y, lng = ~ LONGITUDE.y, weight = 1, radius = ~ nb_flight*5, color = 'blue',popup = content2, fillOpacity = 0.5)
      
      
      temp_destination <- agg_flights_by_airline_map %>%
        filter(AIRPORT.x == input$Airport & AIRLINE.y == input$Airline)
      
      
      for(i in 1:nrow(temp_destination))
        map <-map %>%
        addPolylines(
          lat=c(temp_destination[i,]$LATITUDE.x,temp_destination[i,]$LATITUDE.y),
          lng=c(temp_destination[i,]$LONGITUDE.x,temp_destination[i,]$LONGITUDE.y),
          weight = 1
        )
    }
    map
  })
}
