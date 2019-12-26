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
