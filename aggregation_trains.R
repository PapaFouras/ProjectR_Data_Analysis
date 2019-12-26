aggregation_trains <- function(input, agg_val){
  #toutes les données
  if(input$Annee == 'ALL YEARS' & input$Gare.de.départ == 'ALL TRAIN STATIONS') {
    valeur <- agg_train %>%
      select(agg_val)
  } 
  #Données groupées par Gare pour toutes les années 
  else if (input$Année == 'ALL YEARS') {
    valeur <- agg_train_by_Gare_Départ %>%
      filter(Gare.de.départ == input$Gare.de.départ) %>%
      select(agg_val)
  }
  #Données groupées par Année pour toutes les Gares
  else if (input$Gare.de.départ == 'ALL TRAIN STATIONS') {
    valeur <- agg_train_by_Année %>%
      filter(Année == input$Année) %>%
      select(agg_val)
  } 
  #Données groupées par Année et par Gare 
  else {
    valeur <- agg_train_by_Année_Gare_Départ %>%
      filter(Année == input$Année & Gare.de.départ == input$Gare.de.départ) %>%
      ungroup() %>%
      select(agg_val)
  }
  return(valeur)
}