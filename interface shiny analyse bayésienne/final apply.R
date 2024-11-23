#install.packages("shiny")

library(shiny)
library(shinydashboard)
library(extraDistr)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Gestion des Stocks Bayésienne pour BVA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calcul Bayésien et Coût", tabName = "bayesianCostCalc", icon = icon("calculator")),
      menuItem("Autres Fonctionnalités", tabName = "other", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Ajout d'un slogan BVA en haut de la page
    tags$div(class = "header-logo", tags$img(src = "BVAA.png", height ="50px"), style = "text-align: center; padding: 20px;"),
    tabItems(
      tabItem(tabName = "bayesianCostCalc",
              fluidRow(
                box(title = "Entrées", status = "primary", solidHeader = TRUE,
                    numericInput("gearboxesArriving", "Nombre de boîtes de vitesses à réparer", value = 11),
                    numericInput("deliveryDelay", "Délai de livraison en jours (en cas de rupture)", value = 5),
                    selectInput("ageClass", "Classe d'âge des composants", choices = 2:2),
                    numericInput("stockPieces", "Nombre de pièces en stock", value = 0),
                    actionButton("applyButton", "Calculer", class = "btn-primary")  # Bouton Apply
                    
                ),
                box(title = "Résultats", status = "info", solidHeader = TRUE,
                    textOutput("maxPiecesNeeded"),
                    textOutput("coutTotal"),
                    textOutput("recommandation")
                )
              )
      ),
      tabItem(tabName = "other",
              fluidRow(
                box(title = "Entrées", status = "primary", solidHeader = TRUE,
                    numericInput("cout_piece", "Cout d'une piece", value = 190.57),
                    numericInput("cout_transport", "cout du transport", value = 54.20),
                    numericInput("cout_arret_chaine", "cout arret dune chaine", value = 296.78),
                    numericInput("amende_retard", "amende en cas du retard", value = 24.96),
                    numericInput("perte_client", "Perte de client en cas de retard", value = 81.88),
                    numericInput("cout_depot", "cout de depot ", value = 300),
                    numericInput("cout_par_jour_retard", "cout par jour de retard", value = 150),
                    actionButton("updateButton", "Mettre à jour les coûts", class = "btn-primary")
                    
                ),
                h2("Contenu de l'onglet Autres Fonctionnalités")
              )
      )
    )
  )
)




server <- function(input, output) {
  observeEvent(input$applyButton, {  # Réaction au clic sur le bouton "Calculer"
    # Paramètres pour la distribution Beta-Binomiale
    a <- 2
    b <- 3
    n_obs <- 25
    y_obs <- 2
    n_new <- input$gearboxesArriving
    
    # Calcul des probabilités conditionnelles
    a_post <- a + y_obs
    b_post <- n_obs - y_obs + b
    prob_conditional <- sapply(0:n_new, function(i) dbbinom(i, n_new, a_post, b_post))
    max_indice <- which.max(prob_conditional) - 1
    
    # Coûts unitaires (exemple)
    cout_piece <- 190.57
    cout_transport <- 54.20
    cout_arret_chaine <- 296.78
    amende_retard <- 24.96
    perte_client <- 81.88
    cout_depot <- 300
    cout_par_jour_retard <- 150
    
    # Déterminer si un achat est nécessaire
    pieces_a_acheter <- max(0, max_indice - input$stockPieces)
    
    # Condition si le stock actuel est suffisant
    if (input$stockPieces > max_indice) {
      cout_surstockage <- 0
      cout_rupture_stock <- 0
      recommandation <- "Aucun achat de pièces nécessaire, le stock actuel est suffisant."
    } else if(input$stockPieces == max_indice) {
      cout_surstockage <- 1 * (cout_piece + cout_transport) + cout_depot
      cout_rupture_stock <- cout_arret_chaine + amende_retard + perte_client + input$deliveryDelay * cout_par_jour_retard + cout_depot
      recommandation <- paste("Acheter ", 1, " pièces pour éviter la rupture de stock et minimiser les coûts.")
    } else {
      # Calcul des coûts si achat nécessaire
      cout_surstockage <- (pieces_a_acheter + 1) * (cout_piece + cout_transport) + cout_depot
      cout_rupture_stock <- (pieces_a_acheter - 1) * cout_piece + cout_arret_chaine + amende_retard + perte_client + input$deliveryDelay * cout_par_jour_retard + cout_depot
      recommandation <- paste("Acheter ", pieces_a_acheter + 1, " pièces pour éviter la rupture de stock et minimiser les coûts.")
    }
    
    # Mise à jour des textes de sortie
    output$maxPiecesNeeded <- renderText({
      paste("Nombre maximal de pièces nécessaires : ", max_indice, 
            "\nProbabilité associée : ", round(max(prob_conditional), 4))
    })
    
    output$coutTotal <- renderText({
      if (input$stockPieces > max_indice) {
        "Aucun coût supplémentaire n'est nécessaire, le stock actuel est suffisant."
      } else if(input$stockPieces == max_indice) {
        paste("Coût total en cas de surstockage (achat de ",  1, " pièces) : ", round(cout_surstockage, 2), "€\n",
              "Coût total en cas de rupture de stock (achat de ", pieces_a_acheter, " pièces) : ", round(cout_rupture_stock, 2), "€")
      } else {
        paste("Coût total en cas de surstockage (achat de ", pieces_a_acheter + 1, " pièces) : ", round(cout_surstockage, 2), "€\n",
              "Coût total en cas de rupture de stock (achat de ", pieces_a_acheter, " pièces) : ", round(cout_rupture_stock, 2), "€")
      }
    })
    
    output$recommandation <- renderText({ recommandation })
  })
}
shinyApp(ui, server)

