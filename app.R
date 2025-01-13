
library(shiny)
library(shinyStorePlus)
library(bslib)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(waiter)
library(shinycssloaders)
library(DT)
#Scaping
library(rvest)
library(dplyr)
#--------------------------------------------------------------------------------------------
# Ajouter cette fonction après les libraries
search_chess_books <- function(query, api_key) {
  base_url <- "https://www.googleapis.com/books/v1/volumes"
  
  # Construire la requête
  full_query <- paste(query, "chess", sep = " ")
  
  # Faire la requête à l'API
  response <- GET(
    url = base_url,
    query = list(
      q = full_query,
      key = api_key,
      maxResults = 10
    )
  )
  
  # Parser la réponse
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content))
    
    # Vérifier si des livres ont été trouvés
    if ("items" %in% names(content)) {
      books <- content$items$volumeInfo
      
      # Créer un dataframe avec les informations importantes
      results <- data.frame(
        titre = sapply(books$title, function(x) ifelse(is.null(x), "Titre inconnu", x)),
        auteurs = sapply(books$authors, function(x) ifelse(is.null(x), "Auteur inconnu", paste(unlist(x), collapse = ", "))),
        date = sapply(books$publishedDate, function(x) ifelse(is.null(x), "Date inconnue", x)),
        description = sapply(books$description, function(x) 
          ifelse(is.null(x), "Pas de description", substr(x, 1, 200))),
        stringsAsFactors = FALSE
      )
      
      return(results)
    }
  }
  return(data.frame())
}
# Fonction pour calculer le elo fide estimer
get_all_correlations <- function() {
  # Coefficients de corrélation stockés directement
  coefficients <- list(
    chess_com = list(
      coefficients = list(
        bullet = c(intercept = -112.4543786, slope = 1.036516455),
        blitz = c(intercept = 0.0, slope = 1.0),
        rapid = c(intercept = 461.6457348, slope = 0.7513439745),
        average = c(intercept = 116.3971187, slope = 0.9292868098)  # Moyenne des coefficients
      ),
      r_squared = list(
        bullet = 0.85,
        blitz = 0.88,
        rapid = 0.82,
        average = 0.85  # Moyenne des R²
      )
    ),
    lichess = list(
      coefficients = list(
        bullet = c(intercept = 373.1784651, slope = 0.8660009037),
        blitz = c(intercept = 616.0098942, slope = 0.7658478335),
        rapid = c(intercept = 948.7785739, slope = 0.6298191743),
        average = c(intercept = 645.9889777, slope = 0.7538893038)  # Moyenne des coefficients
      ),
      r_squared = list(
        bullet = 0.82,
        blitz = 0.85,
        rapid = 0.80,
        average = 0.82  # Moyenne des R²
      )
    )
  )
  
  return(coefficients)
}

calculate_fide <- function(chess_elo, lichess_elo, platform, time_control) {
  # Debug logs
  cat("Entrée de calculate_fide:\n")
  cat("chess_elo:", chess_elo, "\n")
  cat("lichess_elo:", lichess_elo, "\n")
  cat("platform:", platform, "\n")
  cat("time_control:", time_control, "\n")
  
  # Vérification préliminaire des entrées
  if ((platform == "chess" && (is.null(chess_elo) || is.na(chess_elo))) || 
      (platform == "lichess" && (is.null(lichess_elo) || is.na(lichess_elo))) || 
      (platform == "both" && (is.null(chess_elo) || is.na(chess_elo)) && 
       (is.null(lichess_elo) || is.na(lichess_elo)))) {
    cat("Données insuffisantes pour l'estimation\n")
    return(NULL)
  }
  
  # Récupérer les données de corrélation
  correlations <- get_all_correlations()
  
  # Fonction pour appliquer la régression linéaire
  apply_regression <- function(elo, platform_data, game_type) {
    if (is.null(elo) || is.null(platform_data) || is.na(elo)) return(NULL)
    
    # Vérifier si le type de jeu existe dans les coefficients
    if (!game_type %in% names(platform_data$coefficients)) return(NULL)
    
    # Obtenir les coefficients pour ce type de jeu
    coef <- platform_data$coefficients[[game_type]]
    if (is.null(coef)) return(NULL)
    
    r2 <- platform_data$r_squared[[game_type]]
    if (is.null(r2)) return(NULL)
    
    # Calculer l'estimation FIDE
    elo <- as.numeric(elo)
    coef <- as.numeric(coef)
    fide_estimate <- coef[1] + coef[2] * elo
    
    return(list(estimate = fide_estimate, r2 = r2))
  }
  
  # Convertir time_control en format correspondant aux données
  time_control_map <- list(
    "bullet" = "bullet",
    "blitz" = "blitz",
    "rapid" = "rapid",
    "average" = c("bullet", "blitz", "rapid")
  )
  
  game_types <- time_control_map[[time_control]]
  if (is.null(game_types)) return(NULL)
  
  # Initialiser les estimations
  chess_estimate <- NULL
  lichess_estimate <- NULL
  
  # Calculer pour Chess.com
  if (!is.null(chess_elo) && !is.na(chess_elo) && (platform %in% c("chess", "both"))) {
    if (time_control == "average") {
      chess_estimates <- lapply(game_types, function(type) {
        apply_regression(chess_elo, correlations$chess_com, type)
      })
      chess_estimates <- Filter(Negate(is.null), chess_estimates)
      if (length(chess_estimates) > 0) {
        chess_estimate <- list(
          estimate = mean(sapply(chess_estimates, function(x) x$estimate)),
          r2 = mean(sapply(chess_estimates, function(x) x$r2))
        )
      }
    } else {
      chess_estimate <- apply_regression(chess_elo, correlations$chess_com, time_control)
    }
  }
  
  # Calculer pour Lichess
  if (!is.null(lichess_elo) && !is.na(lichess_elo) && (platform %in% c("lichess", "both"))) {
    if (time_control == "average") {
      lichess_estimates <- lapply(game_types, function(type) {
        apply_regression(lichess_elo, correlations$lichess, type)
      })
      lichess_estimates <- Filter(Negate(is.null), lichess_estimates)
      if (length(lichess_estimates) > 0) {
        lichess_estimate <- list(
          estimate = mean(sapply(lichess_estimates, function(x) x$estimate)),
          r2 = mean(sapply(lichess_estimates, function(x) x$r2))
        )
      }
    } else {
      lichess_estimate <- apply_regression(lichess_elo, correlations$lichess, time_control)
    }
  }
  
  # Retourner l'estimation selon la plateforme choisie
  if (platform == "chess" && !is.null(chess_estimate)) {
    return(round(chess_estimate$estimate))
  } else if (platform == "lichess" && !is.null(lichess_estimate)) {
    return(round(lichess_estimate$estimate))
  } else if (platform == "both" && (!is.null(chess_estimate) || !is.null(lichess_estimate))) {
    if (is.null(chess_estimate)) return(round(lichess_estimate$estimate))
    if (is.null(lichess_estimate)) return(round(chess_estimate$estimate))
    
    # Moyenne pondérée basée sur les R²
    total_weight <- chess_estimate$r2 + lichess_estimate$r2
    weighted_estimate <- (chess_estimate$estimate * chess_estimate$r2 + 
                            lichess_estimate$estimate * lichess_estimate$r2) / total_weight
    return(round(weighted_estimate))
  }
  
  return(NULL)
}

get_player_ratings <- function(chess_data, lichess_data, time_control = NULL) {
  # Debug logs
  cat("Entrée de get_player_ratings:\n")
  cat("chess_data null?", is.null(chess_data), "\n")
  cat("lichess_data null?", is.null(lichess_data), "\n")
  
  ratings <- list(
    chess = list(),
    lichess = list()
  )
  
  # Extraction des Elos Chess.com
  if (!is.null(chess_data) && !is.null(chess_data$complete_stats)) {
    cat("Extraction des Elos Chess.com...\n")
    
    # Bullet
    if (!is.null(chess_data$complete_stats$chess_bullet) && 
        !is.null(chess_data$complete_stats$chess_bullet$last) &&
        !is.null(chess_data$complete_stats$chess_bullet$last$rating)) {
      ratings$chess$bullet <- chess_data$complete_stats$chess_bullet$last$rating
      cat("Bullet Chess.com:", ratings$chess$bullet, "\n")
    }
    
    # Blitz
    if (!is.null(chess_data$complete_stats$chess_blitz) && 
        !is.null(chess_data$complete_stats$chess_blitz$last) &&
        !is.null(chess_data$complete_stats$chess_blitz$last$rating)) {
      ratings$chess$blitz <- chess_data$complete_stats$chess_blitz$last$rating
      cat("Blitz Chess.com:", ratings$chess$blitz, "\n")
    }
    
    # Rapid
    if (!is.null(chess_data$complete_stats$chess_rapid) && 
        !is.null(chess_data$complete_stats$chess_rapid$last) &&
        !is.null(chess_data$complete_stats$chess_rapid$last$rating)) {
      ratings$chess$rapid <- chess_data$complete_stats$chess_rapid$last$rating
      cat("Rapid Chess.com:", ratings$chess$rapid, "\n")
    }
  }
  
  # Extraction des Elos Lichess
  if (!is.null(lichess_data) && !is.null(lichess_data$perfs)) {
    cat("Extraction des Elos Lichess...\n")
    
    # Bullet
    if (!is.null(lichess_data$perfs$bullet) && !is.null(lichess_data$perfs$bullet$rating)) {
      ratings$lichess$bullet <- lichess_data$perfs$bullet$rating
      cat("Bullet Lichess:", ratings$lichess$bullet, "\n")
    }
    
    # Blitz
    if (!is.null(lichess_data$perfs$blitz) && !is.null(lichess_data$perfs$blitz$rating)) {
      ratings$lichess$blitz <- lichess_data$perfs$blitz$rating
      cat("Blitz Lichess:", ratings$lichess$blitz, "\n")
    }
    
    # Rapid
    if (!is.null(lichess_data$perfs$rapid) && !is.null(lichess_data$perfs$rapid$rating)) {
      ratings$lichess$rapid <- lichess_data$perfs$rapid$rating
      cat("Rapid Lichess:", ratings$lichess$rapid, "\n")
    }
  }
  
  # Calcul des moyennes si demandé
  if (!is.null(time_control) && time_control == "average") {
    # Moyenne Chess.com
    chess_ratings <- c(ratings$chess$bullet, ratings$chess$blitz, ratings$chess$rapid)
    chess_ratings <- chess_ratings[!is.null(chess_ratings) & !is.na(chess_ratings)]
    ratings$chess$average <- if (length(chess_ratings) > 0) mean(chess_ratings) else NULL
    
    if (!is.null(ratings$chess$average)) {
      cat("Moyenne Chess.com:", ratings$chess$average, "\n")
    }
    
    # Moyenne Lichess
    lichess_ratings <- c(ratings$lichess$bullet, ratings$lichess$blitz, ratings$lichess$rapid)
    lichess_ratings <- lichess_ratings[!is.null(lichess_ratings) & !is.na(lichess_ratings)]
    ratings$lichess$average <- if (length(lichess_ratings) > 0) mean(lichess_ratings) else NULL
    
    if (!is.null(ratings$lichess$average)) {
      cat("Moyenne Lichess:", ratings$lichess$average, "\n")
    }
  }
  
  return(ratings)
}

get_chess_com_rating_history <- function(username) {
  # Format de l'URL : année/mois
  current_date <- Sys.Date()
  # Récupérer l'archive du mois en cours
  year <- format(current_date, "%Y")
  month <- format(current_date, "%m")
  
  url <- sprintf("https://api.chess.com/pub/player/%s/games/%s/%s", username, year, month)
  response <- GET(url)
  
  ratings_df <- data.frame(
    Date = character(),
    Rating = numeric(),
    Type = character(),
    stringsAsFactors = FALSE
  )
  
  if (status_code(response) == 200) {
    games <- fromJSON(rawToChar(response$content))$games
    
    if (length(games) > 0) {
      for (game in games) {
        game_type <- case_when(
          game$time_class == "bullet" ~ "Bullet",
          game$time_class == "blitz" ~ "Blitz",
          game$time_class == "rapid" ~ "Rapid",
          TRUE ~ "Other"
        )
        
        if (game_type != "Other") {
          # Déterminer si le joueur était blanc ou noir
          is_white <- game$white$username == tolower(username)
          player_data <- if(is_white) game$white else game$black
          
          if (!is.null(player_data$rating)) {
            ratings_df <- rbind(ratings_df, data.frame(
              Date = as.Date(as.POSIXct(game$end_time, origin="1970-01-01")),
              Rating = player_data$rating,
              Type = game_type,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  return(ratings_df)
}


# Fonctions utilitaires pour les API
get_chess_com_player <- function(username) {
  base_url <- paste0("https://api.chess.com/pub/player/", username)
  response <- GET(base_url)
  if (status_code(response) == 200) {
    return(fromJSON(rawToChar(response$content)))
  }
  return(NULL)
}

get_lichess_player <- function(username) {
  base_url <- paste0("https://lichess.org/api/user/", username)
  response <- GET(base_url)
  if (status_code(response) == 200) {
    return(fromJSON(rawToChar(response$content)))
  }
  return(NULL)
}

# Thème personnalisé
mon_theme <- bs_theme(
  version = 5,
  bootswatch = "pulse",
  primary = "#34495e",
  secondary = "#2c3e50",
  success = "#18bc9c",
  "navbar-bg" = "#2c3e50",
  "navbar-light-brand-color" = "#ffffff",
  "navbar-light-color" = "#ffffff",
  "card-border-radius" = "15px",
  "body-bg" = "#f5f6fa",
  "font-family-base" = "Roboto, sans-serif"
)

ui <- page_navbar(
  theme = mon_theme,
  title = div(
    style = "display: flex; align-items: center;",
    tags$img(src = "https://www.chess.com/favicon.ico", height = "30px", style = "margin-right: 10px;"),
    "L'Échiquier Comparatif"
  ),
  
  nav_panel(
    title = "Profil Joueur",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        style = "background-color: #ffffff; padding: 20px; border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
        div(style = "text-align: center; margin-bottom: 20px;", 
            h4("Recherche de Profils", style = "color: #2c3e50;"), 
            tags$hr()),
        
        # Joueur 1
        h5("Joueur Analysé", style = "color: #2c3e50; margin-top: 15px;"),
        textInput("username1_chess", "Chess.com", "Regalien", placeholder = "Pseudo Chess.com"),
        checkboxInput("sync_usernames1", "Utiliser le même pseudo pour Lichess", FALSE),
        conditionalPanel(
          condition = "!input.sync_usernames1",
          textInput("username1_lichess", "Lichess", "", placeholder = "Pseudo Lichess")
        ),
        
        # Case à cocher pour activer la comparaison
        checkboxInput("enable_comparison", "Activer la comparaison avec un autre joueur", FALSE),
        
        # Joueur 2 (conditionnel)
        conditionalPanel(
          condition = "input.enable_comparison == true",
          h5("Joueur à Comparer", style = "color: #2c3e50; margin-top: 15px;"),
          textInput("username2_chess", "Chess.com", "", placeholder = "Pseudo Chess.com"),
          checkboxInput("sync_usernames2", "Utiliser le même pseudo", FALSE),
          conditionalPanel(
            condition = "!input.sync_usernames2",
            textInput("username2_lichess", "Lichess", "", placeholder = "Pseudo Lichess")
          )
        ),
        
        div(style = "margin-top: 20px;", 
            actionButton("search", "Rechercher", 
                         class = "btn-primary w-100",
                         style = "background-color: #2c3e50;")),
        tags$hr(),
        div(style = "font-size: 0.9em; color: #666;", 
            "Vous pouvez utiliser des pseudos différents pour chaque site")
      ),
      
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Informations des Joueurs"),
          withSpinner(uiOutput("player_info"))
        ),
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Statistiques Détaillées"),
          withSpinner(uiOutput("player_stats"))
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header(class = "bg-primary text-white", "Comparaison des Classements"),
        card_body(
          selectInput(
            "graph_type",
            "Type de graphique :",
            choices = c(
              "Comparaison par cadence" = "cadence",
              "Évolution dans le temps" = "evolution",
              "Distribution des Elo" = "distribution"
            ),
            selected = "cadence"
          ),
          withSpinner(plotOutput("ratings_comparison", height = "400px"))
        )
      ) 
    )
  ),
  
  
  
  nav_panel(
    title = "Echiquier d'Analyse",
    card(
      full_screen = TRUE,
      card_header(class = "bg-primary text-white", "Echiquier d'Analyse Lichess"),
      card_body(
        tags$iframe(
          src = "https://lichess.org/analysis/embed",
          width = "100%",
          height = "800px",
          style = "border: none; border-radius: 10px;"
        )
      )
    )
  ),
  
  nav_panel(
    title = "Estimation Elo FIDE",
    layout_sidebar(
      sidebar = sidebar(
        style = "background-color: #ffffff; padding: 20px; border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
        h4("Paramètres d'estimation", style = "color: #2c3e50; margin-bottom: 20px;"),
        
        # Sélection de la plateforme principale
        radioButtons("platform_choice", "Plateforme principale :",
                     choices = c("Chess.com" = "chess", 
                                 "Lichess" = "lichess",
                                 "Les deux" = "both"),
                     selected = "chess"),
        # Cadence à utiliser
        radioButtons("time_control", "Cadence à utiliser :",
                     choices = c("Bullet" = "bullet",
                                 "Blitz" = "blitz", 
                                 "Rapid" = "rapid",
                                 "Moyenne globale" = "average"),
                     selected = "average"), 
        
        hr(),
        
        # Notes explicatives
        div(
          style = "font-size: 0.9em; color: #666;",
          p("L'estimation est basée sur une corrélation observée entre les Elos des différentes plateformes."),
          p("La précision peut varier selon plusieurs facteurs :"),
          tags$ul(
            tags$li("Nombre de parties jouées"),
            tags$li("Stabilité du classement"),
            tags$li("Période d'activité")
          )
        )
      ),
      layout_column_wrap(
        width = 1,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Estimation"
          ),
          card_body(
            uiOutput("fide_estimation")
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Explications et Méthodologie"
          ),
          card_body(
            p(strong("Méthodologie d'estimation :")),
            p("L'estimation de l'Elo FIDE est calculée en utilisant une régression linéaire basée sur des données réelles de corrélation obtenues de chessgoals.com."),
            tags$ul(
              tags$li("Les données sont mises à jour régulièrement pour maintenir la précision"),
              tags$li("Une régression linéaire spécifique est appliquée pour chaque plateforme et type de jeu"),
              tags$li("En cas d'utilisation des deux plateformes, une moyenne pondérée est calculée en fonction des coefficients de détermination (R²)")
            ),
            p(strong("Facteurs de confiance :")),
            p("La fiabilité de l'estimation dépend de plusieurs facteurs :"),
            tags$ul(
              tags$li("Qualité de la corrélation (R²) entre les Elos en ligne et FIDE"),
              tags$li("Nombre de parties jouées sur chaque plateforme"),
              tags$li("Stabilité du classement en ligne"),
              tags$li("Récence des parties jouées")
            ),
            p(strong("Note importante :")),
            p("Cette estimation est basée sur des données statistiques et peut varier selon :"),
            tags$ul(
              tags$li("La cadence de jeu choisie (Blitz, Rapid, ou moyenne)"),
              tags$li("La plateforme sélectionnée (Chess.com, Lichess, ou les deux)"),
              tags$li("La variation naturelle des corrélations dans le temps")
            ),
            div(
              style = "margin-top: 20px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
              p("Cette estimation reste indicative et ne constitue pas un classement FIDE officiel. Le véritable Elo FIDE ne peut être obtenu qu'en participant à des tournois homologués FIDE.")
            )
          )
        )
      )
    )
  ),
  #UI Classement fide 
  nav_panel(
    title = "Classement FIDE France",
    card(
      full_screen = TRUE,
      card_header(
        class = "bg-primary text-white",
        "Classement FIDE des joueurs français"
      ),
      card_body(
        div(
          style = "margin-bottom: 20px;",
          actionButton("load_fide_ratings", "Charger le classement", class = "btn-primary")
        ),
        DTOutput("fide_ratings_table")
      )
    )
  ),
  
  nav_panel(
    title = "Recherche de Livres",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        style = "background-color: #ffffff; padding: 20px; border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
        div(
          style = "text-align: center; margin-bottom: 20px;",
          h4("Recherche de Livres d'Échecs", style = "color: #2c3e50;"),
          tags$hr()
        ),
        textInput("search_query", "Terme de recherche:", 
                  placeholder = "Entrez votre recherche..."),
        actionButton("search_btn", "Rechercher", 
                     class = "btn-primary w-100",
                     style = "background-color: #2c3e50;"),
        tags$hr(),
        div(
          style = "font-size: 0.9em; color: #666;",
          "Recherchez des livres d'échecs en utilisant des mots-clés"
        )
      ),
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-primary text-white",
          "Résultats de la recherche"
        ),
        card_body(
          withSpinner(uiOutput("books_results"))
        )
      )
    )
  ),
  
  # Interface Paramétre Avec Gestion des API 
  nav_panel(
    title = "Paramètres",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        style = "background-color: #ffffff; padding: 20px; border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
        div(
          style = "text-align: center; margin-bottom: 20px;",
          h4("Configuration API", style = "color: #2c3e50;"),
          tags$hr()
        ),
        # Uniquement la clé API Google Books
        textInput("google_books_api", "Clé API Google Books",
                  value = "",
                  placeholder = "Entrez votre clé API Google Books"),
        
        actionButton("save_api_keys", "Sauvegarder la clé API",
                     class = "btn-primary w-100",
                     style = "background-color: #2c3e50;"),
        
        tags$hr(),
        div(
          style = "font-size: 0.9em; color: #666;",
          "La clé API est stockée localement dans votre navigateur.",
          br(),
          "Elle est nécessaire uniquement pour la recherche de livres d'échecs."
        )
      ),
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-primary text-white",
          "Guide de configuration de l'API Google Books"
        ),
        card_body(
          h4("Comment obtenir une clé API Google Books"),
          tags$ol(
            tags$li("Visitez la ", tags$a(href="https://console.developers.google.com/", "Console Google Cloud")),
            tags$li("Créez un nouveau projet ou sélectionnez un projet existant"),
            tags$li("Activez l'API Google Books"),
            tags$li("Créez des identifiants (clé API)")
          ),
          tags$hr(),
          div(
            class = "alert alert-info",
            icon("info-circle"),
            "Note : Les API Chess.com et Lichess sont publiques et ne nécessitent pas de clé API."
          ),
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            "Important : Ne partagez jamais votre clé API Google Books avec d'autres personnes."
          )
        )
      )
    )
  ),
  
  
  
  # CSS personnalisé
  tags$head(
    tags$style(HTML("
      .card {
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
        border-radius: 15px !important;
      }
      .btn-primary {
        transition: all 0.3s ease;
      }
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
      }
      .player-info-section {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 10px;
        margin-bottom: 15px;
      }
    ")),
    tags$link(rel = "stylesheet", href = "https://use.fontawesome.com/releases/v5.15.4/css/all.css")
  )
)

server <- function(input, output, session) {
  #Valeur reactive pour stocker la clé API  de GoogleBook
  api_key <- reactiveVal("")
  
  # Observer pour sauvegarder la clé API
  observeEvent(input$save_api_keys, {
    api_key(input$google_books_api)
    showModal(modalDialog(
      title = "Confirmation",
      "La clé API a été sauvegardée avec succès !",
      easyClose = TRUE
    ))
  })
  
  # Variables réactives pour le contrôle et les données
  loading <- reactiveVal(FALSE)
  fide_ratings <- reactiveVal(NULL)
  
  # Observateur pour le chargement du classement
  observeEvent(input$load_fide_ratings, {
    loading(TRUE)
    showModal(modalDialog(
      title = "Chargement en cours",
      div(
        "Récupération des données Chess.com pour les joueurs français...",
        br(), br(),
        actionButton("stop_loading", "Arrêter le chargement", class = "btn-danger")
      ),
      footer = NULL
    ))
    
    tryCatch({
      # Lister les joueurs français
      base_url <- "https://api.chess.com/pub/country/FR/players"
      response <- GET(base_url)
      
      if (!loading()) {
        removeModal()
        return(NULL)
      }
      
      if (status_code(response) == 200) {
        players_list <- fromJSON(rawToChar(response$content))$players
        
        # Initialiser le dataframe pour les résultats
        ratings_data <- data.frame(
          Rang = integer(),
          Nom = character(),
          Elo = integer(),
          Federation = character(),
          stringsAsFactors = FALSE
        )
        
        # Traiter les 500 premiers joueurs
        player_count <- min(25, length(players_list))
        
        for (i in 1:player_count) {
          if (!loading()) {
            removeModal()
            return(NULL)
          }
          
          # Mettre à jour le modal avec la progression
          updateModalText <- sprintf(
            "Récupération des données... (%d/%d joueurs)",
            i, player_count
          )
          
          # Extraire le nom d'utilisateur de l'URL
          player_username <- sub(".*/player/", "", players_list[i])
          
          # Obtenir les statistiques du joueur
          stats_url <- sprintf("https://api.chess.com/pub/player/%s/stats", player_username)
          stats_response <- GET(stats_url)
          
          if (status_code(stats_response) == 200) {
            stats_data <- fromJSON(rawToChar(stats_response$content))
            # Récupérer le meilleur Elo parmi les différentes cadences
            elos <- numeric()
            
            # Ajouter les Elos disponibles au vecteur
            if (!is.null(stats_data$chess_rapid$last$rating)) {
              elos <- c(elos, stats_data$chess_rapid$last$rating)
            }
            if (!is.null(stats_data$chess_blitz$last$rating)) {
              elos <- c(elos, stats_data$chess_blitz$last$rating)
            }
            if (!is.null(stats_data$chess_bullet$last$rating)) {
              elos <- c(elos, stats_data$chess_bullet$last$rating)
            }
            
            # Calculer le meilleur Elo seulement si le vecteur n'est pas vide
            best_elo <- if (length(elos) > 0) {
              max(elos)
            } else {
              NA
            }
            
            if (!is.na(best_elo)) {
              ratings_data <- rbind(ratings_data, data.frame(
                Rang = nrow(ratings_data) + 1,
                Nom = player_username,
                Elo = best_elo,
                Federation = "FRA",
                stringsAsFactors = FALSE
              ))
            }
            
          }
          
          # Pause pour respecter la limite de l'API
          Sys.sleep(0.1)
        }
        
        # Trier par Elo et mettre à jour les rangs
        if (nrow(ratings_data) > 0) {
          ratings_data <- ratings_data[order(-ratings_data$Elo), ]
          ratings_data$Rang <- seq_len(nrow(ratings_data))
          fide_ratings(ratings_data)
        }
      }
      
      loading(FALSE)
      removeModal()
      
    }, error = function(e) {
      loading(FALSE)
      removeModal()
      showModal(modalDialog(
        title = "Erreur",
        paste("Impossible de récupérer les données:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Observateur pour le bouton d'arrêt
  observeEvent(input$stop_loading, {
    loading(FALSE)
    removeModal()
    showModal(modalDialog(
      title = "Chargement interrompu",
      "Le chargement des données a été arrêté.",
      easyClose = TRUE
    ))
  })
  
  # Rendu du tableau FIDE
  
  output$fide_ratings_table <- renderDT({
    req(fide_ratings())
    
    datatable(
      fide_ratings(),
      options = list(
        pageLength = 50,
        lengthMenu = list(c(10, 25, 50), c('10', '25', '50')),  # Options de pagination réduites
        order = list(list(2, 'desc')),  # Tri par Elo (index 2)
        dom = 'lfrtip',  # Retrait du bouton d'export pour plus de rapidité
        processing = TRUE,
        deferRender = TRUE,  # Améliore les performances
        scroller = TRUE,     # Utilise le défilement virtuel
        scrollY = "600px",
        scrollCollapse = TRUE
      ),
      class = 'cell-border stripe',
      rownames = FALSE,
      selection = 'none',   # Désactive la sélection pour améliorer les performances
      filter = 'none'       # Désactive les filtres pour améliorer les performances
    )
  })
  
  player_data <- eventReactive(input$search, {
    
    # Joueur 1
    chess_com_data <- NULL
    lichess_data <- NULL
    if (input$username1_chess != "") {
      chess_com_data <- get_chess_com_player(input$username1_chess)
      # Obtenir les stats des parties pour Chess.com
      if (!is.null(chess_com_data)) {
        stats_url <- paste0("https://api.chess.com/pub/player/", input$username1_chess, "/stats")
        stats_response <- GET(stats_url)
        if (status_code(stats_response) == 200) {
          chess_com_data$complete_stats <- fromJSON(rawToChar(stats_response$content))
        }
      }
    }
    
    lichess_username <- if (input$sync_usernames1) input$username1_chess else input$username1_lichess
    if (lichess_username != "") {
      lichess_data <- get_lichess_player(lichess_username)
    }
    
    # Joueur 2 (seulement si la comparaison est activée)
    chess_com_data2 <- NULL
    lichess_data2 <- NULL
    if (input$enable_comparison) {
      if (input$username2_chess != "") {
        chess_com_data2 <- get_chess_com_player(input$username2_chess)
        # Obtenir les stats des parties pour Chess.com (Joueur 2)
        if (!is.null(chess_com_data2)) {
          stats_url2 <- paste0("https://api.chess.com/pub/player/", input$username2_chess, "/stats")
          stats_response2 <- GET(stats_url2)
          if (status_code(stats_response2) == 200) {
            chess_com_data2$complete_stats <- fromJSON(rawToChar(stats_response2$content))
          }
        }
      }
      
      lichess_username2 <- if (input$sync_usernames2) input$username2_chess else input$username2_lichess
      if (lichess_username2 != "") {
        lichess_data2 <- get_lichess_player(lichess_username2)
      }
    }
    
    list(
      chess_com = chess_com_data,
      lichess = lichess_data,
      chess_com2 = chess_com_data2,
      lichess2 = lichess_data2
    )
  })
  observe({
    if (input$sync_usernames1) {
      updateTextInput(session, "username1_lichess", value = input$username1_chess)
    }
  })
  
  observe({
    if (input$sync_usernames2) {
      updateTextInput(session, "username2_lichess", value = input$username2_chess)
    }
  })
  
  output$player_info <- renderUI({
    data <- player_data()
    if (is.null(data$chess_com) && is.null(data$lichess)) {
      return(div(class = "alert alert-warning", "Aucun joueur trouvé."))
    }
    
    tagList(
      if (!is.null(data$chess_com)) {
        div(
          class = "player-info-section",
          # En-tête avec logo Chess.com et titre
          div(
            style = "display: flex; align-items: center; margin-bottom: 15px;",
            tags$img(src = "https://www.chess.com/favicon.ico", height = "20px", 
                     style = "margin-right: 10px;"),
            h4("Chess.com", style = "margin: 0; color: #2c3e50;")
          ),
          # Photo de profil et informations
          div(
            style = "display: flex; gap: 20px; align-items: start;",
            # Photo de profil
            if (!is.null(data$chess_com$avatar)) {
              tags$img(
                src = data$chess_com$avatar,
                height = "100px",
                style = "border-radius: 50%; border: 3px solid #2c3e50;"
              )
            },
            # Informations du joueur
            div(
              p(strong("Nom: "), data$chess_com$username),
              p(strong("Pays: "), 
                if (!is.null(data$chess_com$country)) {
                  # Extraction du code pays de l'URL et conversion
                  country_code <- sub(".*/([^/]+)$", "\\1", data$chess_com$country)
                  switch(country_code,
                         "FR" = "France",
                         "US" = "États-Unis",
                         "GB" = "Royaume-Uni",
                         country_code) # Par défaut, affiche le code si pas de correspondance
                }
              ),
              p(strong("Inscription: "), 
                if (!is.null(data$chess_com$joined)) {
                  # Conversion de la date avec gestion d'erreur
                  tryCatch({
                    date_str <- gsub("\\.", "/", substr(data$chess_com$joined, 1, 5))
                    date_str
                  }, error = function(e) {
                    "Date non disponible"
                  })
                } else {
                  "Date non disponible"
                }
              ),
              if (!is.null(data$chess_com$complete_stats)) {
                total_games <- sum(
                  data$chess_com$complete_stats$chess_rapid$record$win,
                  data$chess_com$complete_stats$chess_rapid$record$loss,
                  data$chess_com$complete_stats$chess_rapid$record$draw,
                  data$chess_com$complete_stats$chess_blitz$record$win,
                  data$chess_com$complete_stats$chess_blitz$record$loss,
                  data$chess_com$complete_stats$chess_blitz$record$draw,
                  data$chess_com$complete_stats$chess_bullet$record$win,
                  data$chess_com$complete_stats$chess_bullet$record$loss,
                  data$chess_com$complete_stats$chess_bullet$record$draw,
                  na.rm = TRUE
                )
                p(strong("Parties jouées: "), format(total_games, big.mark = " "))
              },
              if (!is.null(data$chess_com$league)) {
                p(strong("Ligue: "), data$chess_com$league)
              },
              
              if (!is.null(data$chess_com$followers)) {
                p(strong("Abonnés: "), format(data$chess_com$followers, big.mark = " "))
              }
            )
          )
        )
      },
      if (!is.null(data$lichess)) {
        div(
          class = "player-info-section",
          div(
            style = "display: flex; align-items: center; margin-bottom: 15px;",
            tags$img(src = "https://lichess.org/favicon.ico", height = "20px", 
                     style = "margin-right: 10px;"),
            h4("Lichess", style = "margin: 0; color: #2c3e50;")
          ),
          div(
            style = "display: flex; gap: 20px; align-items: start;",
            if (!is.null(data$lichess$profile$avatar)) {
              tags$img(
                src = data$lichess$profile$avatar,
                height = "100px",
                style = "border-radius: 50%; border: 3px solid #2c3e50;"
              )
            },
            div(
              p(strong("Nom: "), data$lichess$username),
              p(strong("Parties jouées: "), format(data$lichess$count$all, big.mark = " ")),
              p(strong("Inscription: "), 
                format(as.POSIXct(data$lichess$createdAt/1000, origin="1970-01-01", tz="UTC"), 
                       "%d/%m/%Y")),
              if (!is.null(data$lichess$profile$country)) {
                p(strong("Pays: "), data$lichess$profile$country)
              },
              
              if (!is.null(data$lichess$profile$bio)) {
                p(strong("Bio: "), data$lichess$profile$bio)
              }
            )
          )
        )
      },  
      
      # Ajout des informations du deuxième joueur si la comparaison est activée
      if (input$enable_comparison) {
        div(
          style = "margin-top: 20px; border-top: 2px solid #2c3e50; padding-top: 20px;",
          h3("Informations du deuxième joueur", style = "color: #2c3e50; margin-bottom: 20px;"),
          
          # Informations Chess.com du deuxième joueur
          if (!is.null(data$chess_com2)) {
            div(
              class = "player-info-section",
              div(
                style = "display: flex; align-items: center; margin-bottom: 15px;",
                tags$img(src = "https://www.chess.com/favicon.ico", height = "20px", 
                         style = "margin-right: 10px;"),
                h4("Chess.com", style = "margin: 0; color: #2c3e50;")
              ),
              div(
                style = "display: flex; gap: 20px; align-items: start;",
                if (!is.null(data$chess_com2$avatar)) {
                  tags$img(
                    src = data$chess_com2$avatar,
                    height = "100px",
                    style = "border-radius: 50%; border: 3px solid #2c3e50;"
                  )
                },
                div(
                  p(strong("Nom: "), data$chess_com2$username),
                  p(strong("Pays: "), 
                    if (!is.null(data$chess_com2$country)) {
                      country_code <- sub(".*/([^/]+)$", "\\1", data$chess_com2$country)
                      switch(country_code,
                             "FR" = "France",
                             "US" = "États-Unis",
                             "GB" = "Royaume-Uni",
                             country_code)
                    }
                  ),
                  p(strong("Inscription: "), 
                    if (!is.null(data$chess_com2$joined)) {
                      format(as.Date(data$chess_com2$joined, format = "%Y.%m.%d"), "%d/%m/%Y")
                    }
                  ),
                  if (!is.null(data$chess_com2$complete_stats)) {
                    div(
                      style = "margin-top: 10px; border-top: 1px solid #eee; padding-top: 10px;",
                      p(strong("Statistiques par cadence:")),
                      # Bullet
                      if (!is.null(data$chess_com2$complete_stats$chess_bullet)) {
                        div(
                          style = "display: flex; align-items: center; margin: 5px 0;",
                          tags$img(
                            src = "https://www.chess.com/bundles/web/images/color-icons/bullet.svg",
                            height = "20px",
                            style = "margin-right: 10px;"
                          ),
                          sprintf(
                            "Bullet: %d ⚡ (V: %d, D: %d, N: %d)",
                            data$chess_com2$complete_stats$chess_bullet$last$rating,
                            data$chess_com2$complete_stats$chess_bullet$record$win,
                            data$chess_com2$complete_stats$chess_bullet$record$loss,
                            data$chess_com2$complete_stats$chess_bullet$record$draw
                          )
                        )
                      },
                      # Blitz
                      if (!is.null(data$chess_com2$complete_stats$chess_blitz)) {
                        div(
                          style = "display: flex; align-items: center; margin: 5px 0;",
                          tags$img(
                            src = "https://www.chess.com/bundles/web/images/color-icons/blitz.svg",
                            height = "20px",
                            style = "margin-right: 10px;"
                          ),
                          sprintf(
                            "Blitz: %d ⚡ (V: %d, D: %d, N: %d)",
                            data$chess_com2$complete_stats$chess_blitz$last$rating,
                            data$chess_com2$complete_stats$chess_blitz$record$win,
                            data$chess_com2$complete_stats$chess_blitz$record$loss,
                            data$chess_com2$complete_stats$chess_blitz$record$draw
                          )
                        )
                      },
                      # Rapid
                      if (!is.null(data$chess_com2$complete_stats$chess_rapid)) {
                        div(
                          style = "display: flex; align-items: center; margin: 5px 0;",
                          tags$img(
                            src = "https://www.chess.com/bundles/web/images/color-icons/rapid.svg",
                            height = "20px",
                            style = "margin-right: 10px;"
                          ),
                          sprintf(
                            "Rapid: %d ⚡ (V: %d, D: %d, N: %d)",
                            data$chess_com2$complete_stats$chess_rapid$last$rating,
                            data$chess_com2$complete_stats$chess_rapid$record$win,
                            data$chess_com2$complete_stats$chess_rapid$record$loss,
                            data$chess_com2$complete_stats$chess_rapid$record$draw
                          )
                        )
                      }
                    )
                  }
                )
              )
            )
          },
          
          # Informations Lichess du deuxième joueur
          if (!is.null(data$lichess2)) {
            div(
              class = "player-info-section",
              div(
                style = "display: flex; align-items: center; margin-bottom: 15px;",
                tags$img(src = "https://lichess.org/favicon.ico", height = "20px", 
                         style = "margin-right: 10px;"),
                h4("Lichess", style = "margin: 0; color: #2c3e50;")
              ),
              div(
                style = "display: flex; gap: 20px; align-items: start;",
                if (!is.null(data$lichess2$profile$avatar)) {
                  tags$img(
                    src = data$lichess2$profile$avatar,
                    height = "100px",
                    style = "border-radius: 50%; border: 3px solid #2c3e50;"
                  )
                },
                div(
                  p(strong("Nom: "), data$lichess2$username),
                  p(strong("Parties jouées: "), format(data$lichess2$count$all, big.mark = " ")),
                  p(strong("Inscription: "), 
                    format(as.POSIXct(data$lichess2$createdAt/1000, origin="1970-01-01", tz="UTC"), 
                           "%d/%m/%Y")),
                  if (!is.null(data$lichess2$perfs)) {
                    div(
                      style = "margin-top: 10px; border-top: 1px solid #eee; padding-top: 10px;",
                      p(strong("Statistiques par cadence:")),
                      # Bullet
                      if (!is.null(data$lichess2$perfs$bullet)) {
                        div(
                          style = "display: flex; align-items: center; margin: 5px 0;",
                          tags$i(class = "fas fa-bolt", 
                                 style = "margin-right: 10px; font-size: 20px;"),
                          sprintf(
                            "Bullet: %d ⚡ (Parties: %d)",
                            data$lichess2$perfs$bullet$rating,
                            data$lichess2$perfs$bullet$games
                          )
                        )
                      },
                      # Blitz
                      if (!is.null(data$lichess2$perfs$blitz)) {
                        div(
                          style = "display: flex; align-items: center; margin: 5px 0;",
                          tags$i(class = "fas fa-flash", 
                                 style = "margin-right: 10px; font-size: 20px;"),
                          sprintf(
                            "Blitz: %d ⚡ (Parties: %d)",
                            data$lichess2$perfs$blitz$rating,
                            data$lichess2$perfs$blitz$games
                          )
                        )
                      },
                      # Rapid
                      if (!is.null(data$lichess2$perfs$rapid)) {
                        div(
                          style = "display: flex; align-items: center; margin: 5px 0;",
                          tags$i(class = "fas fa-clock", 
                                 style = "margin-right: 10px; font-size: 20px;"),
                          sprintf(
                            "Rapid: %d ⚡ (Parties: %d)",
                            data$lichess2$perfs$rapid$rating,
                            data$lichess2$perfs$rapid$games
                          )
                        )
                      }
                    )
                  }
                )
              )
            )
          }
        )
      }
    )
  })
  output$player_stats <- renderUI({
    data <- player_data()
    
    tagList(
      # Stats Chess.com
      if (!is.null(data$chess_com$complete_stats)) {
        div(
          class = "player-info-section",
          div(
            style = "display: flex; align-items: center; margin-bottom: 15px;",
            tags$img(src = "https://www.chess.com/favicon.ico", height = "20px", 
                     style = "margin-right: 10px;"),
            h4("Chess.com - Statistiques par cadence", style = "margin: 0; color: #2c3e50;")
          ),
          # Bullet
          if (!is.null(data$chess_com$complete_stats$chess_bullet)) {
            div(
              style = "display: flex; align-items: center; margin: 5px 0;",
              tags$img(
                src = "https://www.chess.com/bundles/web/images/color-icons/bullet.svg",
                height = "20px",
                style = "margin-right: 10px;"
              ),
              sprintf(
                "Bullet: %d ⚡ (V: %d, D: %d, N: %d)",
                data$chess_com$complete_stats$chess_bullet$last$rating,
                data$chess_com$complete_stats$chess_bullet$record$win,
                data$chess_com$complete_stats$chess_bullet$record$loss,
                data$chess_com$complete_stats$chess_bullet$record$draw
              )
            )
          },
          # Blitz
          if (!is.null(data$chess_com$complete_stats$chess_blitz)) {
            div(
              style = "display: flex; align-items: center; margin: 5px 0;",
              tags$img(
                src = "https://www.chess.com/bundles/web/images/color-icons/blitz.svg",
                height = "20px",
                style = "margin-right: 10px;"
              ),
              sprintf(
                "Blitz: %d ⚡ (V: %d, D: %d, N: %d)",
                data$chess_com$complete_stats$chess_blitz$last$rating,
                data$chess_com$complete_stats$chess_blitz$record$win,
                data$chess_com$complete_stats$chess_blitz$record$loss,
                data$chess_com$complete_stats$chess_blitz$record$draw
              )
            )
          },
          # Rapid
          if (!is.null(data$chess_com$complete_stats$chess_rapid)) {
            div(
              style = "display: flex; align-items: center; margin: 5px 0;",
              tags$img(
                src = "https://www.chess.com/bundles/web/images/color-icons/rapid.svg",
                height = "20px",
                style = "margin-right: 10px;"
              ),
              sprintf(
                "Rapid: %d ⚡ (V: %d, D: %d, N: %d)",
                data$chess_com$complete_stats$chess_rapid$last$rating,
                data$chess_com$complete_stats$chess_rapid$record$win,
                data$chess_com$complete_stats$chess_rapid$record$loss,
                data$chess_com$complete_stats$chess_rapid$record$draw
              )
            )
          }
        )
      },
      
      # Stats Lichess
      if (!is.null(data$lichess$perfs)) {
        div(
          class = "player-info-section",
          div(
            style = "display: flex; align-items: center; margin-bottom: 15px;",
            tags$img(src = "https://lichess.org/favicon.ico", height = "20px", 
                     style = "margin-right: 10px;"),
            h4("Lichess - Statistiques par cadence", style = "margin: 0; color: #2c3e50;")
          ),
          # Bullet
          if (!is.null(data$lichess$perfs$bullet)) {
            div(
              style = "display: flex; align-items: center; margin: 5px 0;",
              tags$i(class = "fas fa-bolt", 
                     style = "margin-right: 10px; font-size: 20px;"),
              sprintf(
                "Bullet: %d ⚡ (Parties: %d)",
                data$lichess$perfs$bullet$rating,
                data$lichess$perfs$bullet$games
              )
            )
          },
          # Blitz
          if (!is.null(data$lichess$perfs$blitz)) {
            div(
              style = "display: flex; align-items: center; margin: 5px 0;",
              tags$i(class = "fas fa-flash", 
                     style = "margin-right: 10px; font-size: 20px;"),
              sprintf(
                "Blitz: %d ⚡ (Parties: %d)",
                data$lichess$perfs$blitz$rating,
                data$lichess$perfs$blitz$games
              )
            )
          },
          # Rapid
          if (!is.null(data$lichess$perfs$rapid)) {
            div(
              style = "display: flex; align-items: center; margin: 5px 0;",
              tags$i(class = "fas fa-clock", 
                     style = "margin-right: 10px; font-size: 20px;"),
              sprintf(
                "Rapid: %d ⚡ (Parties: %d)",
                data$lichess$perfs$rapid$rating,
                data$lichess$perfs$rapid$games
              )
            )
          }
        )
      }
    )
  })
  
  calculate_fide <- function(chess_elo, lichess_elo, platform, time_control) {
    # Debug logs
    cat("Entrée de calculate_fide:\n")
    cat("chess_elo:", chess_elo, "\n")
    cat("lichess_elo:", lichess_elo, "\n")
    cat("platform:", platform, "\n")
    cat("time_control:", time_control, "\n")
    
    # Récupérer les données de corrélation
    correlations <- get_all_correlations()
    cat("Coefficients récupérés:\n")
    cat("Chess.com:", correlations$chess_com$coefficients[[time_control]], "\n")
    cat("Lichess:", correlations$lichess$coefficients[[time_control]], "\n")
    
    # Fonction pour appliquer la régression linéaire
    apply_regression <- function(elo, platform_data, game_type) {
      if (is.null(elo) || is.na(elo)) return(NULL)
      
      # Obtenir les coefficients
      coef <- platform_data$coefficients[[game_type]]
      if (is.null(coef)) {
        cat("Pas de coefficients trouvés pour", game_type, "\n")
        return(NULL)
      }
      
      # Convertir en numérique et calculer
      elo <- as.numeric(elo)
      intercept <- as.numeric(coef[1])
      slope <- as.numeric(coef[2])
      
      fide_estimate <- intercept + slope * elo
      r2 <- platform_data$r_squared[[game_type]]
      
      cat("Calcul pour", game_type, ":", intercept, "+", slope, "*", elo, "=", fide_estimate, "\n")
      
      return(list(estimate = fide_estimate, r2 = r2))
    }
    
    # Calculer les estimations selon la plateforme
    chess_estimate <- NULL
    lichess_estimate <- NULL
    
    if (!is.null(chess_elo) && !is.na(chess_elo) && (platform %in% c("chess", "both"))) {
      if (time_control == "average") {
        # Pour la moyenne, calculer pour chaque type et faire la moyenne
        estimates <- list()
        for (type in c("bullet", "blitz", "rapid")) {
          est <- apply_regression(chess_elo, correlations$chess_com, type)
          if (!is.null(est)) estimates[[type]] <- est
        }
        if (length(estimates) > 0) {
          chess_estimate <- list(
            estimate = mean(sapply(estimates, function(x) x$estimate)),
            r2 = mean(sapply(estimates, function(x) x$r2))
          )
        }
      } else {
        chess_estimate <- apply_regression(chess_elo, correlations$chess_com, time_control)
      }
    }
    
    if (!is.null(lichess_elo) && !is.na(lichess_elo) && (platform %in% c("lichess", "both"))) {
      if (time_control == "average") {
        estimates <- list()
        for (type in c("bullet", "blitz", "rapid")) {
          est <- apply_regression(lichess_elo, correlations$lichess, type)
          if (!is.null(est)) estimates[[type]] <- est
        }
        if (length(estimates) > 0) {
          lichess_estimate <- list(
            estimate = mean(sapply(estimates, function(x) x$estimate)),
            r2 = mean(sapply(estimates, function(x) x$r2))
          )
        }
      } else {
        lichess_estimate <- apply_regression(lichess_elo, correlations$lichess, time_control)
      }
    }
    
    # Retourner l'estimation finale
    if (platform == "chess" && !is.null(chess_estimate)) {
      return(round(chess_estimate$estimate))
    } else if (platform == "lichess" && !is.null(lichess_estimate)) {
      return(round(lichess_estimate$estimate))
    } else if (platform == "both") {
      if (is.null(chess_estimate) && is.null(lichess_estimate)) return(NULL)
      if (is.null(chess_estimate)) return(round(lichess_estimate$estimate))
      if (is.null(lichess_estimate)) return(round(chess_estimate$estimate))
      
      # Moyenne pondérée par R²
      total_weight <- chess_estimate$r2 + lichess_estimate$r2
      weighted_estimate <- (chess_estimate$estimate * chess_estimate$r2 + 
                              lichess_estimate$estimate * lichess_estimate$r2) / total_weight
      return(round(weighted_estimate))
    }
    
    return(NULL)
  }
  
  
  output$ratings_comparison <- renderPlot({
    data <- player_data()
    
    if(input$graph_type == "cadence") {
      # Code pour le graphique par cadence
      ratings_j1 <- get_player_ratings(data$chess_com, data$lichess)
      ratings_j2 <- if(!is.null(data$chess_com2) || !is.null(data$lichess2)) {
        get_player_ratings(data$chess_com2, data$lichess2)
      } else NULL
      
      ratings_df <- data.frame(
        Site = character(),
        Rating = numeric(),
        Player = character(),
        Type = character(),
        stringsAsFactors = FALSE
      )
      
      for (type in c("bullet", "blitz", "rapid")) {
        if (!is.null(ratings_j1$chess[[type]])) {
          ratings_df <- rbind(ratings_df, data.frame(
            Site = "Chess.com",
            Rating = ratings_j1$chess[[type]],
            Player = "Joueur 1",
            Type = tools::toTitleCase(type)
          ))
        }
        if (!is.null(ratings_j1$lichess[[type]])) {
          ratings_df <- rbind(ratings_df, data.frame(
            Site = "Lichess",
            Rating = ratings_j1$lichess[[type]],
            Player = "Joueur 1",
            Type = tools::toTitleCase(type)
          ))
        }
      }
      
      if (!is.null(ratings_j2)) {
        for (type in c("bullet", "blitz", "rapid")) {
          if (!is.null(ratings_j2$chess[[type]])) {
            ratings_df <- rbind(ratings_df, data.frame(
              Site = "Chess.com",
              Rating = ratings_j2$chess[[type]],
              Player = "Joueur 2",
              Type = tools::toTitleCase(type)
            ))
          }
          if (!is.null(ratings_j2$lichess[[type]])) {
            ratings_df <- rbind(ratings_df, data.frame(
              Site = "Lichess",
              Rating = ratings_j2$lichess[[type]],
              Player = "Joueur 2",
              Type = tools::toTitleCase(type)
            ))
          }
        }
      }
      
      ggplot(ratings_df, aes(x = Type, y = Rating, fill = interaction(Site, Player))) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
        theme_minimal() +
        scale_fill_manual(
          values = c(
            "Chess.com.Joueur 1" = "#2c3e50",
            "Lichess.Joueur 1" = "#3498db",
            "Chess.com.Joueur 2" = "#e74c3c",
            "Lichess.Joueur 2" = "#e67e22"
          ),
          name = "Site / Joueur"
        ) +
        labs(
          title = "Comparaison des Classements Elo par Type de Partie",
          y = "Classement Elo",
          x = "Type de partie"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.position = "bottom",
          legend.title = element_text(size = 12)
        )
    } else if(input$graph_type == "evolution") {
      # Code pour le graphique d'évolution
      dates <- seq(from = Sys.Date() - 30, to = Sys.Date(), by = "day")
      evolution_df <- data.frame(
        Date = rep(dates, 4),
        Rating = sample(1000:2000, length(dates) * 4),
        Type = rep(c("Blitz", "Rapid"), each = length(dates) * 2),
        Site = rep(c("Chess.com", "Lichess"), each = length(dates)),
        Player = rep("Joueur 1", length(dates) * 4),
        stringsAsFactors = FALSE
      )
      
      ggplot(evolution_df, aes(x = Date, y = Rating, color = interaction(Site, Player, Type))) +
        geom_line() +
        geom_point(size = 1) +
        theme_minimal() +
        scale_color_manual(
          values = c(
            "Chess.com.Joueur 1.Blitz" = "#2c3e50",
            "Chess.com.Joueur 1.Rapid" = "#34495e",
            "Lichess.Joueur 1.Blitz" = "#3498db",
            "Lichess.Joueur 1.Rapid" = "#2980b9"
          ),
          name = "Site / Joueur / Type"
        ) +
        labs(
          title = "Évolution des Classements dans le Temps",
          y = "Classement Elo",
          x = "Date"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.position = "bottom",
          legend.title = element_text(size = 12)
        )
    } else {
      # Graphique de distribution
      ratings_df <- data.frame(
        Rating = rnorm(1000, mean = 1500, sd = 200),
        Source = rep("Exemple", 1000)
      )
      
      ggplot(ratings_df, aes(x = Rating)) +
        geom_density(fill = "#2c3e50", alpha = 0.5) +
        theme_minimal() +
        labs(
          title = "Distribution des Classements",
          y = "Densité",
          x = "Classement Elo"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )}
  })
  output$fide_estimation <- renderUI({
    data <- player_data()
    
    # Récupération des ratings
    ratings <- get_player_ratings(data$chess_com, data$lichess)
    
    # Debug des ratings disponibles
    cat("Ratings disponibles pour l'estimation FIDE:\n")
    print(ratings)
    
    # Récupérer les Elos selon le time_control
    chess_elo <- if (input$time_control == "average") {
      chess_ratings <- c(ratings$chess$bullet, ratings$chess$blitz, ratings$chess$rapid)
      chess_ratings <- chess_ratings[!is.null(chess_ratings) & !is.na(chess_ratings)]
      if (length(chess_ratings) > 0) mean(chess_ratings) else NULL
    } else {
      ratings$chess[[input$time_control]]
    }
    
    lichess_elo <- if (input$time_control == "average") {
      lichess_ratings <- c(ratings$lichess$bullet, ratings$lichess$blitz, ratings$lichess$rapid)
      lichess_ratings <- lichess_ratings[!is.null(lichess_ratings) & !is.na(lichess_ratings)]
      if (length(lichess_ratings) > 0) mean(lichess_ratings) else NULL
    } else {
      ratings$lichess[[input$time_control]]
    }
    
    # Debug des Elos calculés
    cat("Elos à utiliser pour l'estimation:\n")
    cat("Chess Elo:", chess_elo, "\n")
    cat("Lichess Elo:", lichess_elo, "\n")
    
    # Calcul de l'estimation FIDE
    fide_estimate <- calculate_fide(chess_elo, lichess_elo, input$platform_choice, input$time_control)
    
    # Niveau de confiance
    confidence <- if (!is.null(chess_elo) && !is.null(lichess_elo)) {
      "Élevé (basé sur les deux plateformes)"
    } else if (!is.null(chess_elo) || !is.null(lichess_elo)) {
      "Moyen (basé sur une seule plateforme)"
    } else {
      "Faible (données insuffisantes)"
    }
    
    # Affichage des résultats
    div(
      style = "text-align: center;",
      if (!is.null(fide_estimate)) {
        tagList(
          div(
            h2(style = "color: #2c3e50; margin-bottom: 30px;",
               "Estimation Elo FIDE"),
            div(
              style = "font-size: 48px; color: #18bc9c; margin: 20px 0;",
              fide_estimate
            ),
            div(
              style = "margin: 20px 0;",
              p(strong("Niveau de confiance : "), 
                span(style = "color: #3498db;", confidence))
            ),
            div(
              style = "color: #7f8c8d; font-size: 0.9em;",
              "Cette estimation est basée sur vos classements en ligne.",
              br(),
              "Elle peut varier de ±100 points par rapport à un véritable classement FIDE."
            )
          )
        )
      } else {
        div(
          class = "alert alert-warning",
          "Impossible de calculer une estimation FIDE avec les données disponibles.",
          br(),
          "Veuillez vérifier que vous avez des classements sur les plateformes sélectionnées."
        )
      }
    )
  })
  # Réactif pour les résultats de recherche avec la clé API des parametre
  search_results <- reactive({
    req(input$search_query)
    current_api_key <- api_key()
    
    if (current_api_key == "") {
      showModal(modalDialog(
        title = "Erreur",
        "Veuillez d'abord configurer votre clé API Google Books dans l'onglet Paramètres",
        easyClose = TRUE
      ))
      return(data.frame())
    }
    
    withProgress(
      message = "Recherche en cours...",
      value = 0.5,
      {
        search_chess_books(input$search_query, current_api_key)
      }
    )
  })
  
  # Observer pour mettre à jour automatiquement la recherche de livres en fonction de l'estimation FIDE
  observe({
    data <- player_data()
    
    # Récupérer les ratings
    ratings <- get_player_ratings(data$chess_com, data$lichess)
    
    # Récupérer les Elos selon le time_control
    chess_elo <- if (input$time_control == "average") {
      chess_ratings <- c(ratings$chess$bullet, ratings$chess$blitz, ratings$chess$rapid)
      chess_ratings <- chess_ratings[!is.null(chess_ratings) & !is.na(chess_ratings)]
      if (length(chess_ratings) > 0) mean(chess_ratings) else NULL
    } else {
      ratings$chess[[input$time_control]]
    }
    
    lichess_elo <- if (input$time_control == "average") {
      lichess_ratings <- c(ratings$lichess$bullet, ratings$lichess$blitz, ratings$lichess$rapid)
      lichess_ratings <- lichess_ratings[!is.null(lichess_ratings) & !is.na(lichess_ratings)]
      if (length(lichess_ratings) > 0) mean(lichess_ratings) else NULL
    } else {
      ratings$lichess[[input$time_control]]
    }
    
    # Calculer l'estimation FIDE
    fide_estimate <- calculate_fide(chess_elo, lichess_elo, input$platform_choice, input$time_control)
    
    # Déterminer le terme de recherche en fonction du niveau
    search_term <- if (!is.null(fide_estimate)) {
      if (fide_estimate < 1200) {
        "beginner chess book"
      } else if (fide_estimate < 1600) {
        "intermediate chess book"
      } else if (fide_estimate < 2000) {
        "advanced chess strategy"
      } else if (fide_estimate < 2400) {
        "master chess strategy"
      } else {
        "grandmaster chess"
      }
    } else {
      "chess book"  # terme par défaut si pas d'estimation
    }
    
    # Mettre à jour le champ de recherche
    updateTextInput(session, "search_query", value = search_term)
  })
  # Afficher les résultats
  output$books_results <- renderUI({
    results <- search_results()
    if (nrow(results) > 0) {
      tagList(
        lapply(1:nrow(results), function(i) {
          div(
            style = "margin-bottom: 20px; padding: 15px; background-color: white; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            div(
              style = "display: flex; gap: 20px;",
              # Informations du livre
              div(
                style = "flex-grow: 1;",
                h4(style = "color: #2c3e50; margin-bottom: 10px;", 
                   results$titre[i]),
                p(style = "color: #7f8c8d; margin-bottom: 5px;",
                  strong("Auteur(s) : "), 
                  results$auteurs[i]),
                p(style = "color: #7f8c8d; margin-bottom: 5px;",
                  strong("Date de publication : "), 
                  results$date[i]),
                p(style = "color: #34495e; margin-top: 10px;",
                  results$description[i])
              )
            )
          )
        })
      )
    } else {
      div(
        class = "alert alert-warning",
        "Aucun livre trouvé pour cette recherche."
      )
    }
  })
  
}

shinyApp(ui = ui, server = server)
