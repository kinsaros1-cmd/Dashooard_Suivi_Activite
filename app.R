# ================================================================================
# APPLICATION INS - SYSTÈME INTÉGRÉ DE SUIVI DES ACTIVITÉS
# Organisation: Institut National de la Statistique (INS)
# ================================================================================

# --- CHARGEMENT DES PACKAGES ---
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)
library(tidyr)

# ================================================================================
# CONFIGURATION GLOBALE
# ================================================================================

# --- BASE DE DONNÉES UTILISATEURS ---
# Fichier pour stocker les utilisateurs
nom_fichier_users <- "users_db.csv"

# Fonction pour charger les utilisateurs
load_users <- function() {
  if(file.exists(nom_fichier_users)) {
    read.csv2(nom_fichier_users, stringsAsFactors = FALSE, encoding = "UTF-8")
  } else {
    # Créer la base initiale si elle n'existe pas
    users_db_default <- data.frame(
      user_name = c("DG", "DER", "DSDS", "DSE", "DSAE", "KINSA", "POATY", "KAYA", "JEAN"),
      user_pw = c("pw000", "pw001", "pw002", "pw005", "pw006", "1234", "5678", "ki@1", "pw999"),
      role = c("Directeur général", "Direction_Centrale", "Direction_Centrale", 
               "Direction_Centrale", "Direction_Centrale", "RESPONSABLE", "RESPONSABLE", 
               "RESPONSABLE", "RESPONSABLE"),
      direction = c("Direction Générale", "Direction des Enquêtes et Recensement", 
                    "Direction des Statistiques Démographiques et Sociales",
                    "Direction des Statistiques Economiques", 
                    "Direction des Statistiques et des Analyses Economiques",
                    "Direction des Enquêtes et Recensement",
                    "Direction des Statistiques Economiques",
                    "Direction des Statistiques Démographiques et Sociales",
                    "Direction des Statistiques et des Analyses Economiques"),
      service = c("", "Méthodologie générale", "Statistiques de l'Education",
                  "Secteurs productifs", "Conjoncture",
                  "Méthodologie générale", "Secteurs productifs", 
                  "Statistiques de l'Education", "Conjoncture"),
      bureau = c("", "Elaboration des documents", "Suivi indicateurs",
                 "Secteur primaire", "Suivi inflation",
                 "Elaboration des documents", "Secteur primaire",
                 "Suivi indicateurs", "Suivi inflation"),
      stringsAsFactors = FALSE
    )
    write.csv2(users_db_default, nom_fichier_users, row.names = FALSE)
    users_db_default
  }
}

# Charger les utilisateurs au démarrage
users_db <- load_users()

# --- CONFIGURATION DES ÉTAPES DU PROCESSUS ---
etapes_config <- list(
  list(id = "TDR", label = "Termes de référence global", doc = TRUE, ordre = 1),
  list(id = "ANO", label = "Demande d'Avis de Non Objection", doc = FALSE, ordre = 2),
  list(id = "ETP", label = "Mise en place de l'Équipe Technique", doc = TRUE, ordre = 3),
  list(id = "TP", label = "Travaux préparatoires", doc = TRUE, ordre = 4),
  list(id = "FP", label = "Formation du personnel", doc = TRUE, ordre = 5),
  list(id = "CDT", label = "Collecte de données sur le terrain", doc = TRUE, ordre = 6),
  list(id = "TAD", label = "Traitement / Apurement des données", doc = TRUE, ordre = 7),
  list(id = "ARR", label = "Analyse et Rédaction du rapport", doc = TRUE, ordre = 8),
  list(id = "VR", label = "Validation du rapport", doc = TRUE, ordre = 9),
  list(id = "PML", label = "Publication / Mise en ligne", doc = TRUE, ordre = 10)
)

# --- TYPES D'ACTIVITÉS ---
types_activites <- c("Sélectionnez le type", "Enquête", "Recensement", 
                     "Annuaire", "Administratif-service", "Autre")

# ================================================================================
# INTERFACE UTILISATEUR
# ================================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # --- EN-TÊTE ---
  dashboardHeader(
    title = span(
      img(src = "logo.png", height = "40px", onerror="this.style.display='none';"),
      "INS - Suivi des Activités"
    ),
    titleWidth = 350,
    dropdownMenuOutput("notif_menu")
  ),
  
  # --- BARRE LATÉRALE ---
  dashboardSidebar(
    width = 280,
    useShinyjs(),
    
    sidebarMenu(
      id = "sidebar_menu",
      
      # Menu Connexion
      menuItem("Connexion", tabName = "login", icon = icon("sign-in-alt")),
      
      # Menu Saisie (visible après connexion)
      menuItem("Saisie des activités", tabName = "saisie", 
               icon = icon("edit"), startExpanded = FALSE,
               menuSubItem("Nouvelle activité", tabName = "nouvelle_activite", icon = icon("plus-circle")),
               menuSubItem("Mes activités", tabName = "mes_activites", icon = icon("list"))
      ),
      
      # Menu Tableau de bord
      menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
      
      # Menu Analyses
      menuItem("Analyses", tabName = "analyses", icon = icon("chart-line"),
               menuSubItem("Vue d'ensemble", tabName = "vue_ensemble", icon = icon("eye")),
               menuSubItem("Performance", tabName = "performance", icon = icon("trophy")),
               menuSubItem("Planification", tabName = "planification", icon = icon("calendar"))
      ),
      
      # Menu Documents
      menuItem("Documents", tabName = "documents", icon = icon("folder-open")),
      
      # Menu Rapports
      menuItem("Rapports", tabName = "rapports", icon = icon("file-alt")),
      
      # Menu Administration (visible pour DG et Directions)
      menuItem("Administration", tabName = "admin", icon = icon("cog"))
    ),
    
    hr(),
    
    # Informations utilisateur
    div(
      id = "user_info_box",
      style = "padding: 15px; display: none;",
      uiOutput("user_info_sidebar"),
      br(),
      actionButton("logout_btn", "Déconnexion", 
                   icon = icon("sign-out-alt"),
                   class = "btn-danger btn-sm btn-block")
    )
  ),
  
  # --- CORPS PRINCIPAL ---
  dashboardBody(
    
    # CSS personnalisé
    tags$head(
      tags$style(HTML("
        /* Style général */
        .content-wrapper, .right-side {
          background-color: #ecf0f5;
        }
        
        /* Logo et branding */
        .main-header .logo {
          background-color: #3c8dbc !important;
        }
        
        /* Étapes du processus */
        .step-row {
          transition: all 0.3s;
          border-radius: 8px;
          margin-bottom: 12px;
          padding: 15px;
          background: white;
          border: 1px solid #ddd;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        .step-row:hover {
          box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        }
        
        .disabled-step {
          opacity: 0.5;
          background: #f9f9f9 !important;
        }
        
        .disabled-step .bootstrap-switch {
          cursor: not-allowed !important;
        }
        
        .disabled-step .btn,
        .disabled-step input[type='file'] {
          opacity: 0.6;
        }
        
        .step-completed {
          border-left: 4px solid #00a65a;
        }
        
        .step-in-progress {
          border-left: 4px solid #f39c12;
        }
        
        /* Style pour la checkbox de l'étape 1 */
        #row_TDR input[type='checkbox'] {
          width: 25px;
          height: 25px;
          cursor: pointer;
          margin: 0 auto;
          display: block;
        }
        
        #row_TDR .checkbox {
          margin-top: 0;
          margin-bottom: 0;
        }
        
        /* Info boxes personnalisées */
        .info-box-custom {
          border-left: 5px solid #007bff;
          background: #e7f3ff;
          padding: 20px;
          border-radius: 8px;
          margin-bottom: 15px;
        }
        
        /* Badges de statut */
        .status-badge {
          padding: 6px 12px;
          border-radius: 4px;
          font-weight: bold;
          font-size: 12px;
          display: inline-block;
        }
        
        .status-encours { background: #f39c12; color: white; }
        .status-termine { background: #00a65a; color: white; }
        .status-enretard { background: #dd4b39; color: white; }
        .status-nonentamee { background: #d2d6de; color: #444; }
        
        /* Badge fichier joint */
        .file-status-badge {
          font-size: 0.85em;
          margin-top: 8px;
          display: block;
          color: #28a745;
          font-weight: 500;
        }
        
        /* Logo container */
        .logo-container {
          text-align: center;
          padding: 30px;
          background: white;
          border-radius: 8px;
          margin-bottom: 20px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        /* Boîtes de connexion */
        .login-box {
          max-width: 500px;
          margin: 50px auto;
        }
        
        /* Progress bar personnalisée */
        .progress {
          height: 25px;
          border-radius: 5px;
          box-shadow: inset 0 1px 2px rgba(0,0,0,.1);
        }
        
        .progress-bar {
          font-size: 14px;
          line-height: 25px;
          font-weight: 600;
        }
        
        /* Cards améliorées */
        .box.box-solid.box-primary > .box-header {
          background: #3c8dbc;
          color: #fff;
        }
        
        .small-box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .small-box:hover {
          box-shadow: 0 4px 8px rgba(0,0,0,0.15);
          transform: translateY(-2px);
          transition: all 0.3s;
        }
        
        /* Boutons */
        .btn {
          border-radius: 4px;
          font-weight: 500;
        }
        
        /* Tables */
        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background: #3c8dbc !important;
          border-color: #3c8dbc !important;
          color: white !important;
        }
        
        /* Alertes */
        .alert {
          border-radius: 6px;
          border-left: 4px solid;
        }
        
        .alert-success {
          border-left-color: #00a65a;
        }
        
        .alert-danger {
          border-left-color: #dd4b39;
        }
        
        .alert-info {
          border-left-color: #3c8dbc;
        }
        
        /* Timeline */
        .timeline-item {
          padding: 15px;
          margin-bottom: 10px;
          background: white;
          border-left: 3px solid #3c8dbc;
          border-radius: 4px;
        }
      "))
    ),
    
    # Contenu des onglets
    tabItems(
      
      # ========================================================================
      # ONGLET CONNEXION
      # ========================================================================
      tabItem(
        tabName = "login",
        fluidRow(
          column(
            width = 12,
            div(
              class = "logo-container",
              img(src = "logo.png", height = "120px", alt = "Logo INS", 
                  onerror="this.src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=';")
            )
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("lock"), " Authentification"), 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            class = "login-box",
            
            textInput("username", "Nom d'utilisateur :", 
                     placeholder = "Entrez votre identifiant"),
            passwordInput("password", "Mot de passe :", 
                         placeholder = "Entrez votre mot de passe"),
            
            actionButton("login_btn", "Se connecter", 
                        class = "btn-primary btn-lg btn-block",
                        icon = icon("sign-in-alt")),
            br(),
            uiOutput("login_message"),
            
            hr(),
            
            div(
              style = "background: #f4f4f4; padding: 15px; border-radius: 6px;",
              h5(icon("info-circle"), " Comptes de test disponibles :"),
              tags$table(
                class = "table table-sm",
                tags$tr(
                  tags$th("Rôle"),
                  tags$th("Utilisateur"),
                  tags$th("Mot de passe")
                ),
                tags$tr(
                  tags$td(tags$strong("Directeur Général")),
                  tags$td("DG"),
                  tags$td("pw000")
                ),
                tags$tr(
                  tags$td(tags$strong("Direction DER")),
                  tags$td("DER"),
                  tags$td("pw001")
                ),
                tags$tr(
                  tags$td(tags$strong("Responsable")),
                  tags$td("KINSA"),
                  tags$td("1234")
                )
              )
            )
          )
        )
      ),
      
      # ========================================================================
      # ONGLET NOUVELLE ACTIVITÉ (SAISIE)
      # ========================================================================
      tabItem(
        tabName = "nouvelle_activite",
        
        fluidRow(
          column(
            width = 12,
            h2(icon("plus-circle"), " Saisie d'une nouvelle activité"),
            hr()
          )
        ),
        
        # Barre de progression
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              status = "info",
              solidHeader = TRUE,
              title = "Progression de la saisie",
              uiOutput("progress_bar_ui")
            )
          )
        ),
        
        # Boutons d'action
        fluidRow(
          column(
            width = 12,
            actionButton("new_form_btn", "Nouvelle fiche / Réinitialiser", 
                        icon = icon("file-medical"), 
                        class = "btn-warning"),
            actionButton("save_draft_btn", "Enregistrer brouillon", 
                        icon = icon("save"), 
                        class = "btn-info")
          )
        ),
        
        br(),
        
        # Formulaire de saisie
        fluidRow(
          box(
            title = tagList(icon("edit"), " Informations sur l'activité"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 3,
                textInput("ref_activite", "Référence :",
                         placeholder = "Ex: 1.2.2.01")
              ),
              column(
                width = 3,
                pickerInput("type_activite", "Type d'activité :",
                           choices = types_activites,
                           selected = "Sélectionnez le type")
              ),
              column(
                width = 6,
                textInput("nom_activite", "Nom de l'activité (Unique) :",
                         placeholder = "Ex: Enquête harmonisée sur les conditions de vie")
              )
            ),
            
            fluidRow(
              column(
                width = 4,
                dateInput("date_debut", "Date de début :",
                         value = Sys.Date(),
                         format = "dd/mm/yyyy",
                         language = "fr")
              ),
              column(
                width = 4,
                dateInput("date_fin", "Date de fin prévue :",
                         value = Sys.Date() + 180,
                         format = "dd/mm/yyyy",
                         language = "fr")
              ),
              column(
                width = 4,
                uiOutput("duree_activite_ui")
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                textAreaInput("observation_activite", "Observations :",
                             rows = 3,
                             placeholder = "Remarques, commentaires, précisions...")
              )
            )
          )
        ),
        
        # Étapes du processus
        fluidRow(
          box(
            title = tagList(icon("tasks"), " Étapes du processus"),
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            p(class = "text-muted", 
              icon("info-circle"), 
              " Cochez les étapes complétées et joignez les documents justificatifs. ",
              "Les étapes suivent un ordre séquentiel."),
            
            hr(),
            
            uiOutput("etapes_form_ui")
          )
        ),
        
        # Bouton d'enregistrement final
        fluidRow(
          column(
            width = 12,
            div(
              style = "text-align: right;",
              actionButton("save_data_btn", "Enregistrer l'activité", 
                          icon = icon("save"), 
                          class = "btn-success btn-lg"),
              br(), br()
            )
          )
        )
      ),
      
      # ========================================================================
      # ONGLET MES ACTIVITÉS
      # ========================================================================
      tabItem(
        tabName = "mes_activites",
        
        fluidRow(
          column(
            width = 12,
            h2(icon("list"), " Mes activités enregistrées"),
            hr()
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("table"), " Liste de mes activités"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("table_mes_activites"),
            
            br(),
            
            actionButton("refresh_table_btn", "Actualiser", 
                        icon = icon("sync"), 
                        class = "btn-info")
          )
        )
      ),
      
      # ========================================================================
      # ONGLET TABLEAU DE BORD
      # ========================================================================
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          column(
            width = 12,
            h2(icon("dashboard"), " Tableau de bord - Vue d'ensemble"),
            hr()
          )
        ),
        
        # Value boxes
        fluidRow(
          valueBoxOutput("vbox_total", width = 3),
          valueBoxOutput("vbox_encours", width = 3),
          valueBoxOutput("vbox_termine", width = 3),
          valueBoxOutput("vbox_avancement", width = 3)
        ),
        
        # Graphiques
        fluidRow(
          box(
            title = tagList(icon("chart-pie"), " Répartition par statut"),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("pie_statut", height = 320)
          ),
          
          box(
            title = tagList(icon("chart-bar"), " Avancement par direction"),
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("bar_direction", height = 320)
          )
        ),
        
        # Tables d'alertes
        fluidRow(
          box(
            title = tagList(icon("exclamation-triangle"), " Activités en retard"),
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            DTOutput("table_retard")
          ),
          
          box(
            title = tagList(icon("clock"), " Échéances proches (30 jours)"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            DTOutput("table_echeances")
          )
        ),
        
        # Top 5 activités par direction
        fluidRow(
          box(
            title = tagList(icon("trophy"), " Top 10 Activités par Avancement"),
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("top_activites_avancement", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("hourglass-half"), " Top 10 Activités embryonnaires par Avancement"),
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("top_activites_embryonnaires", height = 400)
          )
        )
      ),
      
      # ========================================================================
      # ONGLET VUE D'ENSEMBLE
      # ========================================================================
      tabItem(
        tabName = "vue_ensemble",
        
        fluidRow(
          column(
            width = 12,
            h2(icon("eye"), " Vue d'ensemble des activités"),
            hr()
          )
        ),
        
        # Filtres
        fluidRow(
          box(
            title = tagList(icon("filter"), " Filtres"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            
            fluidRow(
              column(3, selectInput("filter_direction", "Direction :",
                                   choices = NULL)),
              column(3, selectInput("filter_service", "Service :",
                                   choices = NULL)),
              column(3, selectInput("filter_statut", "Statut :",
                                   choices = c("Tous", "En cours", "Terminée", 
                                             "En retard", "Non entamée"))),
              column(3, br(), 
                     actionButton("reset_filters_btn", "Réinitialiser",
                                icon = icon("undo"),
                                class = "btn-warning btn-block"))
            )
          )
        ),
        
        # Table des activités
        fluidRow(
          box(
            title = tagList(icon("table"), " Liste complète des activités"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("table_toutes_activites"),
            
            br(),
            
            downloadButton("download_excel_btn", "Télécharger (Excel)",
                          class = "btn-success")
          )
        )
      ),
      
      # ========================================================================
      # ONGLET PERFORMANCE
      # ========================================================================
      tabItem(
        tabName = "performance",
        
        fluidRow(
          column(
            width = 12,
            h2(icon("trophy"), " Analyses de performance"),
            hr()
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("building"), " Performance par service"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("performance_service", height = 400)
          ),
          
          box(
            title = tagList(icon("user-tie"), " Performance par responsable"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("performance_responsable", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("percentage"), " Distribution des avancements"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("histogram_avancement", height = 350)
          )
        )
      ),
      
      # ========================================================================
      # ONGLET PLANIFICATION
      # ========================================================================
      tabItem(
        tabName = "planification",
        
        fluidRow(
          column(
            width = 12,
            h2(icon("calendar"), " Planification et timeline"),
            hr()
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("calendar-alt"), " Chronologie des activités"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("timeline_chart", height = 500)
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("clock"), " Activités du mois en cours"),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            DTOutput("table_mois_courant")
          ),
          
          box(
            title = tagList(icon("forward"), " Activités du mois prochain"),
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("table_mois_prochain")
          )
        )
      ),
      
      # ========================================================================
      # ONGLET DOCUMENTS
      # ========================================================================
      tabItem(
        tabName = "documents",
        
        fluidRow(
          column(
            width = 12,
            h2(icon("folder-open"), " Gestion des documents"),
            hr()
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("download"), " Téléchargement des pièces jointes"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            uiOutput("documents_list_ui")
          )
        )
      ),
      
      # ========================================================================
      # ONGLET RAPPORTS
      # ========================================================================
      tabItem(
        tabName = "rapports",
        
        fluidRow(
          column(
            width = 12,
            h2(icon("file-alt"), " Rapports de synthèse"),
            hr()
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("cog"), " Paramètres du rapport"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(4, selectInput("report_direction", "Direction :",
                                   choices = NULL)),
              column(4, dateRangeInput("report_dates", "Période :",
                                       start = Sys.Date() - 90,
                                       end = Sys.Date(),
                                       language = "fr",
                                       format = "dd/mm/yyyy")),
              column(4, br(),
                     actionButton("generate_report_btn", "Générer le rapport",
                                class = "btn-primary btn-block",
                                icon = icon("play")))
            )
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("table"), " Synthèse statistique"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            uiOutput("report_summary_ui")
          )
        ),
        
        # Graphique d'évolution de l'avancement
        fluidRow(
          box(
            title = tagList(icon("chart-line"), " Évolution de l'avancement"),
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("evolution_avancement_plot", height = 500)
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("list-alt"), " Détail des activités"),
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("report_table"),
            
            br(),
            
            downloadButton("download_report_btn", "Télécharger le rapport (Excel)",
                          class = "btn-primary")
          )
        )
      ),
      
      # ========================================================================
      # ONGLET ADMINISTRATION
      # ========================================================================
      tabItem(
        tabName = "admin",
        
        fluidRow(
          column(
            width = 12,
            h2(icon("cog"), " Administration"),
            hr()
          )
        ),
        
        # Gestion des utilisateurs - Section complète
        fluidRow(
          box(
            title = tagList(icon("users"), " Gestion des utilisateurs"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 12,
                actionButton("add_user_btn", "Ajouter un utilisateur",
                            icon = icon("user-plus"),
                            class = "btn-success"),
                actionButton("refresh_users_btn", "Actualiser",
                            icon = icon("sync"),
                            class = "btn-info"),
                br(), br()
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                DTOutput("table_users")
              )
            )
          )
        ),
        
        # Sauvegarde et statistiques
        fluidRow(
          box(
            title = tagList(icon("database"), " Sauvegarde des données"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            p("Dernière sauvegarde : ", textOutput("last_backup_time", inline = TRUE)),
            
            br(),
            
            actionButton("backup_btn", "Créer une sauvegarde",
                        icon = icon("save"),
                        class = "btn-warning"),
            
            downloadButton("download_backup_btn", "Télécharger sauvegarde",
                          class = "btn-info")
          ),
          
          box(
            title = tagList(icon("chart-line"), " Statistiques globales"),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            uiOutput("admin_stats_ui")
          )
        )
      )
    )
  )
)

# ================================================================================
# LOGIQUE SERVEUR
# ================================================================================

server <- function(input, output, session) {
  
  # Configuration
  options(shiny.maxRequestSize = 50 * 1024^2)
  
  # Augmenter le timeout pour les uploads et éviter les problèmes de fichiers temp
  options(shiny.upload.timeout = 300000)  # 5 minutes au lieu de 30 secondes
  
  nom_fichier_csv <- "data_ins_suivi.csv"
  
  # Créer le dossier attachments s'il n'existe pas
  if(!dir.exists("attachments")) dir.create("attachments")
  
  # Charger la structure organisationnelle
  org_structure <- tryCatch({
    cat("DEBUG: Chargement de Organisation.csv...\n")
    data <- read.csv2("Organisation.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
    cat("DEBUG: Organisation.csv chargé -", nrow(data), "lignes\n")
    cat("DEBUG: Colonnes:", paste(names(data), collapse=", "), "\n")
    cat("DEBUG: Directions uniques:", paste(unique(data$direction), collapse=", "), "\n")
    data
  }, error = function(e) {
    cat("ERREUR: Impossible de charger Organisation.csv:", e$message, "\n")
    cat("SOLUTION: Créer un fichier Organisation.csv avec colonnes: direction;service;bureau\n")
    # Structure par défaut si le fichier n'existe pas
    data.frame(
      direction = character(0),
      service = character(0),
      bureau = character(0),
      stringsAsFactors = FALSE
    )
  })
  
  # Session disconnect handler - nettoyer en cas de déconnexion
  session$onSessionEnded(function() {
    cat("Session terminée - nettoyage...\n")
  })
  
  # --- VARIABLES RÉACTIVES ---
  auth <- reactiveValues(
    logged_in = FALSE,
    user_info = NULL
  )
  
  form_state <- reactiveValues(
    reference = "",
    nom_activite = "",
    type_activite = "Sélectionnez le type",
    date_debut = Sys.Date(),
    date_fin = Sys.Date() + 180,
    observation = "",
    docs_existants = list(),
    editing_activity = NULL
  )
  
  # Base de données
  db <- reactiveVal(NULL)
  
  # Charger les données au démarrage
  observe({
    if(file.exists(nom_fichier_csv)) {
      tryCatch({
        cat("DEBUG: Chargement de", nom_fichier_csv, "\n")
        data <- read.csv2(nom_fichier_csv, stringsAsFactors = FALSE, 
                         encoding = "UTF-8", check.names = FALSE, 
                         na.strings = c("", "NA"))
        
        cat("DEBUG: CSV chargé,", nrow(data), "lignes,", ncol(data), "colonnes\n")
        
        # Nettoyer les colonnes Val_ pour éviter les problèmes de NA
        for(e in etapes_config) {
          val_col <- paste0("Val_", e$id)
          if(val_col %in% names(data)) {
            # Remplacer les NA par "Non"
            data[[val_col]][is.na(data[[val_col]])] <- "Non"
            # S'assurer que c'est soit "Oui" soit "Non"
            data[[val_col]][!data[[val_col]] %in% c("Oui", "Non")] <- "Non"
          }
        }
        
        db(data)
        cat("DEBUG: Données chargées avec succès\n")
      }, error = function(e) {
        # Si erreur de lecture, créer une nouvelle base
        cat("ERREUR chargement CSV:", e$message, "\n")
        db(create_empty_db())
      })
    } else {
      cat("DEBUG: Fichier CSV non trouvé, création d'une base vide\n")
      db(create_empty_db())
    }
  })
  
  # Fonction pour créer une base vide
  create_empty_db <- function() {
    cols <- c("Responsable", "Direction", "Service", "Bureau", "Type_Activite", 
              "Reference", "Activite", "Observation", "Avancement", "Statut", 
              "Date_inscription", "DATE_DEBUT", "DATE_FIN")
    
    for(e in etapes_config) {
      cols <- c(cols, paste0("Val_", e$id))
      cols <- c(cols, paste0("Date_", e$id))
    }
    
    data.frame(matrix(ncol = length(cols), nrow = 0, 
                     dimnames = list(NULL, cols)),
              stringsAsFactors = FALSE)
  }
  
  # --- MASQUER LES MENUS AU DÉMARRAGE ---
  observe({
    shinyjs::hide(selector = "a[data-toggle='tab'][href='#shiny-tab-nouvelle_activite']")
    shinyjs::hide(selector = "a[data-toggle='tab'][href='#shiny-tab-mes_activites']")
    shinyjs::hide(selector = "a[data-toggle='tab'][href='#shiny-tab-dashboard']")
    shinyjs::hide(selector = "a[data-toggle='tab'][href='#shiny-tab-vue_ensemble']")
    shinyjs::hide(selector = "a[data-toggle='tab'][href='#shiny-tab-performance']")
    shinyjs::hide(selector = "a[data-toggle='tab'][href='#shiny-tab-planification']")
    shinyjs::hide(selector = "a[data-toggle='tab'][href='#shiny-tab-documents']")
    shinyjs::hide(selector = "a[data-toggle='tab'][href='#shiny-tab-rapports']")
    shinyjs::hide(selector = "a[data-toggle='tab'][href='#shiny-tab-admin']")
  })
  
  # ================================================================================
  # AUTHENTIFICATION
  # ================================================================================
  
  observeEvent(input$login_btn, {
    # Charger les utilisateurs à jour
    current_users <- load_users()
    
    user_match <- current_users %>%
      filter(user_name == input$username, user_pw == input$password)
    
    if(nrow(user_match) == 1) {
      auth$logged_in <- TRUE
      auth$user_info <- as.list(user_match)
      
      # Afficher les menus selon le rôle
      shinyjs::show(selector = "a[data-toggle='tab'][href='#shiny-tab-nouvelle_activite']")
      shinyjs::show(selector = "a[data-toggle='tab'][href='#shiny-tab-mes_activites']")
      shinyjs::show(selector = "a[data-toggle='tab'][href='#shiny-tab-dashboard']")
      shinyjs::show(selector = "a[data-toggle='tab'][href='#shiny-tab-vue_ensemble']")
      shinyjs::show(selector = "a[data-toggle='tab'][href='#shiny-tab-performance']")
      shinyjs::show(selector = "a[data-toggle='tab'][href='#shiny-tab-planification']")
      shinyjs::show(selector = "a[data-toggle='tab'][href='#shiny-tab-documents']")
      shinyjs::show(selector = "a[data-toggle='tab'][href='#shiny-tab-rapports']")
      
      # Seul le Directeur Général peut accéder à l'administration
      if(auth$user_info$role == "Directeur général") {
        shinyjs::show(selector = "a[data-toggle='tab'][href='#shiny-tab-admin']")
      }
      
      # Afficher info utilisateur
      shinyjs::show("user_info_box")
      
      # Aller au tableau de bord
      updateTabItems(session, "sidebar_menu", "dashboard")
      
      # Message de succès
      output$login_message <- renderUI({
        div(
          class = "alert alert-success",
          icon("check-circle"),
          " Connexion réussie ! Bienvenue ",
          strong(auth$user_info$user_name)
        )
      })
      
      # Mettre à jour les filtres
      update_filters()
      
    } else {
      output$login_message <- renderUI({
        div(
          class = "alert alert-danger",
          icon("times-circle"),
          " Identifiants incorrects. Veuillez réessayer."
        )
      })
    }
  })
  
  # Déconnexion
  observeEvent(input$logout_btn, {
    session$reload()
  })
  
  # Info utilisateur sidebar
  output$user_info_sidebar <- renderText({
    if(!is.null(auth$user_info)) {
      
      paste0(tags$span(style = "color: blue; font-weight: bold;",
                       "Utilisateur : ", auth$user_info$user_name, "\n",
                       #       "Rôle : ", auth$user_info$role, "\n",
                       "Direction : ", auth$user_info$direction, "\n",
                       "Service : ", auth$user_info$service
      ))
    }
  })
  
  # ================================================================================
  # SAISIE - NOUVELLE ACTIVITÉ
  # ================================================================================
  
  # Barre de progression
  output$progress_bar_ui <- renderUI({
    req(auth$logged_in)
    
    etapes_ids <- sapply(etapes_config, function(e) paste0("val_", e$id))
    etapes_vals <- sapply(etapes_ids, function(id) {
      val <- input[[id]]
      if(is.null(val)) return(FALSE)
      return(val)
    })
    
    pct <- round(sum(etapes_vals) / length(etapes_config) * 100)
    
    div(
      class = "progress",
      div(
        class = "progress-bar progress-bar-striped active",
        role = "progressbar",
        style = paste0("width: ", pct, "%;"),
        paste0(pct, "%")
      )
    )
  })
  
  # Durée de l'activité
  output$duree_activite_ui <- renderUI({
    req(input$date_debut, input$date_fin)
    
    debut <- as.Date(input$date_debut)
    fin <- as.Date(input$date_fin)
    duree <- as.numeric(difftime(fin, debut, units = "days"))
    
    div(
      style = "padding-top: 7px;",
      tags$label("Durée :"),
      p(
        style = "font-size: 16px; font-weight: bold; color: #3c8dbc;",
        paste(duree, "jours")
      )
    )
  })
  
  # Formulaire des étapes
  output$etapes_form_ui <- renderUI({
    req(auth$logged_in)
    
    # Debug : confirmer que cette fonction s'exécute
    cat("DEBUG: Génération du formulaire des étapes\n")
    
    lapply(seq_along(etapes_config), function(i) {
      e <- etapes_config[[i]]
      
      # Vérifier si un fichier existe déjà
      doc_key <- paste0("Doc_", e$id)
      existing_file <- form_state$docs_existants[[doc_key]]
      has_file <- !is.null(existing_file) && existing_file != ""
      
      # Déterminer la classe CSS
      is_completed <- isTRUE(input[[paste0("val_", e$id)]])
      css_class <- "step-row"
      if(is_completed) {
        css_class <- paste(css_class, "step-completed")
      }
      
      div(
        id = paste0("row_", e$id),
        class = css_class,
        
        fluidRow(
          column(
            width = 6,
            tags$strong(
              style = "font-size: 14px;",
              paste0(i, ". ", e$label)
            )
          ),
          
          column(
            width = 2,
            # Utiliser checkboxInput pour l'étape 1, switchInput pour les autres
            if(i == 1) {
              # Étape 1 : Checkbox simple et fiable avec indicateur
              # IMPORTANT : Préserver la valeur actuelle
              current_value <- isTRUE(input[[paste0("val_", e$id)]])
              div(
                style = "text-align: center; padding: 10px;",
                checkboxInput(
                  inputId = paste0("val_", e$id),
                  label = NULL,
                  value = current_value,  # Utiliser la valeur actuelle, pas toujours FALSE !
                  width = "100%"
                ),
                uiOutput(paste0("status_", e$id))
              )
            } else {
              # Étapes 2-10 : Switch normal
              # Préserver aussi la valeur pour les switches
              current_value <- isTRUE(input[[paste0("val_", e$id)]])
              switchInput(
                inputId = paste0("val_", e$id),
                value = current_value,  # Utiliser la valeur actuelle
                size = "small",
                onStatus = "success",
                offStatus = "danger"
              )
            }
          ),
          
          column(
            width = 2,
            if(is_completed) {
              dateInput(
                inputId = paste0("date_", e$id),
                label = NULL,
                value = Sys.Date(),
                format = "dd/mm/yyyy"
              )
            }
          ),
          
          column(
            width = 2,
            if(e$doc) {
              tagList(
                fileInput(
                  inputId = paste0("file_", e$id),
                  label = NULL,
                  buttonLabel = "Joindre",
                  accept = c(".pdf", ".doc", ".docx", ".xls", ".xlsx")
                ),
                if(has_file) {
                  span(
                    class = "file-status-badge",
                    icon("check-circle"),
                    " Fichier joint"
                  )
                }
              )
            }
          )
        )
      )
    })
  })
  
  # Indicateur ON/OFF pour l'étape 1 (TDR)
  output$status_TDR <- renderUI({
    if(isTRUE(input$val_TDR)) {
      tags$span(style = "color: #00a65a; font-weight: bold; font-size: 11px;", "✓ ON")
    } else {
      tags$span(style = "color: #dd4b39; font-weight: bold; font-size: 11px;", "✗ OFF")
    }
  })
  
  # Logique séquentielle des étapes - SIMPLIFIÉ
  # Observer uniquement pour les étapes 2 à 10 (pas l'étape 1 !)
  
  # Étape 2 : ANO
  observeEvent(input$val_ANO, {
    if(!is.null(input$val_ANO) && input$val_ANO && !isTRUE(input$val_TDR)) {
      updateSwitchInput(session, "val_ANO", value = FALSE)
      showNotification("Veuillez d'abord compléter l'étape 1 : Termes de référence global", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Étape 3 : ETP
  observeEvent(input$val_ETP, {
    if(!is.null(input$val_ETP) && input$val_ETP && !isTRUE(input$val_ANO)) {
      updateSwitchInput(session, "val_ETP", value = FALSE)
      showNotification("Veuillez d'abord compléter l'étape 2 : Demande d'Avis de Non Objection", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Étape 4 : TP
  observeEvent(input$val_TP, {
    if(!is.null(input$val_TP) && input$val_TP && !isTRUE(input$val_ETP)) {
      updateSwitchInput(session, "val_TP", value = FALSE)
      showNotification("Veuillez d'abord compléter l'étape 3 : Mise en place de l'Équipe Technique", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Étape 5 : FP
  observeEvent(input$val_FP, {
    if(!is.null(input$val_FP) && input$val_FP && !isTRUE(input$val_TP)) {
      updateSwitchInput(session, "val_FP", value = FALSE)
      showNotification("Veuillez d'abord compléter l'étape 4 : Travaux préparatoires", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Étape 6 : CDT
  observeEvent(input$val_CDT, {
    if(!is.null(input$val_CDT) && input$val_CDT && !isTRUE(input$val_FP)) {
      updateSwitchInput(session, "val_CDT", value = FALSE)
      showNotification("Veuillez d'abord compléter l'étape 5 : Formation du personnel", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Étape 7 : TAD
  observeEvent(input$val_TAD, {
    if(!is.null(input$val_TAD) && input$val_TAD && !isTRUE(input$val_CDT)) {
      updateSwitchInput(session, "val_TAD", value = FALSE)
      showNotification("Veuillez d'abord compléter l'étape 6 : Collecte de données sur le terrain", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Étape 8 : ARR
  observeEvent(input$val_ARR, {
    if(!is.null(input$val_ARR) && input$val_ARR && !isTRUE(input$val_TAD)) {
      updateSwitchInput(session, "val_ARR", value = FALSE)
      showNotification("Veuillez d'abord compléter l'étape 7 : Traitement / Apurement des données", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Étape 9 : VR
  observeEvent(input$val_VR, {
    if(!is.null(input$val_VR) && input$val_VR && !isTRUE(input$val_ARR)) {
      updateSwitchInput(session, "val_VR", value = FALSE)
      showNotification("Veuillez d'abord compléter l'étape 8 : Analyse et Rédaction du rapport", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Étape 10 : PML
  observeEvent(input$val_PML, {
    if(!is.null(input$val_PML) && input$val_PML && !isTRUE(input$val_VR)) {
      updateSwitchInput(session, "val_PML", value = FALSE)
      showNotification("Veuillez d'abord compléter l'étape 9 : Validation du rapport", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Gestion visuelle des étapes (griser celles qui sont inaccessibles)
  observe({
    req(auth$logged_in)
    
    # Étape 2 grisée si étape 1 non complétée
    if(!isTRUE(input$val_TDR)) {
      shinyjs::addClass(id = "row_ANO", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_ANO", class = "disabled-step")
    }
    
    # Étape 3 grisée si étape 2 non complétée
    if(!isTRUE(input$val_ANO)) {
      shinyjs::addClass(id = "row_ETP", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_ETP", class = "disabled-step")
    }
    
    # Étape 4 grisée si étape 3 non complétée
    if(!isTRUE(input$val_ETP)) {
      shinyjs::addClass(id = "row_TP", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_TP", class = "disabled-step")
    }
    
    # Étape 5 grisée si étape 4 non complétée
    if(!isTRUE(input$val_TP)) {
      shinyjs::addClass(id = "row_FP", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_FP", class = "disabled-step")
    }
    
    # Étape 6 grisée si étape 5 non complétée
    if(!isTRUE(input$val_FP)) {
      shinyjs::addClass(id = "row_CDT", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_CDT", class = "disabled-step")
    }
    
    # Étape 7 grisée si étape 6 non complétée
    if(!isTRUE(input$val_CDT)) {
      shinyjs::addClass(id = "row_TAD", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_TAD", class = "disabled-step")
    }
    
    # Étape 8 grisée si étape 7 non complétée
    if(!isTRUE(input$val_TAD)) {
      shinyjs::addClass(id = "row_ARR", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_ARR", class = "disabled-step")
    }
    
    # Étape 9 grisée si étape 8 non complétée
    if(!isTRUE(input$val_ARR)) {
      shinyjs::addClass(id = "row_VR", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_VR", class = "disabled-step")
    }
    
    # Étape 10 grisée si étape 9 non complétée
    if(!isTRUE(input$val_VR)) {
      shinyjs::addClass(id = "row_PML", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_PML", class = "disabled-step")
    }
  })
  
  # Réinitialiser le formulaire
  observeEvent(input$new_form_btn, {
    reset_form()
  })
  
  reset_form <- function() {
    form_state$reference <- ""
    form_state$nom_activite <- ""
    form_state$type_activite <- "Sélectionnez le type"
    form_state$date_debut <- Sys.Date()
    form_state$date_fin <- Sys.Date() + 180
    form_state$observation <- ""
    form_state$docs_existants <- list()
    form_state$editing_activity <- NULL
    
    updateTextInput(session, "ref_activite", value = "")
    updateTextInput(session, "nom_activite", value = "")
    updatePickerInput(session, "type_activite", selected = "Sélectionnez le type")
    updateDateInput(session, "date_debut", value = Sys.Date())
    updateDateInput(session, "date_fin", value = Sys.Date() + 180)
    updateTextAreaInput(session, "observation_activite", value = "")
    
    for(e in etapes_config) {
      if(e$id == "TDR") {
        # Étape 1 : checkbox
        updateCheckboxInput(session, paste0("val_", e$id), value = FALSE)
      } else {
        # Étapes 2-10 : switch
        updateSwitchInput(session, paste0("val_", e$id), value = FALSE)
      }
    }
  }
  
  # Enregistrer l'activité
  observeEvent(input$save_data_btn, {
    
    # Validation
    if(is.null(input$nom_activite) || input$nom_activite == "") {
      showModal(modalDialog(
        title = tagList(icon("exclamation-triangle"), " Erreur"),
        "Veuillez saisir un nom d'activité.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    if(input$type_activite == "Sélectionnez le type") {
      showModal(modalDialog(
        title = tagList(icon("exclamation-triangle"), " Erreur"),
        "Veuillez sélectionner un type d'activité.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # Entourer le code d'enregistrement dans un tryCatch
    tryCatch({
      # Calculer l'avancement
      etapes_vals <- sapply(etapes_config, function(e) {
        isTRUE(input[[paste0("val_", e$id)]])
      })
      avancement <- round(sum(etapes_vals) / length(etapes_config) * 100)
      
      # Déterminer le statut
      date_fin <- as.Date(input$date_fin)
      today <- Sys.Date()
      
      if(avancement == 100) {
        statut <- "Terminée"
      } else if(avancement == 0) {
        statut <- "Non entamée"
      } else if(date_fin < today) {
        statut <- "En retard"
      } else {
        statut <- "En cours"
      }
      
      # Créer la nouvelle ligne
      new_data <- list(
        Responsable = auth$user_info$user_name,
        Direction = auth$user_info$direction,
        Service = auth$user_info$service,
        Bureau = auth$user_info$bureau,
        Type_Activite = input$type_activite,
        Reference = input$ref_activite,
        Activite = input$nom_activite,
        Observation = input$observation_activite,
        Avancement = avancement,
        Statut = statut,
        Date_inscription = format(Sys.time(), "%d/%m/%Y %H:%M"),
        DATE_DEBUT = format(as.Date(input$date_debut), "%Y-%m-%d"),
        DATE_FIN = format(as.Date(input$date_fin), "%Y-%m-%d")
      )
      
      # Ajouter les étapes et documents
      for(e in etapes_config) {
        val_id <- paste0("val_", e$id)
        new_data[[paste0("Val_", e$id)]] <- if(isTRUE(input[[val_id]])) "Oui" else "Non"
        
        date_id <- paste0("date_", e$id)
        if(isTRUE(input[[val_id]]) && !is.null(input[[date_id]])) {
          new_data[[paste0("Date_", e$id)]] <- format(as.Date(input[[date_id]]), "%Y-%m-%d")
        } else {
          new_data[[paste0("Date_", e$id)]] <- ""
        }
      }
      
      # Gérer les fichiers joints
      for(e in etapes_config) {
        if(e$doc) {
          file_input <- input[[paste0("file_", e$id)]]
          
          if(!is.null(file_input)) {
            # Nouveau fichier uploadé
            # Vérifier que le fichier temporaire existe encore
            if(file.exists(file_input$datapath)) {
              file_path <- file.path(
                "attachments",
                paste0(
                  gsub("[^[:alnum:]_-]", "_", input$nom_activite),
                  "_", e$id, "_",
                  file_input$name
                )
              )
              
              # Copier avec gestion d'erreur
              tryCatch({
                file.copy(file_input$datapath, file_path, overwrite = TRUE)
                new_data[[paste0("Doc_", e$id)]] <- file_path
              }, error = function(e) {
                # En cas d'erreur, logger et continuer
                cat("ERREUR copie fichier:", e$message, "\n")
                new_data[[paste0("Doc_", e$id)]] <- ""
              })
            } else {
              # Le fichier temporaire n'existe plus
              cat("AVERTISSEMENT: Fichier temporaire introuvable pour", e$id, "\n")
              new_data[[paste0("Doc_", e$id)]] <- ""
            }
          } else {
            # Conserver le fichier existant
            existing <- form_state$docs_existants[[paste0("Doc_", e$id)]]
            new_data[[paste0("Doc_", e$id)]] <- if(!is.null(existing)) existing else ""
          }
        }
      }
      
      # Convertir en data frame
      new_row <- as.data.frame(new_data, stringsAsFactors = FALSE)
      
      # Ajouter à la base
      current_db <- db()
      
      # Vérifier si current_db est NULL ou vide
      if(is.null(current_db) || nrow(current_db) == 0) {
        # Créer une nouvelle base avec juste cette ligne
        updated_db <- new_row
      } else {
        # Supprimer l'ancienne ligne si on modifie
        if(!is.null(form_state$editing_activity)) {
          current_db <- current_db %>%
            filter(Activite != form_state$editing_activity)
        } else {
          # Sinon, vérifier les doublons
          current_db <- current_db %>%
            filter(Activite != input$nom_activite)
        }
        
        # Ajouter la nouvelle ligne
        updated_db <- bind_rows(current_db, new_row)
      }
      
      db(updated_db)
      
      # Sauvegarder
      write.csv2(updated_db, file = nom_fichier_csv, row.names = FALSE)
      
      # Message de succès
      showModal(modalDialog(
        title = tagList(icon("check-circle"), " Succès"),
        "L'activité a été enregistrée avec succès !",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
      # Réinitialiser
      reset_form()
      
    }, error = function(e) {
      # En cas d'erreur, afficher un message détaillé
      showModal(modalDialog(
        title = tagList(icon("times-circle"), " Erreur d'enregistrement"),
        paste0("Une erreur s'est produite lors de l'enregistrement : ", e$message),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
      # Logger l'erreur dans la console
      cat("ERREUR D'ENREGISTREMENT:\n")
      print(e)
    })
  })
  
  # ================================================================================
  # MES ACTIVITÉS
  # ================================================================================
  
  output$table_mes_activites <- renderDT({
    req(auth$logged_in, db())
    
    mes_activites <- db() %>%
      filter(Responsable == auth$user_info$user_name) %>%
      select(Reference, Activite, Type_Activite, DATE_DEBUT, DATE_FIN, 
             Avancement, Statut, Date_inscription) %>%
      mutate(
        Actions = paste0(
          '<button class="btn btn-sm btn-info" onclick="Shiny.setInputValue(\'edit_activity\', \'', 
          Activite, '\', {priority: \'event\'})"><i class="fa fa-edit"></i> Modifier</button>'
        )
      )
    
    datatable(
      mes_activites,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      colnames = c("Réf", "Activité", "Type", "Début", "Fin", 
                  "Avancement", "Statut", "Date inscription", "Actions")
    ) %>%
      formatStyle(
        'Avancement',
        background = styleColorBar(c(0, 100), '#28a745'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Statut',
        backgroundColor = styleEqual(
          c("En cours", "Terminée", "En retard", "Non entamée"),
          c("#f39c12", "#00a65a", "#dd4b39", "#d2d6de")
        ),
        color = 'white',
        fontWeight = 'bold'
      )
  })
  
  # Édition d'une activité
  observeEvent(input$edit_activity, {
    req(db())
    
    tryCatch({
      cat("DEBUG: Tentative d'édition de l'activité:", input$edit_activity, "\n")
      
      activity_data <- db() %>%
        filter(Activite == input$edit_activity)
      
      if(nrow(activity_data) > 0) {
        act <- activity_data[1, ]
        
        cat("DEBUG: Activité trouvée, chargement...\n")
        cat("DEBUG: Colonnes disponibles:", paste(names(act), collapse=", "), "\n")
        
        # Charger les données dans le formulaire
        form_state$editing_activity <- act$Activite
        
        updateTextInput(session, "ref_activite", value = act$Reference)
        updateTextInput(session, "nom_activite", value = act$Activite)
        updatePickerInput(session, "type_activite", selected = act$Type_Activite)
        
        # Conversion sécurisée des dates
        date_debut <- tryCatch(as.Date(act$DATE_DEBUT), error = function(e) Sys.Date())
        date_fin <- tryCatch(as.Date(act$DATE_FIN), error = function(e) Sys.Date() + 180)
        
        updateDateInput(session, "date_debut", value = date_debut)
        updateDateInput(session, "date_fin", value = date_fin)
        
        # Observation (avec gestion des NA)
        obs_value <- if(!is.na(act$Observation)) act$Observation else ""
        updateTextAreaInput(session, "observation_activite", value = obs_value)
        
        # Charger les étapes
        temp_docs <- list()
        for(e in etapes_config) {
          val_col <- paste0("Val_", e$id)
          if(val_col %in% names(act)) {
            # Conversion robuste en booléen
            val_text <- as.character(act[[val_col]])
            val_bool <- !is.na(val_text) && val_text == "Oui"
            
            # Utiliser le bon type d'update selon l'étape
            if(e$id == "TDR") {
              # Étape 1 : checkbox
              updateCheckboxInput(
                session, 
                paste0("val_", e$id), 
                value = val_bool
              )
            } else {
              # Étapes 2-10 : switch
              updateSwitchInput(
                session, 
                paste0("val_", e$id), 
                value = val_bool
              )
            }
          }
          
          if(e$doc) {
            doc_col <- paste0("Doc_", e$id)
            if(doc_col %in% names(act) && !is.na(act[[doc_col]]) && act[[doc_col]] != "") {
              temp_docs[[doc_col]] <- act[[doc_col]]
            }
          }
        }
        
        form_state$docs_existants <- temp_docs
        
        # Naviguer vers le formulaire
        updateTabItems(session, "sidebar_menu", "nouvelle_activite")
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = tagList(icon("times-circle"), " Erreur de chargement"),
        paste0("Impossible de charger l'activité pour modification : ", e$message),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      cat("ERREUR édition activité:", e$message, "\n")
      print(e)
    })
  })
  
  # Actualiser la table
  observeEvent(input$refresh_table_btn, {
    # Force re-render
    output$table_mes_activites <- renderDT({
      req(auth$logged_in, db())
      
      mes_activites <- db() %>%
        filter(Responsable == auth$user_info$user_name) %>%
        select(Reference, Activite, Type_Activite, DATE_DEBUT, DATE_FIN, 
               Avancement, Statut, Date_inscription) %>%
        mutate(
          Actions = paste0(
            '<button class="btn btn-sm btn-info" onclick="Shiny.setInputValue(\'edit_activity\', \'', 
            Activite, '\', {priority: \'event\'})"><i class="fa fa-edit"></i> Modifier</button>'
          )
        )
      
      datatable(
        mes_activites,
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
          )
        ),
        colnames = c("Réf", "Activité", "Type", "Début", "Fin", 
                    "Avancement", "Statut", "Date inscription", "Actions")
      ) %>%
        formatStyle(
          'Avancement',
          background = styleColorBar(c(0, 100), '#28a745'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Statut',
          backgroundColor = styleEqual(
            c("En cours", "Terminée", "En retard", "Non entamée"),
            c("#f39c12", "#00a65a", "#dd4b39", "#d2d6de")
          ),
          color = 'white',
          fontWeight = 'bold'
        )
    })
  })
  
  # ================================================================================
  # TABLEAU DE BORD
  # ================================================================================
  
  # Fonction pour filtrer les données selon le rôle
  get_filtered_data <- reactive({
    req(auth$logged_in, db())
    
    data <- db()
    
    if(auth$user_info$role == "RESPONSABLE") {
      data <- data %>%
        filter(Responsable == auth$user_info$user_name)
    } else if(auth$user_info$role == "Direction_Centrale") {
      data <- data %>%
        filter(Direction == auth$user_info$direction)
    }
    # Le DG voit tout
    
    return(data)
  })
  
  # Fonction pour calculer les statistiques
  calculate_stats <- function(data) {
    list(
      total = nrow(data),
      en_cours = sum(data$Statut == "En cours", na.rm = TRUE),
      termine = sum(data$Statut == "Terminée", na.rm = TRUE),
      en_retard = sum(data$Statut == "En retard", na.rm = TRUE),
      non_entamee = sum(data$Statut == "Non entamée", na.rm = TRUE),
      avg_avancement = mean(as.numeric(data$Avancement), na.rm = TRUE)
    )
  }
  
  # Value boxes
  output$vbox_total <- renderValueBox({
    data <- get_filtered_data()
    stats <- calculate_stats(data)
    
    valueBox(
      stats$total,
      "Activités totales",
      icon = icon("tasks"),
      color = "primary"
    )
  })
  
  output$vbox_encours <- renderValueBox({
    data <- get_filtered_data()
    stats <- calculate_stats(data)
    
    valueBox(
      stats$en_cours,
      "En cours",
      icon = icon("spinner"),
      color = "warning"
    )
  })
  
  output$vbox_termine <- renderValueBox({
    data <- get_filtered_data()
    stats <- calculate_stats(data)
    
    valueBox(
      stats$termine,
      "Terminées",
      icon = icon("check-circle"),
      color = "success"
    )
  })
  
  output$vbox_avancement <- renderValueBox({
    data <- get_filtered_data()
    stats <- calculate_stats(data)
    
    valueBox(
      paste0(round(stats$avg_avancement, 1), "%"),
      "Avancement moyen",
      icon = icon("chart-line"),
      color = "info"
    )
  })
  
  # Graphique pie - Répartition par statut
  output$pie_statut <- renderPlotly({
    data <- get_filtered_data()
    
    data_pie <- data %>%
      count(Statut) %>%
      arrange(desc(n))
    
    colors <- c(
      "En cours" = "#f39c12",
      "Terminée" = "#00a65a",
      "En retard" = "#dd4b39",
      "Non entamée" = "#d2d6de"
    )
    
    plot_ly(
      data_pie,
      labels = ~Statut,
      values = ~n,
      type = "pie",
      marker = list(colors = colors[data_pie$Statut]),
      textposition = 'inside',
      textinfo = 'label+percent'
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # Graphique bar - Avancement par direction
  output$bar_direction <- renderPlotly({
    data <- get_filtered_data()
    
    data_bar <- data %>%
      group_by(Direction) %>%
      summarise(
        Avancement = mean(as.numeric(Avancement), na.rm = TRUE),
        Nombre = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Avancement))
    
    plot_ly(
      data_bar,
      x = ~Direction,
      y = ~Avancement,
      type = "bar",
      text = ~paste0(round(Avancement, 1), "% (", Nombre, " act.)"),
      textposition = "outside",
      marker = list(color = "#3c8dbc")
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Avancement moyen (%)")
      )
  })
  
  # Table - Activités en retard
  output$table_retard <- renderDT({
    data <- get_filtered_data()
    
    data_retard <- data %>%
      filter(Statut == "En retard") %>%
      select(Reference, Activite, Responsable, Avancement, DATE_FIN) %>%
      head(10)
    
    if(nrow(data_retard) == 0) {
      data_retard <- data.frame(
        Message = "Aucune activité en retard"
      )
    }
    
    datatable(
      data_retard,
      options = list(
        pageLength = 5,
        dom = 't',
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      rownames = FALSE
    )
  })
  
  # Table - Échéances proches
  output$table_echeances <- renderDT({
    data <- get_filtered_data()
    
    today <- Sys.Date()
    
    data_echeances <- data %>%
      mutate(DATE_FIN = as.Date(DATE_FIN)) %>%
      filter(DATE_FIN >= today, DATE_FIN <= today + 30) %>%
      mutate(Jours_restants = as.numeric(DATE_FIN - today)) %>%
      arrange(Jours_restants) %>%
      select(Reference, Activite, Responsable, DATE_FIN, Jours_restants) %>%
      head(10)
    
    if(nrow(data_echeances) == 0) {
      data_echeances <- data.frame(
        Message = "Aucune échéance dans les 30 prochains jours"
      )
    }
    
    datatable(
      data_echeances,
      options = list(
        pageLength = 5,
        dom = 't',
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      rownames = FALSE
    )
  })
  
  # Top 10 Activités par Avancement
  output$top_activites_avancement <- renderPlotly({
    data <- get_filtered_data()
    
    # Sélectionner les 10 activités les plus avancées
    top_data <- data %>%
      arrange(desc(Avancement)) %>%
      head(10) %>%
      mutate(
        Label = paste0(substr(Activite, 1, 40), 
                      ifelse(nchar(Activite) > 40, "...", "")),
        Couleur = case_when(
          Avancement >= 80 ~ "#016B24",  # Vert 
          Avancement >= 50 ~ "#00a65a",  # Bleu
          Avancement >= 30 ~ "#03F754",  # Orange
          TRUE ~ "#63FD96"               # Rouge
          # Avancement >= 80 ~ "#00a65a",  # Vert #016B24
          # Avancement >= 50 ~ "#3c8dbc",  # Bleu
          # Avancement >= 30 ~ "#f39c12",  # Orange
          # TRUE ~ "#dd4b39"               # Rouge
        )
      ) %>%
      arrange(Avancement)  # Inverser pour affichage
    
    plot_ly(
      top_data,
      y = ~Label,
      x = ~Avancement,
      type = "bar",
      orientation = 'h',
      text = ~paste0("Direction: ", Direction, "<br>",
                    "Avancement: ", Avancement, " %"),
      hoverinfo = "text",
      marker = list(color = ~Couleur),
      textposition = "outside",
      texttemplate = "%{x}%"
    ) %>%
      layout(
        xaxis = list(title = "Avancement (%)", range = c(0, 105)),
        yaxis = list(title = ""),
        margin = list(l = 250),
        showlegend = FALSE
      )
  })
  
  # Top 10 Activités embryonnaires par Avancement
  output$top_activites_embryonnaires <- renderPlotly({
    data <- get_filtered_data()
    
    # Sélectionner les 10 activités embryonnaires (faible avancement)
    bottom_data <- data %>%
      filter(Avancement < 50) %>%  # Embryonnaires = moins de 50%
      arrange(Avancement) %>%
      head(10) %>%
      mutate(
        Label = paste0(substr(Activite, 1, 40), 
                      ifelse(nchar(Activite) > 40, "...", "")),
        Couleur = case_when(
          Avancement >= 30 ~ "#FF8B8B",  # Orange
          Avancement >= 15 ~ "#FF1919",  # Orange foncé
          TRUE ~ "#B80000"               # Rouge
          # Avancement >= 30 ~ "#f39c12",  # Orange
          # Avancement >= 15 ~ "#ff851b",  # Orange foncé
          # TRUE ~ "#dd4b39"               # Rouge
        )
      ) %>%
      arrange(desc(Avancement))  # Inverser pour affichage (du plus haut au plus bas)
    
    if(nrow(bottom_data) == 0) {
      # Si aucune activité embryonnaire, afficher un message
      plot_ly() %>%
        layout(
          annotations = list(
            list(
              text = "Aucune activité embryonnaire (toutes > 50%)",
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5,
              showarrow = FALSE,
              font = list(size = 16)
            )
          ),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    } else {
      plot_ly(
        bottom_data,
        y = ~Label,
        x = ~Avancement,
        type = "bar",
        orientation = 'h',
        text = ~paste0("Direction: ", Direction, "<br>",
                      "Avancement: ", Avancement, " %"),
        hoverinfo = "text",
        marker = list(color = ~Couleur),
        textposition = "outside",
        texttemplate = "%{x}%"
      ) %>%
        layout(
          xaxis = list(title = "Avancement (%)", range = c(0, 55)),
          yaxis = list(title = ""),
          margin = list(l = 250),
          showlegend = FALSE
        )
    }
  })
  
  # ================================================================================
  # VUE D'ENSEMBLE
  # ================================================================================
  
  # Mise à jour des filtres
  update_filters <- function() {
    req(db())
    
    data <- get_filtered_data()
    
    directions <- c("Toutes", unique(data$Direction))
    services <- c("Tous", unique(data$Service))
    
    updateSelectInput(session, "filter_direction", choices = directions)
    updateSelectInput(session, "filter_service", choices = services)
    updateSelectInput(session, "report_direction", choices = directions)
  }
  
  # Réinitialiser les filtres
  observeEvent(input$reset_filters_btn, {
    updateSelectInput(session, "filter_direction", selected = "Toutes")
    updateSelectInput(session, "filter_service", selected = "Tous")
    updateSelectInput(session, "filter_statut", selected = "Tous")
  })
  
  # Données filtrées pour affichage
  get_display_data <- reactive({
    data <- get_filtered_data()
    
    if(!is.null(input$filter_direction) && input$filter_direction != "Toutes") {
      data <- data %>%
        filter(Direction == input$filter_direction)
    }
    
    if(!is.null(input$filter_service) && input$filter_service != "Tous") {
      data <- data %>%
        filter(Service == input$filter_service)
    }
    
    if(!is.null(input$filter_statut) && input$filter_statut != "Tous") {
      data <- data %>%
        filter(Statut == input$filter_statut)
    }
    
    return(data)
  })
  
  # Table toutes activités
  output$table_toutes_activites <- renderDT({
    data <- get_display_data()
    
    data_display <- data %>%
      select(Reference, Activite, Type_Activite, Direction, Service, 
             Responsable, DATE_DEBUT, DATE_FIN, Statut, Avancement)
    
    datatable(
      data_display,
      filter = 'top',
      extensions = 'Buttons',
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'excel', 'pdf'),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      rownames = FALSE,
      colnames = c("Réf", "Activité", "Type", "Direction", "Service",
                  "Responsable", "Début", "Fin", "Statut", "Avancement")
    ) %>%
      formatStyle(
        'Statut',
        backgroundColor = styleEqual(
          c("En cours", "Terminée", "En retard", "Non entamée"),
          c("#f39c12", "#00a65a", "#dd4b39", "#d2d6de")
        ),
        color = 'white',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Avancement',
        background = styleColorBar(c(0, 100), '#3c8dbc'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Téléchargement Excel
  output$download_excel_btn <- downloadHandler(
    filename = function() {
      paste0("activites_ins_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv2(get_display_data(), file, row.names = FALSE)
    }
  )
  
  # ================================================================================
  # PERFORMANCE
  # ================================================================================
  
  # Performance par service
  output$performance_service <- renderPlotly({
    data <- get_filtered_data()
    
    data_service <- data %>%
      group_by(Service) %>%
      summarise(
        Avancement_Moyen = mean(as.numeric(Avancement), na.rm = TRUE),
        Nb_Activites = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Avancement_Moyen)) %>%
      head(15)
    
    plot_ly(
      data_service,
      x = ~Avancement_Moyen,
      y = ~reorder(Service, Avancement_Moyen),
      type = "bar",
      orientation = 'h',
      text = ~paste0(round(Avancement_Moyen, 1), "% (", Nb_Activites, " act.)"),
      textposition = "outside",
      marker = list(color = "#3c8dbc")
    ) %>%
      layout(
        xaxis = list(title = "Avancement moyen (%)"),
        yaxis = list(title = ""),
        margin = list(l = 200)
      )
  })
  
  # Performance par responsable
  output$performance_responsable <- renderPlotly({
    data <- get_filtered_data()
    
    data_resp <- data %>%
      group_by(Responsable) %>%
      summarise(
        Avancement_Moyen = mean(as.numeric(Avancement), na.rm = TRUE),
        Nb_Activites = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Avancement_Moyen)) %>%
      head(15)
    
    plot_ly(
      data_resp,
      x = ~Avancement_Moyen,
      y = ~reorder(Responsable, Avancement_Moyen),
      type = "bar",
      orientation = 'h',
      text = ~paste0(round(Avancement_Moyen, 1), "% (", Nb_Activites, " act.)"),
      textposition = "outside",
      marker = list(color = "#3c8dbc")
    ) %>%
      layout(
        xaxis = list(title = "Avancement moyen (%)"),
        yaxis = list(title = "")
      )
  })
  
  # Histogramme des avancements
  output$histogram_avancement <- renderPlotly({
    data <- get_filtered_data()
    
    plot_ly(
      data,
      x = ~as.numeric(Avancement),
      type = "histogram",
      nbinsx = 20,
      marker = list(
        color = "#3c8dbc",
        line = list(color = "#fff", width = 1)
      )
    ) %>%
      layout(
        xaxis = list(title = "Avancement (%)"),
        yaxis = list(title = "Nombre d'activités"),
        bargap = 0.1
      )
  })
  
  # ================================================================================
  # PLANIFICATION
  # ================================================================================
  
  # Timeline
  output$timeline_chart <- renderPlotly({
    data <- get_filtered_data()
    
    data_timeline <- data %>%
      mutate(
        DATE_DEBUT = as.Date(DATE_DEBUT),
        DATE_FIN = as.Date(DATE_FIN)
      ) %>%
      filter(!is.na(DATE_DEBUT), !is.na(DATE_FIN)) %>%
      arrange(DATE_DEBUT) %>%
      head(30)
    
    if(nrow(data_timeline) == 0) {
      return(plotly_empty())
    }
    
    # Créer le graphique Gantt
    fig <- plot_ly()
    
    for(i in 1:nrow(data_timeline)) {
      act <- data_timeline[i, ]
      
      fig <- fig %>%
        add_trace(
          x = c(act$DATE_DEBUT, act$DATE_FIN),
          y = c(i, i),
          type = "scatter",
          mode = "lines",
          line = list(width = 20, color = "#3c8dbc"),
          text = paste0(
            act$Reference, " - ", act$Activite, "<br>",
            "Début: ", format(act$DATE_DEBUT, "%d/%m/%Y"), "<br>",
            "Fin: ", format(act$DATE_FIN, "%d/%m/%Y"), "<br>",
            "Avancement: ", act$Avancement, "%"
          ),
          hoverinfo = "text",
          showlegend = FALSE
        )
    }
    
    fig %>%
      layout(
        xaxis = list(title = "Période"),
        yaxis = list(
          title = "",
          ticktext = data_timeline$Reference,
          tickvals = 1:nrow(data_timeline),
          tickmode = "array"
        ),
        margin = list(l = 100)
      )
  })
  
  # Activités du mois courant
  output$table_mois_courant <- renderDT({
    data <- get_filtered_data()
    
    today <- Sys.Date()
    debut_mois <- floor_date(today, "month")
    fin_mois <- ceiling_date(today, "month") - days(1)
    
    data_mois <- data %>%
      mutate(
        DATE_DEBUT = as.Date(DATE_DEBUT),
        DATE_FIN = as.Date(DATE_FIN)
      ) %>%
      filter(
        (DATE_DEBUT >= debut_mois & DATE_DEBUT <= fin_mois) |
        (DATE_FIN >= debut_mois & DATE_FIN <= fin_mois) |
        (DATE_DEBUT <= debut_mois & DATE_FIN >= fin_mois)
      ) %>%
      select(Reference, Activite, Responsable, Avancement, Statut)
    
    if(nrow(data_mois) == 0) {
      data_mois <- data.frame(Message = "Aucune activité ce mois-ci")
    }
    
    datatable(
      data_mois,
      options = list(
        pageLength = 10,
        dom = 't',
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      rownames = FALSE
    )
  })
  
  # Activités du mois prochain
  output$table_mois_prochain <- renderDT({
    data <- get_filtered_data()
    
    prochain_mois <- floor_date(Sys.Date() %m+% months(1), "month")
    fin_prochain <- ceiling_date(prochain_mois, "month") - days(1)
    
    data_mois <- data %>%
      mutate(
        DATE_DEBUT = as.Date(DATE_DEBUT),
        DATE_FIN = as.Date(DATE_FIN)
      ) %>%
      filter(
        (DATE_DEBUT >= prochain_mois & DATE_DEBUT <= fin_prochain) |
        (DATE_FIN >= prochain_mois & DATE_FIN <= fin_prochain) |
        (DATE_DEBUT <= prochain_mois & DATE_FIN >= fin_prochain)
      ) %>%
      select(Reference, Activite, Responsable, DATE_DEBUT, DATE_FIN)
    
    if(nrow(data_mois) == 0) {
      data_mois <- data.frame(Message = "Aucune activité prévue le mois prochain")
    }
    
    datatable(
      data_mois,
      options = list(
        pageLength = 10,
        dom = 't',
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      rownames = FALSE,
      colnames = c("Réf", "Activité", "Responsable", "Début", "Fin")
    )
  })
  
  # ================================================================================
  # DOCUMENTS
  # ================================================================================
  
  output$documents_list_ui <- renderUI({
    req(auth$logged_in, db())
    
    # Filtrer selon le rôle
    data <- get_filtered_data()
    
    # Extraire tous les documents
    doc_cols <- grep("^Doc_", names(data), value = TRUE)
    
    all_files <- data %>%
      select(Activite, Reference, all_of(doc_cols)) %>%
      pivot_longer(
        cols = all_of(doc_cols),
        names_to = "Etape",
        values_to = "Fichier"
      ) %>%
      filter(Fichier != "", !is.na(Fichier))
    
    if(nrow(all_files) == 0) {
      return(
        div(
          class = "alert alert-info",
          icon("info-circle"),
          " Aucun document disponible."
        )
      )
    }
    
    # Créer la liste des téléchargements
    tagList(
      lapply(1:nrow(all_files), function(i) {
        file_info <- all_files[i, ]
        file_path <- file_info$Fichier
        
        if(file.exists(file_path)) {
          etape_label <- gsub("Doc_", "", file_info$Etape)
          
          output_id <- paste0("download_doc_", i)
          
          output[[output_id]] <- downloadHandler(
            filename = function() basename(file_path),
            content = function(f) file.copy(file_path, f)
          )
          
          div(
            style = "margin-bottom: 10px; padding: 10px; background: #f9f9f9; border-radius: 5px;",
            fluidRow(
              column(
                width = 8,
                p(
                  style = "margin: 0;",
                  strong(file_info$Reference), " - ", file_info$Activite, br(),
                  tags$small(
                    class = "text-muted",
                    "Étape : ", etape_label
                  )
                )
              ),
              column(
                width = 4,
                div(
                  style = "text-align: right;",
                  downloadButton(
                    output_id,
                    basename(file_path),
                    class = "btn-sm btn-info"
                  )
                )
              )
            )
          )
        }
      })
    )
  })
  
  # ================================================================================
  # RAPPORTS
  # ================================================================================
  
  # Données du rapport
  rapport_data <- eventReactive(input$generate_report_btn, {
    data <- get_filtered_data()
    
    if(!is.null(input$report_direction) && input$report_direction != "Toutes") {
      data <- data %>%
        filter(Direction == input$report_direction)
    }
    
    if(!is.null(input$report_dates)) {
      data <- data %>%
        mutate(
          DATE_DEBUT = as.Date(DATE_DEBUT),
          DATE_FIN = as.Date(DATE_FIN)
        ) %>%
        filter(
          DATE_DEBUT >= input$report_dates[1] |
          DATE_FIN <= input$report_dates[2]
        )
    }
    
    return(data)
  })
  
  # Synthèse du rapport
  output$report_summary_ui <- renderUI({
    req(rapport_data())
    
    stats <- calculate_stats(rapport_data())
    
    fluidRow(
      column(
        3,
        div(
          class = "small-box bg-aqua",
          div(
            class = "inner",
            h3(stats$total),
            p("Total activités")
          ),
          div(class = "icon", icon("tasks"))
        )
      ),
      column(
        3,
        div(
          class = "small-box bg-green",
          div(
            class = "inner",
            h3(stats$termine),
            p("Terminées")
          ),
          div(class = "icon", icon("check-circle"))
        )
      ),
      column(
        3,
        div(
          class = "small-box bg-yellow",
          div(
            class = "inner",
            h3(stats$en_cours),
            p("En cours")
          ),
          div(class = "icon", icon("spinner"))
        )
      ),
      column(
        3,
        div(
          class = "small-box bg-red",
          div(
            class = "inner",
            h3(stats$en_retard),
            p("En retard")
          ),
          div(class = "icon", icon("exclamation-triangle"))
        )
      )
    )
  })
  
  # Table du rapport
  output$report_table <- renderDT({
    req(rapport_data())
    
    data_report <- rapport_data() %>%
      select(Reference, Activite, Type_Activite, Direction, Service,
             Responsable, DATE_DEBUT, DATE_FIN, Statut, Avancement, Observation)
    
    datatable(
      data_report,
      extensions = 'Buttons',
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'excel', 'pdf', 'print'),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Statut',
        backgroundColor = styleEqual(
          c("En cours", "Terminée", "En retard", "Non entamée"),
          c("#f39c12", "#00a65a", "#dd4b39", "#d2d6de")
        ),
        color = 'white',
        fontWeight = 'bold'
      )
  })
  
  # Graphique d'évolution de l'avancement
  output$evolution_avancement_plot <- renderPlotly({
    req(rapport_data())
    
    data_plot <- rapport_data() %>%
      mutate(
        DATE_DEBUT = as.Date(DATE_DEBUT),
        # Extraire les initiales du service (2-3 premières lettres)
        Service_Code = substr(Service, 1, 2)
      ) %>%
      filter(!is.na(DATE_DEBUT), !is.na(Avancement))
    
    # Créer une palette de couleurs pour les services
    services_uniques <- unique(data_plot$Service_Code)
    nb_services <- length(services_uniques)
    
    # Palette de couleurs variées
    couleurs <- c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6", 
                  "#1abc9c", "#e67e22", "#34495e", "#95a5a6", "#d35400",
                  "#c0392b", "#2980b9", "#27ae60", "#f1c40f", "#8e44ad")
    
    # Assigner des couleurs aux services
    palette_services <- setNames(
      rep(couleurs, length.out = nb_services),
      services_uniques
    )
    
    plot_ly(
      data_plot,
      x = ~DATE_DEBUT,
      y = ~Avancement,
      type = "scatter",
      mode = "markers",
      color = ~Service_Code,
      colors = palette_services,
      text = ~paste0(
        "<b>", Activite, "</b><br>",
        "Service: ", Service, "<br>",
        "Direction: ", Direction, "<br>",
        "Avancement: ", Avancement, " %<br>",
        "Début: ", format(DATE_DEBUT, "%d/%m/%Y"), "<br>",
        "Responsable: ", Responsable
      ),
      hoverinfo = "text",
      marker = list(
        size = 12,
        line = list(color = 'white', width = 1),
        opacity = 0.8
      )
    ) %>%
      layout(
        xaxis = list(
          title = "Date de début",
          type = "date",
          tickformat = "%b %Y",
          gridcolor = "#e0e0e0"
        ),
        yaxis = list(
          title = "Avancement (%)",
          range = c(0, 105),
          gridcolor = "#e0e0e0"
        ),
        legend = list(
          title = list(text = "<b>Service</b>"),
          orientation = "v",
          x = 1.02,
          y = 1,
          bgcolor = "rgba(255,255,255,0.9)",
          bordercolor = "#d0d0d0",
          borderwidth = 1
        ),
        hovermode = "closest",
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        margin = list(r = 150)
      )
  })
  
  # Téléchargement du rapport
  output$download_report_btn <- downloadHandler(
    filename = function() {
      direction <- if(input$report_direction == "Toutes") "toutes" else input$report_direction
      paste0("rapport_", gsub(" ", "_", direction), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv2(rapport_data(), file, row.names = FALSE)
    }
  )
  
  # ================================================================================
  # ADMINISTRATION
  # ================================================================================
  
  # Variable réactive pour les utilisateurs
  users <- reactiveVal(load_users())
  
  # Table des utilisateurs
  output$table_users <- renderDT({
    req(auth$logged_in)
    # Seulement le Directeur Général peut gérer les utilisateurs
    req(auth$user_info$role == "Directeur général")
    
    users_display <- users() %>%
      select(user_name, role, direction, service, bureau) %>%
      mutate(
        Actions = sapply(user_name, function(id) {
          paste0(
            '<button class="btn btn-xs btn-info" onclick="Shiny.setInputValue(\'edit_user\', \'', 
            id, '\', {priority: \'event\'})"><i class="fa fa-edit"></i> Modifier</button> ',
            '<button class="btn btn-xs btn-danger" onclick="Shiny.setInputValue(\'delete_user\', \'', 
            id, '\', {priority: \'event\'})"><i class="fa fa-trash"></i> Supprimer</button>'
          )
        })
      )
    
    datatable(
      users_display,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      colnames = c("Utilisateur", "Rôle", "Direction", "Service", "Bureau", "Actions")
    )
  })
  
  # Actualiser la table
  observeEvent(input$refresh_users_btn, {
    users(load_users())
  })
  
  # Ajouter un utilisateur
  observeEvent(input$add_user_btn, {
    tryCatch({
      cat("DEBUG: Bouton 'Ajouter utilisateur' cliqué\n")
      
      # Vérifier que la structure org est chargée
      if(nrow(org_structure) == 0) {
        cat("DEBUG: org_structure est vide\n")
        showModal(modalDialog(
          title = tagList(icon("exclamation-triangle"), " Erreur"),
          "Le fichier Organisation.csv n'est pas chargé ou est vide. Impossible d'ajouter un utilisateur.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
      
      cat("DEBUG: Préparation du formulaire d'ajout\n")
      
      # Préparer les listes complètes
      directions_list <- sort(unique(org_structure$direction))
      all_services <- sort(unique(org_structure$service))
      all_bureaux <- sort(unique(org_structure$bureau))
      
      cat("DEBUG: Directions:", length(directions_list), "Services:", length(all_services), "Bureaux:", length(all_bureaux), "\n")
      
      cat("DEBUG: Création du modal...\n")
      
      showModal(modalDialog(
        title = tagList(icon("user-plus"), " Ajouter un utilisateur"),
        size = "l",
        
        fluidRow(
          column(6, textInput("new_user_name", "Nom d'utilisateur :", placeholder = "Ex: NOUVEAU")),
          column(6, passwordInput("new_user_pw", "Mot de passe :", placeholder = "Mot de passe"))
        ),
        
        fluidRow(
          column(6, selectInput("new_user_role", "Rôle :",
                               choices = c("Directeur général", "Direction_Centrale", "RESPONSABLE"),
                               selected = "RESPONSABLE")),
          column(6, selectInput("new_user_direction", "Direction :",
                               choices = c("Sélectionnez..." = "", directions_list)))
        ),
        
        fluidRow(
          column(6, textInput("new_user_service", "Service :",
                             placeholder = "Ex: Méthodologies générales")),
          column(6, textInput("new_user_bureau", "Bureau :",
                             placeholder = "Ex: Méthodologies et plan de sondage"))
        ),
        
        tags$div(class = "alert alert-info",
                 icon("info-circle"), 
                 strong(" Aide à la saisie :"),
                 tags$ul(
                   tags$li("Copiez les noms exacts depuis le fichier Organisation.csv"),
                   tags$li("Respectez les majuscules et accents"),
                   tags$li("Exemples : 'Méthodologies générales', 'Statistiques de l'Education'")
                 )),
        
        footer = tagList(
          modalButton("Annuler"),
          actionButton("confirm_add_user", "Ajouter", class = "btn-success", icon = icon("check"))
        )
      ))
      
      cat("DEBUG: Modal simple affiché\n")
      cat("DEBUG: Fin de l'observeEvent add_user_btn\n")
      
    }, error = function(e) {
      cat("ERREUR CRITIQUE lors de l'ajout utilisateur:\n")
      cat("Message:", e$message, "\n")
      print(e)
      showNotification(
        paste("Erreur:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # Plus de cascades - tout est dans les pickerInput avec recherche
  
  # Confirmer l'ajout
  observeEvent(input$confirm_add_user, {
    req(input$new_user_name, input$new_user_pw)
    
    # Validation des champs - maintenant ce sont des textInput
    if(is.null(input$new_user_direction) || input$new_user_direction == "" ||
       is.null(input$new_user_service) || input$new_user_service == "" ||
       is.null(input$new_user_bureau) || input$new_user_bureau == "") {
      showNotification("Veuillez sélectionner Direction, Service ET Bureau !", 
                      type = "error", duration = 5)
      return()
    }
    
    # Vérifier si l'utilisateur existe déjà
    if(input$new_user_name %in% users()$user_name) {
      showNotification("Cet utilisateur existe déjà !", type = "error", duration = 3)
      return()
    }
    
    # Créer le nouvel utilisateur
    new_user <- data.frame(
      user_name = input$new_user_name,
      user_pw = input$new_user_pw,
      role = input$new_user_role,
      direction = input$new_user_direction,
      service = input$new_user_service,
      bureau = input$new_user_bureau,
      stringsAsFactors = FALSE
    )
    
    # Ajouter à la base
    updated_users <- rbind(users(), new_user)
    write.csv2(updated_users, nom_fichier_users, row.names = FALSE)
    users(updated_users)
    
    removeModal()
    showNotification(paste("Utilisateur", input$new_user_name, "ajouté avec succès !"), 
                    type = "message", duration = 3)
  })
  
  # Modifier un utilisateur
  observeEvent(input$edit_user, {
    req(input$edit_user)
    
    cat("DEBUG: Modification de l'utilisateur:", input$edit_user, "\n")
    
    user_data <- users() %>% filter(user_name == input$edit_user)
    
    if(nrow(user_data) > 0) {
      user <- user_data[1, ]
      
      # Préparer les listes
      directions_list <- sort(unique(org_structure$direction))
      
      showModal(modalDialog(
        title = tagList(icon("user-edit"), " Modifier l'utilisateur : ", user$user_name),
        size = "l",
        
        fluidRow(
          column(6, textInput("edit_user_name", "Nom d'utilisateur :", value = user$user_name)),
          column(6, passwordInput("edit_user_pw", "Nouveau mot de passe :", 
                                 placeholder = "Laisser vide pour ne pas changer"))
        ),
        
        fluidRow(
          column(6, selectInput("edit_user_role", "Rôle :",
                               choices = c("Directeur général", "Direction_Centrale", "RESPONSABLE"),
                               selected = user$role)),
          column(6, selectInput("edit_user_direction", "Direction :",
                               choices = c("Sélectionnez..." = "", directions_list),
                               selected = user$direction))
        ),
        
        fluidRow(
          column(6, textInput("edit_user_service", "Service :",
                             value = user$service,
                             placeholder = "Ex: Méthodologies générales")),
          column(6, textInput("edit_user_bureau", "Bureau :",
                             value = user$bureau,
                             placeholder = "Ex: Méthodologies et plan de sondage"))
        ),
        
        tags$div(class = "alert alert-info",
                 icon("info-circle"), 
                 strong(" Aide :"),
                 " Saisissez les noms exacts depuis Organisation.csv"),
        
        footer = tagList(
          modalButton("Annuler"),
          actionButton("confirm_edit_user", "Modifier", class = "btn-primary", icon = icon("save"))
        )
      ))
      
      cat("DEBUG: Formulaire de modification affiché\n")
    }
  })
  
  # Plus de cascades pour la modification non plus
  
  # Confirmer la modification
  observeEvent(input$confirm_edit_user, {
    req(input$edit_user_name)
    
    # Validation des champs - maintenant ce sont des textInput
    if(is.null(input$edit_user_direction) || input$edit_user_direction == "" ||
       is.null(input$edit_user_service) || input$edit_user_service == "" ||
       is.null(input$edit_user_bureau) || input$edit_user_bureau == "") {
      showNotification("Veuillez sélectionner Direction, Service ET Bureau !", 
                      type = "error", duration = 5)
      return()
    }
    
    # Récupérer les utilisateurs actuels
    current_users <- users()
    
    # Trouver l'index de l'utilisateur à modifier
    idx <- which(current_users$user_name == input$edit_user)
    
    if(length(idx) > 0) {
      # Mettre à jour les informations
      current_users$user_name[idx] <- input$edit_user_name
      
      # Ne changer le mot de passe que s'il est fourni
      if(!is.null(input$edit_user_pw) && input$edit_user_pw != "") {
        current_users$user_pw[idx] <- input$edit_user_pw
      }
      
      current_users$role[idx] <- input$edit_user_role
      current_users$direction[idx] <- input$edit_user_direction
      current_users$service[idx] <- input$edit_user_service
      current_users$bureau[idx] <- input$edit_user_bureau
      
      # Sauvegarder
      write.csv2(current_users, nom_fichier_users, row.names = FALSE)
      users(current_users)
      
      removeModal()
      showNotification(paste("Utilisateur", input$edit_user_name, "modifié avec succès !"), 
                      type = "message", duration = 3)
    }
  })
  
  # Supprimer un utilisateur
  observeEvent(input$delete_user, {
    req(input$delete_user)
    
    # Ne pas permettre la suppression de son propre compte
    if(input$delete_user == auth$user_info$user_name) {
      showNotification("Vous ne pouvez pas supprimer votre propre compte !", type = "error", duration = 3)
      return()
    }
    
    showModal(modalDialog(
      title = tagList(icon("exclamation-triangle"), " Confirmer la suppression"),
      paste0("Êtes-vous sûr de vouloir supprimer l'utilisateur '", input$delete_user, "' ?"),
      br(), br(),
      tags$p(style = "color: red;", 
             icon("warning"), 
             " Cette action est irréversible !"),
      
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_delete_user", "Supprimer", 
                    class = "btn-danger", icon = icon("trash"))
      )
    ))
  })
  
  # Confirmer la suppression
  observeEvent(input$confirm_delete_user, {
    req(input$delete_user)
    
    # Supprimer l'utilisateur
    updated_users <- users() %>% filter(user_name != input$delete_user)
    write.csv2(updated_users, nom_fichier_users, row.names = FALSE)
    users(updated_users)
    
    removeModal()
    showNotification("Utilisateur supprimé avec succès !", type = "message", duration = 3)
  })
  
  # Dernière sauvegarde
  output$last_backup_time <- renderText({
    if(file.exists(nom_fichier_csv)) {
      info <- file.info(nom_fichier_csv)
      format(info$mtime, "%d/%m/%Y %H:%M:%S")
    } else {
      "Aucune sauvegarde"
    }
  })
  
  # Créer une sauvegarde
  observeEvent(input$backup_btn, {
    backup_file <- paste0("backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    
    if(file.exists(nom_fichier_csv)) {
      file.copy(nom_fichier_csv, backup_file)
      
      showModal(modalDialog(
        title = tagList(icon("check-circle"), " Succès"),
        paste("Sauvegarde créée :", backup_file),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  # Télécharger la sauvegarde
  output$download_backup_btn <- downloadHandler(
    filename = function() {
      paste0("backup_ins_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(file.exists(nom_fichier_csv)) {
        file.copy(nom_fichier_csv, file)
      }
    }
  )
  
  # Statistiques admin
  output$admin_stats_ui <- renderUI({
    req(db())
    
    data <- db()
    
    stats <- list(
      total_users = length(unique(data$Responsable)),
      total_directions = length(unique(data$Direction)),
      total_services = length(unique(data$Service)),
      total_activites = nrow(data),
      taux_completion = round(mean(data$Avancement == 100, na.rm = TRUE) * 100, 1)
    )
    
    tagList(
      fluidRow(
        column(4, p(strong("Nombre d'utilisateurs :"), stats$total_users)),
        column(4, p(strong("Nombre de directions :"), stats$total_directions)),
        column(4, p(strong("Nombre de services :"), stats$total_services))
      ),
      hr(),
      fluidRow(
        column(6, p(strong("Total d'activités :"), stats$total_activites)),
        column(6, p(strong("Taux de complétion :"), paste0(stats$taux_completion, "%")))
      )
    )
  })
  
  # ================================================================================
  # NOTIFICATIONS (menu déroulant)
  # ================================================================================
  
  output$notif_menu <- renderMenu({
    req(auth$logged_in)
    
    data <- get_filtered_data()
    
    # Activités en retard
    n_retard <- sum(data$Statut == "En retard", na.rm = TRUE)
    
    # Échéances proches (7 jours)
    today <- Sys.Date()
    n_echeances <- data %>%
      mutate(DATE_FIN = as.Date(DATE_FIN)) %>%
      filter(DATE_FIN >= today, DATE_FIN <= today + 7) %>%
      nrow()
    
    # Total notifications
    total_notifs <- n_retard + n_echeances
    
    dropdownMenu(
      type = "notifications",
      badgeStatus = if(total_notifs > 0) "warning" else "success",
      icon = icon("bell"),
      headerText = paste(total_notifs, "notification(s)"),
      
      if(n_retard > 0) {
        notificationItem(
          text = paste(n_retard, "activité(s) en retard"),
          icon = icon("exclamation-triangle"),
          status = "danger"
        )
      },
      
      if(n_echeances > 0) {
        notificationItem(
          text = paste(n_echeances, "échéance(s) dans 7 jours"),
          icon = icon("clock"),
          status = "warning"
        )
      },
      
      if(total_notifs == 0) {
        notificationItem(
          text = "Tout est à jour !",
          icon = icon("check"),
          status = "success"
        )
      }
    )
  })
}

# ================================================================================
# LANCEMENT DE L'APPLICATION
# ================================================================================

shinyApp(ui, server)
