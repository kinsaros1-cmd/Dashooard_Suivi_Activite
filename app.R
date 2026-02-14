# ================================================================================
# APPLICATION INS - SYSTÈME INTÉGRÉ DE SUIVI DES ACTIVITÉS
# Version: 2.0 - Professionnelle
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
    stop("Le fichier users_db.csv est manquant. Veuillez le placer dans le répertoire de l'application.")
  }
}

# Charger les utilisateurs au démarrage
users_db <- load_users()

# --- CONFIGURATION DES ÉTAPES DU PROCESSUS ---
etapes_config <- list(
  list(id = "ETP", label = "Mise en place de l'Équipe Technique", doc = TRUE, ordre = 1),
  list(id = "TDR", label = "Termes de référence global", doc = TRUE, ordre = 2),
  list(id = "TP", label = "Travaux préparatoires", doc = TRUE, ordre = 3),
  list(id = "ANO", label = "Demande d'Avis de Non Objection", doc = FALSE, ordre = 4),
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
      
      # Menu Connexion (toujours visible)
      menuItem("Connexion", tabName = "login", icon = icon("sign-in-alt")),
      
      # Tous les autres menus (visibles UNIQUEMENT après connexion)
      uiOutput("authenticated_menus_ui")
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
            
            hr()
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
            
            # ============================================================
            # BLOC 1 : RÉFÉRENCES
            # ============================================================
            tags$h4(icon("bookmark"), " Références", style = "color: #3c8dbc; margin-top: 0;"),
            tags$p(class = "text-muted", style = "font-size: 12px;",
                  "Format : X.X.X.XX (Ex: 1.2.1.03). Pour les activités non budgétisées, utiliser : 0.0.0.00"),
            
            fluidRow(
              column(
                width = 4,
                textInput("ref_activite", "Réf. INS :",
                         value = "0.0.0.00",
                         placeholder = "Ex: 1.2.2.01"),
                uiOutput("alert_ref_ins_ui")
              ),
              column(
                width = 4,
                textInput("ref_hiswaca", "Réf. HISWACA :",
                         value = "0.0.0.00",
                         placeholder = "Ex: 1.3.7.01")
              ),
              column(
                width = 4,
                textInput("ref_etat", "Réf. État :",
                         value = "0.0.0.00",
                         placeholder = "Ex: 1.1.5.02")
              )
            ),
            
            hr(),
            
            # ============================================================
            # BLOC 2 : IDENTIFICATION DE L'ACTIVITÉ
            # ============================================================
            tags$h4(icon("tag"), " Identification de l'activité", style = "color: #3c8dbc;"),
            
            fluidRow(
              column(
                width = 4,
                pickerInput("type_activite", "Type d'activité :",
                           choices = types_activites,
                           selected = "Sélectionnez le type")
              ),
              column(
                width = 8,
                textInput("nom_activite", "Nom de l'activité (Unique) :",
                         placeholder = "Ex: Enquête harmonisée sur les conditions de vie")
              )
            ),
            
            hr(),
            
            # ============================================================
            # BLOC 3 : PLANIFICATION
            # ============================================================
            tags$h4(icon("calendar-alt"), " Planification", style = "color: #3c8dbc;"),
            
            fluidRow(
              column(
                width = 4,
                dateInput("date_debut", "Date de début :",
                         value = Sys.Date(),
                         min = "2026-01-01",
                         max = "2026-12-31",
                         format = "dd/mm/yyyy",
                         language = "fr")
              ),
              column(
                width = 4,
                dateInput("date_fin", "Date de fin prévue :",
                         value = Sys.Date() + 180,
                         min = "2026-01-01",
                         max = "2026-12-31",
                         format = "dd/mm/yyyy",
                         language = "fr")
              ),
              column(
                width = 4,
                uiOutput("duree_activite_ui")
              )
            ),
            
            hr(),
            
            # ============================================================
            # BLOC 4 : FINANCEMENT
            # ============================================================
            tags$h4(icon("dollar-sign"), " Financement", style = "color: #3c8dbc;"),
            
            fluidRow(
              column(
                width = 12,
                tags$label("Source de financement :"),
                checkboxGroupInput("source_financement", 
                                  label = NULL,
                                  choices = c("État" = "Etat",
                                            "HISWACA" = "HISWACA",
                                            "BAD" = "BAD",
                                            "Autre" = "Autre"),
                                  inline = TRUE)
              )
            ),
            
            # Champ conditionnel pour préciser "Autre"
            fluidRow(
              column(
                width = 12,
                uiOutput("autre_financement_ui")
              )
            ),
            
            # Coûts et Gap
            fluidRow(
              column(
                width = 4,
                numericInput("cout_initial", "Coût total initial (FCFA) :",
                            value = 0,
                            min = 0,
                            step = 1000)
              ),
              column(
                width = 4,
                numericInput("cout_consomme", "Coût consommé (FCFA) :",
                            value = 0,
                            min = 0,
                            step = 1000)
              ),
              column(
                width = 4,
                uiOutput("gap_cout_ui")
              )
            ),
            
            hr(),
            
            # ============================================================
            # BLOC 5 : OBSERVATIONS
            # ============================================================
            tags$h4(icon("comment"), " Observations", style = "color: #3c8dbc;"),
            
            fluidRow(
              column(
                width = 12,
                textAreaInput("observation_activite", label = NULL,
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
              "Le statut du document détermine le score : Draft (3/10), En cours (6/10), Finalisé(e) (10/10)."),
            
            # En-têtes de colonnes
            fluidRow(
              column(3, tags$strong("Étape")),
              column(1, tags$strong(style = "text-align: center;", "Activé")),
              column(2, tags$strong("Date début")),
              column(2, tags$strong("Date fin")),
              column(2, tags$strong("Document")),
              column(2, tags$strong("Statut doc."))
              # Colonne Score masquée
            ),
            
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
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("top_activites_avancement", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("hourglass-half"), " Top 10 Activités embryonnaires par Avancement"),
            status = "warning",
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
            width = 6,
            plotlyOutput("performance_service", height = 400)
          ),
          
          box(
            title = tagList(icon("user-tie"), " Performance par responsable"),
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("performance_responsable", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("percentage"), " Distribution des avancements"),
            status = "warning",
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
            
            # Filtre de recherche
            fluidRow(
              column(
                width = 6,
                textInput("search_documents", 
                         label = NULL,
                         placeholder = "🔍 Rechercher par référence, activité, ou responsable...")
              ),
              column(
                width = 3,
                selectInput("filter_doc_direction",
                           label = NULL,
                           choices = c("Toutes directions" = ""),
                           selected = "")
              ),
              column(
                width = 3,
                actionButton("clear_doc_filters", "Réinitialiser filtres",
                            icon = icon("redo"),
                            class = "btn-warning btn-sm")
              )
            ),
            
            hr(),
            
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
    editing_activity = NULL,
    form_refresh_trigger = 0  # Trigger pour forcer le re-render
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
  
  # Info utilisateur sidebar - Affichage amélioré
  output$user_info_sidebar <- renderUI({
    if(!is.null(auth$user_info)) {
      # Gérer le cas où Service pourrait être NA ou vide
      service_display <- if(!is.null(auth$user_info$service) && 
                            !is.na(auth$user_info$service) && 
                            auth$user_info$service != "" && 
                            auth$user_info$service != "NA") {
        auth$user_info$service
      } else {
        "-"
      }
      
      div(
        style = "background: #d4edda; padding: 15px; border-radius: 5px; border-left: 4px solid #28a745;",
        
        # Icône et message de bienvenue
        div(
          style = "margin-bottom: 10px;",
          icon("user-circle", class = "fa-lg"),
          tags$strong(
            style = "margin-left: 8px; font-size: 16px;",
            paste0("Bienvenue, ", auth$user_info$user_name)
          )
        ),
        
        # Direction
        div(
          style = "margin-top: 8px;",
          tags$strong("Direction : "),
          tags$span(auth$user_info$direction)
        ),
        
        # Service
        div(
          style = "margin-top: 5px;",
          tags$strong("Service : "),
          tags$span(service_display)
        )
      )
    }
  })
  
  # Menus de l'application (visibles UNIQUEMENT après connexion réussie)
  output$authenticated_menus_ui <- renderUI({
    if(auth$logged_in && !is.null(auth$user_info)) {
      tagList(
        # Menu Saisie (masqué pour le Directeur Général)
        if(auth$user_info$role != "Direction_generale") {
          menuItem("Saisie des activités", tabName = "saisie", 
                   icon = icon("edit"), startExpanded = FALSE,
                   menuSubItem("Nouvelle activité", tabName = "nouvelle_activite", icon = icon("plus-circle")),
                   menuSubItem("Mes activités", tabName = "mes_activites", icon = icon("list"))
          )
        },
        
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
        
        # Menu Administration (UNIQUEMENT pour le DG)
        if(auth$user_info$role == "Direction_generale") {
          menuItem("Administration", tabName = "admin", icon = icon("cog"))
        }
      )
    }
  })
  
  # ================================================================================
  # SAISIE - NOUVELLE ACTIVITÉ
  # ================================================================================
  
  # Barre de progression
  output$progress_bar_ui <- renderUI({
    req(auth$logged_in)
    
    # Calculer l'avancement basé sur les statuts de documents
    total_score <- 0
    
    for(e in etapes_config) {
      if(isTRUE(input[[paste0("val_", e$id)]])) {
        if(e$doc) {
          # Étape avec document : score selon le statut
          doc_status <- input[[paste0("doc_status_", e$id)]]
          
          # Si le statut n'est pas encore défini, utiliser draft par défaut
          if(is.null(doc_status)) {
            doc_status <- "draft"
          }
          
          score <- if(doc_status == "draft") {
            3
          } else if(doc_status == "non_valide") {
            6
          } else if(doc_status == "valide") {
            10
          } else {
            3  # Par défaut
          }
          total_score <- total_score + score
        } else {
          # Pas de document requis (ANO) : score automatique de 10
          total_score <- total_score + 10
        }
      }
    }
    
    pct <- round(total_score)
    
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
  
  # Alerte en temps réel pour la référence INS
  output$alert_ref_ins_ui <- renderUI({
    ref_ins <- input$ref_activite
    
    if(!is.null(ref_ins) && ref_ins != "") {
      # Vérifier si c'est "0.0.0.00"
      if(ref_ins == "0.0.0.00") {
        tags$small(
          style = "color: #dd4b39; font-weight: bold;",
          icon("times-circle"),
          " La référence INS ne peut pas être '0.0.0.00'"
        )
      } else {
        # Vérifier l'unicité
        donnees_existantes <- db()
        
        if(!is.null(donnees_existantes) && nrow(donnees_existantes) > 0) {
          # Si on est en mode édition, exclure l'activité en cours
          if(!is.null(form_state$editing_activity)) {
            donnees_a_verifier <- donnees_existantes[donnees_existantes$Reference != form_state$editing_activity$Reference, ]
          } else {
            donnees_a_verifier <- donnees_existantes
          }
          
          if("Reference" %in% names(donnees_a_verifier)) {
            refs_existantes <- donnees_a_verifier$Reference
            
            if(ref_ins %in% refs_existantes) {
              # Référence déjà utilisée
              tags$small(
                style = "color: #dd4b39; font-weight: bold;",
                icon("exclamation-triangle"),
                " Cette référence est déjà utilisée !"
              )
            } else {
              # Référence unique et valide
              tags$small(
                style = "color: #00a65a; font-weight: bold;",
                icon("check-circle"),
                " Référence disponible"
              )
            }
          }
        } else {
          # Base vide, référence OK
          tags$small(
            style = "color: #00a65a; font-weight: bold;",
            icon("check-circle"),
            " Référence disponible"
          )
        }
      }
    }
  })
  
  # Champ conditionnel pour préciser "Autre" source de financement
  output$autre_financement_ui <- renderUI({
    if("Autre" %in% input$source_financement) {
      textInput("autre_financement_detail", "Précisez la source :",
               placeholder = "Ex: PNUD, Union Européenne, etc.")
    }
  })
  
  # Calculer et afficher le gap de coût
  output$gap_cout_ui <- renderUI({
    req(input$cout_initial, input$cout_consomme)
    
    initial <- as.numeric(input$cout_initial)
    consomme <- as.numeric(input$cout_consomme)
    gap <- initial - consomme
    pct <- if(initial > 0) round((consomme / initial) * 100) else 0
    
    couleur <- if(consomme > initial) {
      "#dd4b39"  # Rouge si dépassement
    } else if(pct > 80) {
      "#f39c12"  # Orange si > 80%
    } else {
      "#00a65a"  # Vert sinon
    }
    
    div(
      style = "padding-top: 7px;",
      tags$label("Gap (Reste) :"),
      p(
        style = paste0("font-size: 16px; font-weight: bold; color: ", couleur, ";"),
        format(gap, big.mark = " ", scientific = FALSE), " FCFA"
      ),
      tags$small(
        style = paste0("color: ", couleur, ";"),
        paste0("Consommé : ", pct, "%")
      )
    )
  })
  
  # Durée de l'activité
  output$duree_activite_ui <- renderUI({
    req(input$date_debut, input$date_fin)
    
    debut <- as.Date(input$date_debut)
    fin <- as.Date(input$date_fin)
    duree <- as.numeric(difftime(fin, debut, units = "days"))
    
    if(duree < 0) {
      # Durée négative : afficher un message d'erreur
      div(
        style = "padding-top: 7px;",
        tags$label("Durée :"),
        div(
          class = "alert alert-danger",
          style = "margin-top: 5px; padding: 10px;",
          icon("exclamation-triangle"),
          strong(" ERREUR : "),
          "La date de fin ne peut pas être antérieure à la date de début !",
          br(),
          span(style = "font-size: 14px;", paste("Durée actuelle :", duree, "jours"))
        )
      )
    } else {
      # Durée positive : affichage normal
      couleur <- if(duree > 365) {
        "#dd4b39"  # Rouge si > 1 an
      } else if(duree > 180) {
        "#f39c12"  # Orange si > 6 mois
      } else {
        "#00a65a"  # Vert sinon
      }
      
      div(
        style = "padding-top: 7px;",
        tags$label("Durée :"),
        p(
          style = paste0("font-size: 16px; font-weight: bold; color: ", couleur, ";"),
          paste(duree, "jours"),
          if(duree > 365) {
            span(
              style = "font-size: 12px; color: #dd4b39; margin-left: 10px;",
              icon("info-circle"),
              " Attention : durée > 1 an"
            )
          }
        )
      )
    }
  })
  
  # Formulaire des étapes
  output$etapes_form_ui <- renderUI({
    req(auth$logged_in)
    
    # Réagir au trigger pour forcer le re-render
    form_state$form_refresh_trigger
    
    # Debug : confirmer que cette fonction s'exécute
    cat("DEBUG: Génération du formulaire des étapes (trigger:", form_state$form_refresh_trigger, ")\n")
    
    lapply(seq_along(etapes_config), function(i) {
      e <- etapes_config[[i]]
      
      # Vérifier si un fichier existe déjà (ancien ou nouveau)
      doc_key <- paste0("Doc_", e$id)
      existing_file <- form_state$docs_existants[[doc_key]]
      has_old_file <- !is.null(existing_file) && existing_file != ""
      has_new_file <- !is.null(input[[paste0("file_", e$id)]])
      has_file <- has_old_file || has_new_file
      
      # Déterminer si l'étape est complétée
      is_completed <- isTRUE(input[[paste0("val_", e$id)]])
      
      # Vérifier les conditions de déblocage
      can_activate <- TRUE
      blocked_reason <- ""
      
      # NOUVELLE LOGIQUE DE SÉQUENTIALITÉ
      # Étape 1 (ETP), 2 (TDR), 3 (TP) : Indépendantes
      # Étape 4 (ANO) : Dépend de l'étape 2 (TDR) avec statut "validé"
      # Étapes 5-10 : Séquentialité stricte avec statut "validé"
      
      if(i == 4) {
        # Étape 4 (ANO) : vérifier que l'étape 2 (TDR) est validée ET le document est "validé"
        tdr_completed <- isTRUE(input$val_TDR)
        tdr_doc_status <- input$doc_status_TDR
        
        if(!tdr_completed) {
          can_activate <- FALSE
          blocked_reason <- "L'étape 2 (Termes de référence) doit être validée d'abord"
        } else if(is.null(tdr_doc_status) || tdr_doc_status != "valide") {
          can_activate <- FALSE
          blocked_reason <- "Le document de l'étape 2 (TDR) doit avoir le statut 'Validé'"
        }
      } else if(i >= 5) {
        # À partir de l'étape 5, vérifier que l'étape précédente est validée ET le document est "validé"
        prev_step <- etapes_config[[i-1]]
        prev_completed <- isTRUE(input[[paste0("val_", prev_step$id)]])
        
        if(!prev_completed) {
          can_activate <- FALSE
          blocked_reason <- paste0("L'étape ", i-1, " (", prev_step$label, ") doit être validée d'abord")
        } else if(prev_step$doc) {
          # Si l'étape précédente a un document, vérifier son statut
          prev_doc_status <- input[[paste0("doc_status_", prev_step$id)]]
          if(is.null(prev_doc_status) || prev_doc_status != "valide") {
            can_activate <- FALSE
            blocked_reason <- paste0("Le document de l'étape ", i-1, " doit avoir le statut 'Validé'")
          }
        }
      }
      
      # Pour toutes les étapes avec document : fichier obligatoire
      if(e$doc && !has_file && !is_completed) {
        can_activate <- FALSE
        if(blocked_reason == "") {
          blocked_reason <- "Veuillez joindre un fichier avant de valider cette étape"
        }
      }
      
      # Déterminer la classe CSS
      css_class <- "step-row"
      if(is_completed) {
        css_class <- paste(css_class, "step-completed")
      }
      
      div(
        id = paste0("row_", e$id),
        class = css_class,
        
        fluidRow(
          column(
            width = 3,
            tags$strong(
              style = "font-size: 14px;",
              paste0(i, ". ", e$label)
            ),
            # Afficher l'avertissement si l'étape est bloquée
            if(!can_activate && !is_completed) {
              div(
                style = "margin-top: 5px; color: #dd4b39; font-size: 12px;",
                icon("lock"),
                " ", blocked_reason
              )
            }
          ),
          
          column(
            width = 1,
            # Toutes les étapes utilisent maintenant switchInput
            {
              current_value <- isTRUE(input[[paste0("val_", e$id)]])
              switchInput(
                inputId = paste0("val_", e$id),
                value = current_value,
                size = "small",
                onStatus = "success",
                offStatus = "danger",
                disabled = !can_activate && !is_completed
              )
            }
          ),
          
          # Date de début (sauf pour ANO)
          column(
            width = 2,
            if(e$id != "ANO") {
              tagList(
                dateInput(
                  inputId = paste0("date_debut_", e$id),
                  label = NULL,
                  value = if(is_completed) Sys.Date() else NULL,
                  min = "2026-01-01",
                  max = min(as.Date("2026-12-31"), Sys.Date()),  # Ne peut pas être postérieure à aujourd'hui
                  format = "dd/mm/yyyy",
                  language = "fr"
                ),
                # Alerte si date < date début activité OU date > aujourd'hui
                uiOutput(paste0("alert_date_debut_", e$id))
              )
            }
          ),
          
          # Date de fin (sauf pour ANO)
          column(
            width = 2,
            if(e$id != "ANO") {
              tagList(
                dateInput(
                  inputId = paste0("date_fin_", e$id),
                  label = NULL,
                  value = if(is_completed) Sys.Date() else NULL,
                  min = "2026-01-01",
                  max = "2026-12-31",
                  format = "dd/mm/yyyy",
                  language = "fr"
                ),
                # Alerte si date fin < date début OU si étape dépasse durée prévue
                uiOutput(paste0("alert_date_fin_", e$id))
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
          ),
          
          column(
            width = 2,
            if(e$doc && (has_file || is_completed)) {
              # Statut du document (seulement si fichier joint)
              # Préserver la valeur actuelle pour éviter la réinitialisation
              current_status <- input[[paste0("doc_status_", e$id)]]
              if(is.null(current_status)) current_status <- "draft"
              
              selectInput(
                inputId = paste0("doc_status_", e$id),
                label = NULL,
                choices = c("Draft" = "draft",
                           "En cours" = "non_valide",
                           "Finalisé(e)" = "valide"),
                selected = current_status,
                width = "100%"
              )
            }
          )
        )
      )
    })
  })
  
  # Outputs pour afficher les scores de chaque étape
  lapply(etapes_config, function(e) {
    output[[paste0("score_", e$id)]] <- renderUI({
      if(isTRUE(input[[paste0("val_", e$id)]])) {
        # Étape activée : calculer le score selon le statut du document
        if(e$doc) {
          doc_status <- input[[paste0("doc_status_", e$id)]]
          score <- if(is.null(doc_status) || doc_status == "draft") {
            3
          } else if(doc_status == "non_valide") {
            6
          } else if(doc_status == "valide") {
            10
          } else {
            3  # Par défaut
          }
          
          couleur <- if(score == 10) "#00a65a" else if(score == 6) "#f39c12" else "#dd4b39"
          
          tags$div(
            style = paste0("text-align: center; font-weight: bold; color: ", couleur, "; font-size: 14px;"),
            paste0(score, "/10")
          )
        } else {
          # Pas de document requis (ANO) : score automatique de 10
          tags$div(
            style = "text-align: center; font-weight: bold; color: #00a65a; font-size: 14px;",
            "10/10"
          )
        }
      }
    })
  })
  
  # Alertes pour les dates des étapes
  lapply(etapes_config, function(e) {
    if(e$id != "ANO") {
      # Alerte pour date début
      output[[paste0("alert_date_debut_", e$id)]] <- renderUI({
        date_debut_etape <- input[[paste0("date_debut_", e$id)]]
        date_debut_activite <- input$date_debut
        aujourd_hui <- Sys.Date()
        
        if(!is.null(date_debut_etape)) {
          date_debut_etape <- as.Date(date_debut_etape)
          
          # Vérifier si date > aujourd'hui
          if(date_debut_etape > aujourd_hui) {
            tags$small(
              style = "color: #dd4b39; font-weight: bold;",
              icon("times-circle"),
              " Date future ! (après aujourd'hui)"
            )
          } else if(!is.null(date_debut_activite) && date_debut_etape < as.Date(date_debut_activite)) {
            # Vérifier si date < date début activité
            tags$small(
              style = "color: #dd4b39; font-weight: bold;",
              icon("exclamation-triangle"),
              " Avant début activité"
            )
          }
        }
      })
      
      # Alerte pour date fin
      output[[paste0("alert_date_fin_", e$id)]] <- renderUI({
        date_debut_etape <- input[[paste0("date_debut_", e$id)]]
        date_fin_etape <- input[[paste0("date_fin_", e$id)]]
        date_fin_activite <- input$date_fin
        
        if(!is.null(date_debut_etape) && !is.null(date_fin_etape)) {
          duree_etape <- as.numeric(difftime(as.Date(date_fin_etape), as.Date(date_debut_etape), units = "days"))
          
          if(duree_etape < 0) {
            # Date fin < date début
            tags$small(
              style = "color: #dd4b39; font-weight: bold;",
              icon("times-circle"),
              " Date fin < date début !"
            )
          } else if(!is.null(date_fin_activite) && as.Date(date_fin_etape) > as.Date(date_fin_activite)) {
            # Étape dépasse la date de fin de l'activité
            tags$small(
              style = "color: #f39c12; font-weight: bold;",
              icon("exclamation-triangle"),
              " Dépasse fin activité"
            )
          } else if(duree_etape > 60) {
            # Étape prend plus de 2 mois
            tags$small(
              style = "color: #f39c12;",
              icon("clock"),
              paste0(" ", duree_etape, " jours (>2 mois)")
            )
          } else {
            # Durée normale
            tags$small(
              style = "color: #00a65a;",
              icon("check-circle"),
              paste0(" ", duree_etape, " jours")
            )
          }
        }
      })
    }
  })
  
  # Logique séquentielle des étapes - NOUVELLE VERSION
  # Étapes 1, 2, 3 (ETP, TDR, TP) : INDÉPENDANTES - pas de observeEvent
  # Étape 4 (ANO) : Dépend UNIQUEMENT de l'étape 2 (TDR) avec statut "validé"
  # Étapes 5-10 : Séquentialité stricte avec statut "validé"
  
  # Étape 4 (ANO) : Dépend de l'étape 2 (TDR) avec statut validé
  observeEvent(input$val_ANO, {
    if(!is.null(input$val_ANO) && input$val_ANO) {
      if(!isTRUE(input$val_TDR)) {
        updateSwitchInput(session, "val_ANO", value = FALSE)
        showNotification("Veuillez d'abord compléter l'étape 2 : Termes de référence global", type = "warning", duration = 3)
      } else if(is.null(input$doc_status_TDR) || input$doc_status_TDR != "valide") {
        updateSwitchInput(session, "val_ANO", value = FALSE)
        showNotification("Le document de l'étape 2 (TDR) doit avoir le statut 'Validé'", type = "warning", duration = 3)
      }
    }
  }, ignoreInit = TRUE)
  
  # Étape 5 (FP) : Dépend de l'étape 4 (ANO)
  observeEvent(input$val_FP, {
    if(!is.null(input$val_FP) && input$val_FP && !isTRUE(input$val_ANO)) {
      updateSwitchInput(session, "val_FP", value = FALSE)
      showNotification("Veuillez d'abord compléter l'étape 4 : Demande d'Avis de Non Objection", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Étape 6 (CDT) : Dépend de l'étape 5 (FP) avec statut validé
  observeEvent(input$val_CDT, {
    if(!is.null(input$val_CDT) && input$val_CDT) {
      if(!isTRUE(input$val_FP)) {
        updateSwitchInput(session, "val_CDT", value = FALSE)
        showNotification("Veuillez d'abord compléter l'étape 5 : Formation du personnel", type = "warning", duration = 3)
      } else if(is.null(input$doc_status_FP) || input$doc_status_FP != "valide") {
        updateSwitchInput(session, "val_CDT", value = FALSE)
        showNotification("Le document de l'étape 5 (FP) doit avoir le statut 'Validé'", type = "warning", duration = 3)
      }
    }
  }, ignoreInit = TRUE)
  
  # Étape 7 (TAD) : Dépend de l'étape 6 (CDT) avec statut validé
  observeEvent(input$val_TAD, {
    if(!is.null(input$val_TAD) && input$val_TAD) {
      if(!isTRUE(input$val_CDT)) {
        updateSwitchInput(session, "val_TAD", value = FALSE)
        showNotification("Veuillez d'abord compléter l'étape 6 : Collecte de données sur le terrain", type = "warning", duration = 3)
      } else if(is.null(input$doc_status_CDT) || input$doc_status_CDT != "valide") {
        updateSwitchInput(session, "val_TAD", value = FALSE)
        showNotification("Le document de l'étape 6 (CDT) doit avoir le statut 'Validé'", type = "warning", duration = 3)
      }
    }
  }, ignoreInit = TRUE)
  
  # Étape 8 (ARR) : Dépend de l'étape 7 (TAD) avec statut validé
  observeEvent(input$val_ARR, {
    if(!is.null(input$val_ARR) && input$val_ARR) {
      if(!isTRUE(input$val_TAD)) {
        updateSwitchInput(session, "val_ARR", value = FALSE)
        showNotification("Veuillez d'abord compléter l'étape 7 : Traitement / Apurement des données", type = "warning", duration = 3)
      } else if(is.null(input$doc_status_TAD) || input$doc_status_TAD != "valide") {
        updateSwitchInput(session, "val_ARR", value = FALSE)
        showNotification("Le document de l'étape 7 (TAD) doit avoir le statut 'Validé'", type = "warning", duration = 3)
      }
    }
  }, ignoreInit = TRUE)
  
  # Étape 9 (VR) : Dépend de l'étape 8 (ARR) avec statut validé
  observeEvent(input$val_VR, {
    if(!is.null(input$val_VR) && input$val_VR) {
      if(!isTRUE(input$val_ARR)) {
        updateSwitchInput(session, "val_VR", value = FALSE)
        showNotification("Veuillez d'abord compléter l'étape 8 : Analyse et Rédaction du rapport", type = "warning", duration = 3)
      } else if(is.null(input$doc_status_ARR) || input$doc_status_ARR != "valide") {
        updateSwitchInput(session, "val_VR", value = FALSE)
        showNotification("Le document de l'étape 8 (ARR) doit avoir le statut 'Validé'", type = "warning", duration = 3)
      }
    }
  }, ignoreInit = TRUE)
  
  # Étape 10 (PML) : Dépend de l'étape 9 (VR) avec statut validé
  observeEvent(input$val_PML, {
    if(!is.null(input$val_PML) && input$val_PML) {
      if(!isTRUE(input$val_VR)) {
        updateSwitchInput(session, "val_PML", value = FALSE)
        showNotification("Veuillez d'abord compléter l'étape 9 : Validation du rapport", type = "warning", duration = 3)
      } else if(is.null(input$doc_status_VR) || input$doc_status_VR != "valide") {
        updateSwitchInput(session, "val_PML", value = FALSE)
        showNotification("Le document de l'étape 9 (VR) doit avoir le statut 'Validé'", type = "warning", duration = 3)
      }
    }
  }, ignoreInit = TRUE)
  
  # Gestion visuelle des étapes (griser celles qui sont inaccessibles)
  observe({
    req(auth$logged_in)
    
    # Étapes 1, 2, 3 (ETP, TDR, TP) : JAMAIS grisées (indépendantes)
    # Pas de code ici pour ces étapes
    
    # Étape 4 (ANO) grisée si étape 2 (TDR) non complétée
    if(!isTRUE(input$val_TDR)) {
      shinyjs::addClass(id = "row_ANO", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_ANO", class = "disabled-step")
    }
    
    # Étape 5 (FP) grisée si étape 4 (ANO) non complétée
    if(!isTRUE(input$val_ANO)) {
      shinyjs::addClass(id = "row_FP", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_FP", class = "disabled-step")
    }
    
    # Étape 6 (CDT) grisée si étape 5 (FP) non complétée
    if(!isTRUE(input$val_FP)) {
      shinyjs::addClass(id = "row_CDT", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_CDT", class = "disabled-step")
    }
    
    # Étape 7 (TAD) grisée si étape 6 (CDT) non complétée
    if(!isTRUE(input$val_CDT)) {
      shinyjs::addClass(id = "row_TAD", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_TAD", class = "disabled-step")
    }
    
    # Étape 8 (ARR) grisée si étape 7 (TAD) non complétée
    if(!isTRUE(input$val_TAD)) {
      shinyjs::addClass(id = "row_ARR", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_ARR", class = "disabled-step")
    }
    
    # Étape 9 (VR) grisée si étape 8 (ARR) non complétée
    if(!isTRUE(input$val_ARR)) {
      shinyjs::addClass(id = "row_VR", class = "disabled-step")
    } else {
      shinyjs::removeClass(id = "row_VR", class = "disabled-step")
    }
    
    # Étape 10 (PML) grisée si étape 9 (VR) non complétée
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
    form_state$docs_existants <- list()  # IMPORTANT : vider la liste des docs
    form_state$editing_activity <- NULL
    form_state$form_refresh_trigger <- form_state$form_refresh_trigger + 1  # Forcer le re-render
    
    updateTextInput(session, "ref_activite", value = "0.0.0.00")
    updateTextInput(session, "nom_activite", value = "")
    updatePickerInput(session, "type_activite", selected = "Sélectionnez le type")
    updateDateInput(session, "date_debut", value = Sys.Date())
    updateDateInput(session, "date_fin", value = Sys.Date() + 180)
    updateTextAreaInput(session, "observation_activite", value = "")
    updateCheckboxGroupInput(session, "source_financement", selected = character(0))
    updateTextInput(session, "autre_financement_detail", value = "")
    updateTextInput(session, "ref_hiswaca", value = "0.0.0.00")
    updateTextInput(session, "ref_etat", value = "0.0.0.00")
    updateNumericInput(session, "cout_initial", value = 0)
    updateNumericInput(session, "cout_consomme", value = 0)
    
    for(e in etapes_config) {
      # Toutes les étapes utilisent maintenant switch
      updateSwitchInput(session, paste0("val_", e$id), value = FALSE)
    }
    
    # Forcer le re-render du formulaire des étapes pour actualiser les badges "Fichier joint"
    cat("DEBUG: Formulaire réinitialisé - docs_existants vidé - trigger:", form_state$form_refresh_trigger, "\n")
  }
  
  # Enregistrer l'activité
  observeEvent(input$save_data_btn, {
    
    # Validation du nom
    if(is.null(input$nom_activite) || input$nom_activite == "") {
      showModal(modalDialog(
        title = tagList(icon("exclamation-triangle"), " Erreur"),
        "Veuillez saisir un nom d'activité.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # Validation du type
    if(input$type_activite == "Sélectionnez le type") {
      showModal(modalDialog(
        title = tagList(icon("exclamation-triangle"), " Erreur"),
        "Veuillez sélectionner un type d'activité.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # Validation de la cohérence des dates
    debut <- as.Date(input$date_debut)
    fin <- as.Date(input$date_fin)
    duree <- as.numeric(difftime(fin, debut, units = "days"))
    
    if(duree < 0) {
      showModal(modalDialog(
        title = tagList(icon("exclamation-triangle"), " Erreur de dates"),
        div(
          p(strong("La date de fin ne peut pas être antérieure à la date de début !")),
          p(paste("Date de début :", format(debut, "%d/%m/%Y"))),
          p(paste("Date de fin :", format(fin, "%d/%m/%Y"))),
          p(style = "color: red;", paste("Durée :", duree, "jours"))
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # ============================================================================
    # VALIDATION DE L'UNICITÉ DE LA RÉFÉRENCE INS
    # ============================================================================
    ref_ins <- input$ref_activite
    
    # 1. Vérifier que la référence INS n'est pas "0.0.0.00"
    if(is.null(ref_ins) || ref_ins == "" || ref_ins == "0.0.0.00") {
      showModal(modalDialog(
        title = tagList(icon("exclamation-triangle"), " Référence INS invalide"),
        div(
          p(strong("La référence INS est obligatoire et ne peut pas être '0.0.0.00' !")),
          p("La référence '0.0.0.00' est réservée uniquement pour les références État et HISWACA."),
          p(style = "color: #f39c12;", icon("lightbulb"), 
            " Exemple de référence valide : 1.2.1.03, 2.5.3.12, etc."),
          p(style = "color: #3c8dbc;", "Format attendu : X.X.X.XX")
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # 2. Vérifier l'unicité de la référence INS dans toute la base
    donnees_existantes <- db()
    
    if(!is.null(donnees_existantes) && nrow(donnees_existantes) > 0) {
      # Si on est en mode édition, exclure l'activité en cours de modification
      if(!is.null(form_state$editing_activity)) {
        # Exclure la ligne en cours d'édition (même référence = modification, pas doublon)
        donnees_a_verifier <- donnees_existantes[donnees_existantes$Reference != form_state$editing_activity$Reference, ]
      } else {
        donnees_a_verifier <- donnees_existantes
      }
      
      # Vérifier si la référence existe déjà
      if("Reference" %in% names(donnees_a_verifier)) {
        refs_existantes <- donnees_a_verifier$Reference
        
        if(ref_ins %in% refs_existantes) {
          # Trouver l'activité qui utilise déjà cette référence
          activite_existante <- donnees_a_verifier[donnees_a_verifier$Reference == ref_ins, ][1, ]
          
          showModal(modalDialog(
            title = tagList(icon("exclamation-triangle"), " Référence INS déjà utilisée"),
            div(
              p(strong(paste0("La référence '", ref_ins, "' est déjà utilisée !"))),
              p("Cette référence est déjà attribuée à l'activité suivante :"),
              tags$div(
                style = "background: #f4f4f4; padding: 10px; border-left: 4px solid #dd4b39; margin: 10px 0;",
                p(tags$strong("Activité : "), activite_existante$Activite),
                p(tags$strong("Type : "), activite_existante$Type_Activite),
                p(tags$strong("Responsable : "), activite_existante$Responsable),
                p(tags$strong("Direction : "), activite_existante$Direction)
              ),
              p(style = "color: red;", icon("times-circle"), 
                " Chaque activité doit avoir une référence INS unique."),
              p(style = "color: #3c8dbc;", icon("lightbulb"), 
                " Veuillez choisir une autre référence pour votre activité.")
            ),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }
      }
    }
    
    # Validation de la séquentialité des étapes et des fichiers
    etapes_validees <- sapply(etapes_config, function(e) {
      isTRUE(input[[paste0("val_", e$id)]])
    })
    
    # Vérifier que chaque étape validée a un fichier (sauf ANO)
    for(i in seq_along(etapes_config)) {
      e <- etapes_config[[i]]
      if(etapes_validees[i] && e$doc) {
        # Vérifier si un fichier est joint ou existe déjà
        has_new_file <- !is.null(input[[paste0("file_", e$id)]])
        doc_key <- paste0("Doc_", e$id)
        has_old_file <- !is.null(form_state$docs_existants[[doc_key]]) && 
                        form_state$docs_existants[[doc_key]] != ""
        
        if(!has_new_file && !has_old_file) {
          showModal(modalDialog(
            title = tagList(icon("exclamation-triangle"), " Fichier manquant"),
            div(
              p(strong(paste0("L'étape ", i, " (", e$label, ") est validée mais aucun fichier n'est joint !"))),
              p("Veuillez joindre un document justificatif avant de valider cette étape.")
            ),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }
      }
    }
    
    # Vérifier la séquentialité spécifique
    # Étape 4 (ANO) : nécessite étape 2 (TDR)
    if(etapes_validees[4]) {  # ANO est à l'index 4
      if(!etapes_validees[2]) {  # TDR est à l'index 2
        showModal(modalDialog(
          title = tagList(icon("exclamation-triangle"), " Erreur de séquentialité"),
          div(
            p(strong("L'étape 4 (Demande d'Avis de Non Objection) ne peut pas être validée !")),
            p("L'étape 2 (Termes de référence global) doit être validée d'abord."),
            p(style = "color: #f39c12;", icon("info-circle"), 
              " L'ANO nécessite l'approbation préalable des TDR.")
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
    }
    
    # Valider les dates des étapes
    date_debut_activite <- as.Date(input$date_debut)
    date_fin_activite <- as.Date(input$date_fin)
    
    for(i in seq_along(etapes_config)) {
      e <- etapes_config[[i]]
      
      if(e$id != "ANO" && etapes_validees[i]) {
        date_debut_etape <- input[[paste0("date_debut_", e$id)]]
        date_fin_etape <- input[[paste0("date_fin_", e$id)]]
        
        # Vérifier que les dates sont renseignées
        if(is.null(date_debut_etape) || is.null(date_fin_etape)) {
          showModal(modalDialog(
            title = tagList(icon("exclamation-triangle"), " Dates manquantes"),
            div(
              p(strong(paste0("L'étape ", i, " (", e$label, ") nécessite des dates !"))),
              p("Veuillez renseigner la date de début et la date de fin de cette étape.")
            ),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }
        
        date_debut_etape <- as.Date(date_debut_etape)
        date_fin_etape <- as.Date(date_fin_etape)
        aujourd_hui <- Sys.Date()
        
        # Vérifier que date début étape <= aujourd'hui
        if(date_debut_etape > aujourd_hui) {
          showModal(modalDialog(
            title = tagList(icon("exclamation-triangle"), " Date future"),
            div(
              p(strong(paste0("L'étape ", i, " (", e$label, ") : date de début future !"))),
              p(paste0("Date début étape : ", format(date_debut_etape, "%d/%m/%Y"))),
              p(paste0("Date actuelle : ", format(aujourd_hui, "%d/%m/%Y"))),
              p(style = "color: red;", "La date de début de l'étape ne peut pas être postérieure à aujourd'hui.")
            ),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }
        
        # Vérifier que date début étape >= date début activité
        if(date_debut_etape < date_debut_activite) {
          showModal(modalDialog(
            title = tagList(icon("exclamation-triangle"), " Date incohérente"),
            div(
              p(strong(paste0("L'étape ", i, " (", e$label, ") : date de début incorrecte !"))),
              p(paste0("Date début étape : ", format(date_debut_etape, "%d/%m/%Y"))),
              p(paste0("Date début activité : ", format(date_debut_activite, "%d/%m/%Y"))),
              p(style = "color: red;", "La date de début de l'étape ne peut pas être antérieure à la date de début de l'activité.")
            ),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }
        
        # Vérifier que date fin étape >= date début étape
        if(date_fin_etape < date_debut_etape) {
          showModal(modalDialog(
            title = tagList(icon("exclamation-triangle"), " Durée négative"),
            div(
              p(strong(paste0("L'étape ", i, " (", e$label, ") : durée négative !"))),
              p(paste0("Date début : ", format(date_debut_etape, "%d/%m/%Y"))),
              p(paste0("Date fin : ", format(date_fin_etape, "%d/%m/%Y"))),
              p(style = "color: red;", "La date de fin ne peut pas être antérieure à la date de début.")
            ),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }
      }
    }
    
    # Vérifier la séquentialité à partir de l'étape 5
    for(i in 5:length(etapes_config)) {
      if(etapes_validees[i]) {
        # Vérifier que l'étape précédente est validée
        if(!etapes_validees[i-1]) {
          e_current <- etapes_config[[i]]
          e_prev <- etapes_config[[i-1]]
          showModal(modalDialog(
            title = tagList(icon("exclamation-triangle"), " Erreur de séquentialité"),
            div(
              p(strong(paste0("L'étape ", i, " (", e_current$label, ") ne peut pas être validée !"))),
              p(paste0("L'étape ", i-1, " (", e_prev$label, ") doit être validée d'abord.")),
              p(style = "color: #f39c12;", icon("info-circle"), 
                " Les étapes 5 à 10 doivent être validées dans l'ordre.")
            ),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }
      }
    }
    
    # Entourer le code d'enregistrement dans un tryCatch
    tryCatch({
      # Calculer l'avancement basé sur les statuts de documents
      total_score <- 0
      
      cat("DEBUG: Calcul de l'avancement\n")
      
      for(e in etapes_config) {
        if(isTRUE(input[[paste0("val_", e$id)]])) {
          if(e$doc) {
            # Étape avec document : score selon le statut
            doc_status <- input[[paste0("doc_status_", e$id)]]
            
            # Si le statut n'est pas encore défini, vérifier si un fichier est joint
            if(is.null(doc_status)) {
              doc_status <- "draft"  # Valeur par défaut
            }
            
            score <- if(doc_status == "draft") {
              3
            } else if(doc_status == "non_valide") {
              6
            } else if(doc_status == "valide") {
              10
            } else {
              3  # Par défaut
            }
            
            cat("DEBUG: Étape", e$id, "- Statut:", doc_status, "- Score:", score, "\n")
            total_score <- total_score + score
          } else {
            # Pas de document requis (ANO) : score automatique de 10
            cat("DEBUG: Étape", e$id, "(sans doc) - Score: 10\n")
            total_score <- total_score + 10
          }
        }
      }
      
      cat("DEBUG: Score total:", total_score, "\n")
      
      # Avancement = total_score sur 100 (10 étapes × 10 points max)
      avancement <- round(total_score)
      
      cat("DEBUG: Avancement final:", avancement, "%\n")
      
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
        Source_Financement = if(is.null(input$source_financement) || length(input$source_financement) == 0) {
          ""
        } else {
          paste(input$source_financement, collapse = ", ")
        },
        Autre_Financement = if(!is.null(input$autre_financement_detail)) input$autre_financement_detail else "",
        Ref_HISWACA = if(!is.null(input$ref_hiswaca)) input$ref_hiswaca else "",
        Ref_Etat = if(!is.null(input$ref_etat)) input$ref_etat else "",
        Cout_Initial = if(!is.null(input$cout_initial)) input$cout_initial else 0,
        Cout_Consomme = if(!is.null(input$cout_consomme)) input$cout_consomme else 0,
        Gap_Cout = if(!is.null(input$cout_initial) && !is.null(input$cout_consomme)) {
          input$cout_initial - input$cout_consomme
        } else 0,
        Observation = input$observation_activite,
        Avancement = avancement,
        Statut = statut,
        Date_inscription = format(Sys.time(), "%d/%m/%Y %H:%M"),
        DATE_DEBUT = format(as.Date(input$date_debut), "%Y-%m-%d"),
        DATE_FIN = format(as.Date(input$date_fin), "%Y-%m-%d")
      )
      
      # Ajouter les étapes, documents et statuts
      for(e in etapes_config) {
        val_id <- paste0("val_", e$id)
        new_data[[paste0("Val_", e$id)]] <- if(isTRUE(input[[val_id]])) "Oui" else "Non"
        
        date_id <- paste0("date_", e$id)
        if(isTRUE(input[[val_id]]) && !is.null(input[[date_id]])) {
          new_data[[paste0("Date_", e$id)]] <- format(as.Date(input[[date_id]]), "%Y-%m-%d")
        } else {
          new_data[[paste0("Date_", e$id)]] <- ""
        }
        
        # Sauvegarder le statut du document
        if(e$doc) {
          doc_status_id <- paste0("doc_status_", e$id)
          doc_status <- input[[doc_status_id]]
          new_data[[paste0("DocStatus_", e$id)]] <- if(!is.null(doc_status)) doc_status else "draft"
        }
        
        # Sauvegarder les dates de début et fin (sauf pour ANO)
        if(e$id != "ANO") {
          date_debut_id <- paste0("date_debut_", e$id)
          if(!is.null(input[[date_debut_id]])) {
            new_data[[paste0("DateDebut_", e$id)]] <- format(as.Date(input[[date_debut_id]]), "%Y-%m-%d")
          } else {
            new_data[[paste0("DateDebut_", e$id)]] <- ""
          }
          
          date_fin_id <- paste0("date_fin_", e$id)
          if(!is.null(input[[date_fin_id]])) {
            new_data[[paste0("DateFin_", e$id)]] <- format(as.Date(input[[date_fin_id]]), "%Y-%m-%d")
          } else {
            new_data[[paste0("DateFin_", e$id)]] <- ""
          }
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
      select(Reference, Activite, Type_Activite, Source_Financement, DATE_DEBUT, DATE_FIN, 
             Avancement, Statut, Date_inscription) %>%
      mutate(
        # Gérer les NA dans Source_Financement
        Source_Financement = ifelse(is.na(Source_Financement) | Source_Financement == "", 
                                    "Non spécifié", Source_Financement),
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
      colnames = c("Réf", "Activité", "Type", "Source Financement", "Début", "Fin", 
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
        
        # Source de financement (avec gestion des NA)
        if("Source_Financement" %in% names(act) && !is.na(act$Source_Financement) && act$Source_Financement != "") {
          # Séparer les sources multiples
          sources <- strsplit(as.character(act$Source_Financement), ", ")[[1]]
          updateCheckboxGroupInput(session, "source_financement", selected = sources)
        } else {
          updateCheckboxGroupInput(session, "source_financement", selected = character(0))
        }
        
        # Charger les nouveaux champs
        if("Autre_Financement" %in% names(act) && !is.na(act$Autre_Financement)) {
          updateTextInput(session, "autre_financement_detail", value = act$Autre_Financement)
        }
        
        if("Ref_HISWACA" %in% names(act) && !is.na(act$Ref_HISWACA)) {
          updateTextInput(session, "ref_hiswaca", value = act$Ref_HISWACA)
        }
        
        if("Ref_Etat" %in% names(act) && !is.na(act$Ref_Etat)) {
          updateTextInput(session, "ref_etat", value = act$Ref_Etat)
        }
        
        if("Cout_Initial" %in% names(act) && !is.na(act$Cout_Initial)) {
          updateNumericInput(session, "cout_initial", value = as.numeric(act$Cout_Initial))
        }
        
        if("Cout_Consomme" %in% names(act) && !is.na(act$Cout_Consomme)) {
          updateNumericInput(session, "cout_consomme", value = as.numeric(act$Cout_Consomme))
        }
        
        # Charger les étapes
        temp_docs <- list()
        for(e in etapes_config) {
          val_col <- paste0("Val_", e$id)
          if(val_col %in% names(act)) {
            # Conversion robuste en booléen
            val_text <- as.character(act[[val_col]])
            val_bool <- !is.na(val_text) && val_text == "Oui"
            
            # Toutes les étapes utilisent maintenant switch
            updateSwitchInput(
              session, 
              paste0("val_", e$id), 
              value = val_bool
            )
          }
          
          # Charger le document existant
          if(e$doc) {
            doc_col <- paste0("Doc_", e$id)
            if(doc_col %in% names(act) && !is.na(act[[doc_col]]) && act[[doc_col]] != "") {
              temp_docs[[doc_col]] <- act[[doc_col]]
            }
            
            # Charger le statut du document
            doc_status_col <- paste0("DocStatus_", e$id)
            if(doc_status_col %in% names(act) && !is.na(act[[doc_status_col]])) {
              updateSelectInput(
                session,
                paste0("doc_status_", e$id),
                selected = as.character(act[[doc_status_col]])
              )
            }
          }
          
          # Charger les dates de début et fin (sauf pour ANO)
          if(e$id != "ANO") {
            date_debut_col <- paste0("DateDebut_", e$id)
            if(date_debut_col %in% names(act) && !is.na(act[[date_debut_col]]) && act[[date_debut_col]] != "") {
              updateDateInput(
                session,
                paste0("date_debut_", e$id),
                value = as.Date(act[[date_debut_col]])
              )
            }
            
            date_fin_col <- paste0("DateFin_", e$id)
            if(date_fin_col %in% names(act) && !is.na(act[[date_fin_col]]) && act[[date_fin_col]] != "") {
              updateDateInput(
                session,
                paste0("date_fin_", e$id),
                value = as.Date(act[[date_fin_col]])
              )
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
    
    # Filtrage selon le rôle hiérarchique
    if(auth$user_info$role == "Chef_bureau") {
      # Chef de bureau : voit uniquement ses propres activités
      data <- data %>%
        filter(Responsable == auth$user_info$user_name)
    } else if(auth$user_info$role == "Chef_service") {
      # Chef de service : voit toutes les activités de son service
      data <- data %>%
        filter(Service == auth$user_info$service)
    } else if(auth$user_info$role == "Direction_centrale") {
      # Directeur central : voit toutes les activités de sa direction
      data <- data %>%
        filter(Direction == auth$user_info$direction)
    }
    # Direction_generale (DG) : voit tout (pas de filtre)
    
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
          Avancement >= 80 ~ "#00a65a",  # Vert
          Avancement >= 50 ~ "#3c8dbc",  # Bleu
          Avancement >= 30 ~ "#f39c12",  # Orange
          TRUE ~ "#dd4b39"               # Rouge
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
          Avancement >= 30 ~ "#f39c12",  # Orange
          Avancement >= 15 ~ "#ff851b",  # Orange foncé
          TRUE ~ "#dd4b39"               # Rouge
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
      marker = list(color = "#00a65a")
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
        color = "#f39c12",
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
      head(30)  # Limiter à 30 activités pour la lisibilité
    
    if(nrow(data_timeline) == 0) {
      return(plotly_empty())
    }
    
    # Ajouter une colonne pour le label Y (nom court de l'activité)
    data_timeline <- data_timeline %>%
      mutate(
        Label = paste0(Reference, " - ", 
                      ifelse(nchar(Activite) > 40, 
                             paste0(substr(Activite, 1, 40), "..."), 
                             Activite)),
        # Couleur selon l'avancement
        Couleur = case_when(
          Avancement >= 80 ~ "#00a65a",  # Vert
          Avancement >= 50 ~ "#3c8dbc",  # Bleu
          Avancement >= 30 ~ "#f39c12",  # Orange
          TRUE ~ "#dd4b39"               # Rouge
        ),
        # Texte hover
        HoverText = paste0(
          "<b>", Reference, " - ", Activite, "</b><br>",
          "Type: ", Type_Activite, "<br>",
          "Début: ", format(DATE_DEBUT, "%d/%m/%Y"), "<br>",
          "Fin: ", format(DATE_FIN, "%d/%m/%Y"), "<br>",
          "Durée: ", as.numeric(DATE_FIN - DATE_DEBUT), " jours<br>",
          "Avancement: ", Avancement, "%<br>",
          "Statut: ", Statut, "<br>",
          "Responsable: ", Responsable
        )
      )
    
    # Créer le diagramme de Gantt
    fig <- plot_ly()
    
    # Ajouter une barre pour chaque activité
    for(i in 1:nrow(data_timeline)) {
      act <- data_timeline[i, ]
      
      fig <- fig %>%
        add_trace(
          type = "bar",
          orientation = "h",
          x = as.numeric(act$DATE_FIN - act$DATE_DEBUT),
          y = act$Label,
          base = as.numeric(act$DATE_DEBUT - as.Date("2026-01-01")),
          marker = list(
            color = act$Couleur,
            line = list(color = "rgba(0,0,0,0.2)", width = 1)
          ),
          text = act$HoverText,
          hoverinfo = "text",
          showlegend = FALSE
        )
    }
    
    # Configuration de l'axe X pour afficher les mois de 2026
    mois_2026 <- seq(as.Date("2026-01-01"), as.Date("2026-12-31"), by = "month")
    mois_labels <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                     "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
    mois_values <- as.numeric(mois_2026 - as.Date("2026-01-01"))
    
    fig %>%
      layout(
        title = list(
          text = "",
          font = list(size = 14)
        ),
        xaxis = list(
          title = "Période (Année 2026)",
          ticktext = mois_labels,
          tickvals = mois_values,
          tickangle = -45,
          showgrid = TRUE,
          gridcolor = "rgba(128,128,128,0.3)",
          gridwidth = 1,
          zeroline = TRUE,
          zerolinecolor = "rgba(0,0,0,0.5)",
          zerolinewidth = 2,
          range = c(0, 365)  # Toute l'année 2026
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 10),
          categoryorder = "trace",
          showgrid = TRUE,
          gridcolor = "rgba(128,128,128,0.2)",
          gridwidth = 1
        ),
        margin = list(l = 300, r = 50, b = 120, t = 50),
        hovermode = "closest",
        plot_bgcolor = "rgba(240,240,240,0.3)",
        paper_bgcolor = "white",
        bargap = 0.15,
        height = max(400, nrow(data_timeline) * 30)  # Hauteur dynamique
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
    
    # Appliquer les filtres de recherche
    search_term <- input$search_documents
    filter_dir <- input$filter_doc_direction
    
    if(!is.null(search_term) && search_term != "") {
      data <- data %>%
        filter(
          grepl(search_term, Reference, ignore.case = TRUE) |
          grepl(search_term, Activite, ignore.case = TRUE) |
          grepl(search_term, Responsable, ignore.case = TRUE)
        )
    }
    
    if(!is.null(filter_dir) && filter_dir != "") {
      data <- data %>%
        filter(Direction == filter_dir)
    }
    
    # Extraire tous les documents
    doc_cols <- grep("^Doc_", names(data), value = TRUE)
    
    all_files <- data %>%
      select(Activite, Reference, Responsable, Direction, all_of(doc_cols)) %>%
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
          " Aucun document disponible avec ces critères de recherche."
        )
      )
    }
    
    # Créer la liste des téléchargements
    tagList(
      tags$p(
        class = "text-muted",
        icon("file"),
        paste0(" ", nrow(all_files), " document(s) trouvé(s)")
      ),
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
                    "Étape : ", etape_label, " | ",
                    "Responsable : ", file_info$Responsable, " | ",
                    "Direction : ", file_info$Direction
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
        size = ~Avancement / 5,  # Taille proportionnelle : 0% = 0px, 100% = 20px
        sizemode = 'diameter',
        line = list(color = 'white', width = 1),
        opacity = 0.7
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
                               choices = c("Direction_generale", "Direction_centrale", "Chef_service", "Chef_bureau"),
                               selected = "Chef_bureau")),
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
                               choices = c("Direction_generale", "Direction_centrale", "Chef_service", "Chef_bureau"),
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
