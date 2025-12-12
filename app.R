library(shiny)
library(dplyr)
library(ggplot2)

## ---------------------------------------------------------------
## 0. Load data
## ---------------------------------------------------------------

shots_all    <- readRDS("data/shots_all.rds")
shots_made   <- readRDS("data/shots_made.rds")
shots_missed <- readRDS("data/shots_missed.rds")

pre_seasons  <- c("2017-18", "2018-19")
post_seasons <- setdiff(sort(unique(shots_all$season)), pre_seasons)

x_limits <- range(shots_all$shot_x, na.rm = TRUE)
y_limits <- range(shots_all$shot_y, na.rm = TRUE)

all_teams   <- sort(unique(shots_all$shot_team))
all_players <- sort(unique(shots_all$shooter))

pick_dataset <- function(type = c("all", "made", "missed")) {
  type <- match.arg(type)
  switch(type, all = shots_all, made = shots_made, missed = shots_missed)
}

## ---------------------------------------------------------------
## 1. Helpers
## ---------------------------------------------------------------

infer_free_throw_flag <- function(df) {
  ft <- rep(NA, nrow(df))
  
  if ("free_throw" %in% names(df)) {
    ft <- df$free_throw
  } else if ("is_free_throw" %in% names(df)) {
    ft <- df$is_free_throw
  } else if ("ft" %in% names(df)) {
    ft <- df$ft
  } else if ("event_type" %in% names(df)) {
    ft <- grepl("free", df$event_type, ignore.case = TRUE)
  } else if ("shot_type" %in% names(df)) {
    ft <- grepl("free", df$shot_type, ignore.case = TRUE)
  } else if ("description" %in% names(df)) {
    ft <- grepl("free throw|freethrow", df$description, ignore.case = TRUE)
  }
  
  if (is.character(ft)) ft <- toupper(ft) == "TRUE"
  if (is.numeric(ft))   ft <- ft == 1
  as.logical(ft)
}

infer_three_pt_flag <- function(df) {
  if (!("three_pt" %in% names(df))) return(rep(NA, nrow(df)))
  
  tp <- df$three_pt
  if (is.character(tp)) tp <- toupper(tp) == "TRUE"
  if (is.numeric(tp))   tp <- tp == 1
  as.logical(tp)
}

make_shot_summary <- function(data,
                              season = "All seasons",
                              player = "All players",
                              min_attempts = 1) {
  
  if (nrow(data) == 0) return(tibble::tibble())
  
  season_on <- season != "All seasons"
  player_on <- player != "All players"
  
  # K rule: if player selected => K=5 regardless of season
  K <- if (player_on) 5 else if (season_on) 20 else 35
  
  # Season-relative prior (relative to CURRENT FILTERED SUBSET)
  mu <- mean(data$shot_outcome == "made")
  alpha <- mu * K
  beta  <- (1 - mu) * K
  
  data %>%
    group_by(shot_x, shot_y) %>%
    summarise(
      attempts = n(),
      makes = sum(shot_outcome == "made"),
      success_rate = makes / attempts,
      .groups = "drop"
    ) %>%
    filter(attempts >= min_attempts) %>%
    mutate(
      shrunk_rate = (makes + alpha) / (attempts + alpha + beta),
      K = K,
      prior_mu = mu
    )
}

plot_shot_map <- function(summary_df, title) {
  ggplot(summary_df,
         aes(x = shot_x, y = shot_y,
             color = shrunk_rate, size = attempts)) +
    geom_point(alpha = 0.75) +
    scale_color_viridis_c(option = "magma") +
    coord_fixed() +
    labs(
      title = title,
      x = "shot_x",
      y = "shot_y",
      color = "Shrunk FG%",
      size = "Attempts"
    ) +
    theme_minimal()
}

## ---------------------------------------------------------------
## NEW: Choice helper restricted by era + optional season + optional team
## ---------------------------------------------------------------

get_choices <- function(era = c("pre","post"),
                        season_choice = "All seasons",
                        team_choice = "All teams") {
  era <- match.arg(era)
  
  era_seasons <- if (era == "pre") pre_seasons else post_seasons
  
  df <- shots_all %>% dplyr::filter(.data$season %in% era_seasons)
  
  if (!is.null(season_choice) && season_choice != "All seasons") {
    df <- df %>% dplyr::filter(.data$season == season_choice)
  }
  
  if (!is.null(team_choice) && team_choice != "All teams") {
    df <- df %>% dplyr::filter(.data$shot_team == team_choice)
  }
  
  list(
    teams   = sort(unique(df$shot_team)),
    players = sort(unique(df$shooter))
  )
}

should_load_players <- function(season_choice, team_choice) {
  (season_choice != "All seasons") || (team_choice != "All teams")
}

## ---------------------------------------------------------------
## 2. UI
## ---------------------------------------------------------------

ui <- fluidPage(
  titlePanel("D1 Men’s Shot Chart Explorer"),
  
  tabsetPanel(
    
    tabPanel(
      "2017–18 & 2018–19",
      sidebarLayout(
        sidebarPanel(
          selectInput("season_pre", "Season:",
                      choices = c("All seasons", pre_seasons),
                      selected = "All seasons"),
          
          selectInput("team_pre", "Team:",
                      choices = c("All teams", all_teams),
                      selected = "All teams"),
          
          # IMPORTANT: start small; populate dynamically
          selectizeInput("player_pre", "Player:",
                         choices = c("All players"),
                         selected = "All players"),
          
          selectInput("ft_pre", "Free throws:",
                      choices = c("All shots" = "all",
                                  "Free throws only" = "ft_only",
                                  "No free throws" = "no_ft"),
                      selected = "all"),
          
          selectInput("tp_pre", "3-point shots:",
                      choices = c("All shots" = "all",
                                  "3-point only" = "tp_only",
                                  "No 3-point (2PT only)" = "no_tp"),
                      selected = "all"),
          
          radioButtons("shot_result_pre", "Makes / Misses:",
                       choices = c("All shots" = "all",
                                   "Makes only" = "made",
                                   "Misses only" = "missed"),
                       selected = "all"),
          
          radioButtons("plot_type_pre", "Plot type:",
                       choices = c("Points" = "points",
                                   "Density heatmap" = "heatmap",
                                   "Efficiency (Shrunk FG%)" = "eff"),
                       selected = "points")
        ),
        mainPanel(
          plotOutput("plot_pre", height = "550px"),
          hr(),
          h4("Summary for current filters"),
          tableOutput("stats_pre"),
          hr(),
          h4("Binomial logistic regression"),
          verbatimTextOutput("glm_pre")
        )
      )
    ),
    
    tabPanel(
      "2019–20 and Later",
      sidebarLayout(
        sidebarPanel(
          selectInput("season_post", "Season:",
                      choices = c("All seasons", post_seasons),
                      selected = "All seasons"),
          
          selectInput("team_post", "Team:",
                      choices = c("All teams", all_teams),
                      selected = "All teams"),
          
          # IMPORTANT: start small; populate dynamically
          selectizeInput("player_post", "Player:",
                         choices = c("All players"),
                         selected = "All players"),
          
          selectInput("ft_post", "Free throws:",
                      choices = c("All shots" = "all",
                                  "Free throws only" = "ft_only",
                                  "No free throws" = "no_ft"),
                      selected = "all"),
          
          selectInput("tp_post", "3-point shots:",
                      choices = c("All shots" = "all",
                                  "3-point only" = "tp_only",
                                  "No 3-point (2PT only)" = "no_tp"),
                      selected = "all"),
          
          radioButtons("shot_result_post", "Makes / Misses:",
                       choices = c("All shots" = "all",
                                   "Makes only" = "made",
                                   "Misses only" = "missed"),
                       selected = "all"),
          
          radioButtons("plot_type_post", "Plot type:",
                       choices = c("Points" = "points",
                                   "Density heatmap" = "heatmap",
                                   "Efficiency (Shrunk FG%)" = "eff"),
                       selected = "points")
        ),
        mainPanel(
          plotOutput("plot_post", height = "550px"),
          hr(),
          h4("Summary for current filters"),
          tableOutput("stats_post"),
          hr(),
          h4("Binomial logistic regression"),
          verbatimTextOutput("glm_post")
        )
      )
    )
  )
)

## ---------------------------------------------------------------
## 3. SERVER
## ---------------------------------------------------------------

server <- function(input, output, session) {
  
  ## -----------------------------------------------------------
  ## Dynamic dropdown updates:
  ##  - Always update teams based on (era + season)
  ##  - Only generate players when season or team is selected
  ##  - If team is selected, restrict players to that team
  ## -----------------------------------------------------------
  
  observeEvent(list(input$season_pre, input$team_pre), {
    # Update teams based on season within the PRE era
    ch_season <- get_choices("pre", season_choice = input$season_pre, team_choice = "All teams")
    
    updateSelectInput(
      session, "team_pre",
      choices  = c("All teams", ch_season$teams),
      selected = if (input$team_pre %in% c("All teams", ch_season$teams)) input$team_pre else "All teams"
    )
    
    if (!should_load_players(input$season_pre, input$team_pre)) {
      updateSelectizeInput(
        session, "player_pre",
        choices  = c("All players"),
        selected = "All players",
        server   = TRUE
      )
    } else {
      ch <- get_choices("pre", season_choice = input$season_pre, team_choice = input$team_pre)
      
      updateSelectizeInput(
        session, "player_pre",
        choices  = c("All players", ch$players),
        selected = if (input$player_pre %in% c("All players", ch$players)) input$player_pre else "All players",
        server   = TRUE
      )
    }
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$season_post, input$team_post), {
    # Update teams based on season within the POST era
    ch_season <- get_choices("post", season_choice = input$season_post, team_choice = "All teams")
    
    updateSelectInput(
      session, "team_post",
      choices  = c("All teams", ch_season$teams),
      selected = if (input$team_post %in% c("All teams", ch_season$teams)) input$team_post else "All teams"
    )
    
    if (!should_load_players(input$season_post, input$team_post)) {
      updateSelectizeInput(
        session, "player_post",
        choices  = c("All players"),
        selected = "All players",
        server   = TRUE
      )
    } else {
      ch <- get_choices("post", season_choice = input$season_post, team_choice = input$team_post)
      
      updateSelectizeInput(
        session, "player_post",
        choices  = c("All players", ch$players),
        selected = if (input$player_post %in% c("All players", ch$players)) input$player_post else "All players",
        server   = TRUE
      )
    }
  }, ignoreInit = FALSE)
  
  ## -----------------------------------------------------------
  ## Filtering logic
  ## -----------------------------------------------------------
  
  filter_data <- function(era) {
    
    if (era == "pre") {
      season_choice <- input$season_pre
      team_choice   <- input$team_pre
      player_choice <- input$player_pre
      ft_choice     <- input$ft_pre
      tp_choice     <- input$tp_pre
      base          <- pick_dataset(input$shot_result_pre)
      era_seasons   <- pre_seasons
    } else {
      season_choice <- input$season_post
      team_choice   <- input$team_post
      player_choice <- input$player_post
      ft_choice     <- input$ft_post
      tp_choice     <- input$tp_post
      base          <- pick_dataset(input$shot_result_post)
      era_seasons   <- post_seasons
    }
    
    df <- base
    
    # Always restrict to the current era
    df <- df %>% filter(.data$season %in% era_seasons)
    
    # Optional season filter
    if (season_choice != "All seasons") df <- df %>% filter(.data$season == season_choice)
    
    # Team/player
    if (team_choice   != "All teams")   df <- df %>% filter(.data$shot_team == team_choice)
    if (player_choice != "All players") df <- df %>% filter(.data$shooter == player_choice)
    
    # 3PT filter
    tp_flag <- infer_three_pt_flag(df)
    if (tp_choice == "tp_only") {
      df <- df %>% filter(tp_flag %in% TRUE)
    } else if (tp_choice == "no_tp") {
      df <- df %>% filter(tp_flag %in% FALSE)
    }
    
    # Free throw filter
    ft_flag <- infer_free_throw_flag(df)
    if (ft_choice == "ft_only") {
      df <- df %>% filter(ft_flag %in% TRUE)
    } else if (ft_choice == "no_ft") {
      df <- df %>% filter(ft_flag %in% FALSE)
    }
    
    df
  }
  
  ## ---------- PRE ----------
  
  output$plot_pre <- renderPlot({
    df <- filter_data("pre")
    if (nrow(df) == 0) return(NULL)
    
    if (input$plot_type_pre == "eff") {
      ss <- make_shot_summary(df, season = input$season_pre, player = input$player_pre)
      
      plot_shot_map(
        ss,
        paste0("Shrunk FG% (μ=", round(unique(ss$prior_mu), 3),
               ", K=", unique(ss$K), ")")
      )
      
    } else if (input$plot_type_pre == "heatmap") {
      
      ggplot(df, aes(shot_x, shot_y)) +
        stat_bin2d(bins = 50) +
        scale_fill_viridis_c(option = "C", trans = "sqrt") +
        coord_fixed(xlim = x_limits, ylim = y_limits) +
        theme_minimal() +
        labs(fill = "Shot count")
      
    } else {
      
      ggplot(df, aes(shot_x, shot_y)) +
        geom_point(alpha = 0.35, size = 0.7) +
        coord_fixed(xlim = x_limits, ylim = y_limits) +
        theme_minimal()
    }
  })
  
  output$stats_pre <- renderTable({
    df <- filter_data("pre")
    if (nrow(df) == 0) return(NULL)
    
    made_flag <- df$shot_outcome == "made"
    made_num  <- as.numeric(made_flag)
    
    tibble::tibble(
      Metric = c("Attempts", "Makes", "Misses", "FG%",
                 "Correlation(shot_x, made)",
                 "Correlation(shot_y, made)"),
      Value = c(
        nrow(df),
        sum(made_flag),
        sum(!made_flag),
        sprintf("%.1f%%", 100 * mean(made_flag)),
        round(cor(df$shot_x, made_num, use = "complete.obs"), 3),
        round(cor(df$shot_y, made_num, use = "complete.obs"), 3)
      )
    )
  })
  
  output$glm_pre <- renderPrint({
    df <- filter_data("pre")
    if (nrow(df) == 0) return(NULL)
    
    ss <- make_shot_summary(df, season = input$season_pre, player = input$player_pre)
    if (nrow(ss) < 2) {
      cat("Not enough locations to fit regression.")
      return(NULL)
    }
    
    summary(glm(
      cbind(makes, attempts - makes) ~ attempts,
      data = ss,
      family = binomial()
    ))
  })
  
  ## ---------- POST ----------
  
  output$plot_post <- renderPlot({
    df <- filter_data("post")
    if (nrow(df) == 0) return(NULL)
    
    if (input$plot_type_post == "eff") {
      ss <- make_shot_summary(df, season = input$season_post, player = input$player_post)
      
      plot_shot_map(
        ss,
        paste0("Shrunk FG% (μ=", round(unique(ss$prior_mu), 3),
               ", K=", unique(ss$K), ")")
      )
      
    } else if (input$plot_type_post == "heatmap") {
      
      ggplot(df, aes(shot_x, shot_y)) +
        stat_bin2d(bins = 50) +
        scale_fill_viridis_c(option = "C", trans = "sqrt") +
        coord_fixed(xlim = x_limits, ylim = y_limits) +
        theme_minimal() +
        labs(fill = "Shot count")
      
    } else {
      
      ggplot(df, aes(shot_x, shot_y)) +
        geom_point(alpha = 0.35, size = 0.7) +
        coord_fixed(xlim = x_limits, ylim = y_limits) +
        theme_minimal()
    }
  })
  
  output$stats_post <- renderTable({
    df <- filter_data("post")
    if (nrow(df) == 0) return(NULL)
    
    made_flag <- df$shot_outcome == "made"
    made_num  <- as.numeric(made_flag)
    
    tibble::tibble(
      Metric = c("Attempts", "Makes", "Misses", "FG%",
                 "Correlation(shot_x, made)",
                 "Correlation(shot_y, made)"),
      Value = c(
        nrow(df),
        sum(made_flag),
        sum(!made_flag),
        sprintf("%.1f%%", 100 * mean(made_flag)),
        round(cor(df$shot_x, made_num, use = "complete.obs"), 3),
        round(cor(df$shot_y, made_num, use = "complete.obs"), 3)
      )
    )
  })
  
  output$glm_post <- renderPrint({
    df <- filter_data("post")
    if (nrow(df) == 0) return(NULL)
    
    ss <- make_shot_summary(df, season = input$season_post, player = input$player_post)
    if (nrow(ss) < 2) {
      cat("Not enough locations to fit regression.")
      return(NULL)
    }
    
    summary(glm(
      cbind(makes, attempts - makes) ~ attempts,
      data = ss,
      family = binomial()
    ))
  })
}

shinyApp(ui, server)