
library(shiny)

library(dplyr)

library(ggplot2)

## ---------------------------------------------------------------

## 1. Helper objects based on your data

## ---------------------------------------------------------------

# Seasons before vs after 3pt‑line move

#loading the data
shots_all    <- readRDS("data/shots_all.rds")
shots_made   <- readRDS("data/shots_made.rds")
shots_missed <- readRDS("data/shots_missed.rds")


pre_seasons  <- c("2017-18", "2018-19")

post_seasons <- setdiff(sort(unique(shots_all$season)), pre_seasons)

# Court limits for sliders & coord_fixed

x_limits <- range(shots_all$shot_x, na.rm = TRUE)

y_limits <- range(shots_all$shot_y, na.rm = TRUE)

# All teams / players (for dropdowns)

all_teams   <- sort(unique(shots_all$shot_team))

all_players <- sort(unique(shots_all$shooter))

# Choose which base dataset to start from

pick_dataset <- function(type = c("all", "made", "missed")) {
  
  type <- match.arg(type)
  
  switch(
    
    type,
    
    all    = shots_all,
    
    made   = shots_made,
    
    missed = shots_missed
    
  )
  
}

## ---------------------------------------------------------------

## 2. UI

## ---------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("D1 Men’s Shot Chart Explorer"),
  
  
  
  tabsetPanel(
    
    id = "era_tab",
    
    
    
    ## ---------- TAB 1: Pre line‑move ----------
    
    tabPanel(
      
      "2017–18 & 2018–19 (Old 3PT Line)",
      
      sidebarLayout(
        
        sidebarPanel(
          
          width = 3,
          
          
          
          # Tabs *inside* sidebar so you don’t have to scroll forever
          
          tabsetPanel(
            
            id = "filters_pre_tabs",
            
            type = "pills",
            
            
            
            tabPanel(
              
              "Main filters",
              
              br(),
              
              selectInput("season_pre", "Season:", choices = pre_seasons),
              
              
              
              radioButtons(
                
                "shot_result_pre", "Makes / Misses:",
                
                choices = c("All shots" = "all",
                            
                            "Makes only" = "made",
                            
                            "Misses only" = "missed")
                
              ),
              
              
              
              selectInput(
                
                "value_pre", "Shot value:",
                
                choices = c("All shots"   = "all",
                            
                            "3‑point only" = "3",
                            
                            "2‑point only" = "2")
                
              ),
              
              
              
              selectInput(
                
                "team_pre", "Team:",
                
                choices = c("All teams", all_teams)
                
              ),
              
              
              
              selectizeInput(
                
                "player_pre", "Player:",
                
                choices = c("All players", all_players),
                
                selected = "All players",
                
                options = list(placeholder = "Type a player name…")
                
              ),
              
              
              
              radioButtons(
                
                "plot_type_pre", "Plot type:",
                
                choices = c("Points" = "points",
                            
                            "Density heatmap" = "heatmap")
                
              )
              
            ),
            
            
            
            tabPanel(
              
              "Court window",
              
              br(),
              
              sliderInput(
                
                "xrange_pre", "Court X‑range (length):",
                
                min = floor(x_limits[1]),
                
                max = ceiling(x_limits[2]),
                
                value = x_limits
                
              ),
              
              sliderInput(
                
                "yrange_pre", "Court Y‑range (width):",
                
                min = floor(y_limits[1]),
                
                max = ceiling(y_limits[2]),
                
                value = y_limits
                
              )
              
            )
            
          )
          
        ),
        
        
        
        mainPanel(
          
          width = 9,
          
          plotOutput("plot_pre", height = "550px"),
          
          hr(),
          
          h4("Summary for current filters"),
          
          tableOutput("stats_pre")
          
        )
        
      )
      
    ),
    
    
    
    ## ---------- TAB 2: Post line‑move ----------
    
    tabPanel(
      
      "2019–20 and Later (New 3PT Line)",
      
      sidebarLayout(
        
        sidebarPanel(
          
          width = 3,
          
          
          
          tabsetPanel(
            
            id = "filters_post_tabs",
            
            type = "pills",
            
            
            
            tabPanel(
              
              "Main filters",
              
              br(),
              
              selectInput("season_post", "Season:", choices = post_seasons),
              
              
              
              radioButtons(
                
                "shot_result_post", "Makes / Misses:",
                
                choices = c("All shots" = "all",
                            
                            "Makes only" = "made",
                            
                            "Misses only" = "missed")
                
              ),
              
              
              
              selectInput(
                
                "value_post", "Shot value:",
                
                choices = c("All shots"   = "all",
                            
                            "3‑point only" = "3",
                            
                            "2‑point only" = "2")
                
              ),
              
              
              
              selectInput(
                
                "team_post", "Team:",
                
                choices = c("All teams", all_teams)
                
              ),
              
              
              
              selectizeInput(
                
                "player_post", "Player:",
                
                choices = c("All players", all_players),
                
                selected = "All players",
                
                options = list(placeholder = "Type a player name…")
                
              ),
              
              
              
              radioButtons(
                
                "plot_type_post", "Plot type:",
                
                choices = c("Points" = "points",
                            
                            "Density heatmap" = "heatmap")
                
              )
              
            ),
            
            
            
            tabPanel(
              
              "Court window",
              
              br(),
              
              sliderInput(
                
                "xrange_post", "Court X‑range (length):",
                
                min = floor(x_limits[1]),
                
                max = ceiling(x_limits[2]),
                
                value = x_limits
                
              ),
              
              sliderInput(
                
                "yrange_post", "Court Y‑range (width):",
                
                min = floor(y_limits[1]),
                
                max = ceiling(y_limits[2]),
                
                value = y_limits
                
              )
              
            )
            
          )
          
        ),
        
        
        
        mainPanel(
          
          width = 9,
          
          plotOutput("plot_post", height = "550px"),
          
          hr(),
          
          h4("Summary for current filters"),
          
          tableOutput("stats_post")
          
        )
        
      )
      
    )
    
  )
  
)

## ---------------------------------------------------------------

## 3. SERVER

## ---------------------------------------------------------------

server <- function(input, output, session) {
  
  
  
  ## ------------ HELPER: generic filter function ------------
  
  filter_data <- function(era = c("pre", "post")) {
    
    era <- match.arg(era)
    
    
    
    if (era == "pre") {
      
      shot_result <- input$shot_result_pre
      
      season      <- input$season_pre
      
      value       <- input$value_pre
      
      team        <- input$team_pre
      
      player      <- input$player_pre
      
      xrange      <- input$xrange_pre
      
      yrange      <- input$yrange_pre
      
    } else {
      
      shot_result <- input$shot_result_post
      
      season      <- input$season_post
      
      value       <- input$value_post
      
      team        <- input$team_post
      
      player      <- input$player_post
      
      xrange      <- input$xrange_post
      
      yrange      <- input$yrange_post
      
    }
    
    
    
    # Start from the correct base dataset (all / made / missed)
    
    base <- pick_dataset(shot_result)
    
    
    
    df <- base %>%
      
      filter(season == season)
    
    
    
    # Filter by shot value (3 / 2 / all)
    
    if (value == "3") {
      
      df <- df %>% filter(three_pt == "TRUE")
      
    } else if (value == "2") {
      
      df <- df %>% filter(three_pt == "FALSE")
      
    }
    
    
    
    # Team filter
    
    if (!is.null(team) && team != "All teams") {
      
      df <- df %>% filter(shot_team == team)
      
    }
    
    
    
    # Player filter
    
    if (!is.null(player) && player != "All players") {
      
      df <- df %>% filter(shooter == player)
      
    }
    
    
    
    # Court window filter
    
    df <- df %>%
      
      filter(
        
        dplyr::between(shot_x, xrange[1], xrange[2]),
        
        dplyr::between(shot_y, yrange[1], yrange[2])
        
      )
    
    
    
    df
    
  }
  
  
  
  ## ------------ PRE ERA REACTIVES & OUTPUTS ------------
  
  
  
  data_pre <- reactive({
    
    filter_data("pre")
    
  })
  
  
  
  output$plot_pre <- renderPlot({
    
    df <- data_pre()
    
    if (nrow(df) == 0) return(NULL)
    
    
    
    # --------- POINT PLOT ----------
    
    if (input$plot_type_pre == "points") {
      
      p <- ggplot(df, aes(x = shot_x, y = shot_y)) +
        
        coord_fixed(xlim = x_limits, ylim = y_limits) +
        
        theme_minimal() +
        
        labs(
          
          title = paste("Shot Locations —", input$season_pre),
          
          x = "Court length (x)",
          
          y = "Court width (y)"
          
        )
      
      
      
      # When we show *both* makes and misses, use ONE color
      
      if (input$shot_result_pre == "all") {
        
        p <- p + geom_point(alpha = 0.35, size = 0.7, color = "black")
        
      } else if (input$shot_result_pre == "made") {
        
        p <- p + geom_point(alpha = 0.45, size = 0.8, color = "darkgreen")
        
      } else {
        
        p <- p + geom_point(alpha = 0.45, size = 0.8, color = "red3")
        
      }
      
      
      
      p
      
      
      
      # --------- DENSITY HEATMAP ----------
      
    } else {
      
      ggplot(df, aes(x = shot_x, y = shot_y)) +
        
        stat_bin2d(bins = 50) +  # higher density = darker
        
        scale_fill_viridis_c(option = "C", trans = "sqrt") +
        
        coord_fixed(xlim = x_limits, ylim = y_limits) +
        
        theme_minimal() +
        
        labs(
          
          title = paste("Shot Density —", input$season_pre),
          
          x = "Court length (x)",
          
          y = "Court width (y)",
          
          fill = "Shot count"
          
        )
      
    }
    
  })
  
  
  
  output$stats_pre <- renderTable({
    
    df <- data_pre()
    
    if (nrow(df) == 0) return(NULL)
    
    
    
    made_flag <- df$shot_outcome == "made"
    
    made_num  <- as.numeric(made_flag)
    
    
    
    # Simple correlations: location vs made
    
    cor_x <- suppressWarnings(cor(df$shot_x, made_num, use = "complete.obs"))
    
    cor_y <- suppressWarnings(cor(df$shot_y, made_num, use = "complete.obs"))
    
    
    
    tibble::tibble(
      
      Metric = c(
        
        "Attempts",
        
        "Makes",
        
        "Misses",
        
        "FG% (makes / attempts)",
        
        "Correlation(shot_x, made)",
        
        "Correlation(shot_y, made)"
        
      ),
      
      Value = c(
        
        nrow(df),
        
        sum(made_flag, na.rm = TRUE),
        
        sum(!made_flag, na.rm = TRUE),
        
        sprintf("%.1f%%", 100 * mean(made_flag, na.rm = TRUE)),
        
        round(cor_x, 3),
        
        round(cor_y, 3)
        
      )
      
    )
    
  })
  
  
  
  
  
  ## ------------ POST ERA REACTIVES & OUTPUTS ------------
  
  
  
  data_post <- reactive({
    
    filter_data("post")
    
  })
  
  
  
  output$plot_post <- renderPlot({
    
    df <- data_post()
    
    if (nrow(df) == 0) return(NULL)
    
    
    
    if (input$plot_type_post == "points") {
      
      p <- ggplot(df, aes(x = shot_x, y = shot_y)) +
        
        coord_fixed(xlim = x_limits, ylim = y_limits) +
        
        theme_minimal() +
        
        labs(
          
          title = paste("Shot Locations —", input$season_post),
          
          x = "Court length (x)",
          
          y = "Court width (y)"
          
        )
      
      
      
      if (input$shot_result_post == "all") {
        
        p <- p + geom_point(alpha = 0.35, size = 0.7, color = "black")
        
      } else if (input$shot_result_post == "made") {
        
        p <- p + geom_point(alpha = 0.45, size = 0.8, color = "darkgreen")
        
      } else {
        
        p <- p + geom_point(alpha = 0.45, size = 0.8, color = "red3")
        
      }
      
      
      
      p
      
      
      
    } else {
      
      ggplot(df, aes(x = shot_x, y = shot_y)) +
        
        stat_bin2d(bins = 50) +
        
        scale_fill_viridis_c(option = "C", trans = "sqrt") +
        
        coord_fixed(xlim = x_limits, ylim = y_limits) +
        
        theme_minimal() +
        
        labs(
          
          title = paste("Shot Density —", input$season_post),
          
          x = "Court length (x)",
          
          y = "Court width (y)",
          
          fill = "Shot count"
          
        )
      
    }
    
  })
  
  
  
  output$stats_post <- renderTable({
    
    df <- data_post()
    
    if (nrow(df) == 0) return(NULL)
    
    
    
    made_flag <- df$shot_outcome == "made"
    
    made_num  <- as.numeric(made_flag)
    
    
    
    cor_x <- suppressWarnings(cor(df$shot_x, made_num, use = "complete.obs"))
    
    cor_y <- suppressWarnings(cor(df$shot_y, made_num, use = "complete.obs"))
    
    
    
    tibble::tibble(
      
      Metric = c(
        
        "Attempts",
        
        "Makes",
        
        "Misses",
        
        "FG% (makes / attempts)",
        
        "Correlation(shot_x, made)",
        
        "Correlation(shot_y, made)"
        
      ),
      
      Value = c(
        
        nrow(df),
        
        sum(made_flag, na.rm = TRUE),
        
        sum(!made_flag, na.rm = TRUE),
        
        sprintf("%.1f%%", 100 * mean(made_flag, na.rm = TRUE)),
        
        round(cor_x, 3),
        
        round(cor_y, 3)
        
      )
      
    )
    
  })
  
}

shinyApp(ui, server)
