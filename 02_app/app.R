library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridlayout)
library(gt)
library(gtExtras)

# LIBRARIES ----

# Core
library(tidyverse)
library(tidyquant)
library(here)
library(plotly)
library(timetk)

# SCRIPTS ----
# source(here::here("./02_app/sp"))
source(here::here("./02_app/shinyapps_deploy.R"))
source(here::here("./02_app/theme_specialized.R"))

# Data
processed_data_tbl <- read_csv(here("./02_app/bike_orderlines.csv"))

# User Interface ----
ui <- navbarPage(
    tags$head(
        tags$style(HTML("
        
        /* Custom CSS styles */
      .sidebar {
        width: 300px; /* Adjust the width as needed */
      }
      .navbar-brand {
          display: flex;
      }
      nav.navbar {
          background-color: #2c3e50;
      }
      #area0 .card-body {
          background-color: #2e3E50;
      }
    "))
    ),
    
    title = "Sales Dashboard",
    
    collapsible = FALSE,
    
    theme = app_theme,
    
    shiny::tabPanel(
        
        title = "Exploratory Data Analysis",
        
        grid_container(
            layout = c(
                "area0 area1 area3",
                "area0 area1 area2",
                "area0 area4 area2",
                "area5 area5 area5" 
            ),
            
            row_sizes = c(
                "0.13fr",
                "0.33fr",
                "0.32fr",
                "0.19fr"
            ),
            
            col_sizes = c(
                "0.64fr",
                "1.23fr",
                "1.39fr"
            ),
            
            gap_size = "1rem",
            
            grid_card(
                area = "area0",
                
                # Input 1: Date Range
                dateRangeInput(
                    inputId   = "date_range",
                    label     = h4("Date Range"),
                    start     = as_date("2011-01-01"),
                    end       = as_date("2016-01-01"),
                    min       = as_date("2011-01-01"),
                    max       = as_date("2016-01-01"),
                    startview = "month",
                    width     = "100%"
                ),
                
                hr(),
                
                # Input 2: Product Category
                checkboxGroupInput(
                    inputId   = "category_1",
                    label     = h4("Product Type"),
                    choices   = list(Mountain   = "Mountain",
                                     Road       = "Road"),
                    selected  = c("Mountain", "Road"),
                    inline    = F
                )
            ),
            
            grid_card(
                area = "area1",
                
                # Visualization 1: Geo-Spatial plot
                plotlyOutput(
                    outputId = "geospatial_plot",
                    width    = "100%",
                    height   = "350px"
                )
            ),
            
            grid_card(
                area = "area2",
                
                # Visualization 2: Time Series plot
                plotlyOutput(
                    outputId = "timeseries_plot",
                    width    = "100%",
                    height   = "25%"
                )
            ),
            
            grid_card(
                area = "area3",
                
                # Time Unit Controls
                radioGroupButtons(
                    inputId        = "time_unit",
                    label          = h6("Time Unit"),
                    choices        = list(
                        Daily      = "day",
                        Weekly     = "week",
                        Monthly    = "month",
                        Quarterly  = "quarter",
                        Yearly     = "year"
                    ),
                    selected       = "month",
                    direction      = "horizontal",
                    justified      = TRUE,
                    width          = "100%"
                )
            ),
            
            grid_card(
                area = "area4",
               
                gt_output("geo_table")
            ),
            
            grid_card(
              area = "area5",
              h6("Summary"),
              textOutput("geo_text")
            )
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    # Create filtered data based on controls
    processed_data_filtered_tbl <- reactive({
        
        processed_data_tbl %>%
            
            # input 1
            filter(order_date %>% between(
                left = input$date_range[1],
                right = input$date_range[2]
            )) %>%
            
            # Input 2
            filter(category_1 %in% input$category_1)
        
    })
    
    # Geo-Spatial plot
    output$geospatial_plot <- renderPlotly({
        
        processed_data_filtered_tbl() %>%
            
            aggregate_geospatial() %>%
            
            plot_geospatial() %>%
            
            layout(
                title         = list(
                    text      = "<b>Sales By State</b>", 
                    font      = list(
                        size  = 20, 
                        color = "#000000"
                                   ),
                    xanchor   = "left" # Align the title to the left
                ),
                margin        = list(
                    t = 60, 
                    l = 50) # Adjust the top margin for the title
            )
    })
    
    # Time Series plot
    output$timeseries_plot <- renderPlotly({
        
        processed_data_filtered_tbl() %>%
            
            aggregate_time_series(
                time_unit = input$time_unit
                ) %>%
            
            plot_time_series() %>%
            
            layout(
                title          = list(
                    text       = "<b>Sales Over Time</b>",
                    font       = list(
                        size   = 20, 
                        color  = "black"
                        ),
                    xanchor    = "left" # Align the title to the left
                ),
                margin         = list(
                    t = 60, 
                    l = 50) # Adjust the top and left margins
            )
    })
    
    # Display table
    output$geo_table <- render_gt({
        
      # Table 1: Text inputs
      start_date = input$date_range[1]
      end_date = input$date_range[2]
      
      # Table 1: Prepare summary table
      summary_table <- processed_data_filtered_tbl() %>%
        
        group_by(state, city, bikeshop_name) %>%
        
        summarise(Total_Sales = sum(total_price)) %>%
        
        ungroup() %>%
        
        arrange(desc(Total_Sales)) %>%
        
        select(state, city, bikeshop_name, Total_Sales) %>%
        
        slice_head(n = 5)
      
      # Display table 
      summary_table %>% 
        
        mutate(
          above_average = Total_Sales,
          target_col = mean(Total_Sales)
          )%>%
        
        gt() %>%
        
        tab_header(
          title = "Top 5 Locations and BikeShops by Sales Revenue", 
          subtitle = glue::glue("{start_date} to {end_date}")
        ) %>%
        
        gt_color_rows(Total_Sales, palette = c("lightblue", "darkblue")) %>%
       
        fmt_currency() %>%
        gt_theme_538() %>%
        gt_plt_bullet(
          column = above_average,
          target = target_col,
          width = 20,
          palette = c("lightblue", "blue"))
    })
    
    output$geo_text <- renderText({
      
      # Table 1: Text inputs
      start_date = input$date_range[1]
      end_date = input$date_range[2]
      
      summary_text <- processed_data_filtered_tbl() %>%
        
        group_by(state, city, bikeshop_name) %>%
        
        summarise(Total_Sales = sum(total_price)) %>%
        
        ungroup() %>%
        
        arrange(desc(Total_Sales)) %>%
        
        select(state, city, bikeshop_name, Total_Sales) %>%
        
        slice_head(n = 5)
      
      city_1 <- summary_text$city[1]
      city_2 <- summary_text$city[2]
      shop_1 <- summary_text$bikeshop_name[1]
      shop_2 <- summary_text$bikeshop_name[2]
      sales_1 <- summary_text$Total_Sales[1]
      sales_2 <- summary_text$Total_Sales[2]
     
      glue::glue("
                 In the period from {start_date} to {end_date}, the top two cities with the highest total sales in their bikeshops were {city_1} and 
                  {city_2}. The leading bikeshop in {city_1} was called {shop_1} which generated impressive total sales of {scales::dollar(sales_1)}. 
                    Meanwhile, {city_2} boasted the {shop_2} accumulating significant sales of {scales::dollar(sales_2)}. These two cities and their bikeshops 
                      played a crucial role in the biking industry during the mentioned timeframe, contributing significantly to the overall success and 
                        growth of the market.
                 ")
    })
}

shinyApp(ui, server)
