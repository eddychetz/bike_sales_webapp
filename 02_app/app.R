library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridlayout)
library(gt)

# LIBRARIES ----

# Core
library(tidyverse)
library(tidyquant)
library(here)
library(plotly)
library(timetk)

# SCRIPTS ----
source(here::here("./00_themes/theme_specialized.R"))
source(here::here("./00_scripts/shinyapps_deploy.R"))
source(here::here("./00_scripts/utilities.R"))

# Data
processed_data_tbl <- read_csv(here::here("./00_data/bike_orderlines.csv"))
processed_data_tbl

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
    tabPanel(
        title = "Exploratory Data Analysis",
        grid_container(
            layout = c(
                "area0 area1 area3",
                "area0 area1 area2",
                "area0 area4 area4"
            ),
            row_sizes = c(
                "0.47fr",
                "1.23fr",
                "0.75fr"
            ),
            col_sizes = c(
                "0.68fr",
                "1.23fr",
                "1.29fr"
            ),
            gap_size = "1rem",
            grid_card(
                area = "area0",
                dateRangeInput(
                    inputId = "date_range",
                    label   = h4("Date Range"),
                    start     = as_date("2011-01-01"),
                    end     = as_date("2016-01-01"),
                    min     = as_date("2011-01-01"),
                    max     = as_date("2016-01-01"),
                    startview = "month",
                    width   = "100%"
                ),
                hr(),
                checkboxGroupInput(
                    inputId   = "category_1",
                    label     = h4("Product Type"),
                    choices   = list(Mountain = "Mountain",
                                     Road       = "Road"),
                    selected  = c("Mountain", "Road"),
                    inline    = F
                )
            ),
            grid_card(
                area = "area1",
                plotlyOutput(
                    outputId = "geospatial_plot",
                    width    = "100%",
                    height   = "350px"
                )
            ),
            grid_card(
                area = "area2",
                plotlyOutput(
                    outputId = "timeseries_plot",
                    width    = "100%",
                    height   = "400px"
                )
            ),
            grid_card(
                area = "area3",
                radioGroupButtons(
                    inputId = "time_unit",
                    label = h6("Time Unit"),
                    choices = list(
                        Daily = "day",
                        Weekly = "week",
                        Monthly = "month",
                        Quarterly = "quarter",
                        Yearly = "year"
                    ),
                    selected = "month",
                    direction = "horizontal",
                    justified = TRUE,
                    width = "100%"
                )
            ),
            grid_card(
                area = "area4",
                tags$caption(h4("Top 3 Locations and BikeShops by Sales")),
                tableOutput("geo_summary")
            )
        )
    )
)

# Define server logic
server <- function(input, output) {
    
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
    
    output$geospatial_plot <- renderPlotly({
        processed_data_filtered_tbl() %>%
            
            aggregate_geospatial() %>%
            plot_geospatial() %>%
            
            layout(
                title = list(
                    text = "<b>Sales By State</b>", 
                    font = list(size = 20, color = "#000000"),
                    xanchor = "left" # Align the title to the left
                ),
                margin = list(t = 60, l = 50) # Adjust the top margin for the title
            )
    })
    
    output$timeseries_plot <- renderPlotly({
        processed_data_filtered_tbl() %>%
            
            aggregate_time_series(time_unit = input$time_unit) %>%
            plot_time_series() %>%
            
            layout(
                title = list(
                    text = "<b>Sales Over Time</b>",
                    font = list(size = 20, color = "black"),
                    xanchor = "left" # Align the title to the left
                ),
                margin = list(t = 60, l = 50) # Adjust the top and left margins
            )
    })
    
    output$geo_summary <- renderTable({
        
        summary_table <- processed_data_filtered_tbl() %>%
            
            group_by(state, city, bikeshop_name) %>%
            summarise(Total_Sales = sum(total_price)) %>%
            ungroup() %>%
            arrange(desc(Total_Sales)) %>%
            mutate(sales_text = scales::dollar(Total_Sales)) %>%
            select(c( state, city, bikeshop_name, sales_text)) %>%
            slice_head(n = 3)
        
        colnames(summary_table) <- c("State", "City", "BikeShop", "Total Sales")
        
        summary_table %>% 
            gt() %>%
            tab_style(
                style = cell_borders(
                    sides = list("top", "bottom"),
                    color = "#3c2e50",
                    style = "solid",
                    weight = px(1.0)
                ),
                locations = cells_body(
                    rows = everything()
                )
            )
    })
}

shinyApp(ui, server)
