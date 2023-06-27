
# LIBRARIES ----

# Core
library(tidyverse)
library(tidyquant)
library(here)
library(plotly)
library(timetk)
library(gt)
library(gtExtras)

source(here::here("00_scripts/utilities.R"))

processed_data_tbl <- read_csv(here::here("./00_data/bike_orderlines.csv"))
processed_data_tbl

#  USER SELECTIONS ----

processed_data_filtered_tbl <- processed_data_tbl %>%
    
    # input 1
    filter(order_date %>% between(
        left = as_date("2011-01-01"),
        right = as_date("2016-01-01"))
    ) %>%
    
    # Input 2
    filter(category_1 %in% "Mountain")

# VISUALIZATION 1 ----

processed_data_filtered_tbl %>%
    aggregate_geospatial() %>%
    plot_geospatial()

# VISUALIZATION 2 ----

# Input 3
processed_data_filtered_tbl %>%
    
    aggregate_time_series(time_unit = "month") %>%
    plot_time_series()

# TEXT VISUALIZATION ----

# Text 1
processed_data_filtered_tbl %>%
    summarize(Orders = n())

processed_data_filtered_tbl %>%
    summarize(Sales = sum(total_price))

processed_data_filtered_tbl %>%
    
    group_by(state, city, bikeshop_name) %>%
    summarise(Total_Sales = sum(total_price)) %>%
    ungroup() %>%
    arrange(desc(Total_Sales)) %>%
    mutate(sales_text = scales::dollar(Total_Sales)) %>%
    select(c( state, city, bikeshop_name, sales_text)) %>%
    slice_head(n = 5)

# Table 1

summary_table <- processed_data_filtered_tbl %>%
    
    group_by(state, city, bikeshop_name) %>%
    
    summarise(Total_Sales = sum(total_price)) %>%
    
    ungroup() %>%
    
    arrange(desc(Total_Sales)) %>%
    
    # mutate(sales_text = scales::dollar(Total_Sales)) %>%
    
    select(state, city, bikeshop_name, Total_Sales) %>%
    
    slice_head(n = 5)

start_date = as_date("2014-01-01") # input$date_range[1]
end_date = as_date("2016-12-01") # input$date_range[2]

    
# Display table 
summary_table %>%
    
    gt() %>%
    
    tab_header(
        title = "Top 5 Locations and BikeShops by Sales Revenue", 
        subtitle = glue::glue("{start_date} to {end_date}")
    ) %>%
    gt_hulk_col_numeric(Total_Sales) %>%
    fmt_currency() %>%
    gt_plt_bar(Total_Sales, keep_column = T, width = 40)

# Geo-text summary
# Table 1: Text inputs
start_date = as_date("2014-01-01") # input$date_range[1]
end_date = as_date("2016-12-01") # input$date_range[2]

summary_text <- processed_data_filtered_tbl %>%
    mutate(sales_1= scales::)
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

glue::glue("In the period from {start_date} to {end_date}, the top two cities with the highest total sales in their bikeshops were {city_1} and {city_2}. The leading bikeshop in {city_1} was called {shop_1} which generated impressive total sales of {sales_1}. Meanwhile, {city_2} boasted the {shop_2} accumulating significant sales of {sales_2}. These two cities and their bikeshops played a crucial role in the biking industry during the mentioned timeframe, contributing significantly to the overall success and growth of the market.")

