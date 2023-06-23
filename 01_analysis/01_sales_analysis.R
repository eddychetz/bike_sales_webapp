
# LIBRARIES ----

# Core
library(tidyverse)
library(tidyquant)
library(here)
library(plotly)
library(timetk)

source(here::here("00_scripts/utilities.R"))

processed_data_tbl <- read_csv(here::here("./00_data/bike_sales/data_wrangled_student/bike_orderlines.csv"))
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
    select(c(1,2,3,5)) %>%
    top_n(3)
