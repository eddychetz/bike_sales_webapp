
# Aggregate geo-spatial data ----

aggregate_geospatial <- function(data){
    data %>%
        group_by(state) %>%
        summarize(total_sales = sum(total_price)) %>%
        ungroup() %>%
        mutate(sales_text = scales::dollar(total_sales))
}

# Aggregate time series data ----

aggregate_time_series <- function(data, time_unit){
    
    output_tbl <- data %>%
        
        mutate(date = floor_date(order_date, unit = time_unit)) %>%
        
        group_by(date) %>%
        summarize(total_sales = sum(total_price)) %>%
        ungroup() %>%
        
        mutate(sales_text = str_glue("Date: {date}
                                     Revenue: {scales::dollar(total_sales)}"))
    
    output_tbl
}

# Plot Geo-Spatial data ----

plot_geospatial <- function(data){
    
    data %>%
        plot_ly(
            type = 'choropleth',
            locations = ~state,
            locationmode = 'USA-states',
            colorscale = "Cividis", 
            z = ~total_sales,
            text = ~paste("State: ", state, "<br>Sales:", sales_text),
            hoverinfo = "text", # Only show text info in the hover tooltip
            height = "45px"
        ) %>%
        layout(
            geo = list(
                scope = 'usa',
                showframe = T
            ),
            hoverlabel = list(
                bgcolor = "grey0", 
                font = list(
                    size = 20, 
                    color = "white"
                )
            )
        ) %>%
        colorbar(title = "Sales")
}

# Plot time series data
plot_time_series <- function(data){
    
    g <- data %>%
        
        ggplot(aes(date, total_sales)) +
        
        geom_line(color = "#2c3e50") +
        geom_point(color = "#2c3e50", size = 0.1) +
        geom_smooth(method = "loess", span = 0.2) +
        
        theme_tq() +
        expand_limits(y = 0) +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(x = "", y = "")
    
    ggplotly(g)
}