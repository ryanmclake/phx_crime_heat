# Setup / data manipulation
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
# mapping
library(sf)
library(leaflet)
library(leaflet.extras)
# plots
library(ggplot2)
library(plotly)
library(viridis)
library(hrbrthemes)
# shiny stuff
library(shiny)
library(bslib)
library(shinycssloaders)
# predictions
library(prophet)

# Load and preprocess the data
set.seed(2282024)
raw_data_path <- here::here("data/crime_data_raw.csv")
census_geolocated_path <- here::here("data/census_geolocated.csv") # census API data
df_complete_path <- here::here("data/df_complete_forapp.gpkg")
blockgroups_geom_path <- here::here("data/blockgroups_geom.gpkg")

raw_df <- readr::read_csv(raw_data_path) %>%
    rename(block_addr_100 = `100_block_addr`) %>% 
    mutate(zip = as.character(zip)) 

geo_df <- read_csv(census_geolocated_path) %>%
    filter(!is.na(lat),
           !is.na(long),
           `100_block_addr` != "100_block_addr") %>% 
    mutate(zip = as.character(zip),
           lat = as.numeric(lat),
           long = as.numeric(long)
           ) %>%
    rename(block_addr_100 = `100_block_addr`) %>% 
    distinct(block_addr_100, zip, .keep_all = TRUE) %>% 
    select(c("block_addr_100", "zip", "lat", "long"))
 
app_df <- raw_df %>%
    left_join(geo_df, by = c("block_addr_100", "zip")) %>%
    filter(!is.na(lat),!is.na(long)) %>% 
    # filter for now until performance is optimized
    slice_sample(n=1000, replace = F)

# temp <- sf::st_read(df_complete_path)
# app_blockgroups_spatial <- sf::st_read(blockgroups_geom_path)

# temp <- temp %>%
#     mutate(unique_id = row_number(),
#            ucr_crime_category = stringr::str_to_title(ucr_crime_category))
# 
# 
# app_df <- temp %>% 
#     slice_sample(n=5000, replace = F) %>% 
#     st_set_geometry(NULL)
# 
# # Extract spatial data
# app_df_crimes_spatial <- temp %>% 
#     select(geom, unique_id)  # Ensure unique_id is included


#### UI ###########################
setwd("/Users/natebender/Desktop/repo/phx_crime_heat/shiny_folder")

app_sidebar = list(
    selectInput("yearSelect", "Select by Year or Specific Dates", choices = c("All", as.character(2015:2023)), selected = "All", multiple = T),
    sliderInput("dateRange", "",
                min = as.Date(min(app_df$occurred_on, na.rm = TRUE)), 
                max = as.Date(max(app_df$occurred_on, na.rm = TRUE)),
                value = as.Date(range(app_df$occurred_on, na.rm = TRUE)), 
                timeFormat = "%Y-%m-%d",
                step = 1, 
                dragRange = TRUE),
    selectInput("crimeType", "Crime",
                choices = c("All", unique(app_df$ucr_crime_category)),
                selected = "All", multiple = T),
    actionButton("update", "Update"))

ui <- page_navbar(
    theme = bslib::bs_theme(version = 5, bootswatch = "journal"),
    title = "Phoenix Crime App",
    sidebar = bslib::sidebar(app_sidebar, width = 400), # Define sidebar here for consistency across all pages
    nav_spacer(),
### Nav_panel — Map
    # nav_panel(
    #     "Map",
    #     shinycssloaders::withSpinner(leafletOutput("map"), color = "#bf492f", color.background = "white")),
### Nav_panel — Graphs
    nav_panel(
        "Graphs",
        layout_columns(col_widths = c(6, 6, 12),
        shinycssloaders::withSpinner(plotlyOutput("dayOfWeekHeatmap"), color = "#bf492f", color.background = "white"),
        #tableOutput("dayOfWeektable")
        #shinycssloaders::withSpinner(plotlyOutput("crimeGraph"), color = "#bf492f", color.background = "white"),
        #shinycssloaders::withSpinner(plotlyOutput("crimeTypeComparison"), color = "#bf492f", color.background = "white")
        )
    ),
### Nav_panel — Heatmap
    nav_panel(
        "Heatmap",
        #verbatimTextOutput("dataSummary"),
        leafletOutput("spatial_heatmap")
    )
)

#### Server  ###########################
# Update server logic
server <- function(input, output, session) {
    # Create a reactive expression that updates only when the update button is clicked
    filteredData <- eventReactive(input$update, {
        data <- app_df %>%
            filter(occurred_on >= input$dateRange[1] & occurred_on <= input$dateRange[2],
                   ucr_crime_category %in% input$crimeType)
        data
    })
    
    # Automatically update the date slider when year selections change
    observe({
        if("All" %in% input$yearSelect) {
            # If "All" is selected, reset the slider to the full date range
            updateSliderInput(session, "dateRange", 
                              value = range(app_df$occurred_on, na.rm = TRUE))
        } else {
            years <- as.numeric(input$yearSelect)
            startDate <- min(as.Date(paste0(years, "-01-01")))
            endDate <- max(as.Date(paste0(years, "-12-31")))
            updateSliderInput(session, "dateRange", value = c(startDate, endDate))
        }
    })
    
    observeEvent(input$crimeType, {
        # If "All" is selected, select all options
        if ("All" %in% input$crimeType) {
            # Check if "All" is the only option selected or if it was selected last
            if (length(input$crimeType) == 1 || tail(input$crimeType, n = 1) == "All") {
                updateSelectInput(session, "crimeType", 
                                  selected = c("All", unique(app_df$ucr_crime_category)))
            } else {
                # If "All" is selected along with other options but not last, deselect "All"
                updateSelectInput(session, "crimeType", 
                                  selected = setdiff(input$crimeType, "All"))
            }
        }
    })
    
    userSelections <- reactiveValues(
        temporal = NULL,
        spatial = NULL
    )
    
    observe({
        temporalSelection <- event_data("plotly_selected", source = "A")
        if (!is.null(temporalSelection)) {
            # Print the selection to the console for debugging
            print(temporalSelection)
            
            # Update userSelections based on temporal heatmap interaction
            # Here, we directly store the temporalSelection object
            # You might want to extract specific details instead, depending on your needs
         #   userSelections$temporal <- temporalSelection
        }
    })
    
    # Create a reactive expression for spatially joined and aggregated data
    # spatialAggData <- reactive({
    #     # Ensure filteredData is not null before proceeding
    #     if (is.null(filteredData())) {
    #         return(NULL)
    #     }
    #     
    #     # Spatially join filtered data with crime spatial data to get bg geometries
    #     data_with_geom <- filteredData() %>%
    #         left_join(app_blockgroups_spatial, by = "bg_geoid")
    #     
    #     # Aggregate data by bg_geoid to count incidents
    #     incident_counts <- data_with_geom %>%
    #         group_by(bg_geoid) %>%
    #         summarise(incidents = n(), .groups = 'drop')
    #     
    #     # Join aggregated incident counts with block group spatial data
    #     agg_spatial_data <- app_blockgroups_spatial %>%
    #         left_join(incident_counts, by = "bg_geoid")# %>% 
    #         #dplyr::mutate(incidents = tidyr::replace_na(incidents, 0)) # Replace NA in incidents with 0
    #     
    #     data_with_geom <- data_with_geom %>% 
    #         select("bg_geoid", "median_incomeE", "percent_poc")
    #     
    #     agg_spatial_data <- agg_spatial_data %>% 
    #         left_join(data_with_geom, by = "bg_geoid")
    #     
    #     agg_spatial_data
    # })
    
    # Render the map output
    # output$map <- renderLeaflet({
    #     # Check if spatialAggData is not null
    #     if (!is.null(spatialAggData())) {
    #         # Generate color palette based on incident counts
    #         pal <- colorNumeric(palette = "viridis", domain = spatialAggData()$incidents)
    #         
    #         leaflet(data = spatialAggData()) %>%
    #             addTiles() %>%
    #             addPolygons(fillColor = ~pal(incidents),
    #                         color = "#BDBDC3",
    #                         fillOpacity = 0.8,
    #                         weight = 1,
    #                         popup = ~paste("Block Group:", bg_geoid, "<br>Crimes:", incidents, "<br>Median Income:", median_incomeE, "<br>Percent Minority:", percent_poc)) %>%
    #             addLegend(pal = pal, values = ~incidents,
    #                       title = "Crimes",
    #                       opacity = 1)
    #     }
    # })
    # output$map <- renderLeaflet({
    #     pal_incidents <- colorNumeric(palette = "magma", domain = spatialAggData()$incidents)
    #     pal_percent_poc <- colorNumeric(palette = "viridis", domain = spatialAggData()$percent_poc)
    #     pal_income <- colorNumeric(palette = "viridis", domain = spatialAggData()$median_incomeE)
    #     
    #     if (!is.null(spatialAggData())) {
    #         # Base leaflet map
    #         map <- leaflet(data = spatialAggData()) %>%
    #             addTiles() # Add default OpenStreetMap tiles
    #         
    #         # Incidents layer
    #         map <- map %>% addPolygons(fillColor = ~pal_incidents(incidents),
    #                                    fillOpacity = 0.4,
    #                                    color = "#BDBDC3",
    #                                    weight = 1,
    #                                    group = "Incidents",
    #                                    popup = ~paste("Block Group:", bg_geoid, 
    #                                                   "<br>Crimes:", incidents, 
    #                                                   "<br>Median Income:", median_incomeE, 
    #                                                   "<br>Percent Minority:", percent_poc))
    #         
    #         # Percent POC layer
    #         map <- map %>% addPolygons(fillColor = ~pal_income(median_incomeE),
    #                                    fillOpacity = 0.4,
    #                                    color = "#BDBDC3",
    #                                    weight = 1,
    #                                    group = "Median Income",
    #                                    popup = ~paste("Block Group:", bg_geoid, 
    #                                                   "<br>Crimes:", incidents, 
    #                                                   "<br>Median Income:", median_incomeE, 
    #                                                   "<br>Percent Minority:", percent_poc))
    #         
    #         # Legend for Incidents
    #         map <- map %>% addLegend("bottomright", pal = pal_incidents, values = ~incidents,
    #                                  title = "Crimes",
    #                                  opacity = 1,
    #                                  group = "Incidents")
    #         
    #         # Legend for Percent POC
    #         # map <- map %>% addLegend("bottomleft", pal = pal_percent_poc, values = ~percent_poc,
    #         #                          title = "Percent Minority",
    #         #                          opacity = 1,
    #         #                          group = "Percent POC",
    #         #                          labFormat = labelFormat(suffix = "%"))
    #         
    #         # Legend for Income
    #         map <- map %>% addLegend("bottomleft", pal = pal_income, values = ~median_incomeE,
    #                                  title = "Median Income",
    #                                  opacity = 1,
    #                                  group = "Percent POC",
    #                                  labFormat = labelFormat(prefix = "$"))
    #                                  
    #         
    #         # Add layer control
    #         map <- map %>% addLayersControl(overlayGroups = c("Incidents", "Percent POC"),
    #                                         options = layersControlOptions(collapsed = FALSE))
    #         
    #         map
    #     }
    # })
    
# Yearly crime line graph
#     output$crimeGraph <- renderPlotly({
#         # Aggregate filtered data by month
#         monthly_crime_data <- filteredData() %>%
#             mutate(month = as.Date(format(occurred_on, "%Y-%m-01"))) %>% # Ensure 'month' is Date type for scale_x_date
#             group_by(month) %>%
#             summarise(crime_count = n(), .groups = 'drop') %>%
#             arrange(month)
#         
#         date_range <- range(monthly_crime_data$month)
#         years_spanned <- as.numeric(difftime(max(date_range), min(date_range), units = "days")) / 365.25
#         
#         # Decide date_breaks and date_labels based on the number of years spanned
#         if(years_spanned <= 2) {
#             date_breaks <- "1 month"
#             date_labels <- "%b %Y"
#         } else {
#             date_breaks <- "6 months"
#             date_labels <- "%b %Y"
#         }
#         
#         date_breaks <- if(years_spanned <= 2) "1 month" else "6 months"
#         date_labels <- "%b %Y"
#         
#         # Create the ggplot object as before
#         p <- ggplot(monthly_crime_data, aes(x = month, y = crime_count)) +
#             geom_line() +
#             geom_point() +
#             scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
#             geom_vline(xintercept = as.numeric(seq(as.Date(paste0(year(min(monthly_crime_data$month)), "-01-01")), 
#                                                    as.Date(paste0(year(max(monthly_crime_data$month)), "-01-01")), 
#                                                    by = "1 year")), 
#                        linetype = "dashed", 
#                        color = "grey") +
#             theme_minimal() +
#             labs(title = "Total Crime") +
#             xlab("Month") +
#             ylab("Count") +
#             theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
#                   axis.text.y = element_text(size = 12),
#                   axis.title = element_text(size = 14))
#         
#         ggplotly(p)
# })
    
# Temporal heatmap - day of week and time of day
    output$dayOfWeekHeatmap <- renderPlotly({
        # Prepare the data: extract day of the week and hour of the day
        data <- filteredData() %>%
            mutate(dayOfWeek = weekdays(occurred_on),
                   hourOfDay = as.integer(format(occurred_on, "%H")), # Use %H for 24-hour format
                   hourLabel = if_else(hourOfDay == 0, "12 AM",
                                       if_else(hourOfDay < 12, paste(hourOfDay, "AM"),
                                               if_else(hourOfDay == 12, "12 PM", paste(hourOfDay - 12, "PM"))))
                   ) %>%
            group_by(dayOfWeek, hourOfDay) %>%
            summarise(crimeCount = n(),
                      hourLabel = first(hourLabel),
                      .groups = 'drop')

        data$dayOfWeek <- factor(data$dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
        data$hourOfDay <- factor(data$hourLabel, levels = c("12 AM", paste(1:11, "AM"), "12 PM", paste(1:11, "PM")), ordered = TRUE)

        # p <- plot_ly(data, x = ~dayOfWeek, y = ~hourOfDay, z = ~crimeCount, type = "heatmap", colors = "Reds",
        #              colorbar = list(title = "Count")) %>%
        #     layout(title = "Comparison by Day and Hour",
        #            xaxis = list(title = "Day of Week"),
        #            yaxis = list(title = "Hour", categoryorder = "array", categoryarray = c("12 AM", paste(1:11, "AM"), "12 PM", paste(1:11, "PM")))
        #            )
        # p
        
        gg_heatmap <- ggplot(data, aes(x = dayOfWeek, y = hourLabel, fill = crimeCount)) +
            geom_tile() +
            scale_fill_gradient(low = "white", high = "red") +
            labs(x = "Day of Week", y = "Hour of Day", title = "Crime Count by Day and Hour") +
            theme_minimal()
        
        p <- ggplotly(gg_heatmap, source = "A") %>%
            layout(dragmode = "select")
        
        p
    })

    
    # output$dayOfWeektable <- renderTable({
    #     # Prepare the data: extract day of the week and hour of the day
    #     data <- filteredData() %>%
    #         mutate(dayOfWeek = weekdays(occurred_on),
    #                hourOfDay = as.integer(format(occurred_on, "%H")), # Use %H for 24-hour format
    #                hourLabel = if_else(hourOfDay == 0, "12 AM",
    #                                    if_else(hourOfDay < 12, paste(hourOfDay, "AM"),
    #                                            if_else(hourOfDay == 12, "12 PM", paste(hourOfDay - 12, "PM"))))
    #         ) %>%
    #         group_by(dayOfWeek, hourOfDay) %>%
    #         summarise(crimeCount = n(),
    #                   hourLabel = first(hourLabel),
    #                   .groups = 'drop')
    #     
    #     data$dayOfWeek <- factor(data$dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    #     data$hourOfDay <- factor(data$hourLabel, levels = c("12 AM", paste(1:11, "AM"), "12 PM", paste(1:11, "PM")), ordered = TRUE)
    #     
    #     data
    # })
    
    
# Crime type comparison stacked bar graph
    # output$crimeTypeComparison <- renderPlotly({
    #     crime_data <- filteredData() %>%
    #         group_by(date = as.Date(occurred_on), ucr_crime_category) %>%
    #         summarise(crime_count = n(), .groups = 'drop') %>%
    #         ungroup() %>%
    #         # Calculate average crime counts per category
    #         group_by(ucr_crime_category) %>%
    #         mutate(avg_crime_count = mean(crime_count)) %>%
    #         ungroup() %>%
    #         # Reorder ucr_crime_category based on avg_crime_count (to ensure ascending order from bottom)
    #         mutate(ucr_crime_category = reorder(ucr_crime_category, avg_crime_count))
    #     
    #     # Determine the range of dates and calculate the span of years for dynamic x-axis labels
    #     date_range <- range(crime_data$date)
    #     years_spanned <- as.numeric(difftime(max(date_range), min(date_range), units = "days")) / 365.25
    #     
    #     # Decide date_breaks and date_labels based on the number of years spanned
    #     date_breaks <- if(years_spanned <= 2) "1 month" else "6 months"
    #     date_labels <- "%b %Y"
    #     
    #     p <- ggplot(crime_data, aes(x = date, y = crime_count, fill = ucr_crime_category)) + 
    #     geom_bar(stat = "identity", position = "stack") +
    #     scale_fill_viridis(alpha = .8, discrete = TRUE, 
    #                        option = "rocket",
    #                        end = .6) + # closer to 1 is more yellow
    #     theme_minimal() +
    #     labs(title = "Comparison by Category",  # Plot title
    #          fill = "Category",  # Legend title
    #          x = "Date",  # X-axis title
    #          y = "Count") +  # Y-axis title
    #     scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    #     theme(axis.title = element_text(size = 14),
    #           axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    #           axis.text.y = element_text(size = 12),
    #           legend.position = "right",
    #           plot.title = element_text(hjust = 0))
    #     ggplotly(p)
    # })
    
### Navpanel - Spatial Heatmap
    output$spatial_heatmap <- renderLeaflet({
        data <- filteredData() # Use the already filtered data
        
        # Initialize the leaflet map
        leaflet(data) %>%
            addTiles() %>% # Add default OpenStreetMap tiles
            addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~1,
                       blur = 20, max = 0.05, radius = 15, gradient = heat.colors(10))
    })
    
    
    
### Prediction navpanel
    # output$crimeTrendsPrediction <- renderPlotly({
    #     # Assuming `app_df` has at least 'occurred_on' and 'crime_count' columns
    #     crime_data <- prepare_data_for_prophet(app_df)
    #     forecast <- predict_crime_trends(crime_data)
    #     
    #     # Determine the maximum date in the historical data
    #     max_date <- max(crime_data$ds)
    #     last_year_data <- crime_data %>%
    #         filter(ds >= (max_date %m-% years(1)))
    #     
    #     # Plotting
    #     p <- plot_ly() %>%
    #         add_trace(data = last_year_data, x = ~ds, y = ~y, name = 'Actual - Last Year', type = 'scatter', mode = 'lines',
    #                   line = list(color = 'blue')) %>%
    #         add_trace(data = forecast, x = ~ds, y = ~yhat, name = 'Predicted', type = 'scatter', mode = 'lines',
    #                   line = list(color = 'red')) %>%
    #         add_ribbons(data = forecast, x = ~ds, ymin = ~yhat_lower, ymax = ~yhat_upper, name = 'Confidence Interval',
    #                     fillcolor = 'rgba(255,0,0,0.2)', line = list(color = 'transparent')) %>%
    #         layout(title = "Weekly Predicted Crime Trends",
    #                xaxis = list(title = "Week"),
    #                yaxis = list(title = "Count"))
    #     p
    # })
    
}

# Run the app
shinyApp(ui = ui, server = server)





# *******************************************
# *******************************************
####
# Sample Data
# Sample Data
data(mtcars)

# Shiny App
ui <- fluidPage(
    titlePanel("Linked Brushing with Plotly in Shiny"),
    fluidRow(
        column(6, plotlyOutput("scatterplot")),
        column(6, plotlyOutput("histogram"))
    ),
    actionButton("reset", "Reset Plots")
)

server <- function(input, output, session) {
    # Reactive value as a counter for reset clicks, forcing plot reactivity only on reset
    resetClicks <- reactiveVal(0)
    hist_reset <- reactiveVal(TRUE)
    
    # Reactive value to keep track of whether there's a selection
    hasSelection <- reactiveVal(FALSE)
    
    # Observe reset button clicks and increment resetClicks
    observeEvent(input$reset, {
        resetClicks(resetClicks() + 1)
        hasSelection(FALSE)
    })

    observeEvent(input$reset, {
        hist_reset(TRUE)
    })
        
    # Capture selections to update hasSelection status without forcing plot redraw
    observeEvent(event_data("plotly_selected"), {
        hasSelection(TRUE)
        hist_reset(FALSE)
    }, ignoreNULL = TRUE) # Ignore initial NULL to avoid unnecessary reactivity
    
    # Render the scatterplot
    output$scatterplot <- renderPlotly({
        # Use resetClicks to force reactivity on reset, not on selection
        resetClicks()
        
        p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
        ggplotly(p) %>% layout(dragmode = "select")
    })
    
    # Render the histogram based on scatterplot selection
    output$histogram <- renderPlotly({
        # Triggered by either resetClicks or selection, ensuring histogram updates appropriately
        #req(resetClicks(), hasSelection())

        selected_data <- event_data("plotly_selected")
        data <- if (hist_reset()) {
            mtcars 
            } else {
                mtcars[selected_data$pointNumber + 1, ]
            }
        
        p <- ggplot(data, aes(x = hp)) + geom_histogram(bins = 10, fill = "#bf492f", color = "black")
        ggplotly(p)
    })
}

# Run the app
shinyApp(ui = ui, server = server)