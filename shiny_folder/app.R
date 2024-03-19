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
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
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
 app_blockgroups_spatial <- sf::st_read(blockgroups_geom_path)

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

app_sidebar <- list(
    selectInput(
        "yearSelect",
        "Select by Year or Specific Dates",
        choices = c("All", as.character(2015:2023)),
        selected = "All",
        multiple = TRUE
    ),
    sliderInput(
        inputId = "dateRange",
        label = "Select Date Range",
        min = as.Date(min(app_df$occurred_on), "%Y-%m-%d"),
        max = as.Date(max(app_df$occurred_on), "%Y-%m-%d"),
        value = as.Date(range(app_df$occurred_on, na.rm = TRUE), "%Y-%m-%d"), # Select the full range by default
        timeFormat="%Y-%m-%d",
        ticks = F,
        dragRange = TRUE, # Allow selection of a range
    ),
    selectInput(
        "crimeType", 
        "Crime",
        choices = c("All", unique(app_df$ucr_crime_category)),
        selected = "All", 
        multiple = TRUE
    ),
    actionButton("reset", "Reset"),
    actionButton("update", "Update")
)

ui <- bootstrapPage(
    page_navbar(
        theme = bslib::bs_theme(version = 5, bootswatch = "united"),
        title = "Phoenix Crime App",
        nav_spacer(),
        
### Nav_panel — Map
        nav_panel("Map",
                 icon = icon("bar-chart"),
                 # layout_sidebar(
                 #     sidebar = sidebar(app_sidebar),
                 div(class="outer",
                     tags$head(includeCSS("styles.css")),
                     leafletOutput("spatial_heatmap", width="100%", height="100%"),
                     absolutePanel(id = "controls", class = "panel panel-default",
                                   top = 250, left = 55, width = 300, fixed=TRUE,
                                   draggable = TRUE, height = "auto",
                                   app_sidebar)
                 )
                 ),

### Nav_panel — Graphs
        nav_panel("Graphs",
                  layout_sidebar(
                      sidebar = sidebar(id = "controls", class = "panel panel-default", app_sidebar),
                 navset_card_underline(
                     nav_panel("Temporal Heatmap", shinycssloaders::withSpinner(plotlyOutput("dayOfWeekHeatmap", height = "320px"), 
                                                                                color = "#bf492f", color.background = "white")),
                     nav_panel("Type Comparison", shinycssloaders::withSpinner(plotlyOutput("crimeTypeComparison"), 
                                                                               color = "#bf492f", color.background = "white")),
                     nav_panel("Total Crime", shinycssloaders::withSpinner(plotlyOutput("crimeGraph"), color = "#bf492f", color.background = "white"))
                     )
                 )
        )
)
)


#### Server  ###########################
# Update server logic
server <- function(input, output, session) {
    
    filteredData <- eventReactive(input$update, {
        # First, handle the 'yearSelect' input to filter by years
        if ("All" %in% input$yearSelect) {
            yearFilteredData <- app_df
        } else {
            selectedYears <- as.numeric(input$yearSelect)
            yearFilteredData <- app_df %>%
                filter(year(occurred_on) %in% selectedYears)
        }
        
        print(input$dateRange[1])
        print(input$dateRange[2])
        
        # Then, apply the existing date range and crime type filters
        data <- yearFilteredData %>%
            filter(occurred_on >= input$dateRange[1] &
                       occurred_on <= input$dateRange[2],
                   ucr_crime_category %in% input$crimeType)
        data
    })
    
    # Automatically update the date slider when year selections change
    observe({
        if("All" %in% input$yearSelect) {
            updateSliderInput(session, "dateRange",
                              value = range(app_df$occurred_on, na.rm = TRUE))
        } else {
            years <- as.numeric(input$yearSelect)
            if(length(years) > 0 && !all(is.na(years))) {
                startDate <- as.POSIXct(paste0(min(years), "-01-01"))
                endDate <- as.POSIXct(paste0(max(years), "-12-31"))
                print(startDate)
                print(endDate)
                updateSliderInput(session, "dateRange",
                                  value = c(startDate, endDate))
            }
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
    
    ### Navpanel - Spatial Heatmap
    output$spatial_heatmap <- renderLeaflet({
        data <- filteredData()
        
        if (!is.null(data) && nrow(data) > 0) {
            leaflet() %>%
                addTiles(group = "Default") %>%
                addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri") %>% 
                addPolygons(data = app_blockgroups_spatial,
                            weight = .5, # Line weight
                            color = "#444444", # Line color
                            opacity = .2, # Line opacity
                            fillOpacity = 0.2, # Fill opacity
                            fillColor = "#444444") %>% 
                addHeatmap(data = data, lng = ~long, lat = ~lat, intensity = ~1,
                           blur = 20, max = 0.05, radius = 15, gradient = heat.colors(10)) %>% 
                addLayersControl(baseGroups = c("Default", "Esri"),
                                 #overlayGroups = c("Polygons"),
                                 options = layersControlOptions(collapsed = T))
        } else {
            leaflet() %>%
                addTiles()
        }
    })

# Yearly crime line graph
    output$crimeGraph <- renderPlotly({
        # Aggregate filtered data by month
        monthly_crime_data <- filteredData() %>%
            mutate(month = as.Date(format(occurred_on, "%Y-%m-01"))) %>% # Ensure 'month' is Date type for scale_x_date
            group_by(month) %>%
            summarise(crime_count = n(), .groups = 'drop') %>%
            arrange(month)

        date_range <- range(monthly_crime_data$month)
        years_spanned <- as.numeric(difftime(max(date_range), min(date_range), units = "days")) / 365.25

        # Decide date_breaks and date_labels based on the number of years spanned
        if(years_spanned <= 2) {
            date_breaks <- "1 month"
            date_labels <- "%b %Y"
        } else {
            date_breaks <- "6 months"
            date_labels <- "%b %Y"
        }

        date_breaks <- if(years_spanned <= 2) "1 month" else "6 months"
        date_labels <- "%b %Y"

        # Create the ggplot object as before
        p <- ggplot(monthly_crime_data, aes(x = month, y = crime_count)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
            geom_vline(xintercept = as.numeric(seq(as.Date(paste0(year(min(monthly_crime_data$month)), "-01-01")),
                                                   as.Date(paste0(year(max(monthly_crime_data$month)), "-01-01")),
                                                   by = "1 year")),
                       linetype = "dashed",
                       color = "grey") +
            theme_minimal() +
            labs(title = "Total Crime") +
            xlab("Month") +
            ylab("Count") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                  axis.text.y = element_text(size = 12),
                  axis.title = element_text(size = 14))

        ggplotly(p)
})
    
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

        data$dayOfWeek <- factor(data$dayOfWeek, levels = c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun"))
        data$hourOfDay <- factor(data$hourLabel, levels = c("12 AM", paste(1:11, "AM"), "12 PM", paste(1:11, "PM")), ordered = TRUE)

        p <- plot_ly(data, x = ~dayOfWeek, y = ~hourOfDay, z = ~crimeCount, type = "heatmap", colors = "Reds",
                     colorbar = list(title = "Count")) %>%
            layout(title = "Comparison by Day and Hour",
                   xaxis = list(title = "Day of Week"),
                   yaxis = list(title = "Hour", categoryorder = "array", categoryarray = c("12 AM", paste(1:11, "AM"), "12 PM", paste(1:11, "PM")))
                   )
        p
    })
    
    
# Crime type comparison stacked bar graph
    output$crimeTypeComparison <- renderPlotly({
        crime_data <- filteredData() %>%
            group_by(date = as.Date(occurred_on), ucr_crime_category) %>%
            summarise(crime_count = n(), .groups = 'drop') %>%
            ungroup() %>%
            # Calculate average crime counts per category
            group_by(ucr_crime_category) %>%
            mutate(avg_crime_count = mean(crime_count)) %>%
            ungroup() %>%
            # Reorder ucr_crime_category based on avg_crime_count (to ensure ascending order from bottom)
            mutate(ucr_crime_category = reorder(ucr_crime_category, avg_crime_count))

        # Determine the range of dates and calculate the span of years for dynamic x-axis labels
        date_range <- range(crime_data$date)
        years_spanned <- as.numeric(difftime(max(date_range), min(date_range), units = "days")) / 365.25

        # Decide date_breaks and date_labels based on the number of years spanned
        date_breaks <- if(years_spanned <= 2) "1 month" else "6 months"
        date_labels <- "%b %Y"

        p <- ggplot(crime_data, aes(x = date, y = crime_count, fill = ucr_crime_category)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_viridis(alpha = .8, discrete = TRUE,
                           option = "rocket",
                           end = .6) + # closer to 1 is more yellow
        theme_minimal() +
        labs(title = "Comparison by Category",  # Plot title
             fill = "Category",  # Legend title
             x = "Date",  # X-axis title
             y = "Count") +  # Y-axis title
        scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
        theme(axis.title = element_text(size = 14),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              legend.position = "right",
              plot.title = element_text(hjust = 0))
        ggplotly(p)
    })
    

    
}

# Run the app
shinyApp(ui = ui, server = server)
