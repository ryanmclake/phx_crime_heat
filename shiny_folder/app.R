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
    actionButton("reset", "Reset"),
    actionButton("update", "Update"))

ui <- page_navbar(
    theme = bslib::bs_theme(version = 5, bootswatch = "journal"),
    title = "Phoenix Crime App",
    sidebar = bslib::sidebar(app_sidebar, width = 400), # Define sidebar here for consistency across all pages
    nav_spacer(),
### Nav_panel — Map
### Nav_panel — Graphs
    nav_panel(
        "Graphs",
        layout_columns(col_widths = c(12, 12),
        shinycssloaders::withSpinner(plotlyOutput("dayOfWeekHeatmap", height = "320px"), color = "#bf492f", color.background = "white"),
        #leafletOutput("spatial_heatmap")
        #shinycssloaders::withSpinner(plotlyOutput("crimeGraph"), color = "#bf492f", color.background = "white"),
        #shinycssloaders::withSpinner(plotlyOutput("crimeTypeComparison"), color = "#bf492f", color.background = "white")
        )
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
    
    # Reactive value as a counter for reset clicks, forcing plot reactivity only on reset
    resetClicks <- reactiveVal(0)
    hist_reset <- reactiveVal(TRUE)
    hasSelection <- reactiveVal(FALSE)
    
    # Observe reset button clicks and increment resetClicks
    observeEvent(input$reset, {
        resetClicks(resetClicks() + 1)
        hasSelection(FALSE)
        hist_reset(TRUE)
    })
    
    # Capture selections to update hasSelection status without forcing plot redraw
    observeEvent(event_data("plotly_selected"), {
        selected <- event_data("plotly_selected")
        if (!is.null(selected)) {
            hasSelection(TRUE)
            hist_reset(FALSE)
        }
    }, ignoreNULL = TRUE) # Important for initialization phase
    
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

        data$dayOfWeek <- factor(data$dayOfWeek, levels = c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun"))
        data$hourOfDay <- factor(data$hourLabel, levels = c("12 AM", paste(1:11, "AM"), "12 PM", paste(1:11, "PM")), ordered = TRUE)

        p <- plot_ly(data, x = ~dayOfWeek, y = ~hourOfDay, z = ~crimeCount, type = "heatmap", colors = "Reds",
                     colorbar = list(title = "Count")) %>%
            layout(title = "Comparison by Day and Hour",
                   xaxis = list(title = "Day of Week"),
                   yaxis = list(title = "Hour", categoryorder = "array", categoryarray = c("12 AM", paste(1:11, "AM"), "12 PM", paste(1:11, "PM")))
                   )
        p
        
        # resetClicks()
        # 
        # gg_heatmap <- ggplot(data, aes(x = dayOfWeek, y = hourLabel, fill = crimeCount)) +
        #     geom_tile() +
        #     scale_fill_gradient(low = "white", high = "red") +
        #     labs(x = "Day of Week", y = "Hour of Day", title = "Crime Count by Day and Hour") +
        #     theme_minimal()
        # 
        # p <- ggplotly(gg_heatmap) %>%
        #     layout(dragmode = "select")
        # p
    })

    observe({
        selectedData <- event_data("plotly_selected")
        if (!is.null(selectedData)) {
            print(str(selectedData))
            print(head(selectedData))
        }
    })
    
    
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
        # 'data' is a reactive expression, so it needs to be called as a function to retrieve its value
        reactiveData <- reactive({
            selectedData <- event_data("plotly_selected")
            if (hist_reset() || is.null(selectedData)) {
                return(filteredData())
            } else {
                # Ensure this matches the expected structure of your selectedData
                return(filteredData()[selectedData$pointIndex + 1, ])
            }
        })
        
        # Initialize the leaflet map with reactive data
        leaflet() %>%
            addTiles() %>% # Add default OpenStreetMap tiles
            # Use 'reactiveData()' to get the current value of the reactive expression
            addHeatmap(data = reactiveData(), lng = ~long, lat = ~lat, intensity = ~1,
                       blur = 20, max = 0.05, radius = 15, gradient = heat.colors(10))
    })
    
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
    hasSelection <- reactiveVal(FALSE)
    
    # Observe reset button clicks and increment resetClicks
    observeEvent(input$reset, {
        resetClicks(resetClicks() + 1)
        hasSelection(FALSE)
        hist_reset(TRUE)
    })
        
    # Capture selections to update hasSelection status without forcing plot redraw
    observeEvent(event_data("plotly_selected"), {
        hasSelection(TRUE)
        hist_reset(FALSE)
    }, ignoreNULL = TRUE)
    
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



############
#devtools::install_github("rstudio/crosstalk")
#install.packages("crosstalk")
#install.packages("d3scatter")
#devtools::install_github("jcheng5/d3scatter")
library(d3scatter)
library(crosstalk)

shared_mtcars <- SharedData$new(mtcars)
bscols(widths = c(3,NA,NA),
       list(
           filter_checkbox("cyl", "Cylinders", shared_mtcars, ~cyl, inline = TRUE),
           filter_slider("hp", "Horsepower", shared_mtcars, ~hp, width = "100%"),
           filter_select("auto", "Automatic", shared_mtcars, ~ifelse(am == 0, "Yes", "No"))
       ),
       d3scatter(shared_mtcars, ~wt, ~mpg, ~factor(cyl), width="100%", height=250),
       d3scatter(shared_mtcars, ~hp, ~qsec, ~factor(cyl), width="100%", height=250)
)

shared_quakes <- SharedData$new(quakes[sample(nrow(quakes), 100),])
bscols(
    leaflet(shared_quakes, width = "100%", height = 300) %>%
        addTiles() %>%
        addMarkers(),
    d3scatter(shared_quakes, ~depth, ~mag, width = "100%", height = 300)
)

shared_app_df <- SharedData$new(app_df)
bscols(
    d3scatter(shared_app_df, ~depth, ~mag, width = "100%", height = 300),
    leaflet(shared_app_df, width = "100%", height = 300) %>%
        addTiles() %>%
        addMarkers()
)


#######

library(shiny)
library(plotly)
library(leaflet)
library(dplyr)

ui <- fluidPage(
    titlePanel("Crime Data Visualization"),
    sidebarLayout(
        sidebarPanel(
            selectInput("dayOfWeekSelect", "Select Day of Week",
                        choices = c("All", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                        selected = "All", multiple = TRUE),
            sliderInput("hourOfDaySlider", "Select Hour of Day",
                        min = 0, max = 23, value = c(0, 23), step = 1,
                        ticks = FALSE, animate = TRUE)
        ),
        mainPanel(
            plotlyOutput("dayOfWeekHeatmap"), # Temporal Heatmap in its existing location
            navset_card_underline(
                id = "tabs", # ID for the navigation set
                nav("Spatial Heatmap", leafletOutput("spatial_heatmap")), # Spatial Heatmap tab
                nav("Leaflet Map", leafletOutput("leafletMap")) # Leaflet map tab (if you want another regular map)
            )
        )
    )
)

server <- function(input, output) {
    filteredData <- reactive({
        selectedDays <- input$dayOfWeekSelect
        # Adjust for "All" selection
        if("All" %in% selectedDays) {
            selectedDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
        }
        
        # Prepare your dataset
        app_df %>%
            mutate(dayOfWeek = factor(weekdays(occurred_on, abbreviate = FALSE),
                                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                   hourOfDay = as.integer(format(occurred_on, "%H"))) %>%
            filter(dayOfWeek %in% selectedDays, # Use the possibly adjusted list of days
                   hourOfDay >= input$hourOfDaySlider[1],
                   hourOfDay <= input$hourOfDaySlider[2])
    })
    
    output$dayOfWeekHeatmap <- renderPlotly({
        # Define the levels for days of the week to ensure correct order
        dayLevels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
        
        # Generate all combinations of dayOfWeek and hourOfDay
        allCombinations <- expand.grid(
            dayOfWeek = factor(dayLevels, levels = dayLevels),
            hourOfDay = 0:23
        )
        
        allCombinations$hourLabel <- sprintf("%02d:00", allCombinations$hourOfDay)
        
        # Process your filtered data
        data <- filteredData() %>%
            mutate(dayOfWeek = factor(weekdays(occurred_on, abbreviate = FALSE), levels = dayLevels),
                   hourOfDay = as.integer(format(occurred_on, "%H")),
                   hourLabel = sprintf("%02d:00", hourOfDay)) %>%
            group_by(dayOfWeek, hourLabel, hourOfDay) %>%
            summarise(crimeCount = n(), .groups = 'drop') %>%
            ungroup()
        
        # Ensure all day and hour combinations are present
        data <- right_join(allCombinations, data, by = c("dayOfWeek", "hourLabel", "hourOfDay"))
        
        # Replace NA counts with 0
        data$crimeCount[is.na(data$crimeCount)] <- 0
        
        # Plot
        plot_ly(data, x = ~dayOfWeek, y = ~hourLabel, z = ~crimeCount, type = "heatmap", colors = "Reds") %>%
            layout(title = "Crime Count by Day and Hour",
                   xaxis = list(title = "Day of Week", tickangle = -45, type = "category"),
                   yaxis = list(title = "Hour of Day", dtick = 1))
    })
    
    output$leafletMap <- renderLeaflet({
        data <- filteredData()
        
        leaflet(data) %>%
            addTiles() %>%
            addCircleMarkers(~long, ~lat, popup = ~paste(dayOfWeek, sprintf("%02d:00", hourOfDay)))
    })
    
    output$spatial_heatmap <- renderLeaflet({
        # Use 'filteredData()' directly as it's already a reactive expression
        leaflet(data = filteredData()) %>%
            addTiles() %>%
            addHeatmap(lng = ~long, lat = ~lat, intensity = ~1,
                       blur = 20, max = 0.05, radius = 15, 
                       gradient = heat.colors(10))
    })
}

shinyApp(ui = ui, server = server)