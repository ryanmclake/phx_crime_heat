library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(here)
library(plotly)
library(ggridges)
library(viridis)
library(hrbrthemes)

# Load and preprocess the data
raw_data_path <- here::here("data/crime_data_raw.csv")
census_geolocated_path <- here::here("data/census_geolocated.csv") # census API data

raw_df <- readr::read_csv(raw_data_path) %>% 
    mutate(zip = as.character(zip))

geo_df <- read_csv(census_geolocated_path) %>%
    mutate(zip = as.character(zip),
           lat = as.numeric(lat),
           long = as.numeric(long)) %>%  
    select(c("100_block_addr", "zip", "lat", "long"))

app_df <- raw_df %>% 
    left_join(geo_df, by = c("100_block_addr", "zip")) %>% 
    # filter to just the geolocated addresses for now
    filter(!is.na(lat),!is.na(long)) %>% 
    slice_sample(n=10000, replace = F) # filter for now until performance is optimized


#### UI ###########################
# Update UI definition

app_sidebar = list(
    selectInput("yearSelect", "Select Year", choices = c("All", as.character(2015:2023)), selected = "All", multiple = T),
    sliderInput("dateRange", "Date",
                min = min(app_df$occurred_on, na.rm = TRUE), 
                max = max(app_df$occurred_on, na.rm = TRUE),
                value = range(app_df$occurred_on, na.rm = TRUE), 
                timeFormat = "%Y-%m-%d",
                step = 1, 
                dragRange = TRUE),
    selectInput("crimeType", "Crime",
                choices = c("All", unique(app_df$ucr_crime_category)), selected = "All", multiple = T),
    sliderInput("percentileRange", "Select Percentile Range:",
                min = 10, max = 100, value = c(10, 100), step = 10,
                pre = "", post = "th percentile", ticks = TRUE, animate = TRUE),
#    checkboxInput("enableTopAddresses", "Enable Top Addresses Filter", value = FALSE),
    actionButton("update", "Update"))

ui <- page_navbar(
    theme = bslib::bs_theme(version = 5, bootswatch = "journal"),
    title = "Phoenix Crime App",
    sidebar = bslib::sidebar(app_sidebar, width = 400), # Define sidebar here for consistency across all pages
    nav_spacer(),
    nav_panel(
        "Map",
        leafletOutput("map")),
    nav_panel(
        "Graphs",
        layout_columns(col_widths = c(6, 6, 12),
        plotlyOutput("dayOfWeekHeatmap"),
        plotlyOutput("crimeGraph"),
        plotOutput("crimeTypeComparison")
        )
    ),
    nav_panel( # Add this for the heatmap
        "Heatmap",
        leafletOutput("heatmap")
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
        
        # Calculate incidents per address
        address_counts <- data %>%
            group_by(`100_block_addr`) %>%
            summarise(incidents = n(), .groups = 'drop') %>% 
            mutate(percentile_rank = ntile(incidents, 10)) # Assigns percentile rank from 1 (lowest) to 10 (highest)
        
        # Filter data based on selected percentiles from input
        if (!is.null(input$percentileRange) && 
            !(length(input$percentileRange) == 1 && input$percentileRange[1] == "All")) {
            # Convert selected percentiles to numeric, ignoring "All"
            selected_percentiles_numeric <- as.numeric(input$percentileRange[input$percentileRange != "All"])
            
            data <- data %>%
                semi_join(address_counts %>% filter(percentile_rank %in% selected_percentiles_numeric), by = "100_block_addr")
        }
        
        data
    })
    
    # Automatically update the date slider when year selections change
    observe({
        if("All" %in% input$yearSelect) {
            # If "All" is selected, reset the slider to the full date range
            updateSliderInput(session, "dateRange", 
                              value = range(app_df$occurred_on, na.rm = TRUE))
        } else {
            # For specific year selections, calculate the min start date and max end date
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
    
    # Render the map output using the reactive expression for data
    output$map <- renderLeaflet({
        # Check if filteredData is not NULL to avoid errors before the button is first clicked
        if (!is.null(filteredData())) {
            leaflet(data = filteredData()) %>%
                addTiles() %>%
                addCircleMarkers(~long, ~lat, 
                                 color = "#333333", # Dark gray color
                                 fillColor = "#333333", # Dark gray fill
                                 fillOpacity = 0.2, 
                                 opacity = .2, 
                                 weight = 1,
                                 popup = ~paste(ucr_crime_category, format(occurred_on, "%Y-%m-%d"), sep = "<br>"))
        }
    })
    
    output$crimeGraph <- renderPlotly({
        # Aggregate filtered data by month
        monthly_crime_data <- filteredData() %>%
            mutate(month = as.Date(format(occurred_on, "%Y-%m-01"))) %>% # Ensure 'month' is Date type for scale_x_date
            group_by(month) %>%
            summarise(crime_count = n(), .groups = 'drop') %>%
            arrange(month)
        
        date_range <- range(monthly_crime_data$month)
        start_year <- year(min(date_range))
        end_year <- year(max(date_range))
        years_spanned <- as.numeric(difftime(max(date_range), min(date_range), units = "days")) / 365.25
        
        # Decide date_breaks and date_labels based on the number of years spanned
        if(years_spanned <= 3) {
            date_breaks <- "1 month"
            date_labels <- "%b %Y"
        } else {
            date_breaks <- "6 months"
            date_labels <- "%b %Y"
        }
        
        year_starts <- seq(as.Date(paste0(start_year, "-01-01")), 
                           as.Date(paste0(end_year, "-01-01")), 
                           by = "1 year")
        
        # Create the ggplot object as before
        p <- ggplot(monthly_crime_data, aes(x = month, y = crime_count)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = ifelse((max(monthly_crime_data$month) - min(monthly_crime_data$month)) / 365.25 <= 3, "1 month", "6 months"),
                         date_labels = "%b %Y") +
            geom_vline(xintercept = as.numeric(seq(as.Date(paste0(year(min(monthly_crime_data$month)), "-01-01")), 
                                                   as.Date(paste0(year(max(monthly_crime_data$month)), "-01-01")), 
                                                   by = "1 year")), 
                       linetype = "dashed", 
                       color = "grey") +
            theme_minimal() +
            xlab("Month") +
            ylab("Number of Crimes") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                  axis.text.y = element_text(size = 12),
                  axis.title = element_text(size = 14))
        
        # Convert the ggplot object to a Plotly object
        ggplotly(p)
})
    
    output$heatmap <- renderLeaflet({
        heatdata <- filteredData() # Get the filtered data
        if (!is.null(heatdata)) {
            # Render the heatmap
            heatmap <- heatdata %>%
                leaflet() %>%
                addTiles() %>%
                addHeatmap(lng = heatdata$long, lat = heatdata$lat,
                           #gradient = RColorBrewer::brewer.pal(n=10, name = "Reds"),
                           blur = 40, 
                           max = 0.05, 
                           radius = 25)
        }
    })
    
    output$dayOfWeekHeatmap <- renderPlotly({
        # Prepare the data: extract day of the week and hour of the day
        data <- filteredData() %>%
            mutate(dayOfWeek = weekdays(occurred_on),
                   hourOfDay = as.integer(format(occurred_on, "%H")), # Use %H for 24-hour format
                   hourLabel = if_else(hourOfDay == 0, "12 AM",
                                       if_else(hourOfDay < 12, paste(hourOfDay, "AM"), 
                                               if_else(hourOfDay == 12, "12 PM", paste(hourOfDay - 12, "PM"))))) %>% 
            group_by(dayOfWeek, hourOfDay) %>%
            summarise(crimeCount = n(),
                      hourLabel = first(hourLabel),
                      .groups = 'drop')
        
        # Convert factors to ensure proper order in the plot
        data$dayOfWeek <- factor(data$dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
        # Replace the hourOfDay with hourLabel for plotting
        data$hourOfDay <- factor(data$hourLabel, levels = c("12 AM", paste(1:11, "AM"), "12 PM", paste(1:11, "PM")), ordered = TRUE)
        
        # Create the heatmap
        p <- plot_ly(data, x = ~dayOfWeek, y = ~hourOfDay, z = ~crimeCount, type = "heatmap", colors = "Reds",
                     colorbar = list(title = "Count")) %>%
            layout(xaxis = list(title = "Day of the Week"), 
                   yaxis = list(title = "Hour of the Day", categoryorder = "array", categoryarray = c("12 AM", paste(1:11, "AM"), "12 PM", paste(1:11, "PM")))
                   ) 
        p
    })
    
    output$crimeTypeComparison <- renderPlot({
        # Prepare data for the line chart
        crime_data <- filteredData() %>%
            group_by(date = as.Date(occurred_on), ucr_crime_category) %>%
            summarise(crime_count = n(), .groups = 'drop') %>%
            arrange(date)
        
        # Determine the range of dates and calculate the span of years
        date_range <- range(crime_data$date)
        start_year <- as.numeric(format(min(date_range), "%Y"))
        end_year <- as.numeric(format(max(date_range), "%Y"))
        years_spanned <- as.numeric(difftime(max(date_range), min(date_range), units = "days")) / 365.25
        
        # Decide date_breaks and date_labels based on the number of years spanned
        if(years_spanned <= 3) {
            date_breaks <- "1 month"
            date_labels <- "%b %Y"
        } else {
            date_breaks <- "6 months"
            date_labels <- "%b %Y"
        }
        
        # Generate the line chart with improved x-axis labels
        ggplot(crime_data, aes(x = date, y = crime_count, color = ucr_crime_category)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = date_breaks, date_labels = date_labels) + # Use determined breaks and labels
            labs(title = "Crime Types Comparison Over Time",
                 x = "Date",
                 y = "Number of Crimes",
                 color = "Crime Type") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
}

# Run the app
shinyApp(ui = ui, server = server)