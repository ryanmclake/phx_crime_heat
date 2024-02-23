library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)
library(ggplot2)
library(here)
library(plotly)

# Load and preprocess the data
geolocated_data_path <- here::here("data/crime_data_geolocated.csv") # nominatim data - not using for now
raw_data_path <- here::here("data/crime_data_raw.csv")
census_geolocated_path <- here::here("data/census_geolocated.csv") # census API data


# raw_df <- readr::read_csv(raw_data_path) %>% 
#     mutate(date = as.Date(occurred_on))

# Assuming read_csv is from the readr package and it can handle this file path
geo_df <- read_csv(census_geolocated_path) %>%
    mutate(zip = as.character(zip), 
           date = as.Date(occurred_on), # Correct column name for date
           ucr_crime_category = as.character(ucr_crime_category),
           lat = as.numeric(lat),  # Ensure lat is numeric
           long = as.numeric(long)) %>%  # Ensure long is numeric
    filter(!is.na(lat) & !is.na(long))

# Update UI definition
ui <- fluidPage(
    theme = bslib::bs_theme(version = 5),
    titlePanel("Crime Data Visualization"),
    sidebarLayout(
        sidebarPanel(
            selectInput("yearSelect", "Select Year", 
                        choices = c("All", as.character(2015:2023)), selected = "All", multiple = T),
            sliderInput("dateRange", "Date",
                        min = min(geo_df$date, na.rm = TRUE), 
                        max = max(geo_df$date, na.rm = TRUE),
                        value = range(geo_df$date, na.rm = TRUE), 
                        timeFormat = "%Y-%m-%d",
                        step = 1, 
                        dragRange = TRUE),
            selectInput("crimeType", "Crime",
                        choices = c("All", unique(geo_df$ucr_crime_category)), selected = "All", multiple = T),
            sliderInput("topAddresses", "Top addresses to display", 
                         value = 1, min = 0, max = 50, step = 1),
            checkboxInput("enableTopAddresses", "Enable Top Addresses Filter", value = FALSE),
            actionButton("update", "Update"),
            width = 3 # Set sidebar width to 3 columns
        ),
        mainPanel(
            leafletOutput("map"),
            plotlyOutput("crimeGraph"), # Add a plot output for the crime graph
            width = 9 # Set main panel width to 9 columns
        )
    )
)

# Update server logic
server <- function(input, output, session) {
    # Create a reactive expression that updates only when the update button is clicked
    filteredData <- eventReactive(input$update, {
        data <- geo_df %>%
            filter(date >= input$dateRange[1] & date <= input$dateRange[2],
                   ucr_crime_category %in% input$crimeType)
        
        if (input$enableTopAddresses) {
            # Aggregate data to count incidents per address
            address_counts <- data %>%
                group_by(`100_block_addr`) %>%
                summarise(incidents = n(), .groups = 'drop') %>%
                arrange(desc(incidents)) %>%
                head(input$topAddresses) # Keep only top N addresses
            
            # Filter the main dataset to include only the top N addresses
            data <- data %>%
                semi_join(address_counts, by = "100_block_addr")
        }
        
        data
    })
    
    # Automatically update the date slider when year selections change
    observe({
        if("All" %in% input$yearSelect) {
            # If "All" is selected, reset the slider to the full date range
            updateSliderInput(session, "dateRange", 
                              value = range(geo_df$date, na.rm = TRUE))
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
                                  selected = c("All", unique(geo_df$ucr_crime_category)))
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
                                 popup = ~paste(ucr_crime_category, format(date, "%Y-%m-%d"), sep = "<br>"))
        }
    })
    
    output$crimeGraph <- renderPlotly({
        # Aggregate filtered data by month
        monthly_crime_data <- filteredData() %>%
            mutate(month = as.Date(format(date, "%Y-%m-01"))) %>% # Ensure 'month' is Date type for scale_x_date
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
}

# Run the app
shinyApp(ui = ui, server = server)