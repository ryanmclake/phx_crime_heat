library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)
library(here)

# Load and preprocess the data
geolocated_data_path <- here::here("data/crime_data_geolocated.csv")

# Assuming read_csv is from the readr package and it can handle this file path
geo_df <- read_csv(geolocated_data_path) %>%
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
                        choices = c("All", as.character(2018:2023)), selected = "All", multiple = T),
            sliderInput("dateRange", "Date",
                        min = min(geo_df$date, na.rm = TRUE), 
                        max = max(geo_df$date, na.rm = TRUE),
                        value = range(geo_df$date, na.rm = TRUE), 
                        timeFormat = "%Y-%m-%d",
                        step = 1, 
                        dragRange = TRUE),
            selectInput("crimeType", "Crime",
                        choices = unique(geo_df$ucr_crime_category), 
                        multiple = TRUE,
                        selected = unique(geo_df$ucr_crime_category)),
            actionButton("update", "Update"),
            width = 3 # Set sidebar width to 3 columns
        ),
        mainPanel(
            leafletOutput("map"),
            plotOutput("crimeGraph"), # Add a plot output for the crime graph
            width = 9 # Set main panel width to 9 columns
        )
    )
)

# Update server logic
server <- function(input, output, session) {
    # Create a reactive expression that updates only when the update button is clicked
    filteredData <- eventReactive(input$update, {
        geo_df %>%
            filter(date >= input$dateRange[1] & date <= input$dateRange[2],
                   ucr_crime_category %in% input$crimeType)
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
    
    # Render the map output using the reactive expression for data
    output$map <- renderLeaflet({
        # Check if filteredData is not NULL to avoid errors before the button is first clicked
        if (!is.null(filteredData())) {
            leaflet(data = filteredData()) %>%
                addTiles() %>%
                addCircleMarkers(~long, ~lat, 
                                 color = "#333333", # Dark gray color
                                 fillColor = "#333333", # Dark gray fill
                                 fillOpacity = 0.7, 
                                 opacity = 1, 
                                 weight = 1,
                                 popup = ~paste(ucr_crime_category, format(date, "%Y-%m-%d"), sep = "<br>"))
        }
    })
    
    output$crimeGraph <- renderPlot({
        # Aggregate filtered data by month
        monthly_crime_data <- filteredData() %>%
            mutate(month = as.Date(format(date, "%Y-%m-01"))) %>% # Ensure 'month' is Date type for scale_x_date
            group_by(month) %>%
            summarise(crime_count = n(), .groups = 'drop') %>%
            arrange(month)
        
        # Plot with updated axis labels and formats
        ggplot(monthly_crime_data, aes(x = month, y = crime_count, group = 1)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + # Format X axis labels
            theme_minimal() +
            xlab("Month") +
            ylab("Number of Crimes") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Adjust X axis label size and rotation
                  axis.text.y = element_text(size = 12), # Adjust Y axis label size
                  axis.title = element_text(size = 14)) # Adjust title size for both axes
    })
}

# Run the app
shinyApp(ui = ui, server = server)