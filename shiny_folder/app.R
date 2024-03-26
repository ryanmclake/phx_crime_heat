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
library(RColorBrewer)
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
models_path <- here("models/")
class_fit <- readRDS(file = here(models_path, "class_fit.rds"))
class_recipe <- readRDS(file = here(models_path, "class_recipe.rds"))
regression_fit <- readRDS(file = here(models_path, "regression_fit.rds"))
regression_recipe <- readRDS(file = here(models_path, "regression_recipe.rds"))

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
    slice_sample(n=10000, replace = F)

app_df <- app_df %>%
    mutate(premise_category = case_when(
        str_detect(premise_type, "OFFICE|COMMERCIAL|RENTAL STORAGE|FACILITY|WAREHOUSE|FACTORY|PARKING GARAGE") | str_detect(premise_type, "STOREROOM/SHED \\(COMMERCIAL\\)") ~ "Other Commercial",
        
        str_detect(premise_type, "SINGLE FAMILY HOUSING|HOUSE|APARTMENT|MOBILE|CONDO|TOWNHOUSE|GARAGE|CARPORT|DRIVEWAY|FENCED RESIDENTIAL YARD|GROUP HOME|HOTEL|MOTEL") | str_detect(premise_type, "STOREROOM/SHED \\(RESIDENTIAL\\)") ~ "Residential",
        
        str_detect(premise_type, "GOVERNMENT|PUBLIC|SCHOOL|HOSPITAL|LIGHT RAIL FACILITY|MEDICAL|CHILD CARE|DAY CARE|CHURCH|SYNAGOGUE|TEMPLE|MOSQUE|AIRPORT|COMMUNITY CENTER|NURSING CARE") ~ "Public & Institutional",
        
        str_detect(premise_type, "MARKET|STORE|DEPARTMENT|DISCOUNT|RETAIL|FAST FOOD|GAS|GROCERY|SUPER MARKET|BANK|SAVINGS|CREDIT UNION|RESTAURANT|DRUG|SHOPPING MALL|THEATRE|BAR|LOUNGE|NIGHT CLUB|ADULT ONLY|MOVIE") ~ "Commercial & Retail",
        
        str_detect(premise_type, "PARK|PLAYGROUND|PARKING LOT|STREET|ROADWAY|SIDEWALK|ALLEY|CONSTRUCTION|OPEN SPACE|DESERT|FIELD|WOODS|FENCED YARD|PARKING GARAGE") ~ "Outdoor & Recreational",
        
        str_detect(premise_type, "VEHICLE|BUS STOP|LIGHT RAIL|TRAIN|BUS|FACILITY") ~ "Transport & Utilities",
        
        str_detect(premise_type, "UNKNOWN|OTHER|ABANDONED|CONDEMNED") ~ "Miscellaneous",
        TRUE ~ "Miscellaneous" # Catch-all for anything not matched
    ))

# app_df %>%
#     group_by(premise_category) %>%
#     summarise(count = n()) %>%
#     ggplot(aes(x = reorder(premise_category, -count), y = count, fill = premise_category)) +
#     geom_bar(stat = "identity") +
#     theme_minimal() +
#     labs(title = "Crime Count by Category", x = "Category", y = "Count") +
#     coord_flip() # For better label readability
# 
# average_counts <- app_df %>%
#     group_by(ucr_crime_category) %>%
#     summarise(average_count = mean(count)) %>%
#     arrange(desc(average_count)) %>%
#     mutate(ucr_crime_category = factor(ucr_crime_category, levels = ucr_crime_category))
# 
# # Merge average counts with original data to order crime categories
# app_df <- app_df %>%
#     left_join(average_counts, by = "ucr_crime_category") %>%
#     mutate(premise_category = factor(premise_category, levels = unique(premise_category)))
# 
# # Updated plot
# app_df %>%
#     group_by(premise_category, ucr_crime_category) %>%
#     summarise(count = n(), .groups = 'drop') %>%
#     ggplot(aes(x = premise_category, y = count, fill = ucr_crime_category)) +
#     geom_bar(stat = "identity") +
#     theme_minimal() +
#     labs(title = "Crime Type Distribution within Each Category", x = "Category", y = "Count") +
#     facet_wrap(~premise_category, scales = "free_y") +
#     scale_fill_manual(values = rev(RColorBrewer::brewer.pal(n = length(unique(app_df$ucr_crime_category)), name = "Set3"))) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# app_df %>%
#     group_by(premise_category, ucr_crime_category) %>%
#     summarise(count = n(), .groups = 'drop') %>%
#     mutate(total_count = sum(count), .by = "premise_category") %>%
#     mutate(premise_category = factor(premise_category, levels = unique(premise_category[order(-total_count)]))) %>%
#     mutate(avg_count = ave(count, ucr_crime_category, FUN = mean)) %>%
#     mutate(ucr_crime_category = factor(ucr_crime_category, levels = unique(ucr_crime_category[order(avg_count)]))) %>%
#     # Plot
#     ggplot(aes(x = premise_category, y = count, fill = ucr_crime_category)) +
#     geom_bar(stat = "identity", position = "stack") +
#     theme_minimal() +
#     labs(title = "Crime Type Distribution across Location Categories",
#          x = "Location Category",
#          y = "Count",
#          fill = "Crime Category") +  # Updated legend title
#     scale_fill_brewer(palette = "Set3") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# app_df %>%
#     mutate(year = lubridate::year(occurred_on), month = lubridate::month(occurred_on, label = TRUE)) %>%
#     group_by(premise_category, year, month) %>%
#     summarise(count = n()) %>%
#     ggplot(aes(x = month, y = count, color = premise_category)) +
#     geom_line() +
#     facet_wrap(~year, scales = "free_y") +
#     theme_minimal() +
#     labs(title = "Monthly Crime Trends by Category", x = "Month", y = "Count")
# 
# res_comm_df <- app_df %>%
#   #  filter(premise_category %in% c("Other Commercial", "Commercial & Retail"))
#     #filter(premise_category %in% c("Other Commercial"))
#     filter(premise_category %in% c("Residential"))
# 
# 
# res_comm_df %>%
#     group_by(premise_category, premise_type) %>%
#     summarise(count = n()) %>%
#     ggplot(aes(x = reorder(premise_category, -count), y = count, fill = premise_type)) +
#     geom_bar(stat = "identity") +
#     theme_minimal() +
#     labs(title = "Crime Type Distribution within Each Category", x = "Category", y = "Count") +
#     facet_wrap(~premise_category, scales = "free_y") # Separate plots for each category
# 
# res_comm_df %>%
#     filter(premise_category == "Residential") %>%
#     ggplot(aes(x = premise_type, fill = premise_type)) +
#     geom_bar() +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     labs(title = "Distribution of Premise Types", x = "Premise Type", y = "Count")

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
    tags$head(
        tags$script(HTML("
      $(document).on('shiny:sessioninitialized', function(event) {
        $('.collapse').collapse('hide');
      });
    "))
    ),
    selectInput(
        "yearSelect",
        "Select By Year Or Specific Dates",
        choices = c("All", as.character(2015:2023)),
        selected = "All",
        multiple = TRUE
    ),
    sliderInput(
        inputId = "dateRange",
        label = "",
        min = as.Date(min(app_df$occurred_on), "%Y-%m-%d"),
        max = as.Date(max(app_df$occurred_on), "%Y-%m-%d"),
        value = as.Date(range(app_df$occurred_on, na.rm = TRUE), "%Y-%m-%d"), # Select the full range by default
        timeFormat="%Y-%m-%d",
        ticks = F,
        dragRange = TRUE, # Allow selection of a range
    ),
    accordion(
        accordion_panel(
            "Crime & Location Category",
        selectInput(
        "crimeType", 
        "Crime",
        choices = c("All", unique(app_df$ucr_crime_category)),
        selected = "All", 
        multiple = TRUE
    ),
    selectInput(
        "premiseType", 
        "Premise Type",
        choices = c("All", unique(app_df$premise_category)),
        selected = "All", 
        multiple = TRUE
    ))),
    actionButton("reset", "Reset"),
    actionButton("update", "Update")
)

ui <- bslib::page_navbar(
        theme = bslib::bs_theme(version = 5, 
                                primary = "#bf492f", 
                                font_scale = NULL,
                                base_font = font_google("Roboto"),
                                heading_font = font_google("Open Sans"),
                                preset = "superhero"),
        title = "Phoenix Crime App",
        nav_spacer(),
        sidebar = sidebar(app_sidebar, width = 300),
        
### Nav_panel — Map
        nav_panel("Map",
                 icon = icon("map"),
                 navset_card_underline(
                   nav_panel("Heatmap", leafletOutput("spatial_heatmap", width="100%", height="100%")),
                   nav_panel("Incidents", leafletOutput("incident_map", width="100%", height="100%"))
                 )
                 ),

### Nav_panel — Graphs
        nav_panel("Graphs",
                 icon = icon("bar-chart"),
                 navset_card_underline(
                     nav_panel("Temporal Heatmap", shinycssloaders::withSpinner(plotlyOutput("dayOfWeekHeatmap"), 
                                                                                color = "#bf492f", color.background = "white")),
                     nav_panel("Type Comparison", shinycssloaders::withSpinner(plotlyOutput("crimeTypeComparison"), 
                                                                               color = "#bf492f", color.background = "white")),
                     nav_panel("Total Crime", shinycssloaders::withSpinner(plotlyOutput("crimeGraph"), color = "#bf492f", color.background = "white")),
                     nav_panel("Location Comparison", shinycssloaders::withSpinner(plotlyOutput("location_comparison"), color = "#bf492f", color.background = "white"))
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
        
        #print(input$dateRange[1])
        #print(input$dateRange[2])
        
        # Then, apply the existing date range and crime type filters
        data <- yearFilteredData %>%
            filter(occurred_on >= input$dateRange[1] &
                       occurred_on <= input$dateRange[2],
                   ucr_crime_category %in% input$crimeType,
                   premise_category %in% input$premiseType)
        data
        #print(head(data))
        #print("unique crime types")
        #print(unique(data$ucr_crime_category))
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
    
    observeEvent(input$premiseType, {
        # If "All" is selected, select all options
        if ("All" %in% input$premiseType) {
            # Check if "All" is the only option selected or if it was selected last
            if (length(input$premiseType) == 1 || tail(input$premiseType, n = 1) == "All") {
                updateSelectInput(session, "premiseType", 
                                  selected = c("All", unique(app_df$premise_category)))
            } else {
                # If "All" is selected along with other options but not last, deselect "All"
                updateSelectInput(session, "premiseType", 
                                  selected = setdiff(input$premiseType, "All"))
            }
        }
    })
    
    ### Navpanel - Spatial Heatmap
    output$spatial_heatmap <- renderLeaflet({
        data <- filteredData()
        
        if (!is.null(data) && nrow(data) > 0) {
            leaflet(data) %>%
                addTiles(group = "Default") %>%
                addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri") %>% 
                addPolygons(data = app_blockgroups_spatial,
                            weight = .5, # Line weight
                            color = "#444444", # Line color
                            opacity = .2, # Line opacity
                            fillOpacity = 0.2, # Fill opacity
                            fillColor = "#444444", group = "Phoenix PD Jurisdiction") %>%
                addHeatmap(data = data, lng = ~long, lat = ~lat, intensity = ~.1,
                           blur = 15, max = .6, radius = 10, group = "Heatmap") %>%
                addLayersControl(baseGroups = c("Default", "Esri"),
                                 overlayGroups = c("Heatmap", "Phoenix PD Jurisdiction"),
                                 options = layersControlOptions(collapsed = TRUE))
        } else {
            leaflet() %>%
                addTiles()
        }
    })
    
    output$incident_map <- renderLeaflet({
      data <- filteredData()
      
      if (!is.null(data) && nrow(data) > 0) {
        leaflet(data) %>%
          addTiles(group = "Default") %>%
          addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri") %>% 
          addPolygons(data = app_blockgroups_spatial,
                      weight = .5, # Line weight
                      color = "#444444", # Line color
                      opacity = .2, # Line opacity
                      fillOpacity = 0.2, # Fill opacity
                      fillColor = "#444444", group = "Phoenix PD Jurisdiction") %>%
          addCircleMarkers(lng = ~long, lat = ~lat, radius = 5, color = data$ucr_crime_category, 
                           stroke = FALSE, fillOpacity = 0.7, group = "Incidents") %>%
          addLayersControl(baseGroups = c("Default", "Esri"),
                           overlayGroups = c("Incidents", "Phoenix PD Jurisdiction"),
                           options = layersControlOptions(collapsed = TRUE))
      } else {
        leaflet() %>%
          addTiles()
      }
    })

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
    
    output$crimeGraph <- renderPlotly({
      # Aggregate filtered data by month and crime type, including "Total"
      monthly_crime_type_data <- filteredData() %>%
        mutate(month = as.Date(format(occurred_on, "%Y-%m-01"))) %>%
        group_by(month, ucr_crime_category) %>%
        summarise(crime_count = n(), .groups = 'drop') %>%
        arrange(month, ucr_crime_category)
      
      # Adding "Total" counts by month
      total_monthly_data <- filteredData() %>%
        mutate(month = as.Date(format(occurred_on, "%Y-%m-01"))) %>%
        group_by(month) %>%
        summarise(crime_count = n(), ucr_crime_category = "Total", .groups = 'drop') %>%
        arrange(month)
      
      combined_data <- bind_rows(monthly_crime_type_data, total_monthly_data)
      
      # Determine date range and formatting based on data span
      date_range <- range(combined_data$month)
      years_spanned <- as.numeric(difftime(max(date_range), min(date_range), units = "days")) / 365.25
      date_breaks <- if(years_spanned <= 2) "1 month" else "6 months"
      date_labels <- "%b %Y"
      
      # Determine the unique crime types in your dataset
      crime_types <- unique(combined_data$ucr_crime_category)

      colors <- setNames(brewer.pal(min(length(crime_types), 11), "Set3"), crime_types[crime_types != "Total"])
      colors["Total"] <- "black"
      
      # Create the plot
      p <- ggplot(combined_data, aes(x = month, y = crime_count, color = ucr_crime_category)) +
        geom_line() +
        geom_point() +
        scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
        geom_vline(xintercept = as.numeric(seq(as.Date(paste0(year(min(combined_data$month)), "-01-01")),
                                               as.Date(paste0(year(max(combined_data$month)), "-01-01")),
                                               by = "1 year")),
                   linetype = "dashed",
                   color = "grey") +
        theme_minimal() +
        labs(title = "Crime by Type Over Time", color = "Crime Type") +
        xlab("Month") +
        ylab("Count") +
        scale_color_manual(values = colors) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 14),
              legend.position = "bottom")
      
      # Convert to plotly for interactivity
      ggplotly(p, tooltip = c("x", "y", "color"))
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

        data$dayOfWeek <- factor(data$dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
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
        scale_fill_brewer(palette = "Set3") +
            
        # scale_fill_viridis(alpha = .8, discrete = TRUE,
        #                    option = "rocket",
        #                    end = .6) + # closer to 1 is more yellow
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
    
    
    output$location_comparison <- renderPlotly({
        data <- filteredData() %>%
            group_by(premise_category, ucr_crime_category) %>%
            summarise(count = n(), .groups = 'drop') %>%
            mutate(total_count = sum(count), .by = "premise_category") %>%
            mutate(premise_category = factor(premise_category, levels = unique(premise_category[order(-total_count)]))) %>%
            mutate(avg_count = ave(count, ucr_crime_category, FUN = mean)) %>%
            mutate(ucr_crime_category = factor(ucr_crime_category, levels = unique(ucr_crime_category[order(avg_count)])))
        
        gg <- ggplot(data, aes(x = premise_category, y = count, fill = ucr_crime_category)) +
            geom_bar(stat = "identity", position = "stack") +
            theme_minimal() +
            labs(title = "Crime Type Distribution across Location Categories",
                 x = "Location Category", y = "Count", fill = "Crime Category") +
            scale_fill_brewer(palette = "Set3") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(gg)
    })
    

    
}

# Run the app
shinyApp(ui = ui, server = server)
