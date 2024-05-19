#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(readr)
library(DT)

# Read the CSV file
df <- read_csv('OMT_MDCD_R23_P11_V10_YTD21_GEO.csv')

# Ensure the state abbreviations are correct for Plotly
# Create a named vector of state names to abbreviations
state_abbreviations <- c(
    "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
    "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE",
    "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID",
    "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS",
    "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
    "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
    "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV",
    "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY",
    "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK",
    "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC",
    "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
    "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV",
    "Wisconsin" = "WI", "Wyoming" = "WY", "District of Columbia" = "DC")



# For ALL PLAN TYPES
df <- df %>% filter(Plan_Type == "All")

ui <- fluidPage(
    tags$h1("Understanding Opioid Prescription Trends in the US by State", style = "text-align: center; margin-top: 20px;"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("year1", "Select Year 1", min = min(df$Year), max = max(df$Year), value = min(df$Year)),
            sliderInput("year2", "Select Year 2", min = min(df$Year), max = max(df$Year), value = max(df$Year)),
            selectInput("claimType", "Claim Type", choices = c("Total Claims" = "Tot_Clms", "Total Opioid Claims" = "Tot_Opioid_Clms", "Long Acting Opioid Claims" = "LA_Tot_Opioid_Clms"))
        ),
        mainPanel(
            plotlyOutput("US_Map1"), # Add source here
            plotlyOutput("US_Map2"), # Add source here
            DTOutput("detailsTable")
        )
    )
)

server <- function(input, output) {
    # Reactive value to store the clicked state abbreviation
    clicked_state <- reactiveVal()
    
    observe({
        event_data <- event_data("plotly_click", source = "map1")
        if (!is.null(event_data)) {
            clicked_state(event_data$location)
        }
    })
    
    observe({
        event_data <- event_data("plotly_click", source = "map2")
        if (!is.null(event_data)) {
            clicked_state(event_data$location)
        }
    })
    
    global_min_max <- reactive({
        req(input$claimType) # Ensure input$claimType is available
        df %>%
            filter(Geo_Lvl == "State", !is.na(!!sym(input$claimType))) %>%
            summarise(
                Min = min(!!sym(input$claimType), na.rm = TRUE),
                Max = max(!!sym(input$claimType), na.rm = TRUE)
            ) %>%
            {range(.$Min, .$Max)} # Extract Min and Max into a numeric vector
    })
    
    # Clain type showing on figure
    claimTypeText <- reactive({
        if (input$claimType == "Tot_Clms") {
            "Total Claims"
        } else if (input$claimType == "Tot_Opioid_Clms") {
            "Total Opioid Claims"
        } else {
            "Claims"  # Default text if neither
        }
    })
    
    # Prepare map data for rendering
    prepare_map_data <- function(year) {
        req(input$claimType)
        df %>%
            filter(Year == year, Geo_Lvl == "State", !is.na(!!sym(input$claimType))) %>%
            mutate(state_abbr = ifelse(Geo_Desc %in% names(state_abbreviations), state_abbreviations[Geo_Desc], NA)) %>%
            group_by(state_abbr) %>%
            summarize(Claims = sum(!!sym(input$claimType), na.rm = TRUE), .groups = "drop") %>%
            filter(!is.na(state_abbr))
    }
    
    # Map outputs
    output$US_Map1 <- renderPlotly({
        df_filtered <- prepare_map_data(input$year1)
        plot_geo(df_filtered, locationmode = 'USA-states') %>%
            add_trace(
                z = ~Claims,
                locations = ~state_abbr,
                colorscale = 'Portland',
                color = ~Claims,
                colorbar = list(title = "Claims", nticks = 5),
                zmin = global_min_max()[1],
                zmax = global_min_max()[2]
            ) %>%
            layout(
                title = paste("Year", input$year1,":", claimTypeText()),
                geo = list(scope = 'usa')
            )
    })
    
    output$US_Map2 <- renderPlotly({
        df_filtered <- prepare_map_data(input$year2)
        plot_geo(df_filtered, locationmode = 'USA-states') %>%
            add_trace(
                z = ~Claims,
                locations = ~state_abbr,
                colorscale = 'Portland',
                color = ~Claims,
                colorbar = list(title = "Claims", nticks = 5),
                zmin = global_min_max()[1],
                zmax = global_min_max()[2]
            ) %>%
            layout(
                title = paste("Year", input$year2,":", claimTypeText()),
                geo = list(scope = 'usa')
            )
    })
    
    # Output table for selected state
    output$detailsTable <- renderDT({
        req(clicked_state())  # Make sure a state has been clicked
        state_data <- df %>%
            filter(state_abbr == clicked_state()) %>%
            select(Geo_Desc, Tot_Opioid_Clms, Tot_Clms, Opioid_Prscrbng_Rate,
                   Opioid_Prscrbng_Rate_5Y_Chg, Opioid_Prscrbng_Rate_1Y_Chg,
                   LA_Tot_Opioid_Clms, LA_Opioid_Prscrbng_Rate,
                   LA_Opioid_Prscrbng_Rate_5Y_Chg, LA_Opioid_Prscrbng_Rate_1Y_Chg)
        datatable(state_data, options = list(pageLength = 5, autoWidth = TRUE))
    })
}




shinyApp(ui = ui, server = server)

