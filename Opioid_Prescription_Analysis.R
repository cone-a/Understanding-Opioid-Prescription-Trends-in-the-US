library(shiny)
library(leaflet)
library(tigris)
library(sf)
library(dplyr)
library(DT)
library(readr)

# Load your data
df <- read_csv("OMT_MDCD_R23_P11_V10_YTD21_GEO.csv")

df <- df %>% filter(Plan_Type == "All")

# Use tigris to get state boundaries in sf format
states_sf <- tigris::states(cb = TRUE, class = "sf") %>%
  mutate(state_abbr = STUSPS)

# Map state names to abbreviations in your df
state_abbreviations <- states_sf %>% select(NAME, state_abbr) %>% deframe()
df <- df %>%
  mutate(state_abbr = state_abbreviations[Geo_Desc])




ui <- fluidPage(
  tags$h1("Understanding Opioid Prescription Trends in the US by State", style = "text-align: center; margin-top: 20px;"),
  fluidRow(
    column(3,
           # First sidebarPanel for inputs
           wellPanel(
             tags$h3("Controls for Map 1"),
             sliderInput("year1", "Select Year:", min = min(df$Year), max = max(df$Year), value = min(df$Year)),
             selectInput("claimType1", "Select Claim Type:",
                         choices = c("Total Claims" = "Tot_Clms",
                                     "Total Opioid Claims" = "Tot_Opioid_Clms",
                                     "Long Acting Opioid Claims" = "LA_Tot_Opioid_Clms")),
             tags$h3("Controls for Map 2", style = "margin-top: 20px;"),
             sliderInput("year2", "Select Year:", min = min(df$Year), max = max(df$Year), value = max(df$Year)),
             selectInput("claimType2", "Select Claim Type:",
                         choices = c("Total Claims" = "Tot_Clms",
                                     "Total Opioid Claims" = "Tot_Opioid_Clms",
                                     "Long Acting Opioid Claims" = "LA_Tot_Opioid_Clms")),
             tags$h3("Data Source"),
             tags$a("Variable Explantion PDF", href = "https://data.cms.gov/sites/default/files/2023-05/e74c1f3b-53e3-4549-81a2-77106e336b8f/OMT_MDCD_RY23_230519_DD_508.pdf", target = "_blank"),
             tags$br(),
             tags$a("Source Data Website Link", href = "https://data.cms.gov/summary-statistics-on-use-and-payments/medicare-medicaid-opioid-prescribing-rates/medicaid-opioid-prescribing-rates-by-geography", target = "_blank")
           )
    ),
    column(9,
           # MainPanel content
           fluidRow(
             column(6,
                    tags$h3("Claims Map 1"),
                    leafletOutput("map1")
             ),
             column(6,
                    tags$h3("Claims Map 2", style = "margin-top: 20px;"),
                    leafletOutput("map2")
             )
           ),
           fluidRow(
             column(6,
                    tags$h3("Map 1 State Chosen:", verbatimTextOutput("selectedState1")),
                    DTOutput("stateDataTable1")  # Data table for the first map
             ),
             column(6,
                    tags$h3("Map 2 State Chosen:", verbatimTextOutput("selectedState2"), style = "margin-top: 20px;"),
                    DTOutput("stateDataTable2")   # Data table for the second map
             )
           )
    )
  )
)




server <- function(input, output, session) {
  
  selectedState1 <- reactiveVal(NULL)
  selectedState2 <- reactiveVal(NULL)
  
  # Clain type showing on figure
  claimTypeText1 <- reactive({
    if (input$claimType1 == "Tot_Clms") {
      "Total Claims"
    } else if (input$claimType1 == "Tot_Opioid_Clms") {
      "Total Opioid Claims"
    } else {
      "Long Acting Opioid Claims"  # Default text if neither
    }
  })
  
  
  # Clain type showing on figure
  claimTypeText2 <- reactive({
    if (input$claimType2 == "Tot_Clms") {
      "Total Claims"
    } else if (input$claimType2 == "Tot_Opioid_Clms") {
      "Total Opioid Claims"
    } else {
      "Long Acting Opioid Claims"  # Default text if neither
    }
  })
  
  # Logic for the first map
  data_sf1 <- reactive({
    # Perform the join operation
    joined_data <- states_sf %>% 
      left_join(df %>%
                  filter(Year == input$year1, Geo_Lvl == "State") %>%
                  select(state_abbr, ClaimValue1 = !!sym(input$claimType1)),
                by = "state_abbr")
    
    # Ensure joined_data is treated as an sf object
    joined_data_sf <- st_as_sf(joined_data)
    
    # Transform the spatial data to WGS84 datum
    st_transform(joined_data_sf, crs = 4326)
  })
  
  output$map1 <- renderLeaflet({
    data <- data_sf1()
    pal <- colorNumeric(palette = "viridis", domain = data$ClaimValue1, na.color = "transparent")
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
      addPolygons(fillColor = ~pal(ClaimValue1),
                  color = "#BDBDC3",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.7,
                  popup = ~paste(NAME, ": ", ClaimValue1),
                  layerId = ~state_abbr) %>% # Use state_abbr as layerId) %>%
      addLegend(pal = pal, 
                values = ~ClaimValue1, 
                opacity = 1.0, 
                title = claimTypeText1(),
                position = "bottomright") %>%
      addControl(paste("Year", input$year1,":", claimTypeText1()), position = "topright", className = "map-title")
  })
  
  # Logic for the second map
  data_sf2 <- reactive({
    # Perform the join operation
    joined_data2 <- states_sf %>% 
      left_join(df %>%
                  filter(Year == input$year2, Geo_Lvl == "State") %>%
                  select(state_abbr, ClaimValue2 = !!sym(input$claimType2)),
                by = "state_abbr")
    
    # Ensure joined_data is treated as an sf object
    joined_data_sf2 <- st_as_sf(joined_data2)
    
    # Transform the spatial data to WGS84 datum
    st_transform(joined_data_sf2, crs = 4326)
  })
  
  output$map2 <- renderLeaflet({
    data <- data_sf2()
    pal <- colorNumeric(palette = "viridis", domain = data$ClaimValue2, na.color = "transparent")
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
      addPolygons(fillColor = ~pal(ClaimValue2),
                  color = "#BDBDC3",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.7,
                  popup = ~paste(NAME, ": ", ClaimValue2),
                  layerId = ~state_abbr) %>%
      addLegend(pal = pal, 
                values = ~ClaimValue2, 
                opacity = 1.0, 
                title = claimTypeText2(),
                position = "bottomright") %>%
      addControl(paste("Year", input$year2,":", claimTypeText2()), position = "topright", className = "map-title")
  })
  
  # Capture click events and update selectedState1
  observeEvent(input$map1_shape_click, {
    click <- input$map1_shape_click
    selectedState1(click$id)
  })

  observeEvent(input$map2_shape_click, {
    click <- input$map2_shape_click
    selectedState2(click$id)
  })

  
  
  # Render selected state text for Map 1
  output$selectedState1 <- renderPrint({
    req(selectedState1())
    selectedState1()
  })
  
  # Render selected state text for Map 2
  output$selectedState2 <- renderPrint({
    req(selectedState2())
    selectedState2()
  })
  

  
  # Render data table based on the selected state for map 1
  output$stateDataTable1 <- renderDT({
    req(selectedState1()) # Ensure a state has been selected
    
    # Fetch the data for the selected state and year
    stateData1 <- df %>%
      filter(state_abbr == selectedState1(), Year == input$year1) %>%
      select(
        "Total Opioid Claims" = Tot_Opioid_Clms,
        "Overall Claims" = Tot_Clms,
        "Opioid Prescribing Rate" = Opioid_Prscrbng_Rate,
        "Five Year Change Opioid Prescribing Rate" = Opioid_Prscrbng_Rate_5Y_Chg,
        "One Year Change Opioid Prescribing Rate" = Opioid_Prscrbng_Rate_1Y_Chg,
        "Long Acting Opioid Claims" = LA_Tot_Opioid_Clms,
        "Long Acting Opioid Prescribing Rate" = LA_Opioid_Prscrbng_Rate,
        "Five Year Change Long Acting Opioid Prescribing Rate" = LA_Opioid_Prscrbng_Rate_5Y_Chg,
        "One Year Change Long Acting Opioid Prescribing Rate" = LA_Opioid_Prscrbng_Rate_1Y_Chg
      )
    
    # Check if there's data to display
    if (nrow(stateData1) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for the selected state."), options = list(dom = 't', paging = FALSE, ordering = FALSE)))
    }
    
    # Transpose and make it a long format for display
    stateDataLong1 <- t(stateData1)
    colnames(stateDataLong1) <- NULL
    stateDataLong1 <- data.frame(Variable = rownames(stateDataLong1), Value = stateDataLong1[,1])
    
    DT::datatable(stateDataLong1, options = list(pageLength = 5, autoWidth = TRUE), rownames = FALSE)
  })
  
  # Render data table based on the selected state for map 2
  output$stateDataTable2 <- renderDT({
    req(selectedState2()) # Ensure a state has been selected
    
    stateData2 <- df %>%
      filter(state_abbr == selectedState2(), Year == input$year2) %>%
      select(
        "Total Opioid Claims" = Tot_Opioid_Clms,
        "Overall Claims" = Tot_Clms,
        "Opioid Prescribing Rate" = Opioid_Prscrbng_Rate,
        "Five Year Change Opioid Prescribing Rate" = Opioid_Prscrbng_Rate_5Y_Chg,
        "One Year Change Opioid Prescribing Rate" = Opioid_Prscrbng_Rate_1Y_Chg,
        "Long Acting Opioid Claims" = LA_Tot_Opioid_Clms,
        "Long Acting Opioid Prescribing Rate" = LA_Opioid_Prscrbng_Rate,
        "Five Year Change Long Acting Opioid Prescribing Rate" = LA_Opioid_Prscrbng_Rate_5Y_Chg,
        "One Year Change Long Acting Opioid Prescribing Rate" = LA_Opioid_Prscrbng_Rate_1Y_Chg
      )
    # Transpose and make it a long format for display
    if (nrow(stateData2) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for the selected state."), options = list(dom = 't', paging = FALSE, ordering = FALSE)))
    }
    
  
    
    stateDataLong2 <- t(stateData2)
    colnames(stateDataLong2) <- NULL
    stateDataLong2 <- data.frame(Variable = rownames(stateDataLong2), Value = stateDataLong2[,1])
    
    DT::datatable(stateDataLong2, options = list(pageLength = 5, autoWidth = TRUE), rownames = FALSE)
  })
  
  
  
}

shinyApp(ui, server)




