library(shiny)
library(leaflet)
library(sf)

# --- UI: The User Interface ---
ui <- fluidPage(
  titlePanel("Spatial Boundary Checker"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("region_file", "1. Upload Region CSV (long, lat)", accept = ".csv"),
      fileInput("points_file", "2. Upload Points CSV (long, lat)", accept = ".csv"),
      hr(),
      helpText("Ensure CSVs have 'long' and 'lat' columns.")
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px"),
      tableOutput("results_table")
    )
  )
)

# --- SERVER: The Logic ---
server <- function(input, output) {
  
  # Reactive function to process the data
  processed_data <- reactive({
    req(input$region_file, input$points_file)
    
    # Load Region
    reg_df <- read.csv(input$region_file$datapath)
    reg_mat <- as.matrix(reg_df[, c("long", "lat")])
    reg_poly <- st_polygon(list(reg_mat)) %>% st_sfc(crs = 4326)
    
    # Load Points
    pts_df <- read.csv(input$points_file$datapath)
    pts_sf <- st_as_sf(pts_df, coords = c("long", "lat"), crs = 4326)
    
    # Check intersection
    pts_df$inside <- st_intersects(pts_sf, reg_poly, sparse = FALSE)[,1]
    
    return(list(poly = reg_poly, points = pts_df, pts_sf = pts_sf))
  })
  
  # Render the Leaflet Map
  output$map <- renderLeaflet({
    req(processed_data())
    data <- processed_data()
    
    leaflet() %>%
      addTiles() %>%  # Add standard OpenStreetMap tiles
      addPolygons(data = data$poly, color = "red", weight = 2, fillOpacity = 0.2) %>%
      addCircleMarkers(
        data = data$points, 
        lng = ~long, lat = ~lat,
        color = ~ifelse(inside, "green", "red"),
        radius = 5,
        label = ~paste("Status:", ifelse(inside, "Inside", "Outside"))
      )
  })
  
  # Show the result table below the map
  output$results_table <- renderTable({
    req(processed_data())
    processed_data()$points
  })
}

# Run the application 
shinyApp(ui = ui, server = server)