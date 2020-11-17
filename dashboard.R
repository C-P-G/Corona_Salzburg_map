# Corona Dashboard for Salzburgerland. Displays the active cases on a selected day 
#
# Libaries ------------------------------------------------------------------
library(shiny)
library(leaflet.providers)
library(tidyverse)
library(leaftime)
# for spatial data 
library(raster)
library(rgdal)
#html stuff
library(htmltools)
library(shinydashboard)

# Read in data --------------------------------------------------------------
# Source: https://www.data.gv.at/covid-19/

ac<- readOGR("C:/Users/clair/Documents/04_Uni/01_aktuelles_Semester/Geovisulization/Salzburgerland_Corona.shp") 
ac<- spTransform(ac, CRS("+proj=longlat +datum=WGS84"))

# Format data --------------------------------------------------------------
# Bezirk from character to factor
ac$Bezirk <- as.factor(ac$Bezirk)
# Define date as date without time stamp
ac$Time <- as.Date(ac$Time, format = "%d.%m.%Y")

# create active cases colum Summe Anzahl Fälle minus Summe Geheilte 
ac$active <- as.numeric(ac$AnzahlFa_1 - ac$AnzahlGe_1)

# Color palette for active cases 
bins_total <- c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000)
pal_total <- colorBin("YlOrRd", domain = ac$active, bins = bins_total)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Corona Salzburg"),
    dashboardSidebar(
        dateInput(inputId = "n_date", label = "Select date", value = "2020-08-01",
                  min = "2020-02-26", max = "2020-11-14",
                  format = "dd-mm-yyyy", startview = "month",
                  language = "en", width = NULL)
    ),
    dashboardBody(
        #tell browser to make map fullscreen (apart from other shiny elements)
        tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
        leafletOutput(outputId = "mymap"),
            
        )
    )

# Define server 
server <- function(input, output, session) {
    
    # Daten nach eingegebenen Datum filtern
    dailyData <- reactive(ac[ac$Time == format(input$n_date, "%Y-%m-%d"),])
    
    react <- reactive({
        req(input$n_date)
        dailyData <- reactive(ac[ac$Time == format(input$n_date, "%Y-%m-%d"),])
        
    })
    output$mymap <- renderLeaflet({
        req(input$n_date)
        leaflet(isolate(dailyData())) %>%
            setView(lat = 47.811195,lng= 13.033229, zoom = 9) %>% 
            #Base groups
            addTiles(group = "OSM (default")  %>% 
            addProviderTiles(providers$Esri.WorldGrayCanvas , group = "ProviderTiles") %>% 
            #Overlay groups
            # Anzahl aktive Faelle
            addPolygons(fillColor = ~pal_total(active), # color to case number
                        weight = 2, 
                        opacity = 1, 
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.6,
                        highlight = highlightOptions( # highlight interaction with mouse
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        popup = ~ paste("Active Cases", Bezirk,": ", active), group = "active") %>% 
            addLegend(pal = pal_total, values = ~active, opacity = 0.7, title = "Active corona cases",
                      position = "bottomright", group = "active") %>% 
            # Layers control 
            addLayersControl(
                baseGroups = c("OSM (default)","ProviderTiles"),
                #overlayGroups = c("total", "active"),
                options = layersControlOptions(collapsed = FALSE))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
