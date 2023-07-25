library(shiny)
library(leaflet)
library(dplyr)

clicks <- data.frame(lat = numeric(), lng = numeric(), .nonce = numeric())

ui <- fluidPage(
    leafletOutput("map"),
    actionButton("use_clik_loc", "Check loc")
)

server <- function(input, output) {
    output$map <- renderLeaflet(addTiles(leaflet()))
    
    observeEvent(input$use_clik_loc, {
        last_click <- isolate(as.data.frame(input$map_click))
        clicks <<- clicks |>
            bind_rows(last_click)
        print(clicks)
    })
}

shinyApp(ui, server)