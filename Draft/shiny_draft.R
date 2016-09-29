suppressMessages(install.packages(c("shiny","leaflet")))
suppressMessages(devtools::install_github(repo = "arianbarakat/advanced-r-lab-5-liu",
                                          subdir = "mapuloh"))
library(shiny)
library(mapuloh)
library(leaflet)


ui <- fluidPage(
  titlePanel("Address/Coordinate finder"),

  sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput(inputId = "inputtype",
                    label = "Select input type",
                    choices = c("Address","Coordinates"),
                    selected = "Address"),
        br(),
        uiOutput("ui"),
        br(),
        br(),
        selectInput(inputId = "maptype",
                  label = "Choose map type",
                  c("Standard" = "OpenStreetMap.Mapnik",
                    "Satellite" = "Esri.WorldImagery")),
      br()
      ),
      br(),
      br(),
      tableOutput("mytable")),

    mainPanel(
      leafletOutput("mymap", height = 700)
    )
  ))




server <- function(input,output){

  output$ui <- renderUI({
    switch (input$inputtype,
      "Address" = textInput(inputId = "Address",
                            label = "Adress",
                            value = "Mäster Mattias väg, Linköping, Sweden"),
      "Coordinates" =
        textInput(inputId = "long",
                                  label = "Longitud and Latitud",
                                  value = "58.39701 15.57415")
    )
  })

  ## Plot

  plot_out <- reactive({
      if(input$inputtype == "Address"){
      points <- coord_lookup(address = input$Address)[c("lat","lng")]

      id <- coord_lookup(address = input$Address)["Full_address"]

      leaflet() %>%
        addProviderTiles(input$maptype,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = points, popup = id[,1])

    } else {
      points <- address_lookup(latlong = input$long)[c("lat","lng")]

      id <- address_lookup(latlong = input$long)["Full_address"]

      leaflet() %>%
        addProviderTiles(input$maptype,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = points, popup = id[,1]  )

    }
  })

  output$mymap <- renderLeaflet({
    plot_out()
  })

  # Table with Full address name and coord

  table_out <- reactive({
    if(input$inputtype == "Address"){
      x <- coord_lookup(address = input$Address)
      colnames(x) <- c("Full Address Name","Latitud","Longitud")
      x
    } else {
      x <- address_lookup(latlong = input$long)
      colnames(x) <- c("Full Address Name","Latitud","Longitud")
      x
    }
  })

  output$mytable <- renderTable(table_out(), hover = TRUE, bordered = TRUE, digits = 5)

}


shinyApp(ui = ui, server = server)
