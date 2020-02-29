
library(shiny)
library(leaflet)
library(lubridate)
source("funciones.R")
country.names <- readRDS("country_names.rds")

nombre <- ExtraeTodasEstaciones()
nombre <- nombre$name
yy <- year(Sys.time())
y <- yy

fluidPage(
  tags$head(HTML("<title>HUMBOLDT. Interactive Meteo Data Downloader</title>")),

navbarPage(
  title=div(img(src="220px-Alexander_von_Humboldt_signature.svg.png",width="50%",height="50%")), 
  id="nav",
  tabPanel("Interactive Map",
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")
               ),
               leafletOutput("map", width="100%", height=1000)#,
           )
  ),
  
  tabPanel("Data",
           DT::dataTableOutput("table")
  ),
  
  tabPanel("Graph",dygraphOutput("dygraph1"),dygraphOutput("dygraph2"),dygraphOutput("dygraph3"),dygraphOutput("dygraph4"),dygraphOutput("dygraph5"),dygraphOutput("dygraph6")),
  
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = 20, right = 20, bottom = "auto",
                width = 360, height = 700,
                
                p(),
                selectInput(inputId="pais",label="Country",choice=c("Todos",sort(country.names)),selected="Todos"),
                checkboxInput(inputId="activo", label="ACTIVE", value=TRUE),
                textInput(inputId="location", label="Find location", value = ""),
                sliderInput("year", "Date range", 2010, yy, value = c(yy-1, yy)),
                verbatimTextOutput("out"),  
                actionButton(inputId="go",label="Download observations", icon=icon("database")),
                conditionalPanel("input.go",uiOutput("input_tipodatos")),
                conditionalPanel("input.go",uiOutput("input_estaciones")),
                conditionalPanel("input.go",uiOutput("input_download"))
  )
)
)
