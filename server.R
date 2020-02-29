library(shiny)
library(ggmap)
library(DT)

source("mifuncion.R")
country.names <- readRDS("country_names.rds")

shinyServer(function(input, output,session) {
  estaciones <- ""   
  output$map <- renderLeaflet({
    GeneraMapaEstaciones(pais = input$pais,activo = input$activo, loc=input$location,yy=input$year[2])
  })
  
  estaciones <- ""
  stations <- reactive(ExtraeEstaciones(pais = input$pais,activo = input$activo, loc=input$location,yy=input$year[2]))
  yicon <- makeIcon(iconUrl="yellow_cross.png")
  
  output$out <- renderPrint({
    stations <- stations()
    #browser()
    validate(need(input$map_marker_click, FALSE))
    p <- input$map_marker_click
    estacion <- input$map_marker_click$id
    #browser()
    if (estacion %in% estaciones){
      estaciones <<- estaciones[estaciones!=estacion]
      leafletProxy("map") %>% removeMarker(layerId=estacion)
    } else {
      estaciones <<- c(estaciones,estacion)
      leafletProxy("map") %>% setView(lng=p$lng, lat=p$lat, input$map_zoom) %>% addMarkers(p$lng,p$lat,icon=yicon,layerId=estacion)
    }
    estaciones <<- estaciones[estaciones!=""]
    paste(estaciones,collapse=",")
  })
  
  
  dat <- reactiveValues(dat=NULL)
  dat2 <- reactiveValues(dat=NULL)
  rad <- reactiveValues(dat=NULL)
  
  observeEvent(input$go,{
    progress <- shiny::Progress$new()
    progress$set(message = "Downloading observations", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / (length(estaciones)*2)
      }
      progress$set(value = value, detail = detail)
    }

    dat$dat <- ExtraeVariasEstaciones(estaciones,year1 = input$year[1],year2 = input$year[2],updateProgress)
    if(input$tipodatos=="00"){
      dat2$dat <- dat$dat[dat$dat$minute==0,]
    } else if (input$tipodatos=="cercano"){
      dat2$dat <- FiltrarCercano(dat$dat)
    }else {
      dat2$dat <- dat$dat
    }
  })
  
  output$input_tipodatos <- renderUI({
    if(is.null(input$go)) {return()}
    selectInput(inputId="show_st",label="Show station",choices = as.list(estaciones),selected = estaciones[1])
  })
  
  output$input_estaciones <- renderUI({
    if(is.null(input$go)) {return()}
    selectInput(inputId="tipodatos",label="Mostrar",choices = c("Only hours with 00 minutes"="00","nearest minute to 00"="cercano"),selected = "cercano")
  })
  
  output$input_download <- renderUI({
    if(is.null(input$go)) {return()}
    else {
      if (is.null(input$spe)){
        downloadButton("download","Download observations to file")
      } else {
        if (input$spe==FALSE){
          downloadButton("download","Download observations to file")
        } else {
          downloadButton("download2","Download observations to file")
        }
      }
    }
  })
  
  
  observeEvent(input$go,{
    if(input$tipodatos=="00"){
      dat2$dat <- dat$dat[dat$dat$minute==0,]
    } else if (input$tipodatos=="cercano"){
      dat2$dat <- FiltrarCercano(dat$dat)
    }else {
      dat2$dat <- dat$dat
    }
  })
  
  observeEvent(input$tipodatos,{
    if (!is.null(dat$dat)){
    if(input$tipodatos=="00"){
      dat2$dat <- dat$dat[dat$dat$minute==0,]
    } else if (input$tipodatos=="cercano"){
      dat2$dat <- FiltrarCercano(dat$dat)
    }else {
      dat2$dat <- dat$dat
    }
    }
  })
 
  output$table <- DT::renderDataTable({
    data_show <- dat2$dat
     if (!is.null(input$show_st)){
       data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==input$show_st,]
     } else {
       data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==estaciones[1],]
     }
    data_show
  })
  
  output$dygraph1 <- renderDygraph({
    data_show <- dat2$dat
    if (!is.null(input$show_st)){
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==input$show_st,]
    } else {
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==estaciones[1],]
    }
    GraficosSincronizados(data_show,"temp",input$tipodatos)
  })
  
  output$dygraph2 <- renderDygraph({
    data_show <- dat2$dat
    if (!is.null(input$show_st)){
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==input$show_st,]
    } else {
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==estaciones[1],]
    }
    GraficosSincronizados(data_show,"ws",input$tipodatos)
  })
  
  output$dygraph3 <- renderDygraph({
    data_show <- dat2$dat
    if (!is.null(input$show_st)){
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==input$show_st,]
    } else {
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==estaciones[1],]
    }
    GraficosSincronizados(data_show,"wd",input$tipodatos)
  })
  
  output$dygraph4 <- renderDygraph({
    data_show <- dat2$dat
    if (!is.null(input$show_st)){
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==input$show_st,]
    } else {
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==estaciones[1],]
    }
    GraficosSincronizados(data_show,"atmos_pres",input$tipodatos)
  })
  
  output$dygraph5 <- renderDygraph({
    data_show <- dat2$dat
    if (!is.null(input$show_st)){
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==input$show_st,]
    } else {
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==estaciones[1],]
    }
    GraficosSincronizados(data_show,"rh",input$tipodatos)
  })
  
  output$dygraph6 <- renderDygraph({
    data_show <- dat2$dat
    if (!is.null(input$show_st)){
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==input$show_st,]
    } else {
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==estaciones[1],]
    }
    GraficosSincronizados(data_show,"precip",input$tipodatos)
  })
  
  output$dygraph7 <- renderDygraph({
    data_show <- dat2$dat
    if (!is.null(input$show_st)){
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==input$show_st,]
    } else {
      data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==estaciones[1],]
    }
    GraficosSincronizados(data_show,"rad",input$tipodatos)
  })
  
 output$download <- downloadHandler(
   filename = function() {
     paste(input$show_st,'.csv', sep='')
   },
   content = function(file) {
     data_show <- dat2$dat
     if (!is.null(input$show_st)){
       data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==input$show_st,]
     } else {
       data_show <- data_show[paste(data_show$usaf,data_show$wban,sep="-")==estaciones[1],]
     }
     if ("precip" %in% colnames(data_show)){
      dd <- data_show[,c("time","wd","ws","temp","atmos_pres","rh","precip","precip_period")]
      dd$date <- round(dd$time,units="hours")
      dd <- dd[,c("date","wd","ws","temp","atmos_pres","rh","precip","precip_period")]
     } else {
       dd <- data_show[,c("time","wd","ws","temp","atmos_pres","rh")]
       dd$date <- round(dd$time,units="hours")
       dd <- dd[,c("date","wd","ws","temp","atmos_pres","rh")]
     }
     write.table(dd, file, sep=";", col.names=TRUE, row.names=FALSE, quote=FALSE)
   }
   
 )
 

  
})

