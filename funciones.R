library(lubridate)
library(stationaRy)
library(dplyr)
library(leaflet)
library(ggmap)
library(dygraphs)
library(xts)
library(RCurl)

conf <- data.frame(variable=c("wd","ws","temp","atmos_pres","rh"),nombre=c("Wind direction","Wind speed","Temperature","Pressure","Relative Humidity"))


ExtraeTodasEstaciones <- function(){
  stations <- get_station_metadata()
}

ExtraeEstacionesActivas <- function(y){
  stations <- ExtraeTodasEstaciones()
  lfiles <- getURL(url=sprintf("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/%s/",y),.opts = curlOptions(ftplistonly=TRUE))
  lfiles <- strsplit(lfiles,"\n")[[1]]
  lfiles <- gsub(pattern = sprintf("-%s.gz",y),"",lfiles)
  stations$id <- paste(stations$usaf,stations$wban,sep="-")
  stations <- stations[stations$id %in% lfiles,]
  stations$id <- NULL
  return(stations)
}

GeneraMapaEstaciones <- function(pais=NULL,activo=TRUE,loc="",yy){
  #filtros
  stations <- ExtraeTodasEstaciones()
  stations$sel <- FALSE
  
  #iconos
  
  h_icon <- icons(ifelse(stations$sel,"yellow_cross.png","blue_cross.png"))
  
  #if (activo) { stations <- stations %>% dplyr::filter(end==2015)}
  if (activo) { stations <- ExtraeEstacionesActivas(yy)}
  if (!is.null(pais) & pais!="Todos") { stations <- stations %>% dplyr::filter(country_name==pais)}
  
   if (loc!="") {
     coords <- ggmap::geocode(location = loc)
     leaflet(data=stations) %>% addTiles(urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}")  %>% addMarkers(~lon,~lat, label=~paste(paste(usaf,wban,sep="-"),"<br>",name),labelOptions = labelOptions(noHide = T),layerId=~paste(paste(usaf,wban,sep="-")),clusterOptions = markerClusterOptions(showCoverageOnHover=FALSE),icon=h_icon) %>% setView(lng=coords$lon,lat=coords$lat,zoom=10)
   } else {
    leaflet(data=stations) %>% addTiles(urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}")  %>% addMarkers(~lon,~lat, label=~paste(paste(usaf,wban,sep="-"),name),layerId=~paste(paste(usaf,wban,sep="-")),labelOptions = labelOptions(noHide = T),clusterOptions = markerClusterOptions(),icon=h_icon) %>% setView(lng=0,lat=0,zoom=3)
  }
}

ExtraeEstaciones <-function(pais=NULL,activo=TRUE,loc="",yy){
  stations <- ExtraeTodasEstaciones()
  stations$sel <- FALSE
  if (!is.null(pais) & pais!="Todos") { stations <- stations %>% dplyr::filter(country_name==pais)}
  if (activo) { 
    lfiles <- getURL(url=sprintf("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/%s/",yy),.opts = curlOptions(ftplistonly=TRUE))
    lfiles <- strsplit(lfiles,"\n")[[1]]
    lfiles <- gsub(pattern = sprintf("-%s.gz",yy),"",lfiles)
    stations$id <- paste(stations$usaf,stations$wban,sep="-")
    stations <- stations[stations$id %in% lfiles,]
    stations$id <- NULL
  }
  return(stations)
}

FiltrarCercano <- function(data){
  #primero redondeo la hora
  data$nearest_fecha <- as.POSIXct(round(data$time,units="hours"))
  data$diff <- abs(data$time-data$nearest_fecha)
  data <- data %>% dplyr::group_by(usaf,nearest_fecha) %>% dplyr::slice(which.min(diff))
  data$nearest_fecha <- NULL
  data$diff <- NULL
  return(data)
}

GraficosSincronizados <- function(data,variable,tipo){
    #######################################
  titulo <- sprintf("%s %s",conf[conf$variable==variable,]$nombre,paste(data[1,1],data[1,2],sep="-"))
    if (variable=="precip"){
      periods <- unique(data$precip_period)
      data <- data[!is.na(data$precip_period) & data$precip_period==min(periods,na.rm=TRUE),]
    }
    if (variable=="rad"){
      periods <- unique(data$rad_period)
      data <- data[!is.na(data$rad_period) & data$rad_period==min(periods,na.rm=TRUE),]
    }
  parcial <- data[,c("time",variable)]
  parcial <- xts(parcial[,2],order.by = parcial$time,tzone="UTC")
  dygraph(data = parcial,main = titulo,group = "Grupo") %>% dyAxis("y",valueRange=c(min(as.numeric(parcial[,1]),na.rm=TRUE),max(as.numeric(parcial[,1]),na.rm=TRUE))) %>% dyRangeSelector()
}

ListaArchviosFTP <- function(year1,year2){
  archivos <- vector("list",length(year1:year2))
  n <- 1
  for (y in year1:year2){
    print(y)
    lfiles <- getURL(url=sprintf("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/%s/",y),.opts = curlOptions(ftplistonly=TRUE))
    lfiles <- strsplit(lfiles,"\n")[[1]]
    lfiles <- gsub(pattern = sprintf("-%s.gz",y),"",lfiles)
    saveRDS(lfiles,file=sprintf("lista_de_archivos_%s",y))
  }
}

ListaArchivosFTPCurrentYear <- function(){
  today <- Sys.Date()
  #que solo se ejecute los dias 1 y 15
  if (day(today) %in% c(1,15)){
    lfiles <- getURL(url=sprintf("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/%s/",year(today)),.opts = curlOptions(ftplistonly=TRUE))
    lfiles <- strsplit(lfiles,"\n")[[1]]
    lfiles <- gsub(pattern = sprintf("-%s.gz",y),"",lfiles)
    saveRDS(lfiles,file=sprintf("files_ftp/lista_de_archivos_%s.rds",y))
  }
}
