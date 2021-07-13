#### Identification des fonctions utiles de capemlGIS

library(rgdal)
library(sp)
library(capeml)
library(capemlGIS)
library(sf)
library(stringr)

spatialDataEntity <- readOGR("C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/shapefile/shapefile")
spatialDataEntity_st <- st_read("C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/shapefile/shapefile")

ogrInfo("C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/shapefile/shapefile")
CS_EPSG <- make_EPSG()

CRS(spatialDataEntity)
# Provient de la fonction get_eml_projection() de capemlGIS :

### TOUT CELA NE FONCTIONNE PAS CAR PAS TOUS LES crs ONT PROJ, ZONE, DATUM et UNITS
# # parse individual components of the layer's projection
# entityProj <- paste0("+proj=", str_match(crs(spatialDataEntity), "(proj=)(\\w+)")[,3]) # projection
# entityZone <- paste0("+zone=", str_match(crs(spatialDataEntity), "(zone=)(\\w+)")[,3]) # zone
# entityDatum <- paste0("+datum=", str_match(crs(spatialDataEntity), "(datum=)(\\w+)")[,3]) # datum
# entityUnits <- paste0("+units=", str_match(crs(spatialDataEntity), "(units=)(\\w+)")[,3]) # units 
# 
# 
# # generate a table of EPSG CRSs from rgdal package
# # filter table of EPSG CRSs based on CRS criteria of input file
# # this match is exclusive to Northern hemisphere references
# entityProjection <- rgdal::make_EPSG() %>%
# dplyr::filter(grepl(entityProj, prj4) & grepl(entityZone, prj4) & grepl(entityDatum, prj4) & grepl(entityUnits, prj4) & !grepl("+south", prj4)) %>%
# dplyr::mutate(code = as.integer(code)) %>%
# dplyr::mutate(note = as.character(note)) %>%
# dplyr::mutate(prj4 = as.character(prj4))
# 
# filter <- EPSG %>%filter(grepl(entityProj, EPSG$prj4) & grepl(entityZone, EPSG$prj4) & grepl(entityDatum, EPSG$prj4) & grepl(entityUnits, EPSG$prj4) & !grepl("+south", EPSG$prj4)) 

# Autant obtenir le EPSG directement de wkt ?
# essaie
wkt_file <- st_crs(spatialDataEntity_st)$wkt
# extract EPSG :
locate_EPSG <- str_locate(wkt_file,'\"EPSG\",[:digit:]{4}')

EPSG<- as.numeric(str_sub(wkt_file, start = locate_EPSG[1]+str_length('\"EPSG\",') , end = locate_EPSG[2]))
entityProjection <- CS_EPSG$note[CS_EPSG$code==EPSG]
# modif the EPSG text to match with accepted EML CRS names - 
# ONLY FOR WGS 84
entityProjectionString <- str_c(
  "GCS_",
  str_replace_all(entityProjection, "[^[:alnum:]] ", "") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("84", "1984"),
  sep = "")

# don't need this
# return the EML accepted version of the CRS as a string
unlist(emlCoordSystems[grepl(entityProjectionString, emlCoordSystems$coordSystemName, ignore.case = TRUE),], use.names = FALSE)


# fonction pour obtenir le nom du CS -> ne fonctionne que pour des vectors :
# /!\ need to run this first :
CS_EPSG <- rgdal::make_EPSG()

get_CRS_name <- function(file.path
){
  #import file
  spatialDataEntity_st <- sf::st_read(file.path)
  
  wkt_file <- sf::st_crs(spatialDataEntity_st)$wkt
  # extract EPSG :
  locate_EPSG <- stringr::str_locate(wkt_file,'\"EPSG\",[:digit:]{4,}')
  EPSG<- as.numeric(stringr::str_sub(wkt_file, start = locate_EPSG[1]+stringr::str_length('\"EPSG\",') , end = locate_EPSG[2]))
  entityProjection <- CS_EPSG$note[CS_EPSG$code==EPSG]
  
  # # modif the EPSG text to match with accepted EML CRS names - 
  # # WORKS ONLY FOR WGS 84
  # entityProjectionString <- str_c(
  #   "GCS_",
  #   str_replace_all(entityProjection, "[^[:alnum:]] ", "") %>%
  #     str_replace_all(" ", "_") %>%
  #     str_replace_all("84", "1984"),
  #   sep = "")
  return(entityProjection)
}

name <- get_CRS_name("C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/shapefile/shapefile")
sf::st_crs(sf::st_read("C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/shapefile/shapefile")
)
get_CRS_name(data.path2)
sf::st_crs(sf::st_read(data.path2))


##### fonction qui v?rifie le HorizCoordSystem 
# /!\ Fonctionne pas /!\
# TODO : corriger le problème
verify_CRS <- function(file.path){
  if (stringr::str_detect(file.path,"(.nc$)|(.tiff$)")==TRUE){
    #import data as raster
    suppressWarnings(raster <- raster::raster(file.path))
    # build HorizCoordSysName
    # only works for WGS84 
    if (raster::proj4string(raster) == "+proj=longlat +datum=WGS84 +no_defs"){
      entityProjection <- "GCS_WGS_1984"
      message("CRS is verified")
      return( entityProjection)
    }
    
    #Raise Error
    else {
      stop("Can't manage CRS other then WGS84")
    }
  }
  if (stringr::str_detect(file.path,"(.shp$)|(.geojson$)")==TRUE){
    # import data as vector
    vector <- sf::st_read(file.path)
    # verify EPSG
    wkt_file <- sf::st_crs(spatialDataEntity_st)$wkt
    locate_EPSG <- stringr::str_locate(wkt_file,'\"EPSG\",[:digit:]{4,}')
    EPSG<- as.numeric(stringr::str_sub(wkt_file, start = locate_EPSG[1]+stringr::str_length('\"EPSG\",') , end = locate_EPSG[2]))
    if (EPSG == 4326){
      entityProjection <- "GCS_WGS_1984"
      message("CRS is verified")
      return(entityProjection)
    }
    
    else {
      stop("CRS of data is not WGS84")
    }
  }
  else {
    stop("File type is not managed. Can only read .shp, .geojson, .nc and .tiff spatial files")
  }
}

path <- "C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/Donn?es SIG/Netcdf/dataset-ibi-reanalysis-bio-005-003-monthly-regulargrid_1510914389133.nc"
verify_CRS(path)

# function to get EML compliant CS if possible
get_EML_CS <- function(data.path,
                       file.type,
                       EML_compliant){
  # si GeogCoordSys parmis la liste : les reconnaits et donne le EML compliant CS
  # sinon renvoie "EML_Compliant_CS_not_found"
  
  #verify data.path
  if (file.type == "vector"){
    vector <- sf::st_read(data.path)
    wkt <- sf::st_crs(vector)$wkt
  }
  else if (file.type == "raster"){
    
  }
  locate_geogcrs <- stringr::str_locate(wkt,'(GEOGCRS|BASEGEOGCRS)\\[\"[^[:punct:]]*')
  geogcrs <- stringr::str_sub(wkt, start = locate_geogcrs[1]+1+stringr::str_length("GEOGCRS["), end = locate_geogcrs[2])

  if (missing(EML_compliant)){
    # TODO: add other EML CS 
    if (geogcrs == "WGS 84"){
      EML_CS <- "GCS_WGS_1984"
      message("EML compliant CS found: ", EML_CS)
    }
    else {
      EML_CS <- "EML_Compliant_CS_not_found"
      message("EML compliant CS not found")
    }
  } else{
    EML_CS <- EML_compliant
  }
  return(EML_CS)
}
get_EML_CS(data.path3)
  
  
  
  