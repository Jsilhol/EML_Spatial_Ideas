### idées de fonctions dans la but de faciliter le renseignement/inférer des métadonnées de fichiers SIG vectors

library(dplyr)
library(EML)
# set directory
setwd("C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/Données SIG/GeoJSON")

# TODO: remettre au clair spatial.path, data.path etc ....
template_spatial_attributes <- function(
  path, # path to template
  spatial.path, # path to spatial file
  name.file #name of spatial file 
  ){

  # upload file
  spatial_file <- sf::st_read(spatial.path)
  
  data.path <- stringr::str_replace(string = spatial.path, 
                                    pattern = name.file,
                                    replacement = "")
  
  # Write new csv file with only data attributes (without geometry info)
  message("Writing data.csv")
  write.csv(x = as.data.frame(x = spatial_file, make.names=FALSE) %>% dplyr::select(-geometry),
            file = paste0(data.path,"/data.csv"), row.names = FALSE)
  
  # Make attributes template 
 
  EMLassemblyline::template_table_attributes(
    path = path,
    data.path = data.path,
    data.table = "data.csv",
    write.file = TRUE,
    x = NULL)
}


template_spatial_attributes(path = "C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/Données SIG/GeoJSON",
                            spatial.path = "C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/Données SIG/GeoJSON/data_7.GeoJSON",
                            name.file = "data_7.GeoJSON")

template_table_attributes(path = "C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/Données SIG", data.path = "C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/Données SIG", data.table = "data.csv")




template_spatial_reference <- function(
  path, # lieu de création des templates
  data.path, #path to data directory
  file.name, #character, name of data file
  spatial.type # = "vector" or "raster" depending on file, determines how function will read the file
){
  #### Notes -----------------------------------------------------------
  # TODO :add reading raster files
  # TODO :validate arguments
    # valider type du fichier 
    # valider que path et data.path existent bien
  # TODO :do VertCoordSys filing part ? or remove it entirely ? don't know where the info is stored
  
  #### copy templates in wd --------------------------------------------
  # import geoCoordSys
  #FIXME: templates should be in file.system
  value <- file.copy(from = "C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/Templates_Spatial_Data/GeogCoordSys.txt", to = paste0(path, "/GeogCoordSys.txt"))
  
  #   Send message
  # TODO : Si validation des arguments, les templates ne peuvent pas etre copiés seulment parce qu'ils sont déjà présents
  #         Doc changer le message à "Template ... already exists !"
  if (isTRUE(value)){
    message("GeogCoordSys.txt is now in path")
  } else {
    message("Was not able to copy GeogCoordSys.txt into directory")
  }

  # import ProjCoordSys
  value <- file.copy(from = "C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/Templates_Spatial_Data/ProjCoordSys.txt", to = paste0(path, "/ProjCoordSys.txt"))
  #   Send message
  if (isTRUE(value)){
    message("ProjCoordSys.txt is now in path")
  } else {
    message("Was not able to copy ProjCoordSys.txt into directory")
  }
  # import VertCoordSys
  value <- file.copy(from = "C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/Templates_Spatial_Data/VertCoordSys.txt", to = paste0(path, "/VertCoordSys.txt"))
  #   Send message
  if (isTRUE(value)){
    message("VertCoordSys.txt is now in path")
  } else {
    message("Was not able to copy VertCoordSys.txt into directory")
  }
  
  # import GeogDescription
  value <- file.copy(from = "C:/Users/jhmsl/Desktop/Stage Concarneau/Spatial_data/Templates_Spatial_Data/GeogDescription.txt", to = paste0(path, "/VertCoordSys.txt"))
  #   Send message
  if (isTRUE(value)){
    message("GeogDescription is now in path")
  } else {
    message("Was not able to copy GeogDescription into directory")
  }
  
  #### fill in templates ----------------------------------------------
  #   read metadata of Coordinate Reference System of spatial file 
  
  if (data.type == "raster"){
    spatial_file <- raster::raster(data.path)
    raster_crs <- raster@crs
    wkt <- rgdal::showSRID(CRSargs(raster_crs))
  }
  else if (data.type == "vector"){
    spatial_file <- sf::st_read(data.path)
    wkt <- sf::st_crs(spatial_file)$wkt
  }
  
  #   extract metadata from wkt
  #   TODO: validate each info ??
  #   IDEA: stocker toutes les infos dans des attributs ??
  
  # ***GeoCoordSys***
  #     Name
  locate_geogcrs <- stringr::str_locate(wkt,'(GEOGCRS|BASEGEOGCRS)\\[\"[^[:punct:]]*')
  geogcrs <- stringr::str_sub(wkt, start = locate_geogcrs[1]+1+stringr::str_length("GEOGCRS["), end = locate_geogcrs[2])
  message(paste0("GEOGCRS: ",geogcrs))
  
  #     Datum
  locate_datum <- stringr::str_locate(wkt,'DATUM\\[\"[^[:punct:]]*' )
  datum <- stringr::str_sub(wkt, start = locate_datum[1]+1+stringr::str_length("DATUM["), end = locate_datum[2])
  message(paste0("Datum: ",datum))
  
  #     Spheroid
  # SPHEROIDName
  locate_spheroidName <- stringr::str_locate(wkt,"ELLIPSOID\\[\"[^[:punct:]]*")
  spheroidName <- stringr::str_sub(wkt, start = locate_spheroidName[1]+stringr::str_length("ELLIPSOID[\""), end = locate_spheroidName[2])
  message(paste0("SpheroidName: ",spheroidName))
  
  # SPHEROIDSemiAxisMajor
  locate_spheroidSemiAxisMajor <- stringr::str_locate(wkt,paste0('ELLIPSOID\\[\"',spheroidName,'\",[:digit:]+'))
  spheroidSemiAxisMajor <- stringr::str_sub(wkt, start = locate_spheroidSemiAxisMajor[1]+stringr::str_length(paste0('ELLIPSOID[\"',spheroidName,'\",')), end = locate_spheroidSemiAxisMajor[2])
  message(paste0("SpheroidSemiAxisMajor's lenghth: ",spheroidSemiAxisMajor))
  
  # SPHEROIDDenomFlatRatio
  locate_spheroidDenomFlatRatio <- stringr::str_locate(wkt,paste0('ELLIPSOID\\[\"',spheroidName,'\",',spheroidSemiAxisMajor,',[:digit:]+.[:digit:]+'))
  spheroidDenomFlatRatio <- stringr::str_sub(wkt, start = locate_spheroidDenomFlatRatio[1]+stringr::str_length(paste0('ELLIPSOID[\"',spheroidName,'\",',spheroidSemiAxisMajor,',')), end = locate_spheroidDenomFlatRatio[2])
  message(paste0("SpheroidDenomFlatRatio: ",spheroidDenomFlatRatio))
  
  #     PrimeMeridian
  # PRIMEMERIDIANName
  locate_primeMeridianName<- stringr::str_locate(wkt,'PRIMEM\\[\"[:alpha:]+')
  primeMeridianName <- stringr::str_sub(wkt, start = locate_primeMeridianName[1]+stringr::str_length('PRIMEM[\"'), end = locate_primeMeridianName[2])
  message(paste0("PrimeMeridianName: ",primeMeridianName))
  
  # PRIMEMERIDIANLongitude
  locate_primeMeridianLongitude<- stringr::str_locate(wkt,paste0('PRIMEM\\[\"',primeMeridianName,'\",[([:digit:]{1,3})|([:digit:]{1,3},[:digit:]+)]'))
  primeMeridianLongitude <- stringr::str_sub(wkt, start = locate_primeMeridianLongitude[1]+stringr::str_length(paste0('PRIMEM[\"',primeMeridianName,'\",')), end = locate_primeMeridianLongitude[2])
  message(paste0("PrimeMeridianLongitude: ",primeMeridianLongitude))
  
  #     Unit
  locate_unit<- stringr::str_locate(wkt,'ANGLEUNIT\\[\"[:alpha:]+')
  unit <- stringr::str_sub(wkt, start = locate_unit[1]+stringr::str_length('ANGLEUNIT[\"'), end = locate_unit[2])
  message(paste0("Coordinate System unit: ",unit))
  
  # Fill in template GeogCoordSys
  
  geogCoordSys <- as.data.frame(list(
    "GeogCoordSysName" = geogcrs,
    "DatumName" = datum,
    "SpheroidName" = spheroidName,
    "SpheroidSemiAxisMajor" = spheroidSemiAxisMajor,
    "SpheroidDenomFlatRatio" = spheroidDenomFlatRatio,
    "PrimeMeridianName" = primeMeridianName,
    "PrimeMEridianLongitude" = primeMeridianLongitude,
    "UnitName" = unit))

  # TODO : empecher de runner encore et encore 
  utils::write.table(x = geogCoordSys,
                     file = paste0(path,"/GeogCoordSys.txt"),
                     append = FALSE,
                     sep = "\t",
                     row.name= FALSE,
                     quote = FALSE,
                     fileEncoding = "UTF-8")
  message("GeogCoordSys.txt has been drafted. Please check for errors")
  
  # ***ProjCoordSys***
  #     Name
  message("Extracting ProjCoordSys metadata ...")
  try(locate_projCoordSysName<- stringr::str_locate(wkt,'PROJCRS\\[\"[^\"]+'))
  if (!anyNA(locate_projCoordSysName)){
    message(locate_projCoordSysName)
    projCoordSysName <- stringr::str_sub(wkt, start = locate_projCoordSysName[1]+stringr::str_length('PROJCRS[\"'), end = locate_projCoordSysName[2])
    message(projCoordSysName)
    message(paste0("Projection Coordinate System unit: ",projCoordSysName))
  
    
    #     Parameters
    #       NAME
    locate_parameterName <- stringr::str_locate_all(wkt,'PARAMETER\\[\"[^\"]+\"')
    locate_parameterName <- as.data.frame(locate_parameterName)
    
    for (i in 1:nrow(locate_parameterName)){
      message(locate_parameterName$start[i])
      locate_parameterName$start[i] <- locate_parameterName$start[i] + stringr::str_length('PARAMETER[\"')
      message(locate_parameterName$start[i])
    }
    parameterName <- c()
    for (i in 1:nrow(locate_parameterName)){
      message(parameterName)
      parameterName[i] <- stringr::str_sub(wkt, start = locate_parameterName$start[i], end = locate_parameterName$end[i]-1)
      message(parameterName)
    } 
    parameterName
    message(paste0("Parameters: ",parameterName))
    
    #       VALUE 
    
    value <- c()
    for (i in 1:length(parameters)){
      locatevalue <- stringr::str_locate(wkt, paste0('PARAMETER\\["',parameters[i],'",[:digit:]+'))
      message(locatevalue)
      value[i] <- stringr::str_sub(wkt, start = locatevalue[1]+stringr::str_length(paste0('PARAMETER["',parameters[i],'",')), end = locatevalue[2])
      message(value)
    }
    #       UNIT
    parameterUnit <- c()
    for (i in 1:length(parameters)){
      locate_parameterUnit <- stringr::str_locate(wkt,paste0('PARAMETER\\["',parameters[i],'",', value[i], ',[:space:]*[:alpha:]+UNIT\\["[:alpha:]+'))
      message(locate_parameterUnit)
      full_sentence <- stringr::str_sub(wkt, start = locate_parameterUnit[1], end = locate_parameterUnit[2])
      locate_last_quote <- stringr::str_locate(full_sentence,'UNIT\\["')
      parameterUnit[i] <- stringr::str_sub(wkt, start=locate_last_quote[2]+locate_parameterUnit[1], end = locate_parameterUnit[2])
      message(parameterUnit)
    }
   
    
    # fill in templates
    projCoordSys <- as.data.frame(list("ProjectionName"=projCoordSysName,"ParameterName" = parameters, "ParameterDescription" = c("!Please describe parameter!"), "ParameterValue"= value, "Unit" = parameterUnit ))
    projCoordSys <-  unname(projCoordSys)
    utils::write.table(x = projCoordSys,
                       file = paste0(path,"/ProjCoordSys.txt"),
                       append = TRUE,
                       sep = "\t",
                       row.name= FALSE,
                       quote = FALSE,
                       fileEncoding = "UTF-8")
  } else {message("No Projection metadata found")}

  #Don't know yet where to find altitudeSysDef and DepthSysDef
}

  


make_spatial_vector <- function(
  entityName, # name we want to call the spatial vector
  template.path, # path to templates
  data.path){
  # path to data
  
  # fonction qui ve prendre toutes les infos ammassées et les traduires en EML
  # return une entité EML Spatial Vector
  
  # Cherche : geometry et geometric bject count 
  #           Coverage 
  #           HorizCoordSysName : possible que pour un nombre limité de CS
  #           Entity description 
  
  
  # METADATA already good to go : filed in templates GeogCoordSys, ProjCoordSys, VertCoordSys, GeogDescription et attributes
  
  # 
  # read Vector
  vector_name <-  sf::st_read(data.path)
  
  # set physical ------------------------------------------------------------
  
  # # distribution
  # 
  # fileURL <- yaml::yaml.load_file("config.yaml")$baseURL
  # 
  # fileDistribution <- EML::eml$distribution(
  #   EML::eml$online(url = paste0(fileURL, zipped_name))
  # )
  # 
  # # data format
  # 
  # fileDataFormat <- EML::eml$dataFormat(
  #   externallyDefinedFormat = EML::eml$externallyDefinedFormat(
  #     formatName = "Esri Shapefile (zipped)")
  # )
  # 
  # # file size
  # 
  # fileSize <- EML::eml$size(unit = "byte")
  # fileSize$size <- deparse(file.size(zipped_name))
  # 
  # # authentication
  # 
  # fileAuthentication <- EML::eml$authentication(method = "MD5")
  # fileAuthentication$authentication <- md5sum(zipped_name)
  # 
  # # construct physical
  # 
  # spatialVectorPhysical <- EML::eml$physical(
  #   objectName = zipped_name,
  #   authentication = fileAuthentication,
  #   size = fileSize,
  #   dataFormat = fileDataFormat,
  #   distribution = fileDistribution
  # )
  # 
  
  # Geographic coverage and description 
  geoDescription <- read.table(paste0(path,"/GeogDescription.txt"))
  if (is.na(geoDescription) | is.null(geoDescription) | geoDescription == "") {
    stop("Please fill in GeogDescription.txt")}


  if (sf::st_bbox(vector_name)[["xmin"]] < -180 | sf::st_bbox(vector_name)[["xmin"]] > 180) {
    vector_lat_long <- sf::st_transform(vector_name, crs = 4326)
  
  spatialCoverage <- EML::set_coverage(
    geographicDescription = geoDescription,
    westBoundingCoordinate =  sf::st_bbox(vector_lat_long)[["xmin"]],
    eastBoundingCoordinate =  sf::st_bbox(vector_lat_long)[["xmax"]],
    northBoundingCoordinate = sf::st_bbox(vector_lat_long)[["ymax"]],
    southBoundingCoordinate = sf::st_bbox(vector_lat_long)[["ymin"]]
  )
  
  
} else {
  
  spatialCoverage <- EML::set_coverage(
    geographicDescription = geoDescription,
    westBoundingCoordinate =  sf::st_bbox(vector_name)[["xmin"]],
    eastBoundingCoordinate =  sf::st_bbox(vector_name)[["xmax"]],
    northBoundingCoordinate = sf::st_bbox(vector_name)[["ymax"]],
    southBoundingCoordinate = sf::st_bbox(vector_name)[["ymin"]]
  )
  
  newSV <- EML::eml$spatialVector(
    entityName = entityName,
    entityDescription = description,
    physical = spatialVectorPhysical,
    coverage = spatialCoverage,
    attributeList = attributes,
    geometricObjectCount = nrow(vector_name),
    id = zipped_name
  )
  
}
  # add geometry type -------------------------------------------------------
  
  sfGeometry <- attr(file$geometry, "class")[[1]]
  
  if (grepl("polygon", sfGeometry, ignore.case = TRUE)) {
    if (grepl("multipolygon", sfGeometry, ignore.case = TRUE)){
      objectGeometry <- "MultiPolygon" 
    } else {
      objectGeometry <- "Polygon"
    } 
  } else if (grepl("point", sfGeometry, ignore.case = TRUE)) {
    if (grepl("multipoint", sfGeometry, ignore.case = TRUE)){
      objectGeometry <- "MultiPont"
    } else {
      objectGeometry <- "Point"
    }
  } else if (grepl("linestring", sfGeometry, ignore.case = TRUE)) {
    if (grepl("multilinestrind", sfGeometry, ignore.case = TRUE)){
      objectGeometry <- "MultiLineString"
    } else {
      objectGeometry <- "LineString"
    }
  } else if (grepl("linearRing", sfGeometry, ignore.case = TRUE)) {
    
    objectGeometry <- "LinearRing"
  } else {
    stop(paste0("undetermined geometry: ", attr(vector_name$geometry, "class")[[1]]))
  }
  newSV$geometry <- objectGeometry
  
  
  # add spatial reference  --------------------------------------------------
  # TODO : 
  
  
  horizCoordSysName <- get_EML_CS(data.path)
  
  # Import metadata from Templates
  GeogCoordSys <- as.list(read.table(paste0(path, "/GeogCoordSys.txt"), headers= T, sep="\t"))
  
  GeogCoordSys <- EML::eml$geogCoordSys(
    datum = GeogCoordSys["DatumName"][[1]],
    spheroid = GeogCoordSys["SpheroidName"][[1]],
    primeMeridian = GeogCoordSys["PrimeMeridianName"][[1]],
    unit = GeogCoordSys["UnitName"][[1]],
    name = GeogCoordSys["GeogCoordSysName"][[1]]
  )
  
  ProjCoordSys <- as.list(read.table(paste0(path, "/ProjCoordSys.txt"), headers= T, sep="\t"))
  if (class(ProjCoordSys[[1]])=="logical"){
    message("No projection metadata to add to EML")
  } else {
    Projection <- EML::eml$projection(
      parameter = c(ProjCoordSys["ParameterName"],ProjCoordSys["ParameterDescription"], ProjCoordSys["ParameterValue"]),
      unit = ProjCoordSys["Unit"][[1]],
      name = ProjCoordSys["ProjectionName"][[1]]
    )
  }
  
  VertCoordSys <- read.table(paste0(path, "/VertCoordSys.txt"), headers= T, sep="\t")
  if (class(VertCoordSys[[1]])=="logical"){
    message("No Vertical Coordinate System metadata to add to EML")
  } else {
    Projection <- EMl::eml$projection(
      parameter = list(projCoordSys["ParameterName"],projCoordSys["ParameterDescription"], projCoordSys["ParameterValue"]),
      unit = projCoordSys["UnitName"],
      name = projCoordSys["ProjectionName"]
    )
  }
  
  as.list
  spatial_ref <- EML::eml$spatialReference(
    horizCoordSysName = coord_sys
  )
  
  newSV$spatialReference <- spatial_ref

}

