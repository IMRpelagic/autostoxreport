

#' Creation of a doc object
#' @export
new.word.doc=function(){
  my.doc=officer::read_docx()
  return(my.doc)
}


#' add.page.breake
#'
#'TO be entered
#'
#' @param out a doc object
#' @export
add.page.break=function(doc){
  officer::body_add_break(doc, pos="after")
  return("page break added")
}




#' add.title
#'
#'TO be entered
#'
#' @param out a doc object
#' @export
add.title=function(doc, my.title){
  my.prop=officer::fp_text(font.size = 8.5, bold = TRUE, font.family = "Palantino Linotype")
  the.title=officer::fpar(ftext(my.title, prop=my.prop))
  officer::body_add_fpar(doc, the.title)
  officer::body_add_par(doc, " ")
  return("title added")
}


#' add.img
#'
#'TO be entered
#'
#' @param out a doc object
#' @export
add.image=function(doc, value, h=5, w=5){
  officer::body_add_img(doc, value,
               height=h, width=w,
               style="centered")
  return("image added")
}


#' startup
#'
#' Rerun the project if it has not been done beore
#'
#' @param projectpath path to the stox project
#' @export
startup <- function(projectPath){
  RstoxFramework::runProcesses(projectPath,modelName = 'baseline')
  RstoxFramework::runProcesses(projectPath,modelName = 'analysis')
  RstoxFramework::runProcesses(projectPath,modelName = 'report')
}



#' getStratumPolygonFromGeojsonfFile
#'
#' This function grap the stratum definition
#'
#' @param geojsonFilePath path to the geojsonfile
#' @return out a data.table object of the stratum definition
#' @export
getStratumPolygonFromGeojsonfFile <- function(geojsonFilePath, textFilePath = NULL) {
  stratumPolygon <- RstoxBase::readGeoJSON(geojsonFilePath)
  listOfXY <- RstoxBase::getStratumPolygonList(stratumPolygon)
  listOfXY <-lapply(listOfXY, data.table::setnames, c("V1", "V2"), c("x", "y"))
  stratumNames <- RstoxBase::getStratumNames(stratumPolygon)
  names(listOfXY) <- stratumNames
  out <- data.table::rbindlist(listOfXY, idcol = "StratumName")
  return(out)
}



#' plot_progression
#'
#' This function plot the progression of vessels from a Stox project
#'
#' @param projectPath Path to the stox project
#' @param doc a doc object to store figures to word document
#' @return doc a doc object to store figures to word document
#' @export
plot_progression <- function(projectPath,doc){
  #Get data and processes from baseline
  baseline <- RstoxFramework::getModelData(projectPath, modelName = 'baseline')
  baseline_processes<- RstoxFramework::getProcessTable(projectPath,modelName = 'baseline')

  #Path to where the stratum polygon is
  geojsonFilePath<-paste0(projectPath,'/output/baseline/',baseline_processes[baseline_processes$functionOutputDataType=='StratumPolygon',]$processName,'/',
                          baseline_processes[baseline_processes$functionOutputDataType=='StratumPolygon',]$functionOutputDataType,'.geojson')

  #get stratum polygon
  stratum <- getStratumPolygonFromGeojsonfFile(geojsonFilePath)

  #Loop through all processes with the StoxAcousticData format as an output
  for(bp in baseline_processes[baseline_processes$functionOutputDataType=='StoxAcousticData',]$processName){
    print(bp)

    #Get acoustic data from current process
    data <- baseline[names(baseline)%in% bp]

    #Display map
    gg_plot<-ggplot2::ggplot(data=stratum,aes(x=x,y=y,group=StratumName))+
      ggplot2::geom_polygon(colour='black',fill='red',alpha=0.4)+
      ggplot2::geom_point(data=data[[1]]$Log,aes(x=Longitude,y=Latitude,colour=DateTime,group=NULL),size=0.1)+
      ggplot2::xlab('Longitude')+ggplot2::ylab('Latitude')+ ggplot2::theme(panel.background = element_blank())+
      ggplot2::theme(legend.position = "bottom",
            legend.key.width=unit(0.1,"npc"))

    add.title(doc=doc,my.title = paste0('AutoReport - progress map - ', bp))
    officer::body_add_gg(doc, value = gg_plot, style = "centered" )
    add.page.break(doc = doc)

  }
  return(doc)
}

#' plot_NASC
#'
#' This function reads the stox project and grap the MeanNASCdata object
#' A figure is then made to view the mean NASC values
#'
#' @param projectPath Path to the stox project
#' @param doc a doc object to store figures to word document
#' @return A ggplot figure
#' @export
plot_NASC <- function(projectPath,doc=NULL){
  #Get data and processes from baseline
  baseline <- RstoxFramework::getModelData(projectPath, modelName = 'baseline')
  baseline_processes<- RstoxFramework::getProcessTable(projectPath,modelName = 'baseline')


  #Path to where the stratum polygon is
  geojsonFilePath<-paste0(projectPath,'/output/baseline/',baseline_processes[baseline_processes$functionOutputDataType=='StratumPolygon',]$processName,'/',
                          baseline_processes[baseline_processes$functionOutputDataType=='StratumPolygon',]$functionOutputDataType,'.geojson')

  #get stratum polygon
  stratum <- getStratumPolygonFromGeojsonfFile(geojsonFilePath)

  #Loop through all NASC functions
  for(bp in baseline_processes[baseline_processes$functionOutputDataType=='MeanNASCData',]$processName){
    data <- baseline[names(baseline)%in% bp]
    data <- plyr::join(data[[1]]$Data,data[[1]]$Resolution)
    data <- data[!is.na(data$PSU),]

    #Display map
    gg_plot<-ggplot2::ggplot(data=stratum,aes(x=x,y=y,group=StratumName))+
      ggplot2::geom_polygon(colour='black',fill='red',alpha=0.4)+
      ggplot2::geom_point(data=data,aes(x=Longitude,y=Latitude,group=NULL,size=NASC,colour=NASC),alpha=0.1)+
      ggplot2::geom_point(data=data,aes(x=Longitude,y=Latitude,group=NULL),size=0.1)+
      viridis::scale_color_viridis(option = "plasma")+ ggplot2::theme(panel.background = element_blank())+
      ggplot2::xlab('Longitude')+ggplot2::ylab('Latitude')

    if(!is.null(doc)){
      add.title(doc=doc,my.title = paste0('AutoReport - NASC map - ', bp))
      officer::body_add_gg(doc, value = gg_plot, style = "centered" )
      add.page.break(doc = doc)}else{show(gg_plot)}

  }
return(doc)
}
