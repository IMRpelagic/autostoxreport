

#' Creation of a doc object
#' @export
#' @import officer
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
#' @import officer
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
#' @import officer
add.title=function(doc, my.title){
  my.prop=officer::fp_text(font.size = 8.5, bold = TRUE, font.family = "Palantino Linotype")
  the.title=officer::fpar(officer::ftext(my.title, prop=my.prop))
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
#' @import officer
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
#' @import ggplot2
#' @import officer
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
    gg_plot<-ggplot2::ggplot(data=stratum,ggplot2::aes(x=x,y=y,group=StratumName))+
      ggplot2::geom_polygon(colour='black',fill='red',alpha=0.4)+
      ggplot2::geom_point(data=data[[1]]$Log,ggplot2::aes(x=Longitude,y=Latitude,colour=DateTime,group=NULL),size=0.1)+
      ggplot2::xlab('Longitude')+ggplot2::ylab('Latitude')+ ggplot2::theme(panel.background = ggplot2::element_blank())+
      ggplot2::theme(legend.position = "bottom",
            legend.key.width=ggplot2::unit(0.1,"npc"))

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
#' @import ggplot2
#' @import plyr
#' @import viridis
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
    gg_plot<-ggplot2::ggplot(data=stratum,ggplot2::aes(x=x,y=y,group=StratumName))+
      ggplot2::geom_polygon(colour='black',fill='red',alpha=0.4)+
      ggplot2::geom_point(data=data,ggplot2::aes(x=Longitude,y=Latitude,group=NULL,size=NASC,colour=NASC),alpha=0.1)+
      ggplot2::geom_point(data=data,ggplot2::aes(x=Longitude,y=Latitude,group=NULL),size=0.1)+
      viridis::scale_color_viridis(option = "plasma")+ ggplot2::theme(panel.background = ggplot2::element_blank())+
      ggplot2::xlab('Longitude')+ggplot2::ylab('Latitude')

    if(!is.null(doc)){
      add.title(doc=doc,my.title = paste0('AutoReport - NASC map - ', bp))
      officer::body_add_gg(doc, value = gg_plot, style = "centered" )
      add.page.break(doc = doc)}else{show(gg_plot)}

  }
return(doc)
}



#' plot_Imputed_link
#'
#' This figure show the linkege where the impute has revieced samples from 
#'
#' @param projectPath Path to the stox project
#' @param doc a doc object to store figures to word document
#' @return A ggplot figure
#' @export
#' @import ggplot2
#' @import officer

plot_Imputed_link <- function(projectPath,doc=NULL){
  
  #Get data and processes from baseline
  baseline <- RstoxFramework::getModelData(projectPath, modelName = 'baseline')
  baseline_processes<- RstoxFramework::getProcessTable(projectPath,modelName = 'baseline')
  
  
  #Path to where the stratum polygon is
  geojsonFilePath<-paste0(projectPath,'/output/baseline/',baseline_processes[baseline_processes$functionOutputDataType=='StratumPolygon',]$processName,'/',
                          baseline_processes[baseline_processes$functionOutputDataType=='StratumPolygon',]$functionOutputDataType,'.geojson')
  
  #get stratum polygon
  stratum <- getStratumPolygonFromGeojsonfFile(geojsonFilePath)
  
  for(bp in baseline_processes[baseline_processes$functionOutputDataType=='SuperIndividualsData',]$processName){
    print(bp)
    
    data <- baseline[names(baseline)%in% bp]
    
    if('ReplaceLevel'%in%names(data[[1]])){
      sub_data <- data[[1]]
      sub_data<-sub_data[!is.na(sub_data$ReplaceIndividual),]
      sub_data<-sub_data[sub_data$ReplaceLevel!='Haul',]
      sub_data$Latitude2 <- NaN
      sub_data$Longitude2 <- NaN
      for(rpi in sub_data$ReplaceIndividual){
        lat2 <-(unique(data[[1]][data[[1]]$Individual==rpi,]$Latitude))
        lon2<-(unique(data[[1]][data[[1]]$Individual==rpi,]$Longitude))
        sub_data[sub_data$ReplaceIndividual==rpi,]$Latitude2<-lat2[!is.na(lat2)]
        sub_data[sub_data$ReplaceIndividual==rpi,]$Longitude2<-lon2[!is.na(lon2)]
      }
      
      #Display map
      gg_plot<-ggplot2::ggplot(data=stratum,ggplot2::aes(x=x,y=y,group=StratumName))+
        ggplot2::geom_polygon(colour='black',fill='grey',alpha=0.4)+
        ggplot2::geom_point(data=data[[1]],ggplot2::aes(x=Longitude,y=Latitude,colour=ReplaceLevel,group=NULL),size=1)+
        ggplot2::xlab('Longitude')+
        ggplot2::ylab('Latitude')+
        ggplot2::geom_segment(data=sub_data,ggplot2::aes(xend=Longitude,yend=Latitude,
                                                x = Longitude2,y=Latitude2,group=NULL,colour=ReplaceLevel),
                              arrow=ggplot2::arrow(length=ggplot2::unit(0.5,'cm')))
      show(gg_plot)
      if(!is.null(doc)){
        add.title(doc=doc,my.title = paste0('AutoReport - Impute Linkage - ', bp))
        officer::body_add_gg(doc, value = gg_plot, style = "centered" )
        add.page.break(doc = doc)}else{show(gg_plot)}
    }
  }
  return(doc)
}


#' plot_BootstrapDiag
#'
#' This function plot the progression of vessels from a Stox project
#'
#' @param projectPath Path to the stox project
#' @param doc a doc object to store figures to word document
#' @return doc a doc object to store figures to word document
#' @export
#' @import ggplot2
#' @import officer
plot_BootstrapDiag<- function(projectPath,doc){
  
  
  analysis <- RstoxFramework::getModelData(projectPath, modelName = 'analysis')
  analysis_processes<- RstoxFramework::getProcessTable(projectPath,modelName = 'analysis')
  
  data <- analysis$Bootstrap$ImputeSuperIndividuals
  
  test <- aggregate(Abundance ~ IndividualAge+BootstrapID,data,sum)
  
  Abundance_out <- c()
  for(i in head(unique(test$BootstrapID),-1)){
    Abundance <- c()
    tt<- aggregate(Abundance ~ IndividualAge, test[test$BootstrapID%in%c(0:i+1),],mean)
    Abundance$Abundance_mean <- tt$Abundance
    Abundance$Abundance_age <- tt$IndividualAge
    Abundance$Abundance_sd<-aggregate(Abundance ~ IndividualAge, test[test$BootstrapID%in%c(0:i+1),],sd)$Abundance
    Abundance$BootstrapID <- i+1
    Abundance_out <- rbind(Abundance_out, as.data.frame(Abundance))
  }
  
  gg_plot<-ggplot2::ggplot(data=Abundance_out,ggplot2::aes(x=BootstrapID,y=Abundance_mean,
                                                           ymin=Abundance_mean-Abundance_sd,ymax=Abundance_mean+Abundance_sd))+
    ggplot2::geom_line()+ggplot2::geom_ribbon(alpha=0.4)+ggplot2::facet_wrap(Abundance_age~.,scales = "free_y")
  
  if(!is.null(doc)){
    add.title(doc=doc,my.title = paste0('AutoReport - Bootstrap diagnostic', 'ImputeSuperIndividual'))
    officer::body_add_gg(doc, value = gg_plot, style = "centered" )
    add.page.break(doc = doc)}else{show(gg_plot)}
  
  
}



#' plot_Baseline_processes
#'
#' This function show the how the processes are connected
#'
#' @param projectPath Path to the stox project
#' @param doc a doc object to store figures to word document
#' @return doc a doc object to store figures to word document
#' @export
#' @import DiagrammeR
#' @import officer
plot_Baseline_processes<- function(projectPath,doc){
  
  tt <- RstoxFramework::getProcessTable(projectPath,modelName = 'baseline')
  
  from<-c()
  to<-c()
  for(i in 1:nrow(tt)){
    for(aa in tt[i,]$functionInputs[[1]]){
      if(length(aa)>0){
        to<-c(to,tt[i,]$processName)
        from<-c(from,aa)
      }
    }
  }
  
  processes <- tt$processName
  
  nodes <-DiagrammeR::create_node_df(
    n=length(processes),
    nodes = processes,
    label = FALSE,
    type = "lower",
    style = "filled",
    color = "aqua",
    shape = c("rectangle")
  )
  
  
  mydata <- data.frame(from=from,
                       to=to)
  mydata$from <- as.character(mydata$from)
  mydata$to <- as.character(mydata$to)
  nodesd = unique(c(mydata$from, mydata$to))
  nodes <- DiagrammeR::create_node_df(  n=length(nodesd), label=nodesd, type=nodesd)
  edges <- DiagrammeR::create_edge_df(from = factor(mydata$from, levels = nodesd),
                                      to =  factor(mydata$to, levels = nodesd),
                                      rel = "leading_to")
  
  graph <- DiagrammeR::create_graph(nodes_df = nodes, edges_df = edges)
  
  if(!is.null(doc)){
    tmp_file <- 'tmpfile.png'
    DiagrammeR::export_graph(graph,
                             file_name = tmp_file,
                             file_type = "png")
    add.title(doc=doc,my.title = paste0('AutoReport - Process Map'))
    officer::body_add_img(doc,src = tmp_file,width = 6,height = 6)
    add.page.break(doc = doc)
    file.remove(tmp_file)}else{DiagrammeR::render_graph(graph)}
  
  
}




#' plot_Imputed_link
#'
#' This figure show the linkege where the impute has revieced samples from 
#'
#' @param projectPath Path to the stox project
#' @param doc a doc object to store figures to word document
#' @return A ggplot figure
#' @export
#' @import ggplot2
#' @import officer
plot_TSN_comparrison <- function(projectPath,doc=NULL){
  #Get quick and dirty abundance
  
  #Get the baseline TSN
  baseline <- RstoxFramework::getModelData(projectPath, modelName = 'baseline')
  baseline_processes<- RstoxFramework::getProcessTable(projectPath,modelName = 'baseline')
  
  #Get last SuperIndividualData, this should correspond the imputed version if this has been used
  bp<-tail(baseline_processes[baseline_processes$functionOutputDataType=='SuperIndividualsData',]$processName,-1)
  data_baseline <- baseline[names(baseline)%in% bp][[1]]
  
  #store the result
  out<-c()
  out$mean<-c(sum(data_baseline$Abundance,na.rm = T))
  out$sd <- c(NA)
  out$run<-c('baseline')
  
  #Get the TSN from bootstrap, using the same process input as the one from baseline
  analysis <- RstoxFramework::getModelData(projectPath, modelName = 'analysis')[[1]]
  data_bootstrap <- analysis[names(analysis)%in%tail(baseline_processes[baseline_processes$functionOutputDataType=='SuperIndividualsData',]$processName,-1)][[1]]
  
  out$mean<-c(out$mean,
              mean(aggregate(Abundance ~ BootstrapID, data_bootstrap,sum)$Abundance,na.rm=T))
  out$sd <- c(out$sd,sd(aggregate(Abundance ~ BootstrapID, data_bootstrap,sum)$Abundance))
  out$run<-c(out$run,'bootstrap')
  
  #Get the TSN from sum(sa)/sigma_bs, the quick and dirty method. 
  data_acoustic <- baseline[names(baseline)%in%tail(baseline_processes[baseline_processes$functionOutputDataType=='StoxAcousticData',]$processName,1)]
  data_biotic<-baseline[names(baseline)%in%tail(baseline_processes[baseline_processes$functionOutputDataType=='StoxBioticData',]$processName,1)][[1]]
  
  #Grab the TS equation
  TS_function <-baseline[names(baseline)%in%tail(baseline_processes[baseline_processes$functionOutputDataType=='AcousticTargetStrength',]$processName,1)][[1]]$AcousticTargetStrengthTable
  
  #Do random sampling from the length distribution
  tmp <- c()
  for(ii in 1:1000){
    length <- sample(data_biotic$Individual$IndividualTotalLength,size = 1)
    TS = TS_function$LengthExponent*log10(length)+TS_function$TargetStrength0
    sigma = 10**(TS/10)
    tmp <- c(tmp,sum(data_acoustic[[1]]$NASC$NASC) /sigma)
  }
  
  
  out$mean<-c(out$mean,
              mean(tmp,na.rm=T))
  out$sd <- c(out$sd,sd(tmp))
  out$run<-c(out$run,'sum(sa)/sigma_bs')
  
  
  
  #Get Stratum area to another version of TSN
  bp<-baseline_processes[baseline_processes$functionOutputDataType=='StratumAreaData',]$processName
  StratumArea <- sum(baseline[names(baseline)%in% bp][[1]]$Area)
  
  out$mean<-c(out$mean,
              mean(tmp*StratumArea,na.rm=T))
  out$sd <- c(out$sd,sd(tmp*StratumArea))
  out$run<-c(out$run,'mean(sa)/sigma_bs*Area')
  
  gg_plot <-ggplot(as.data.frame(out),aes(x=run,y=mean,ymin=mean-sd,ymax=mean+sd))+geom_point()+geom_errorbar()
  
  
  if(!is.null(doc)){
    add.title(doc=doc,my.title = paste0('AutoReport - TSN comparrison', ''))
    body_add_gg(doc, value = gg_plot, style = "centered" )
    add.page.break(doc = doc)}else{show(gg_plot)}
  
}

