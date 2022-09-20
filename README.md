# autostoxreport


## Automatically plotting of StoX projects. 
Purpose: These plotting are for diagnostic use only, not for publishing official figures or tables for reports. 

## Installation
It is a requiered that the StoX has been installed. 
To install this packages: 

install.packages('devtools')

devtools::install_github('https://github.com/IMRpelagic/autostoxreport.git',force=T)


## Usage, in R

library(AutoStoXreport) 


#Set the path to the stox project

projectPath <- 'path'


#set the path and name of the report

reportPath <- 'path



#Run StoX to get data in memory

#This is not needed if the project is already is open in the StoX software

RstoxFramework::openProject(projectPath)

RstoxFramework::runProject(projectPath,modelName = 'baseline')

RstoxFramework::runProcesses(projectPath,modelName = 'analysis')



#Create a doc object

doc=new.word.doc()


  
  #Add progression line
  
  plot_BeamKey(projectPath,doc=doc)
  
  
  #Add NASC plot
  
  plot_NASC(doc=doc,projectPath = projectPath)
  
  
  #Add link of where the impute function has recieved information
  
  plot_Imputed_link(doc=doc,projectPath = projectPath)
  
  
  #Diagnostic for Bootstrap to see if it has stabilized
  
  plot_BootstrapDiag(doc=doc,projectPath = projectPath)
  
  
  #Show how the processes in baseline are connected
  
  plot_Baseline_processes(projectPath = projectPath,doc=doc)
  
  
  #Show a quick comparrison on the TSN
  
  plot_TSN_comparrison(projectPath=projectPath,doc = doc)
  
  
  #Save word document
  
  print(doc, target=reportFile)
  
  
  #Close project
  
  RstoxFramework::closeProject(projectPath)

