# autostoxreport


## Automatically plotting of StoX projects. 
Purpose: These plotting are for diagnostic use only, not for publishing official figures or tables for reports. 

## Installation
It is a requiered that the StoX has been installed. 
To install this packages: 

install.packages('devtools')

devtools::install_github('https://github.com/IMRpelagic/autostoxreport.git')


## Usage, in R

library(AutoStoXreport) 


#Set the path to the stox project

projectPath <- 'path'


#set the path and name of the report

reportPath <- 'path


#Startup, run this function if the stox project has not been runned. 

startup(projectPath)



#Create a doc object

doc=new.word.doc()


#Add progression line to inspect the vessel track

plot_progression(projectPath,doc = doc)


#Add NASC plot that show meanNasc values of EDSU

plot_NASC(doc=doc,projectPath = projectPath)



#Add link of where the impute function has recieved information from. 

plot_Imputed_link(doc=doc,projectPath = projectPath)



#Diagnostic for Bootstrap to see if it has stabilized

plot_BootstrapDiag(doc=doc,projectPath = projectPath)


#Show how the processes in baseline are connected

plot_Baseline_processes(projectPath = projectPath,doc=doc)




#write figues to a word document

print(doc, target=reportFile)

