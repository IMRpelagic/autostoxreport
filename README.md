# autostoxreport
Automatic plotting from Stox projects. For diagnostic purposes only


#Copy to R: 


#install: 
devtools::install_github('https://github.com/IMRpelagic/autostoxreport.git')



#Set the path to the stox project
projectPath <- 'path'
reportPath <- 'path

#Startup, run the stox project
startup(projectPath)


#Create a doc object
doc=new.word.doc()

#Add progression line
plot_progression(projectPath,doc = doc)

#Add NASC plot 
plot_NASC(doc=doc,projectPath = projectPath)

#Save word document
print(doc, target=reportPath)
