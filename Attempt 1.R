#Name:Matthew-Pierre Rogers
#Date(started): 19-01-2023
#Link to dataset: https://doi.pangaea.de/10.1594/PANGAEA.907407
#Credits to the original authors


#I should have clear objectives
  # Separate out species and Genus
  # Maybe Group By Genus at first, just to cope with the numbers
  # Does Any genus/phylum grow faster
  # Cor Plots of Specfic gorwth rate with phys chem data
  # anything interesting that shows up
  # Find Something to Visualize
  

#Function Definitions
Import.and.Clean.Data<-function(){
  Seaweed.Envi.Data <- read.delim("C:/Users/mparo/OneDrive/Documents/Programming/R/Practice Projects/Seaweed Growth envi conditions/Seaweed and Envi/Dataset without title.tab", header=FALSE)
  dataset<-Seaweed.Envi.Data
  #Make those row names easier to work with
  titles<-dataset[1,]
  titles[3:8]<-c("NH4.conc","PO4.conc","Biomass.conc","Irradiance","NO3.conc","Specifc.Growth.Rate")
  colnames(dataset)<-titles
  dataset<-dataset[-1,]
  #Ensure everything is the right datatype
  dataset[,3:8]<-sapply(dataset[,3:8],as.numeric)
  dataset<-dataset |> separate(Species, c("Genus","Species"))
  #View(dataset)
  #Caught this while doing some looking around during the exploratory data analysis
  dataset <- dataset |> mutate (Genus = ifelse(Genus == "U", "Ulva",Genus)) 
  dataset <- dataset |> mutate (Genus = ifelse(Genus == "Enteromorpa", "Enteropmorpha", Genus))
  dataset <- dataset |> mutate (Phylum = ifelse(Genus == "Enteropmorpha", "Chlorophyta", Phylum))
  aq<- !grepl("aquatic",dataset$Genus)
  dataset<-dataset[aq,]
  View(dataset)
  return(dataset)
}

#this isn't working explore this link: https://stackoverflow.com/questions/31461640/how-to-access-r-data-frame-column-descriptions-after-read-spss
AddColumnDescriptions<-function(dataset){
  descriptions(dataset[,1:8])<-list(Species              = "Genus and Species of Seaweed",
                                    Phylum               = "Class of Seaweed",
                                    NH4.conc             = "Concentration of NH4+ in micromoles/litre",
                                    PO4.conc             = "Concentration of PO4 3- in micromoles/litre",
                                    Biomass              = "Concentration of Biomass in grams/litre",
                                    Irradiance           = "Photon flux in E ( or micromoles/metre^2/second",
                                    NO3.conc             = "Concentration of NO3-  in micromoles/litre",
                                    Specific.Growth.Rate = "Specific Growth rate in % mass change/day"
  )
  descriptions(dataset)
  
  comment(dataset[,1:8])<-c("Genus and Species of Seaweed","Class of Seaweed","Concentration of NH4+ in micromoles/litre",
                            "Concentration of PO4 3- in micromoles/litre","Concentration of Biomass in grams/litre",
                            "Photon flux in E ( or micromoles/metre^2/second","Concentration of NO3-  in micromoles/litre",
                            "Specific Growth rate in % mass change/day")
  return(dataset)
}

Explore.Data<-function(dataset){
  dataset2<-dataset
  glimpse(dataset2)
  summary(dataset2)
  n<-length(dataset2[,1])
  print(paste("The Dataset has ", n, " records"))
  pairs(dataset2[,4:9])
  #Heatmap/corrplot Theres a ton of NA's. Id have to do some more selective extractuin
  working.data<-dataset2[,4:9] |> na.omit()
  corr<-cor(working.data)
  c2<-corrplot(corr, method = "square", type = "lower",addCoef.col = "black")
  print(c2)
  
  phylum.comp<-dataset |> select(Phylum) |> group_by(Phylum) |> summarise( Count = n())
  phylum.comp.plot<- phylum.comp |> plot_ly(labels = ~Phylum,values =~Count ,type = "pie",
                                            textinfo = 'label+percent', hole = 0.6,
                                            marker = list(colors = c ("#6BF502", "#F8A205","#FB3D15"))) |>
    layout(title =" Amount of each Phyllum in dataset")
  print(phylum.comp.plot)
  
  phylum.growth.plot<-plot_ly(y = ~dataset$Specifc.Growth.Rate,type = "box", color = ~dataset$Phylum)
  phylum.growth.plot
  # above has big annoying outlier, i'll search for and remove it
  outlier<-order(dataset$Specifc.Growth.Rate,decreasing = TRUE)
  outlier.index<-outlier[1]
  t.data<-dataset[-outlier.index,]
  phylum.growth.plot<-plot_ly(y = ~t.data$Specifc.Growth.Rate,type = "box", 
                              color = ~t.data$Phylum,
                              colors =  c("#6BF502", "#F8A205","#FB3D15")
                              ) |>
    layout(title ="Growth Rates of different Phylla of Seaweed in dataset",yaxis = list(title = "Growth rates (% mass change per day)"))
  print(phylum.growth.plot)
  
  
  species.list<-unique(dataset$Species)
  #View(species.list)
  print(paste("There are ",length(species.list), " species in the dataset"))
  #print(paste(species.list))
  # Yeah thats too much to parse.
  genus.list<-unique(dataset$Genus) #shows some odd ones, lets see what we can do
  print(paste("There are ",length(genus.list), " genus in the dataset"))
  print(paste("The Genus in the sample are: "))
  print(paste(sort(genus.list)))
}

Start.Asking.Questions<-function(dataset){
  
  
                                                                                                    
  #do species have differing specific growth rates
  working.dataset<-dataset |> select(Genus, Phylum, Specifc.Growth.Rate) |> group_by(Phylum, Genus) |> summarise(SGR = mean(Specifc.Growth.Rate)) |> na.omit() |> arrange(SGR)
  #View(working.dataset)
  species.growth.plot<- working.dataset |> ggplot(mapping = aes(x = reorder(Genus, -SGR), y = SGR, fill = Phylum))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = c("Chlorophyta" = "#6BF502", "Phaeophyta" = "#F8A205", "Rhodophyta" = "#FB3D15"))+
    labs(title = "Mean Specific Growth Rate of Various Genus of Algae")+
    ylab("Specific Growth rate in % mass per day")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle = -45))
  print(species.growth.plot)# the ggplot, better for pdf and single images
  interact.species.growth.plot<-ggplotly(species.growth.plot)
  print(interact.species.growth.plot) # the plotly, better for html
  
  #Isolate those that have a specific growth rate
  has.growth<-complete.cases(dataset[,9])
  has.growth<-dataset[has.growth,]
  #View(has.growth)
  
}


#Load Packages
library(tidyverse)
library(ggpubr)
library(plotly)
library(common)
library(fmtr)
library(corrplot)

#Main Code
dataset<-Import.and.Clean.Data()
#dataset<-AddColumnDescriptions(dataset)
Explore.Data(dataset)
Start.Asking.Questions(dataset)


#might all be rubbish, 2 measurement data, a lm or correlation be better
