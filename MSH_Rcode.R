#Ashley SMith
# 1/7/20
#Processig MSH data for ecology course
#last updated 11_7_20

#
###
#importing files from MSH
plot_des= read.csv("MSH_PLOT_DESCRIPTORS.csv", header=TRUE, sep = ",")
species_des= read.csv("MSH_SPECIES_DESCRIPTORS.csv", header=TRUE, sep = ",")
species_year= read.csv("MSH_SPECIES_PLOT_YEAR.csv", header=TRUE, sep = ",")
structure_year = read.csv("MSH_STRUCTURE_PLOT_YEAR.csv", header=TRUE, sep = ",")

#
###
#exploring the data
head(plot_des)
head(structure_year)
head(species_year)
head(species_des)

#
###
#working with plot_des
unique(plot_des$SUCCESSION_TYPE)

#
###
#working with plant_des
unique(species_des$Origin)



#determining number of exotic plants 
sum(species_des$Origin =="Exotic")

exotic_plants=subset(species_des, Origin =="Exotic")
head(exotic_plants)

unique(exotic_plants$Species_._authority)
unique(exotic_plants$Raw_code)

ex_plants= c("Cirarv","Hyprad", "Rumace", "Sensyl")

#
###
#working with species_year 

sum(species_year$Cirarv)
sum(species_year$Hyprad)
sum(species_year$Rumace)
sum(species_year$Sensyl)


#did work using rowSums 
nrow(species_year)

species_plot_info= species_year[,1:4]
rich_sum= rowSums(species_year[,5:50]>0)
species_plot_info[,5] =rich_sum
head(species_plot_info)

#plotting species_year produced richness 

library(ggplot2)
ggplot(species_plot_info, aes(x=YEAR, y=V5, colour=factor(PLOT_NUMBER)))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~.)+
  labs(x= "YEAR", title= "richness depedant on year")

#limitning what is ploted

species_plot_info_working= species_plot_info[ species_plot_info$PLOT_NAME %in% c( "LAHR", "PICA", "PICB", "PUPL", "STRD"),]

ggplot(species_plot_info_working, aes(x=YEAR, y=V5, colour=factor(PLOT_NUMBER)))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~.)+
  labs(x= "YEAR", title= "richness depedant on year based on species_year (calculated from plant data")

#ploting richness based on exotic plants 

species_year_exotic= species_year[species_year %in% c("Cirarv","Hyprad", "Rumace", "Sensyl"),]
head(species_year_exotic)

species_year_exotic= species_year[,c("Cirarv","Hyprad", "Rumace", "Sensyl")]

#trying to use subset
species_year_exotic_ss= subset(species_year, select=c("Cirarv","Hyprad", "Rumace", "Senspp"))
species_year_exotic_description= subset(species_year, select=1:4)


species_year_exotic= cbind(species_year_exotic_description,species_year_exotic_ss)

head(species_year_exotic)

#turning species_year_exotic into a richness calculation 
species_plot_info_exotic= species_year[,1:4]
rich_sum_exotic= rowSums(species_year_exotic[,5:8]>0)
species_plot_info_exotic[,5] =rich_sum_exotic
head(species_plot_info_exotic)

#ploting fromm the subset 
ggplot(species_plot_info_exotic, aes(x=YEAR, y=V5, colour=factor(PLOT_NUMBER)))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~.)+
  labs(x= "YEAR", title= "richness depedant on year for exotic plants")

species_plot_info_exotic_working= species_plot_info_exotic[ species_plot_info_exotic$PLOT_NAME %in% c( "LAHR", "PICA", "PICB", "PUPL", "STRD"),]

ggplot(species_plot_info_exotic_working, aes(x=YEAR, y=V5, colour=factor(PLOT_NUMBER)))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~.)+
  labs(x= "YEAR", title= "richness depedant on year for exotic plants")


#
###
#working with structure_year

install.packages("ggplot2")
library(ggplot2)

#
##
#grid based on richness

#by plot name
ggplot(structure_year, aes(x=YEAR, y=RICHNESS, colour=factor(PLOT_NUMBER)))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~.)+
  labs(x= "YEAR", title= "richness depedant on year")

#by plot name and number 
ggplot(structure_year, aes(x=YEAR, y=RICHNESS, colour=factor(PLOT_NUMBER)))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~PLOT_NUMBER)+
  labs(x= "YEAR", title= "richness depedant on year")

#structure_year_working with good curves 
structure_year_working= structure_year[ structure_year$PLOT_NAME %in% c( "LAHR", "PICA", "PICB", "PUPL", "STRD"),]


#ploting structure_year_working 

ggplot(structure_year_working, aes(x=YEAR, y=RICHNESS, colour=factor(PLOT_NUMBER)))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~.)+
  labs(x= "YEAR", title= "richness depedant on year based on structure_year (precalculated")

ggplot(structure_year_working, aes(x=YEAR, y=COVER_., colour=factor(PLOT_NUMBER)))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~.)+
  labs(x= "YEAR", title= "cover depedant on year based on structure_year (precalculated")

head(structure_year_working)




#grid based on cover
ggplot(structure_year, aes(x=YEAR, y=COVER_.))+ 
  geom_point()+
  theme_bw()+
  geom_smooth(se=FALSE)+
  facet_grid(PLOT_NAME~PLOT_NUMBER)+
  labs(x= "YEAR", title= "cover depedant on year")

#grid based on frequentcy
ggplot(structure_year, aes(x=YEAR, y=FREQUENCY))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~PLOT_NUMBER)+
  labs(x= "YEAR", title= "frequency depedant on year")

