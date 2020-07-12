#Ashley Smith
# 1/7/20
#Processig MSH data for ecology course
#last updated 12_7_20

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


#determinging richness via producing a datafram w/ t/f values
#attempting to use transverse function did not work
species_year_t=t(species_year)

species_year_t$V1 > 0

typeof(species_year_t)
typeof(species_year)


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

#using tidyverse and dplyr: did not work 
species_year_exotic= select()
install.packages("tidyverse")
install.packages("rlang")
library(dplyr)
library(tidyverse)


?select

speces_year %>% select(1:3)

structure_year_working= structure_year[ structure_year$PLOT_NAME %in% c( "LAHR", "PICA", "PICB", "PUPL", "STRD"),]



#
###
#working with structure_year
SS1= subset(structure_year,PLOT_NAME=="PICE")
SS2=subset(SS1,PLOT_NUMBER== "4")

head(SS2)

#
###
#looking for nice plots usig in built plot function 
plot(SS2$YEAR, SS2$RICHNESS, xlab="year", 
     ylab ="richness", 
     main = "measureing vegtation sucession", 
     col ="blue",
     pch=19)

lines(SC$YEAR, SC$RICHNESS, xlab="year", 
     ylab ="richness", 
     main = "measureing vegtation sucession", 
     col ="blue",
     pch=19)
plot(SC$YEAR, SC$COVER_., col="red", pch=19)
plot(SC$YEAR, SC$HPRIME, col="green", pch=19)
plot(SC$YEAR, SC$EVENNESS, col="orange", pch=19)
plot(SC$YEAR, SC$FREQUENCY, col="pink", pch=19)


head(structure_year)
f=unique(structure_year$PLOT_NAME)

dput(unique(structure_year$PLOT_NAME))
colnames(structure_year)

?apply
apply(structure_year,f,mean)
str(structure_year)

#
###
#structure_year using ggplot to make plots

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

#subsetting ad removing rows 

#structure_year_working with missing data
structure_year_working= structure_year[ structure_year$PLOT_NAME %in% c( "BUCA", "BUCB", "BUCC", "BUCD", "LAHR", "PICA", "PICB", "PUPL", "STRD"),]

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

