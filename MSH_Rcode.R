#Ashley SMith
# Started 1/7/20
#Processig MSH data for ecology course
# last updated 9_7_20

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

e= c("Cirarv","Hyprad", "Rumace", "Sensyl")

#
###
#working with species_year 

sum(species_year$Cirarv)
sum(species_year$Hyprad)
sum(species_year$Rumace)
sum(species_year$Sensyl)


#
###
#working with structure_plot
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
#using ggplot to make plots

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

#subsetting 
fds_sy = c( "BUCA", "BUCB", "BUCC", "BUCD", "LAHR", "PICA", "PICB", "PUPL", "STRD")

SS2=subset(structure_year,PLOT_NAME == fds_sy)





#grid based on cover
ggplot(structure_year, aes(x=YEAR, y=COVER_.))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~PLOT_NUMBER)+
  labs(x= "YEAR", title= "cover depedant on year")

#grid based on frequentcy
ggplot(structure_year, aes(x=YEAR, y=FREQUENCY))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~PLOT_NUMBER)+
  labs(x= "YEAR", title= "frequency depedant on year")

