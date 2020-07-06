#Ashley SMith
# 1/7/20
#Processig MSH data for ecology course

plot_des= read.csv("MSH_PLOT_DESCRIPTORS.csv", header=TRUE, sep = ",")
species_des= read.csv("MSH_SPECIES_DESCRIPTORS.csv", header=TRUE, sep = ",")
species_year= read.csv("MSH_SPECIES_PLOT_YEAR.csv", header=TRUE, sep = ",")
structure_year = read.csv("MSH_STRUCTURE_PLOT_YEAR.csv", header=TRUE, sep = ",")

head(plot_des)
head(structure_year)


SS1= subset(structure_year,PLOT_NAME=="PICE")
SS2=subset(SS1,PLOT_NUMBER== "4")

head(SS2)

#looking for nice plots 
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

?plot

head(structure_year)
f=unique(structure_year$PLOT_NAME)

dput(unique(structure_year$PLOT_NAME))
colnames(structure_year)

?apply
apply(structure_year,f,mean)
str(structure_year)


install.packages("ggplot2")
library(ggplot2)

p =SC$YEAR
q =SC$RICHNESS

ggplot(aes(x= p, y= q))
head(structure_year)

#grid based on richness
pR= ggplot(structure_year, aes(x=YEAR, y=RICHNESS))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~PLOT_NUMBER)+
  labs(x= "YEAR", title= "richness depedant on year")
pR

#grid based on cover
pC= ggplot(structure_year, aes(x=YEAR, y=COVER_.))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~PLOT_NUMBER)+
  labs(x= "YEAR", title= "cover depedant on year")
pC

#grid based on frequentcy
pf= ggplot(structure_year, aes(x=YEAR, y=FREQUENCY))+ 
  geom_point()+
  theme_bw()+
  facet_grid(PLOT_NAME~PLOT_NUMBER)+
  labs(x= "YEAR", title= "frequency depedant on year")
pf

??geom
