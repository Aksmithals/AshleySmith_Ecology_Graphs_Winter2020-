#introduction to R data analysis: Data cleaning

#for loops
head(base)

#if statements 

plot_name = c("ABPL", "BUCA", "BUCB", "BUCC", "BUCD", "LAHR", "PICA", "PICB", 
              "PICE", "PUPL", "SFTR", "STRD", "TORD")

#saving a for loops via using a container 
result= data.frame(nrow=89,ncol=9) # preparing a container 
for (ipn in plot_name){
  ipn_pn= subset(structure_year,PLOT_NAME==(ipn))
  result[ipn]= ipn_pn
}

#saving a for loop using the magic for library
install.packages("magicfor")
library(magicfor)

magic_for(silent=TRUE)
for (ipn in plot_name){
  ipn_pn= subset(structure_year,PLOT_NAME==(ipn))
}

magic_result_as_dataframe()
#other testing 

nrow(ipn_pn)
ncol(ipn_pn)


plot(ipn_pn$YEAR, ipn_pn$COVER_., col="red", pch=19)

str(plot_name)

#facests

install.packages(ggplot2)
library(ggplot2)

sp <- ggplot(structure_year, aes(x=YEAR), y=RICHNESS) + geom_point(shape=1)

ggplot(structure_year, aes(x=YEAR, y=RICHNESS) + geom_point() + facet_grid(plot_name)

head(structure_year)
