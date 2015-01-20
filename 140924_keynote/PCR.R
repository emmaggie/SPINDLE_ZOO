library(ggplot2)

#setwd('/Volumes/Magdalena_NEW1/MARINA_PAPER/SEPTEMBER_2014')

setwd('/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo Data/140924_keynote')
getwd()
################################################################################################
#DATA SETS
#READ-IN DATA

#comment: prepare data that only holds numerical values
################################################################################################

dir()
list.files('../SEPTEMBER_2014/')
mitotic_metaphase<-read.csv('../SEPTEMBER_2014/mitotic_metaphase.csv')
smooth_data<-read.csv('../SEPTEMBER_2014/MitoticScaling_Binnedmeans_SD_reformatted.csv')
str(mitotic_metaphase)
smooth_data$organism<-gsub('marginalus', 'marginatus',as.character(smooth_data$organism))

original<-read.csv('../SEPTEMBER_2014/original.csv')
################################################################################################
################################################################################################
head(original)

