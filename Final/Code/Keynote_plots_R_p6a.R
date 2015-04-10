#Try piecewise regression for fig 1. 

#Try multivariate regression on meiotic and mitotic spindles for fig 3.
#y is spindle size
#################################################################################################
#prepare data
#################################################################################################


setwd('/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo Data/140924_keynote')
getwd()
################################################################################################
#DATA SETS
#READ-IN DATA
################################################################################################

dir()
list.files('../SEPTEMBER_2014/')
#mitotic_anaphase is the file of interest
meiotic<-read.csv('../SEPTEMBER_2014/meiotic_true.csv',stringsAsFactors=FALSE)
#this has to be narrowed down to
mitotic<-read.csv('../SEPTEMBER_2014/meiotic_false.csv',stringsAsFactors=FALSE)
#mitotic_anaphase<-read.csv('../SEPTEMBER_2014/mitotic_anaphase.csv')
#mitotic_anaphase$organism<-as.character(mitotic_anaphase$organism)
head(meiotic)

original<-read.csv('../SEPTEMBER_2014/original.csv',stringsAsFactors=FALSE)

################################################################################################
#CLEAN UP THE DATA BEFORE MODELLING
################################################################################################
names(meiotic)
for (col in names(meiotic)){
  print(col)
  print(class(meiotic[,col]))
}

meiotic_for_MR<-meiotic
names(meiotic_for_MR)
meiotic_for_MR$astere.to.pole_distance_AVG<-apply(meiotic_for_MR[,c(1,2)],1,mean,rm=TRUE)

#http://stackoverflow.com/questions/4605206/drop-columns-r-data-frame
names(meiotic_for_MR)
meiotic_for_MR<-meiotic_for_MR[-c(1,2)]

meiotic_for_MR$inner_aster_diameter_AVG<-apply(meiotic_for_MR[,c(13,14)],1,mean,na.rm=TRUE)

to_drop<-c('inner_aster_diameter_1_um',"inner_aster_diameter_2_um")
meiotic_for_MR<-meiotic_for_MR[,!names(meiotic_for_MR) %in% to_drop]
#to_drop %in% names(meiotic_for_MR)

meiotic_for_MR$polar_body_diameter_AVG<-apply(meiotic_for_MR[,c(27,28)],1,mean,na.rm=TRUE)
meiotic_for_MR<-meiotic_for_MR[-c(27,28)]

names(meiotic_for_MR)

meiotic_for_MR$outer_aster_diameter_AVG<-apply(meiotic_for_MR[,c(23,24)],1,mean,na.rm=TRUE)
meiotic_for_MR<-meiotic_for_MR[-c(23,24)]

for (col in names(meiotic_for_MR)){
  print(col)
  print(class(meiotic_for_MR[,col]))
}

grep('_CAT',names(meiotic_for_MR),value = TRUE)

for(col in names(meiotic_for_MR)){
  if(class(meiotic_for_MR[,col])=='numeric'){
    print(col)
    print(class(meiotic_for_MR[,col]))
  }
}

