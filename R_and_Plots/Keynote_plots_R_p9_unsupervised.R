#This is the unsupervised part of the multivariate analysis
#following the chater 25 in the R book 

#collinearity is not dealt with yet!!!
################################################################################################
#PREPARE DATA
################################################################################################
#data clean-up is in the notebooks; first one is in the AUGUST_2014 Folder; remaining ones are in the SEPTEMBER_2014 folder; part1 notebook is purely exploratory, it does not contain any relevant information; part2

setwd('/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo Data/R_and_Plots/')
getwd()

dir()
list.files('../SEPTEMBER_2014/')
#this is the complete data set; 
original<-read.csv('../SEPTEMBER_2014/original.csv',stringsAsFactors=FALSE)
dim(original) #2001 x 41
names(original) #41 variables
head(original)
original.SQL<-read.csv('../SEPTEMBER_2014/df2_original_for_SQL.csv',stringsAsFactors=FALSE)
names(original.SQL) #44 variables
dim(original.SQL) #2001 x 44
################################################################################################
#AVG THE DUPLICATE COLUMNS
#remove/average out duplicates 
#astere.to.pole_distance
#inner_aster_diameter
#polar_body_diameter
################################################################################################
names(original.SQL)
grep('pole_distance',names(original.SQL))
names(original.SQL)[c(14,15)]
original.SQL$astere.to.pole_distance_AVG<-apply(original.SQL[,c(14,15)],1,mean,rm=TRUE)
#http://stackoverflow.com/questions/4605206/drop-columns-r-data-frame
names(original.SQL)
original.SQL<-original.SQL[-c(14,15)]

grep('inner_aster',names(original.SQL))
names(original.SQL)[c(14,15)]
original.SQL$inner_aster_diameter_AVG<-apply(original.SQL[,c(14,15)],1,mean,na.rm=TRUE)
to_drop<-c('inner_aster_diameter_1_um',"inner_aster_diameter_2_um")
original.SQL<-original.SQL[,!names(original.SQL) %in% to_drop]

grep('polar_body',names(original.SQL))
names(original.SQL)[c(20,21)]
original.SQL$polar_body_diameter_AVG<-apply(original.SQL[,c(20,21)],1,mean,na.rm=TRUE)
names(original.SQL)
original.SQL<-original.SQL[-c(20,21)]

grep('outer_',names(original.SQL))
names(original.SQL)[c(14,15)]
original.SQL$outer_aster_diameter_AVG<-apply(original.SQL[,c(14,15)],1,mean, na.rm=TRUE)
original.SQL<-original.SQL[-c(14,15)]
names(original.SQL)

dim(original.SQL) #2001 x 40
################################################################################################
#no duplicates
#metadata columns 
################################################################################################
metadata.cols<-c('date','image','objective','microscope','fixation','day')
#in: fixation_CAT, microscope_CAT
names(original.SQL) %in% metadata.cols
original.SQL[1:2,names(original.SQL) %in% metadata.cols]

################################################################################################
#continous columns 
################################################################################################
#continuous.vars #from _p7.R
continuous.vars<-c("cell_diameter_um","distance_between_chromosomes_um","genome","metaphase_plate_aspect_ratio_um","metaphase_plate_lengt_h_um","metaphase_plate_width_um","spindle_aspect_ratio_asters_um","spindle_aspect_ratio_poles_um","spindle_length__asters_um","spindle_length_poles_um","spindle_width_um","astere.to.pole_distance_AVG","inner_aster_diameter_AVG","polar_body_diameter_AVG","outer_aster_diameter_AVG")
length(continuous.vars)
for(i in 1:length(continuous.vars)){
  print(class(unlist(original.SQL[continuous.vars[i]])))
} #numeric

################################################################################################
#categorical columns 
################################################################################################
grep('_CAT',names(original.SQL),value=TRUE)
cats_to_parse<-grep('_CAT',names(original.SQL),value=TRUE)

strsplit(grep('_CAT',names(original.SQL),value = TRUE)[1],'_CAT')[[1]]
length(sapply(cats_to_parse,function(x){strsplit(x,'_CAT')[[1]]}))
sapply(cats_to_parse,function(x){strsplit(x,'_CAT')[[1]]})

cat.cols<-c(grep('_CAT',names(original.SQL),value=TRUE),sapply(cats_to_parse,function(x){strsplit(x,'_CAT')[[1]]}))
length(cat.cols)
names(cat.cols)<-rep('',length(cat.cols))
cat.cols<-c(cat.cols,'meiotic','centrosome',)
names(original.SQL)
################################################################################################
#count columns 
################################################################################################
count.cols<-c("chromosomes","num_of_cells",'num_of_cells_NUM')

################################################################################################
#filter by stage: metaphase
################################################################################################

unique(original.SQL[(original.SQL$stage == 'm' | original.SQL$stage=='mI' | original.SQL$stage=='mII'),]$stage)

unique(original.SQL[(original.SQL$stage != 'm' & original.SQL$stage !='mI' & original.SQL$stage !='mII'),]$stage)

original.SQL.met<-original.SQL[(original.SQL$stage == 'm' | original.SQL$stage=='mI' | original.SQL$stage=='mII'),]

dim(original.SQL.met) #1462 x  40




