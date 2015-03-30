#This is the unsupervised part of the multivariate analysis
#following the chater 25 in the R book 

#collinearity!!!
#install.packages('apcluster')
###############################################################################################
#PREPARE DATA
###############################################################################################
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
###############################################################################################
#AVG THE DUPLICATE COLUMNS
#remove/average out duplicates 
#astere.to.pole_distance
#inner_aster_diameter
#polar_body_diameter
###############################################################################################
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
list.files(path='../SEPTEMBER_2014/',pattern = ".csv")
head(original.SQL)
#write.csv(original.SQL,file='../SEPTEMBER_2014/original_SQL_without_dupes_from_R.csv')
###############################################################################################
#ABOVE: data object has: no duplicates
###############################################################################################
#BELOW: specyfying variable types
###############################################################################################
#1. metadata columns 
###############################################################################################
metadata.cols<-c('date','image','objective','microscope','fixation','day')
#in: fixation_CAT, microscope_CAT
names(original.SQL) %in% metadata.cols
head(original.SQL[1:2,names(original.SQL) %in% metadata.cols], 1L)
head(original.SQL[,!names(original.SQL) %in% metadata.cols], 1L)

#original.SQL.no_meta<-original.SQL[,!names(original.SQL) %in% metadata.cols]
#write.csv(original.SQL.no_meta,file='../SEPTEMBER_2014/original_SQL_WO_dupes_WO_metadata_from_R.csv')

###############################################################################################
#continous columns 
###############################################################################################
#continuous.vars #from _p7.R
continuous.vars<-c("cell_diameter_um","distance_between_chromosomes_um","genome","metaphase_plate_aspect_ratio_um","metaphase_plate_lengt_h_um","metaphase_plate_width_um","spindle_aspect_ratio_asters_um","spindle_aspect_ratio_poles_um","spindle_length__asters_um","spindle_length_poles_um","spindle_width_um","astere.to.pole_distance_AVG","inner_aster_diameter_AVG","polar_body_diameter_AVG","outer_aster_diameter_AVG")
length(continuous.vars)
for(i in 1:length(continuous.vars)){
  print(class(unlist(original.SQL[continuous.vars[i]])))
} #numeric


###############################################################################################
#categorical columns 
###############################################################################################
#original<-read.csv('../SEPTEMBER_2014/original.csv',stringsAsFactors=FALSE)
grep('_CAT',names(original.SQL),value=TRUE)
cats_to_parse<-grep('_CAT',names(original.SQL),value=TRUE)

strsplit(grep('_CAT',names(original.SQL),value = TRUE)[1],'_CAT')[[1]]
length(sapply(cats_to_parse,function(x){strsplit(x,'_CAT')[[1]]}))
sapply(cats_to_parse,function(x){strsplit(x,'_CAT')[[1]]})

cat.cols<-c(grep('_CAT',names(original.SQL),value=TRUE),sapply(cats_to_parse,function(x){strsplit(x,'_CAT')[[1]]}))
length(cat.cols)
names(cat.cols)<-rep('',length(cat.cols))
cat.cols<-c(cat.cols,'meiotic','centrosome')
names(original.SQL)
cat.cols

###############################################################################################
#count columns 
###############################################################################################
count.cols<-c("chromosomes","num_of_cells",'num_of_cells_NUM')

###############################################################################################
#filter by stage: metaphase
###############################################################################################

unique(original.SQL[(original.SQL$stage == 'm' | original.SQL$stage=='mI' | original.SQL$stage=='mII'),]$stage)

unique(original.SQL[(original.SQL$stage != 'm' & original.SQL$stage !='mI' & original.SQL$stage !='mII'),]$stage)

original.SQL.met<-original.SQL[(original.SQL$stage == 'm' | original.SQL$stage=='mI' | original.SQL$stage=='mII'),]

dim(original.SQL.met) #1462 x  40

###how many organisms?
length(unique(original.SQL.met$organism)) #20

###how many rows per organism?
class(split(original.SQL.met,original.SQL.met$organism))
lapply(split(original.SQL.met,original.SQL.met$organism),dim)

#write.csv(original.SQL.met,file='../SEPTEMBER_2014/original_SQL_metaphase_only_from_R.csv')
head(original.SQL.met)
###############################################################################################
#PCA on continous variables
###############################################################################################
names(original.SQL.met)
names(original.SQL.met[,names(original.SQL.met) %in% continuous.vars])
original.SQL.met.cont<-original.SQL.met[,names(original.SQL.met) %in% continuous.vars]
head(original.SQL.met.cont)
dim(original.SQL.met.cont) #1462 x  40

####svd on sparse matrix

#http://stackoverflow.com/questions/4951286/svd-for-sparse-matrix-in-r
#http://www.johnmyleswhite.com/notebook/2011/10/31/using-sparse-matrices-in-r/
#install.packages('irlba')
###############################################
#check classes of variables (should be all numeric)
###############################################
#http://stackoverflow.com/questions/13352815/error-in-svdx-nu-0-0-extent-dimensions
#http://stackoverflow.com/questions/15068981/removal-of-constant-columns-in-r

#http://rtutorialseries.blogspot.com/2012/03/r-tutorial-series-centering-variables.html

sum(sapply(original.SQL.met.cont,class)!='numeric') #all are numeric

no.of.na<-apply(is.na(original.SQL.met.cont),2,sum)
apply(is.na(original.SQL.met.cont),2,length)
no.of.na
fract.of.NAs<-sapply(no.of.na,function(x){(x/1462)*100})

var.per.var<-sapply(original.SQL.met.cont,var,na.rm=TRUE)
var.per.var==0
#scale manually
original.SQL.met.cont.scaled<-data.frame(apply(original.SQL.met.cont,2,scale))
str(original.SQL.met.cont.scaled)
str(scale(original.SQL.met.cont[,1]))
mean(original.SQL.met.cont[,1],na.rm=TRUE)
sd(original.SQL.met.cont[,1],na.rm=TRUE)
head(data.frame(apply(original.SQL.met.cont,2,scale)))
?scale
model.PCA<-prcomp(~., original.SQL.met.cont.scaled, na.action=na.omit,scale=FALSE, center=FALSE)
?prcomp
summary(model.PCA) #5 components capture 90% of variation
#scree plot
str(model.PCA)
plot(model.PCA)
biplot(model.PCA,cex=0.5)


str(model.PCA)

pred<-predict(model.PCA)[,1:5]
head(pred,2L)


=======
>>>>>>> 880f85b808d3c17816b8072f01b368cf124928dc
col.1<-predict(model.PCA)[,1]
as.integer(names(col.1))
plot(original.SQL.met.cont.scaled[as.integer(names(col.1)),1],col.1)
par(mfrow=c(2,1))
hist(original.SQL.met.cont.scaled[,1])
hist(col.1)
model.PCA
par(mfrow=c(1,1))

###############################################################################################
#Factor analysis on continous variables
###############################################################################################
?factanal
#http://www.statmethods.net/advstats/factor.html
#http://stackoverflow.com/questions/15759226/factor-analysis-using-r
model.FACT_ANAL<-factanal(~.,factors=3, data=original.SQL.met.cont, na.action=na.omit)
#length(original.SQL.met.cont[complete.cases(original.SQL.met.cont),])

#install.packages('sem')
#general structure equation
library(sem)
?sem

###############################################################################################
#Cluster analysis on continous variables - baggedTrees NA management
#Cluster analysis on continous variables - baggedTrees
#Cluster analysis on continous variables - baggedTrees
###############################################################################################
#http://topepo.github.io/caret/index.html
#install.packages('caret')
library(caret)
sum(is.na(original.SQL.met.cont)) #2230
DS<-original.SQL.met.cont
dim(DS) #1462x15

#https://github.com/topepo/caret/blob/master/RegressionTests/Code/knnImpute.R
?preProcess
#preprocParams<-preProcess(DS,method="knnImpute") #estimates std params
#won't work if the number of NA's is high
sum(is.na(DS))
preprocParams_baggedTree<-preProcess(DS,method="bagImpute") #estimates std params
#install.packages('ipred')
res_baggedTree<-predict(preprocParams_baggedTree,DS) #fills in NA
sum(is.na(res_baggedTree)) #0
dim(res_baggedTree) #1462x15 - so dims are correct
names(res_baggedTree)


class(res_baggedTree) #data.frame
#moving this data set to Ipython (clean version should just put all above to Jupyter notebook)
#write.csv(res_baggedTree,file='../SEPTEMBER_2014/original_SQL_continuous_only_baggedTree.csv')

library(apcluster)
help(apcluster)
?rnorm
?preferenceRange



###########CHANGE VAR NAMES BELOW!!!!!
=======
res<-predict(preprocParams,DS)
preprocParams_baggedTree<-preProcess(DS,method="bagImpute") #estimates std params
#install.packages('ipred')
res_baggedTree<-predict(preprocParams_baggedTree,DS)
sum(is.na(res_baggedTree))
dim(res_baggedTree)
>>>>>>> 880f85b808d3c17816b8072f01b368cf124928dc
model.kmeans.baggedTree<-kmeans(res_baggedTree,centers=2)

#impute with knn, impute with bags of trees
str(preproc)
str(preproc$data)
attr(preproc$data,"scaled:center")
attr(preproc$data,"scaled:scale")
attr(preproc$data,"dimnames")
head(preproc$data[1:642,1:15])

class(preproc$data[1:642,1:15])
dim(preproc$data[1:642,1:15])
sum(sapply(preproc$data[1:642,1:15],class)=='numeric') #all are numeric
sum(is.na(preproc$data[1:642,1:15]))
model.kmeans<-kmeans(preproc$data[1:642,1:15],centers=2)
model.kmeans[[1]]

res.with.class<-res
res.with.class$organism<-original.SQL.met$organism
res.with.class$meiotic<-original.SQL.met$meiotic
table(res.with.class$meiotic,model.kmeans[[1]])

