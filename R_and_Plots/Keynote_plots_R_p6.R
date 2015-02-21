#Try piecewise regression for fig 1. 
#http://rkbookreviews.wordpress.com/2011/08/27/doing-bayesian-data-analysis-summary/
#Try multivariate regression on meiotic and mitotic spindles for fig 3.
#y is spindle size
library(ggplot2)
#################################################################################################
#prepare data
#################################################################################################

setwd('/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo Data/R_and_Plots/')
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
################################################################################################
#GET NUMERIC COLUMNS ONLY (MULTIPLE REGRESSION)
#REMOVE DUPLICATES
#GET METAPHASE SPINDLES ONLY
#FIX DATA.TYPES and FILTER ON THEM
################################################################################################
################################################################################################

names(meiotic)
for (col in names(meiotic)){
  print(col)
  print(class(meiotic[,col]))
}

################################################################################################
#remove duplicates
################################################################################################

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
names(meiotic_for_MR)
meiotic_for_MR<-meiotic_for_MR[-c(27,28)]

meiotic_for_MR$outer_aster_diameter_AVG<-apply(meiotic_for_MR[,c(23,24)],1,mean, na.rm=TRUE)
meiotic_for_MR<-meiotic_for_MR[-c(23,24)]

names(meiotic_for_MR)
#NOW: all the dupes are removed
################################################################################################
#get metaphase spindles only: use meiotic_for_MR
################################################################################################

meiotic_for_MR$cell_diameter_um
ggplot(data=meiotic_for_MR)+geom_point(aes(y=spindle_length_poles_um,x=log2(cell_diameter_um),colour=stage))

unique(meiotic_for_MR[(meiotic_for_MR$stage == 'm' | meiotic_for_MR$stage=='mI' | meiotic_for_MR$stage=='mII'),]$stage)

meiotic_for_MR_met<-meiotic_for_MR[(meiotic_for_MR$stage == 'm' | meiotic_for_MR$stage=='mI' | meiotic_for_MR$stage=='mII'),]
ggplot(data=meiotic_for_MR_met)+geom_point(aes(y=spindle_length_poles_um,x=log2(cell_diameter_um),colour=stage))

################################################################################################
#fix data types & filter on them: use: meiotic_for_MR_met
################################################################################################

for (col in names(meiotic_for_MR_met)){
  if(class(meiotic_for_MR_met[,col])=='integer'){
    print(col)
    print(class(meiotic_for_MR_met[,col]))
  }
} #not all _CATs are listed - fix it below

names(meiotic_for_MR_met)
class(meiotic_for_MR_met$stage)

### REMOVING 'character' VARIABLES
char_cols<-vector()
for (i in 1:length(names(meiotic_for_MR_met))){
  if(class(meiotic_for_MR_met[,i])=='character'){
    char_cols[i]<-names(meiotic_for_MR_met)[i]
    #print(col)
    #print(class(meiotic[,col]))
  }
}
char_cols<-char_cols[!is.na(char_cols)]

!names(meiotic_for_MR_met) %in% char_cols
names(meiotic_for_MR_met[,!names(meiotic_for_MR_met) %in% char_cols])
meiotic_for_MR_met_num<-meiotic_for_MR_met[,!names(meiotic_for_MR_met) %in% char_cols]
###############################################
#use: meiotic_for_MR_met_num !!!!!
### FIXING data types for _CAT columns
names(meiotic_for_MR_met_num)
grep('_CAT',names(meiotic_for_MR_met_num),value = TRUE)
CAT_cols<-names(meiotic_for_MR_met_num[,grep('_CAT',names(meiotic_for_MR_met_num),value = TRUE)])

for(i in 1:length(CAT_cols)){
  #print(CAT_cols[i])
  #print(class(meiotic_for_MR_met_num[,CAT_cols[i]]))
  meiotic_for_MR_met_num[,CAT_cols[i]]<-as.integer(meiotic_for_MR_met_num[,CAT_cols[i]])
}

for(i in 1:length(CAT_cols)){
  print(class(meiotic_for_MR_met_num[,CAT_cols[i]]))
}

integer_cols<-vector() #this one does not really work
for (i in 1:length(names(meiotic_for_MR_met_num))){
  if(class(meiotic_for_MR_met_num[,i])=='integer'){
    integer_cols[i]<-names(meiotic_for_MR_met_num)[i]
  }
}

names(meiotic_for_MR_met_num)
integer_cols #not metaphase_plate_aspect_ratio_um
integer_cols<-integer_cols[!is.na(integer_cols)]
##add a few that were not captured 
integer_cols<-c(integer_cols,'chromosomes',"num_of_cells_NUM")

meiotic_for_MR_met_num<-meiotic_for_MR_met_num[!names(meiotic_for_MR_met_num) %in% integer_cols]
names(meiotic_for_MR_met_num)


for (col in names(meiotic_for_MR_met_num)){
#  print(col)
  print(class(meiotic_for_MR_met_num[,col]))
}


################################################################################################
################################################################################################
#TRY MULTIPLE REGRESSION USING REMAINING VALUES
#10.13 Multiple regression
#Crawley, Michael J. (2012-11-07). The R Book (Kindle Location 17300). Wiley. Kindle Edition. 
#normality is a problem
#meiotic_for_MR_met_num
################################################################################################
################################################################################################
head(meiotic_for_MR_met_num)
names(meiotic_for_MR_met_num)
dim(meiotic_for_MR_met_num)
#http://docs.ggplot2.org/current/geom_histogram.html
ggplot(data=meiotic_for_MR_met_num)+geom_histogram(aes(x=spindle_length_poles_um))
ggplot(data=meiotic_for_MR_met_num)+geom_histogram(aes(x=spindle_length_poles_um),binwidth=1)
ggplot(data=meiotic_for_MR_met_num)+geom_histogram(aes(x=spindle_length_poles_um,y=..density..),binwidth=1)+geom_density(aes(spindle_length_poles_um),colour='red')
shapiro.test(meiotic_for_MR_met_num$spindle_length_poles_um) #p-value = 0.003256
qqnorm(meiotic_for_MR_met_num$spindle_length_poles_um)
qqline(meiotic_for_MR_met_num$spindle_length_poles_um)
#this is not too good
?pt

###############################################
#check out skeweness and kurtosis
skew<-function(x){
  m3<-sum((x-mean(x,na.rm=TRUE))^3,na.rm=TRUE)/length(x)
  s3<-sqrt(var(x,na.rm=TRUE))^3
  skew<-m3/s3
  se_gamma<-sqrt(6/length(x))  
  t=skew/se_gamma
  #pt is cumulative distribution function (tests equal or less than value given)
  prob=1-pt(t,length(x)-2)
  return(list(skew=skew,se_gamma=se_gamma,prob=prob))
  }
skew(meiotic_for_MR_met_num$spindle_length_poles_um)
#test significance


kurtosis<-function(x){
    m4<-sum((x-mean(x,na.rm=TRUE))^4,na.rm=TRUE)/length(x)
    s4<-var(x,na.rm=TRUE)^2
    kurtosis=m4/s4-3
    se_gamma<-sqrt(24/length(x))
    t=kurtosis/se_gamma
    prob=1-pt(t,length(x)-2)
    return(list(kurtosis=kurtosis,se_gamma=se_gamma,prob=prob))
}

kurtosis(meiotic_for_MR_met_num$spindle_length_poles_um)
#no significant skeweness, nor kurtosis
###############################################
###Trying some transformations:
#1
ggplot(data=meiotic_for_MR_met_num)+geom_histogram(aes(x=log2(spindle_length_poles_um),y=..density..))#+geom_density(aes(spindle_length_poles_um),colour='red')
shapiro.test(log(meiotic_for_MR_met_num$spindle_length_poles_um)) #p-value = 0.003256
qqnorm(log(meiotic_for_MR_met_num$spindle_length_poles_um))
qqline(log(meiotic_for_MR_met_num$spindle_length_poles_um))
#worse

#2
ggplot(data=meiotic_for_MR_met_num)+geom_histogram(aes(x=sqrt(spindle_length_poles_um),y=..density..))#+geom_density(aes(spindle_length_poles_um),colour='red')
shapiro.test(sqrt(meiotic_for_MR_met_num$spindle_length_poles_um)) #1.136e-06
qqnorm(sqrt(meiotic_for_MR_met_num$spindle_length_poles_um))
qqline(sqrt(meiotic_for_MR_met_num$spindle_length_poles_um))
#worse



#the problem is that the spindle length data is bimodal and fails normality test

#step 1: look ar correleations
getwd()
pdf('p6_1_MR_pairs_of_variables_1.pdf')
pairs(meiotic_for_MR_met_num,pch=16)
dev.off()
pdf('p6_1_MR_pairs_of_variables_2.pdf')
pairs(meiotic_for_MR_met_num,panel=panel.smooth,pch=16)
dev.off()

#step 2: checking curvature: try generalized additive model NOT 9!!!! to decide on curvature
#http://ecology.msu.montana.edu/labdsv/R/labs/lab5/lab5.html
library(mgcv)
names(meiotic_for_MR_met_num)
model.1<-gam(meiotic_for_MR_met_num$spindle_length_poles_um ~ s(meiotic_for_MR_met_num[,1]))
#the one below won't work - only the first term works 
#try to do them individually


curvature<-function(data_frame,resp_var_name,expl_var_name){
  model<-gam(unlist(data_frame[resp_var_name]) ~ s(unlist(data_frame[expl_var_name])))
  return(model)
}

#plot(gam(unlist(meiotic_for_MR_met_num["spindle_length_poles_um"])~s(unlist(meiotic_for_MR_met_num["cell_diameter_um"]))))
#class(meiotic_for_MR_met_num["spindle_length_poles_um"]) #data.frame
#class(unlist(meiotic_for_MR_met_num["spindle_length_poles_um"])) #data.frame
#class(meiotic_for_MR_met_num$spindle_length_poles_um) #numeric

#meiotic_for_MR_met_num["cell_diameter_um"]
#curvature(meiotic_for_MR_met_num,"spindle_length_poles_um","cell_diameter_um")

#model.1<-try(gam(meiotic_for_MR_met_num$spindle_length_poles_um ~ s(meiotic_for_MR_met_num[,1])+s(meiotic_for_MR_met_num[,2])+s(meiotic_for_MR_met_num[,3])+s(meiotic_for_MR_met_num[,4])+s(meiotic_for_MR_met_num[,5])+s(meiotic_for_MR_met_num[,6])+s(meiotic_for_MR_met_num[,7])+s(meiotic_for_MR_met_num[,8])+s(meiotic_for_MR_met_num[,11])+s(meiotic_for_MR_met_num[,12])+s(meiotic_for_MR_met_num[,13])+s(meiotic_for_MR_met_num[,14])+s(meiotic_for_MR_met_num[,15])),silent=TRUE)
plot(model.1)
#?gam
names(meiotic_for_MR_met_num)

listing_curvatures<-function(data_frame){
  list_of_gams<-vector('list',length(names(data_frame)))
  for(i in 1:length(names(data_frame))){
    print(names(data_frame)[i])
    
    list_of_gams[[i]]<-try(curvature(data_frame,"spindle_length_poles_um",names(data_frame)[i]))
    }
  list_of_gams<-setNames(list_of_gams,names(data_frame))  
  return(list_of_gams)
}
list_of_gams<-listing_curvatures(meiotic_for_MR_met_num)
#length(list_of_gams) #15
#length(names(meiotic_for_MR_met_num)) #15
#class(names(meiotic_for_MR_met_num))
names(meiotic_for_MR_met_num)
names(list_of_gams)

gam_plotter<-function(list_of_models){
  for(i in 1:length(list_of_models)){
    pdf(paste(i,'_','gam_',names(list_of_models)[i],'.pdf',sep=""))
    try(plot(list_of_models[[i]]),silent=TRUE)
    dev.off()
  }
}
#gam_plotter(list_of_gams) #run to repeat plotting

#step 3: picking interaction terms - trees
library(tree)
names(meiotic_for_MR_met_num[,c(1:8,10:15)]) #

model.2<-tree(meiotic_for_MR_met_num[,c(1:8,10:15)]$spindle_length_poles_um ~ . ,data=meiotic_for_MR_met_num[,c(1:8,10:15)])

#pdf(file='tree_model_meiotic_with_aspect.pdf')
plot(model.2)
text(model.2,cex=0.7)
#dev.off()

####ALTERNATIVE WITHOUT SPINDLE ASPECT RATIO
names(meiotic_for_MR_met_num[,c(1:8,10:15)]) #remove 7 and 8
names(meiotic_for_MR_met_num[,c(1:6,10:15)]) #remove 7 and 8

model.2a<-tree(meiotic_for_MR_met_num[,c(1:6,10:15)]$spindle_length_poles_um ~ . ,data=meiotic_for_MR_met_num[,c(1:6,10:15)])
#meiotic_for_MR_met_num$spindle_length_poles_um
#pdf(file='tree_model_meiotic_with_aspect.pdf')
plot(model.2a)
text(model.2a,cex=0.7)
#dev.off()


str(model.2)

class(summary(model.2)$used)
attr(model.2$terms,'term.labels')

#just checking: (pure ridiculousness)
#model.2a<-tree(meiotic_for_MR_met_num[,c(1:15)]$spindle_length_poles_um ~ . ,data=meiotic_for_MR_met_num[,c(1:15)])
#plot(model.2a)
#text(model.2a,cex=0.7)


class(as.character(summary(model.2)$used))

#str(summary(model.2))
#gettig index of the relevant terms
most_rel_terms<-function(list_of_terms){
  for(i in 1:length(as.character(list_of_terms))){
    print(list_of_terms[i])
  }
  #return()
}
class(as.character(summary(model.2)$used))
most_rel_terms(as.character(summary(model.2)$used))
 
#ggplot(data=meiotic_for_MR_met_num)+geom_point(aes(y=spindle_length_poles_um,x=log2(cell_diameter_um)))#,colour=stage))

head(meiotic_for_MR_met_num[,c(1:8,10:15)],1L)
head(meiotic_for_MR_met_num[,c(8,9)],1L)

#[1] "cell_diameter_um" - curved
#[1] "genome" - curved
#[1] "spindle_aspect_ratio_poles_um"
#[1] "spindle_aspect_ratio_asters_um"
#[1] "metaphase_plate_lengt_h_um"

#gte the index numbers for them
#check curvature for the above 5 terms (plots in the folder)

#DEFINE THE TERMS
cd2<-(meiotic_for_MR_met_num$cell_diameter_um)^2
g2<-(meiotic_for_MR_met_num$genome)^2
cd_g<-meiotic_for_MR_met_num$cell_diameter_um*meiotic_for_MR_met_num$genome
cd_sara<-meiotic_for_MR_met_num$cell_diameter_um*meiotic_for_MR_met_num$spindle_aspect_ratio_asters_um
g_sarp<-meiotic_for_MR_met_num$genome*meiotic_for_MR_met_num$spindle_aspect_ratio_poles_um
sara_mplh<-meiotic_for_MR_met_num$spindle_aspect_ratio_asters_um*meiotic_for_MR_met_num$metaphase_plate_lengt_h_um
cd_g_sarp<-meiotic_for_MR_met_num$cell_diameter_um*meiotic_for_MR_met_num$genome*meiotic_for_MR_met_num$spindle_aspect_ratio_poles_um
cd_sara_mplh<-meiotic_for_MR_met_num$cell_diameter_um*meiotic_for_MR_met_num$spindle_aspect_ratio_asters_um*meiotic_for_MR_met_num$metaphase_plate_lengt_h_um

model.3<-lm(meiotic_for_MR_met_num$spindle_length_poles_um~meiotic_for_MR_met_num$cell_diameter_um+meiotic_for_MR_met_num$genome+meiotic_for_MR_met_num$spindle_aspect_ratio_poles_um+meiotic_for_MR_met_num$spindle_aspect_ratio_asters_um+meiotic_for_MR_met_num$metaphase_plate_lengt_h_um+cd2+g2+cd_g+cd_sara+g_sarp+sara_mplh+cd_g_sarp+cd_sara_mplh)

summary(model.3)

model.3a<-update(model.3,~.-cd_sara_mplh)
summary(model.3a)

model.3b<-update(model.3a,~.-cd_g)
summary(model.3b)

model.3b<-update(model.3b,~.-meiotic_for_MR_met_num$spindle_aspect_ratio_poles_um)
summary(model.3b)

model.3b<-update(model.3b,~.-meiotic_for_MR_met_num$spindle_aspect_ratio_asters_um)
summary(model.3b)



#Warning messages:
#1: In model.matrix.default(mt, mf, contrasts) :
# the response appeared on the right-hand side and was dropped
#2: In model.matrix.default(mt, mf, contrasts) :
# problem with term 3 in model.matrix: no columns are assigned
model.3c<-update(model.3b,~.-cd2)
summary(model.3c)

#plot(model.3c)








###SKIP ME!!!!!
#names = LETTERS[1:26] ## Gives a sequence of the letters of the alphabet
#beta1 = rnorm(26, 5, 2) ## A vector of slopes (one for each letter)
#beta0 = 10 ## A common intercept

#for(i in 1:26){
# x = rnorm(500, 105, 10)
# y = beta0 + beta1[i]*x + 15*rnorm(500)
  
# mypath <- file.path(paste("myplot_", names[i], ".jpg", sep = ""))
  
# png(file=mypath)
# mytitle = paste("my title is", names[i])
# plot(x,y, main = mytitle)
# dev.off()
#}
### DONE HERE!!!
