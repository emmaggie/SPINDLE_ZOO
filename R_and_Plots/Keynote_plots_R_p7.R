##Figure 4 and S4 MITOTIC
#Try piecewise regression for fig 1. 
#http://rkbookreviews.wordpress.com/2011/08/27/doing-bayesian-data-analysis-summary/
#Try multivariate regression on mitotic and mitotic spindles for fig 3.
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
head(mitotic)

original<-read.csv('../SEPTEMBER_2014/original.csv',stringsAsFactors=FALSE)
################################################################################################
################################################################################################
#GET NUMERIC COLUMNS ONLY (MULTIPLE REGRESSION)
#REMOVE DUPLICATES
#GET METAPHASE SPINDLES ONLY
#FIX DATA.TYPES and FILTER ON THEM
################################################################################################
################################################################################################

names(mitotic)
for (col in names(mitotic)){
  print(col)
  print(class(mitotic[,col]))
}

################################################################################################
#remove duplicates
################################################################################################

mitotic_for_MR<-mitotic
names(mitotic_for_MR)
mitotic_for_MR$astere.to.pole_distance_AVG<-apply(mitotic_for_MR[,c(1,2)],1,mean,rm=TRUE)

#http://stackoverflow.com/questions/4605206/drop-columns-r-data-frame
names(mitotic_for_MR)
mitotic_for_MR<-mitotic_for_MR[-c(1,2)]

mitotic_for_MR$inner_aster_diameter_AVG<-apply(mitotic_for_MR[,c(13,14)],1,mean,na.rm=TRUE)

to_drop<-c('inner_aster_diameter_1_um',"inner_aster_diameter_2_um")
mitotic_for_MR<-mitotic_for_MR[,!names(mitotic_for_MR) %in% to_drop]
#to_drop %in% names(mitotic_for_MR)

mitotic_for_MR$polar_body_diameter_AVG<-apply(mitotic_for_MR[,c(27,28)],1,mean,na.rm=TRUE)
names(mitotic_for_MR)
mitotic_for_MR<-mitotic_for_MR[-c(27,28)]

mitotic_for_MR$outer_aster_diameter_AVG<-apply(mitotic_for_MR[,c(23,24)],1,mean, na.rm=TRUE)
mitotic_for_MR<-mitotic_for_MR[-c(23,24)]

names(mitotic_for_MR)
#NOW: all the dupes are removed
#############################################################################################
#get metaphase spindles only: use mitotic_for_MR
#############################################################################################
ggplot(data=mitotic)+geom_point(aes(y=spindle_length_poles_um,x=log2(cell_diameter_um),colour=stage))
ggplot(data=mitotic)+geom_point(aes(y=spindle_length_poles_um,x=cell_diameter_um,colour=stage))



mitotic_for_MR$cell_diameter_um
ggplot(data=mitotic_for_MR)+geom_point(aes(y=spindle_length_poles_um,x=log2(cell_diameter_um),colour=stage))
ggplot(data=mitotic_for_MR)+geom_point(aes(y=spindle_length_poles_um,x=cell_diameter_um,colour=stage))



unique(mitotic_for_MR[(mitotic_for_MR$stage == 'm' | mitotic_for_MR$stage=='mI' | mitotic_for_MR$stage=='mII'),]$stage)

mitotic_for_MR_met<-mitotic_for_MR[(mitotic_for_MR$stage == 'm' | mitotic_for_MR$stage=='mI' | mitotic_for_MR$stage=='mII'),]
ggplot(data=mitotic_for_MR_met)+geom_point(aes(y=spindle_length_poles_um,x=log2(cell_diameter_um),colour=stage))

################################################################################################
#fix data types & filter on them: use: mitotic_for_MR_met
################################################################################################

for (col in names(mitotic_for_MR_met)){
  if(class(mitotic_for_MR_met[,col])=='integer'){
    print(col)
    print(class(mitotic_for_MR_met[,col]))
  }
} #not all _CATs are listed - fix it below

names(mitotic_for_MR_met)
class(mitotic_for_MR_met$stage)

### REMOVING 'character' VARIABLES
char_cols<-vector()
for (i in 1:length(names(mitotic_for_MR_met))){
  if(class(mitotic_for_MR_met[,i])=='character'){
    char_cols[i]<-names(mitotic_for_MR_met)[i]
    #print(col)
    #print(class(mitotic[,col]))
  }
}
char_cols<-char_cols[!is.na(char_cols)]

!names(mitotic_for_MR_met) %in% char_cols
names(mitotic_for_MR_met[,!names(mitotic_for_MR_met) %in% char_cols])
mitotic_for_MR_met_num<-mitotic_for_MR_met[,!names(mitotic_for_MR_met) %in% char_cols]
###############################################
#use: mitotic_for_MR_met_num !!!!!
### FIXING data types for _CAT columns
names(mitotic_for_MR_met_num)
grep('_CAT',names(mitotic_for_MR_met_num),value = TRUE)
CAT_cols<-names(mitotic_for_MR_met_num[,grep('_CAT',names(mitotic_for_MR_met_num),value = TRUE)])

for(i in 1:length(CAT_cols)){
  #print(CAT_cols[i])
  #print(class(mitotic_for_MR_met_num[,CAT_cols[i]]))
  mitotic_for_MR_met_num[,CAT_cols[i]]<-as.integer(mitotic_for_MR_met_num[,CAT_cols[i]])
}

for(i in 1:length(CAT_cols)){
  print(class(mitotic_for_MR_met_num[,CAT_cols[i]]))
}

integer_cols<-vector() #this one does not really work
for (i in 1:length(names(mitotic_for_MR_met_num))){
  if(class(mitotic_for_MR_met_num[,i])=='integer'){
    integer_cols[i]<-names(mitotic_for_MR_met_num)[i]
  }
}

names(mitotic_for_MR_met_num)
integer_cols #not metaphase_plate_aspect_ratio_um
integer_cols<-integer_cols[!is.na(integer_cols)]
##add a few that were not captured 
integer_cols<-c(integer_cols,'chromosomes',"num_of_cells_NUM")

mitotic_for_MR_met_num<-mitotic_for_MR_met_num[!names(mitotic_for_MR_met_num) %in% integer_cols]
names(mitotic_for_MR_met_num)


for (col in names(mitotic_for_MR_met_num)){
  #  print(col)
  print(class(mitotic_for_MR_met_num[,col]))
}


################################################################################################
################################################################################################
#TRY MULTIPLE REGRESSION USING REMAINING VALUES
#10.13 Multiple regression
#Crawley, Michael J. (2012-11-07). The R Book (Kindle Location 17300). Wiley. Kindle Edition. 
#normality is a problem
#mitotic_for_MR_met_num
#############################################################################################
#############################################################################################
head(mitotic_for_MR_met_num)
names(mitotic_for_MR_met_num)
dim(mitotic_for_MR_met_num)
#http://docs.ggplot2.org/current/geom_histogram.html
ggplot(data=mitotic_for_MR_met_num)+geom_histogram(aes(x=spindle_length_poles_um))
ggplot(data=mitotic_for_MR_met_num)+geom_histogram(aes(x=spindle_length_poles_um),binwidth=1)
ggplot(data=mitotic_for_MR_met_num)+geom_histogram(aes(x=spindle_length_poles_um,y=..density..),binwidth=1)+geom_density(aes(spindle_length_poles_um),colour='red')
shapiro.test(mitotic_for_MR_met_num$spindle_length_poles_um) #p-value = 0.003256
qqnorm(mitotic_for_MR_met_num$spindle_length_poles_um)
qqline(mitotic_for_MR_met_num$spindle_length_poles_um)
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
skew(mitotic_for_MR_met_num$spindle_length_poles_um)
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

kurtosis(mitotic_for_MR_met_num$spindle_length_poles_um)
#no significant skeweness, nor kurtosis
###############################################
###Trying some transformations:
#1
ggplot(data=mitotic_for_MR_met_num)+geom_histogram(aes(x=log2(spindle_length_poles_um),y=..density..))#+geom_density(aes(spindle_length_poles_um),colour='red')
shapiro.test(log(mitotic_for_MR_met_num$spindle_length_poles_um)) #p-value = 0.003256
qqnorm(log(mitotic_for_MR_met_num$spindle_length_poles_um))
qqline(log(mitotic_for_MR_met_num$spindle_length_poles_um))
#worse

#2
ggplot(data=mitotic_for_MR_met_num)+geom_histogram(aes(x=sqrt(spindle_length_poles_um),y=..density..))#+geom_density(aes(spindle_length_poles_um),colour='red')
shapiro.test(sqrt(mitotic_for_MR_met_num$spindle_length_poles_um)) #1.136e-06
qqnorm(sqrt(mitotic_for_MR_met_num$spindle_length_poles_um))
qqline(sqrt(mitotic_for_MR_met_num$spindle_length_poles_um))
#worse



#the problem is that the spindle length data is bimodal and fails normality test

#step 1: look ar correleations
getwd()
#pdf('p6_1_MR_pairs_of_variables_1_mitotic.pdf')
pairs(mitotic_for_MR_met_num,pch=16)
#dev.off()
#pdf('p6_1_MR_pairs_of_variables_2_mitotic.pdf')
pairs(mitotic_for_MR_met_num,panel=panel.smooth,pch=16)
#dev.off()

#step 2: checking curvature: try generalized additive model NOT 9!!!! to decide on curvature
#http://ecology.msu.montana.edu/labdsv/R/labs/lab5/lab5.html
library(mgcv)
names(mitotic_for_MR_met_num)
model.1<-gam(mitotic_for_MR_met_num$spindle_length_poles_um ~ s(mitotic_for_MR_met_num[,1]))
#the one below won't work - only the first term works 
#try to do them individually


curvature<-function(data_frame,resp_var_name,expl_var_name){
  model<-gam(unlist(data_frame[resp_var_name]) ~ s(unlist(data_frame[expl_var_name])))
  return(model)
}

#plot(gam(unlist(mitotic_for_MR_met_num["spindle_length_poles_um"])~s(unlist(mitotic_for_MR_met_num["cell_diameter_um"]))))
#class(mitotic_for_MR_met_num["spindle_length_poles_um"]) #data.frame
#class(unlist(mitotic_for_MR_met_num["spindle_length_poles_um"])) #data.frame
#class(mitotic_for_MR_met_num$spindle_length_poles_um) #numeric

#mitotic_for_MR_met_num["cell_diameter_um"]
#curvature(mitotic_for_MR_met_num,"spindle_length_poles_um","cell_diameter_um")

#model.1<-try(gam(mitotic_for_MR_met_num$spindle_length_poles_um ~ s(mitotic_for_MR_met_num[,1])+s(mitotic_for_MR_met_num[,2])+s(mitotic_for_MR_met_num[,3])+s(mitotic_for_MR_met_num[,4])+s(mitotic_for_MR_met_num[,5])+s(mitotic_for_MR_met_num[,6])+s(mitotic_for_MR_met_num[,7])+s(mitotic_for_MR_met_num[,8])+s(mitotic_for_MR_met_num[,11])+s(mitotic_for_MR_met_num[,12])+s(mitotic_for_MR_met_num[,13])+s(mitotic_for_MR_met_num[,14])+s(mitotic_for_MR_met_num[,15])),silent=TRUE)
plot(model.1)
#?gam
names(mitotic_for_MR_met_num)

listing_curvatures<-function(data_frame){
  list_of_gams<-vector('list',length(names(data_frame)))
  for(i in 1:length(names(data_frame))){
    print(names(data_frame)[i])
    
    list_of_gams[[i]]<-try(curvature(data_frame,"spindle_length_poles_um",names(data_frame)[i]))
  }
  list_of_gams<-setNames(list_of_gams,names(data_frame))  
  return(list_of_gams)
}
list_of_gams<-listing_curvatures(mitotic_for_MR_met_num)
#length(list_of_gams) #15
#length(names(mitotic_for_MR_met_num)) #15
#class(names(mitotic_for_MR_met_num))
names(mitotic_for_MR_met_num)
names(list_of_gams)

gam_plotter<-function(list_of_models){
  for(i in 1:length(list_of_models)){
    pdf(paste(i,'_','gam_',names(list_of_models)[i],'_mitotic.pdf',sep=""))
    try(plot(list_of_models[[i]]),silent=TRUE)
    dev.off()
  }
}
#gam_plotter(list_of_gams) #run to repeat plotting

#############################################################################################
#step 3: picking interaction terms - trees
#############################################################################################


library(tree)
#http://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/
#http://plantecology.syr.edu/fridley/bio793/cart.html

names(mitotic_for_MR_met_num[,c(1:8,10:15)]) #

model.2<-tree(mitotic_for_MR_met_num[,c(1:8,10:15)]$spindle_length_poles_um ~ . ,data=mitotic_for_MR_met_num[,c(1:8,10:15)])

#pdf(file='tree_model_mitotic_with_aspect.pdf')
plot(model.2)
text(model.2,cex=0.7)
model.2
str(model.2)
names(model.2$frame)
model.2$frame[model.2$frame$var=="cell_diameter_um",]$dev[2:4]
sum(model.2$frame[model.2$frame$var=="cell_diameter_um",]$dev[2:4])
str(model.2$frame[model.2$frame$var=="cell_diameter_um",])
#how to read trees:
#http://plantecology.syr.edu/fridley/bio793/cart.html
y=mitotic_for_MR_met_num[,c(1:8,10:15)]$spindle_length_poles_um
SSY=sum(sapply(y,function(x)(x-mean(y,na.rm=TRUE))^2),na.rm=TRUE)
SSY.model.2<-sum(sapply(model.2$y,function(x)(x-mean(model.2$y,na.rm=TRUE))^2),na.rm=TRUE)
summary(model.2)
############################################
#calculate fraction of deviance explained:
############################################


residual_dev=sum(model.2$frame[model.2$frame$var=='<leaf>',]$dev)
frac_dev_expl=1-residual_dev/model.2$frame$dev[1]

#dev.off()
class(summary(model.2)$used)

names(mitotic_for_MR_met_num[,c(1:8,10:15)]) 
############################################
#getting the length of branches
#calculate fraction of deviance explained by a particular variable:
############################################
#STEP_1 - get node index for variable of choice and initial deviance associated with it
#works perfectly
get_tree_idx_of_var_x<-function(tree_model,var_name){
  result<-list(idx=row.names(tree_model$frame[tree_model$frame$var==var_name,]),entry_dev=tree_model$frame[tree_model$frame$var==var_name,]$dev)
  return(result)
}
#test:
#get_tree_idx_of_var_x(model.2,'cell_diameter_um')

#STEP_2  - get output node indices and deviances associated with them
get_output_deviance<-function(tree_model,var_name){
  entry_indices=get_tree_idx_of_var_x(tree_model,var_name)$idx
  exit_indices<-vector('list',length(entry_indices))
  for(i in 1:length(entry_indices)){
      exit_indices[[i]]<-c(as.character(as.numeric(entry_indices[i])*2),as.character(as.numeric(entry_indices[i])*2+1))
  }
  output_dev<-vector()
  for(i in 1:length(exit_indices)){
    output_dev[i]<-(sum(tree_model$frame[row.names(tree_model$frame) %in% exit_indices[[i]],]$dev))
  }
  return(sum(output_dev))
}
#test:
get_tree_idx_of_var_x(model.2,'cell_diameter_um')$entry_dev[1]
get_output_deviance(model.2,'cell_diameter_um')
frac_of_dev_cell_diam=get_output_deviance(model.2,'cell_diameter_um')/get_tree_idx_of_var_x(model.2,'cell_diameter_um')$entry_dev[1]

attr(model.2$terms,'term.labels')
#just checking: (pure ridiculousness)
#model.2a<-tree(mitotic_for_MR_met_num[,c(1:15)]$spindle_length_poles_um ~ . ,data=mitotic_for_MR_met_num[,c(1:15)])
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

#ggplot(data=mitotic_for_MR_met_num)+geom_point(aes(y=spindle_length_poles_um,x=log2(cell_diameter_um)))#,colour=stage))

head(mitotic_for_MR_met_num[,c(1:8,10:15)],1L)
head(mitotic_for_MR_met_num[,c(8,9)],1L)

#[1] "cell_diameter_um" - curved
#[1] "inner_aster_diameter_AVG" - maybe curved 
#[1] "genome"  - curved
#[1] "metaphase_plate_lengt_h_um" - not

#gte the index numbers for them
#check curvature for the above 5 terms (plots in the folder)
#############################################################################################
#LINEAR MODEL
#DEFINE THE TERMS
#############################################################################################


cd2<-(mitotic_for_MR_met_num$cell_diameter_um)^2
g2<-(mitotic_for_MR_met_num$genome)^2
iad2<-(mitotic_for_MR_met_num$inner_aster_diameter_AVG)^2
cd_g<-mitotic_for_MR_met_num$cell_diameter_um*mitotic_for_MR_met_num$genome
cd_iad<-mitotic_for_MR_met_num$cell_diameter_um*mitotic_for_MR_met_num$inner_aster_diameter_AVG
cd_mplh<-mitotic_for_MR_met_num$cell_diameter_um*mitotic_for_MR_met_num$metaphase_plate_lengt_h_um
cd_g_iad<-mitotic_for_MR_met_num$cell_diameter_um*mitotic_for_MR_met_num$genome*mitotic_for_MR_met_num$inner_aster_diameter_AVG
cd_g_mplh<-mitotic_for_MR_met_num$cell_diameter_um*mitotic_for_MR_met_num$genome*mitotic_for_MR_met_num$metaphase_plate_lengt_h_um

model.3<-lm(mitotic_for_MR_met_num$spindle_length_poles_um~mitotic_for_MR_met_num$cell_diameter_um+mitotic_for_MR_met_num$inner_aster_diameter_AVG+mitotic_for_MR_met_num$genome+mitotic_for_MR_met_num$metaphase_plate_lengt_h_um+cd2+g2+iad2+cd_g+cd_g_iad+cd_mplh+cd_g_iad+cd_g_mplh)

summary(model.3)

model.3a<-update(model.3,~.-cd_g_mplh)
summary(model.3a)

model.3b<-update(model.3a,~.-cd_mplh)
summary(model.3b)

model.3c<-update(model.3b,~.-cd_g_iad)
summary(model.3c)

model.3d<-update(model.3c,~.-cd_g)
summary(model.3d)

model.3e<-update(model.3d,~.-cd2)
summary(model.3e)

model.3f<-update(model.3e,~.-mitotic_for_MR_met_num$genome)
summary(model.3f)
