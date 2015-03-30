#Figure 1: piecewise regression

library(ggplot2)
##############################################################################################
#prepare data
#get metaphase only
##############################################################################################

setwd('/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo Data/R_and_Plots/')
getwd()
mitotic<-read.csv('../SEPTEMBER_2014/meiotic_false.csv',stringsAsFactors=FALSE)
head(mitotic)

#there is only 'm' class here
unique(mitotic[(mitotic$stage == 'm' | mitotic$stage=='mI' | mitotic$stage=='mII'),]$stage)

mitotic_met<-mitotic[(mitotic$stage == 'm' | mitotic$stage=='mI' | mitotic$stage=='mII'),]

##############################################################################################
#plot spindle size 
#plot the mitotic spindle length as a function of the log2() of the cell size

##############################################################################################

ggplot(data=mitotic_met)+geom_point(aes(y=spindle_length_poles_um,x=log2(cell_diameter_um),colour=stage))

#plot the mitotic spindle length as a function of the cell size
mitotic_met<-mitotic[(mitotic$stage == 'm' | mitotic$stage=='mI' | mitotic$stage=='mII'),]
ggplot(data=mitotic_met)+geom_point(aes(y=spindle_length_poles_um,x=(cell_diameter_um),colour=stage))

#sysctl hw.ncpu


###testing
test<-read.table('/Volumes/Seagate_RED/AAA_Programming/R/BOOKS/THE_R_BOOK/therbook/sasilwood.txt',header=TRUE)
names(test)
dim(test)
str(test)
par(mfrow = c(1,1))
plot(log(test$Species)~log(test$Area),pch=16,col='red',bg='yellow')
plot(test$Species~log(test$Area),pch=16,col='red',bg='yellow')

model.test<-lm(log(test$Species)~log(test$Area))
par(mfrow = c(2,2))
str(model.test)
plot(model.test)

#http://stats.stackexchange.com/questions/76226/interpreting-the-residuals-vs-fitted-values-plot-for-verifying-the-assumptions

