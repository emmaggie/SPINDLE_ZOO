##Figure 3 & S3: 
##final data prep in: A_Marina_spindle_zoo_part4.ipynb

rm(list=ls())
library(ggplot2)

#setwd('/Volumes/Magdalena_NEW1/MARINA_PAPER/SEPTEMBER_2014')

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
mitotic<-read.csv('../SEPTEMBER_2014/meiotic_false.csv',stringsAsFactors=FALSE)
#mitotic_anaphase<-read.csv('../SEPTEMBER_2014/mitotic_anaphase.csv')
#mitotic_anaphase$organism<-as.character(mitotic_anaphase$organism)
head(meiotic)

original<-read.csv('../SEPTEMBER_2014/original.csv',stringsAsFactors=FALSE)

#inner_diam_mit_met=read.csv('../SEPTEMBER_2014/inner_aster_diam_cell_sizeDF_mit_met_Jan2015.csv')
################################################################################################
################################################################################################
#MAPPING COLORS
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
################################################################################################
################################################################################################
gg_color_hue <- function(n) {
  #function to generate colors 
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

cols=gg_color_hue(length(unique(original$organism_CAT))) #these are the colors, change assignment here 
#check the colors out:
plot(1:length(unique(original$organism_CAT)), pch=16, cex=2, col=cols)
my_labels=unique(original$organism)
mapping_vector_lab<-setNames(object=cols,my_labels)
#?setNames
names(mapping_vector_lab)

plot(1:length(mapping_vector_lab), pch=16, cex=2, col=cols)
text(1:length(mapping_vector_lab),names(mapping_vector_lab),cex=0.5) 

################################################################################################
#MODIFIED COLOR ASSIGNMENT TO CURRENT
################################################################################################

new_cols<-c("#F8766D","#00B0F6","#FF6A98","#35A2FF","#C09B00","#E76BF3","#EA8331","#7CAE00","#9590FF","#39B600","#A3A500","#00BB4E","#FF62BC","#00BAE0","#00C1A3","#FA62DB","#00BF7D","#D89000","#00BAE0","#C77CFF")
mapping_vector2<-setNames(object=as.character(new_cols),names(mapping_vector_lab))

plot(1:length(mapping_vector2), pch=16, cex=2, col=mapping_vector2)
#http://www.statmethods.net/advgraphs/axes.html
text(1:length(mapping_vector2),names(mapping_vector2),cex=0.5) 

################################################################################################
################################################################################################
################################################################################################
#PLOTTING
################################################################################################
################################################################################################
################################################################################################

################################################################################################
################################################################################################
#MEIOTIC SPINDLES: F3B
################################################################################################
################################################################################################

#see file: A_Marina_spindle_zoo_part3.ipynb: use it to add means column to plotting data frame


names(meiotic)
unique(meiotic$stage)
unique(mitotic$stage)

#use this one for plotting!!!!
meiotic_for_plot<-meiotic[(which(meiotic$stage=='mI' | meiotic$stage=='mII' | meiotic$stage=='m')),]
meiotic_for_plot[which(meiotic_for_plot$organism=='danio_rerio'),]
levels(meiotic_for_plot$organism)
unique(meiotic_for_plot$stage)

unique(meiotic[(which(meiotic$stage=='mI' | meiotic$stage=='mII' | meiotic$stage=='m')),]$stage)
meiotic[(which(meiotic$stage=='mII')),]
meiotic[(which(meiotic$stage=='m')),]

#1
plot_F3B<-ggplot()
plot_F3B<-plot_F3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_F3B<-plot_F3B+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3B
#ggsave(filename='../Manuscript/Figures/F3/F3B_scaled_1p5.pdf',useDingbats=FALSE)
#dir('../Manuscript/Figures/F3/')
plot_F3B<-plot_F3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
#ggsave(filename='../Manuscript/Figures/F3/F3B_1p5.pdf',useDingbats=FALSE)

#1. split data by organism
#2. calculate mean per organism
mean(meiotic_for_plot[meiotic_for_plot$organism=='xenopus_laevis',]$spindle_length_poles_um,na.rm=TRUE)
mean(meiotic_for_plot[meiotic_for_plot$organism=='xenopus_laevis',]$spindle_length_poles_um,na.rm=TRUE)

#http://www.regular-expressions.info/rlanguage.html
names(meiotic_for_plot)
grep("cell_diameter_um",names(meiotic_for_plot))
grep('spindle_length_poles_um',names(meiotic_for_plot))

by(meiotic_for_plot[,c(3)], meiotic_for_plot$organism,mean,na.rm=TRUE)
by(meiotic_for_plot[,c(36)], meiotic_for_plot$organism,mean,na.rm=TRUE)
names(by(meiotic_for_plot[,c(36)], meiotic_for_plot$organism,mean,na.rm=TRUE))

means_F3B<-cbind(by(meiotic_for_plot[,c(3)], meiotic_for_plot$organism,mean,na.rm=TRUE),by(meiotic_for_plot[,c(36)], meiotic_for_plot$organism,mean,na.rm=TRUE))
means_F3B<-data.frame(means_F3B)
names(means_F3B)<-c("cell_diameter_um",'spindle_length_poles_um')

row.names(means_F3B)
names(means_F3B)

means_F3B$organism<-row.names(means_F3B)
means_F3B

plot_F3B<-ggplot()
plot_F3B<-plot_F3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_F3B<-plot_F3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
plot_F3B<-plot_F3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3B))),size=7,alpha=0.6)
plot_F3B<-plot_F3B+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3B
#ggsave(filename='../Manuscript/Figures/F3/F3B_with_mean_1p5_1.pdf',useDingbats=FALSE)

plot_F3B<-ggplot()
plot_F3B<-plot_F3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5,alpha=0.7)
plot_F3B<-plot_F3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
plot_F3B<-plot_F3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3B))),size=5,alpha=0.8)
plot_F3B<-plot_F3B+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3B
#ggsave(filename='../Manuscript/Figures/F3/F3B_with_mean_1p5_2.pdf',useDingbats=FALSE)

plot_F3B<-ggplot()
plot_F3B<-plot_F3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_F3B<-plot_F3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
plot_F3B<-plot_F3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3B))),size=5,alpha=0.6)
plot_F3B<-plot_F3B+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3B
#ggsave(filename='../Manuscript/Figures/F3/F3B_with_mean_1p5_3.pdf',useDingbats=FALSE)

plot_F3B<-ggplot()
plot_F3B<-plot_F3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_F3B<-plot_F3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
plot_F3B<-plot_F3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3B))),size=6,alpha=0.6)
plot_F3B<-plot_F3B+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3B<-plot_F3B+annotate('text',label=paste("R^2==",(summary.lm(lm(meiotic_for_plot$spindle_length_poles_um~meiotic_for_plot$cell_diameter_um))$r.squared)),x=3,y=40,parse=TRUE)
plot_F3B
#ggsave(filename='../Manuscript/Figures/F3/F3B_with_mean_1p5_4_Rsq.pdf',useDingbats=FALSE)


##MODELLING#####
lm(meiotic_for_plot$spindle_length_poles_um~meiotic_for_plot$cell_diameter_um)
deviance(lm(meiotic_for_plot$spindle_length_poles_um~1)) #15216.72
deviance(lm(meiotic_for_plot$spindle_length_poles_um~meiotic_for_plot$cell_diameter_um)) #12123.58
summary(lm(meiotic_for_plot$spindle_length_poles_um~meiotic_for_plot$cell_diameter_um))
#http://docs.ggplot2.org/current/geom_abline.html
plot_F3B+geom_abline(intercept=20.501764,slope=0.006686,colour='red')

lm(meiotic_for_plot$spindle_length_poles_um~log2(meiotic_for_plot$cell_diameter_um))
summary(lm(meiotic_for_plot$spindle_length_poles_um~log2(meiotic_for_plot$cell_diameter_um)))
deviance(lm(meiotic_for_plot$spindle_length_poles_um~log2(meiotic_for_plot$cell_diameter_um))) #12321.1
plot_F3B+geom_abline(intercept=11.334,slope=1.484,colour='blue')
#lm(meiotic_for_plot$spindle_length_poles_um~log10(meiotic_for_plot$cell_diameter_um))
#plot_F3B+geom_abline(intercept=11.334,slope=4.928 ,colour='blue')

str(lm(meiotic_for_plot$spindle_length_poles_um~meiotic_for_plot$cell_diameter_um))
str(summary.lm(lm(meiotic_for_plot$spindle_length_poles_um~meiotic_for_plot$cell_diameter_um)))

summary.lm(lm(means_F3B$spindle_length_poles_um~means_F3B$cell_diameter_um))$r.squared #0.1945252
summary.lm(lm(means_F3B$spindle_length_poles_um~log2(means_F3B$cell_diameter_um)))$r.squared #0.2321305

my_residuals=lm(meiotic_for_plot$spindle_length_poles_um~meiotic_for_plot$cell_diameter_um)$residuals
ggplot()+geom_histogram(aes(my_residuals),binwidth=2)
ggplot()+geom_histogram(aes(x=my_residuals,y=..density..),binwidth=2)+geom_density(aes(x=my_residuals),colour='red')

#NULL: residuals are normally distributed
shapiro.test(my_residuals) #p-value = 0.002001 NOT NORMAL
qqnorm(my_residuals,pch=16)
qqline(my_residuals,col='red')

my_residuals_log=lm(meiotic_for_plot$spindle_length_poles_um~log2(meiotic_for_plot$cell_diameter_um))$residuals
shapiro.test(my_residuals_log) #p-value = 0.002001 NOT NORMAL
qqnorm(my_residuals_log,pch=16)
qqline(my_residuals_log,col='red') #0.001581
ggplot()+geom_histogram(aes(x=my_residuals_log,y=..density..),binwidth=2)+geom_density(aes(x=my_residuals_log),colour='red')

#plot_F3B+geom_text(data=NULL,x=3,y=35,label='R')
plot_F3B+annotate('text',label=paste("R^2==",(summary.lm(lm(meiotic_for_plot$spindle_length_poles_um~meiotic_for_plot$cell_diameter_um))$r.squared)),x=3,y=40,parse=TRUE)

round(summary.lm(lm(meiotic_for_plot$spindle_length_poles_um~meiotic_for_plot$cell_diameter_um))$r.squared,3)
signif(summary.lm(lm(meiotic_for_plot$spindle_length_poles_um~meiotic_for_plot$cell_diameter_um))$r.squared,3)


#PLOTTING THE MODEL
plot_F3B<-ggplot()
plot_F3B<-plot_F3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_F3B<-plot_F3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
plot_F3B<-plot_F3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3B))),size=7,alpha=0.6)
plot_F3B<-plot_F3B+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3B<-plot_F3B+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3B$spindle_length_poles_um~log2(means_F3B$cell_diameter_um)))$r.squared,3))),x=3,y=40,parse=TRUE)
plot_F3B
#ggsave(filename='../Manuscript/Figures/F3/F3B_with_mean_1p5_1_Rsq.pdf',useDingbats=FALSE)

plot_F3B<-ggplot()
plot_F3B<-plot_F3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5,alpha=0.7)
plot_F3B<-plot_F3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
plot_F3B<-plot_F3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3B))),size=5,alpha=0.8)
plot_F3B<-plot_F3B+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3B<-plot_F3B+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3B$spindle_length_poles_um~log2(means_F3B$cell_diameter_um)))$r.squared,3))),x=3,y=40,parse=TRUE)
plot_F3B
#ggsave(filename='../Manuscript/Figures/F3/F3B_with_mean_1p5_2_Rsq.pdf',useDingbats=FALSE)

plot_F3B<-ggplot()
plot_F3B<-plot_F3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_F3B<-plot_F3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
plot_F3B<-plot_F3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3B))),size=5,alpha=0.6)
plot_F3B<-plot_F3B+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3B<-plot_F3B+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3B$spindle_length_poles_um~log2(means_F3B$cell_diameter_um)))$r.squared,3))),x=3,y=40,parse=TRUE)
plot_F3B
#ggsave(filename='../Manuscript/Figures/F3/F3B_with_mean_1p5_3_Rsq.pdf',useDingbats=FALSE)

plot_F3B<-ggplot()
plot_F3B<-plot_F3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_F3B<-plot_F3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
plot_F3B<-plot_F3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3B))),size=6,alpha=0.6)
plot_F3B<-plot_F3B+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3B<-plot_F3B+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3B$spindle_length_poles_um~log2(means_F3B$cell_diameter_um)))$r.squared,3))),x=3,y=40,parse=TRUE)
plot_F3B
#ggsave(filename='../Manuscript/Figures/F3/F3B_with_mean_1p5_4_Rsq.pdf',useDingbats=FALSE)


################################################################################################
################################################################################################
#MITOTIC SPINDLES: F3C (filter by the number of cells)
################################################################################################
################################################################################################
mitotic_for_plot<-mitotic[(which(mitotic$stage=='mI' | mitotic$stage=='mII' | mitotic$stage=='m')),]

unique(mitotic[(which(mitotic$stage=='mI' | mitotic$stage=='mII' | mitotic$stage=='m')),]$stage)
unique(mitotic$stage)
unique(mitotic_for_plot$stage)
mitotic_for_plot[mitotic_for_plot$stage=='a',]

unique(mitotic_for_plot$num_of_cells_NUM)

unique(mitotic_for_plot[which(mitotic_for_plot$num_of_cells_NUM==1 | mitotic_for_plot$num_of_cells_NUM==2 | mitotic_for_plot$num_of_cells_NUM==4),]$num_of_cells_NUM)
mitotic_for_plot<-mitotic_for_plot[which(mitotic_for_plot$num_of_cells_NUM==1 | mitotic_for_plot$num_of_cells_NUM==2 | mitotic_for_plot$num_of_cells_NUM==4),]

by(mitotic_for_plot$cell_diameter_um, mitotic_for_plot$organism,mean,na.rm=TRUE)
by(mitotic_for_plot$spindle_length_poles_um, mitotic_for_plot$organism,mean,na.rm=TRUE)
names(by(mitotic_for_plot$cell_diameter_um, mitotic_for_plot$organism,mean,na.rm=TRUE))

class(cbind(by(mitotic_for_plot$cell_diameter_um, mitotic_for_plot$organism,mean,na.rm=TRUE),by(mitotic_for_plot$spindle_length_poles_um, mitotic_for_plot$organism,mean,na.rm=TRUE)))

means_F3C<-cbind(by(mitotic_for_plot$cell_diameter_um, mitotic_for_plot$organism,mean,na.rm=TRUE),by(mitotic_for_plot$spindle_length_poles_um, mitotic_for_plot$organism,mean,na.rm=TRUE))
means_F3C<-data.frame(means_F3C)
names(means_F3C)<-c("cell_diameter_um",'spindle_length_poles_um')

row.names(means_F3C)
names(means_F3C)

means_F3C$organism<-row.names(means_F3C)

#WRAP INTO A FUNCTION
create_means_DF<-function(data_frame=NULL,columns_vector){
  list_of_cols=vector('list',length = length(columns_vector))
  for (i in 1:length(columns_vector)){
    #print(grep(col_name,names(data_frame)))   
    #print(col_name)
    #print(by(data_frame[,grep(col_name,names(data_frame))], data_frame$organism,mean,na.rm=TRUE))
    list_of_cols[[i]]<-by(data_frame[,grep(columns_vector[i],names(data_frame))], data_frame$organism,mean,na.rm=TRUE)
  }
  means_df<-data.frame(do.call(cbind,list_of_cols))
  names(means_df)<-columns_vector
  return(means_df)
}
#create_means_DF(meiotic_for_plot,c("cell_diameter_um",'spindle_length_poles_um'))


max(mitotic_for_plot$cell_diameter_um,na.rm=TRUE)
max(meiotic_for_plot$cell_diameter_um,na.rm=TRUE)

plot_F3C<-ggplot()
plot_F3C<-plot_F3C+geom_point(data=mitotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_F3C<-plot_F3C+xlim(c(1,log2(1600)))+ylim(c(0,65))
#plot_F3C
plot_F3C<-plot_F3C+geom_point(data=means_F3C,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3C))),size=7,alpha=0.6)
plot_F3C<-plot_F3C+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
#plot_F3C
plot_F3C<-plot_F3C+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3C$spindle_length_poles_um~log10(means_F3C$cell_diameter_um)))$r.squared,3))),x=3,y=60,parse=TRUE)
plot_F3C
#ggsave(filename='../Manuscript/Figures/F3/F3C_with_mean_1p5_1_Rsq.pdf',useDingbats=FALSE)

plot_F3C<-ggplot()
plot_F3C<-plot_F3C+geom_point(data=mitotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5,alpha=0.7)
plot_F3C<-plot_F3C+ylim(c(0,65))+xlim(c(1,log2(1600)))
plot_F3C<-plot_F3C+geom_point(data=means_F3C,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3C))),size=5,alpha=0.8)
plot_F3C<-plot_F3C+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3C<-plot_F3C+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3C$spindle_length_poles_um~log10(means_F3C$cell_diameter_um)))$r.squared,3))),x=3,y=60,parse=TRUE)
plot_F3C
#ggsave(filename='../Manuscript/Figures/F3/F3C_with_mean_1p5_2_Rsq.pdf',useDingbats=FALSE)

plot_F3C<-ggplot()
plot_F3C<-plot_F3C+geom_point(data=mitotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_F3C<-plot_F3C+ylim(c(0,65))+xlim(c(1,log2(1600)))
plot_F3C<-plot_F3C+geom_point(data=means_F3C,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3C))),size=5,alpha=0.6)
plot_F3C<-plot_F3C+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3C<-plot_F3C+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3C$spindle_length_poles_um~log10(means_F3C$cell_diameter_um)))$r.squared,3))),x=3,y=60,parse=TRUE)
plot_F3C
#ggsave(filename='../Manuscript/Figures/F3/F3C_with_mean_1p5_3_Rsq.pdf',useDingbats=FALSE)

plot_F3C<-ggplot()
plot_F3C<-plot_F3C+geom_point(data=mitotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_F3C<-plot_F3C+ylim(c(0,65))+xlim(c(1,log2(1600)))
plot_F3C<-plot_F3C+geom_point(data=means_F3C,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(row.names(means_F3C))),size=6,alpha=0.6)
plot_F3C<-plot_F3C+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_F3C<-plot_F3C+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3C$spindle_length_poles_um~log10(means_F3C$cell_diameter_um)))$r.squared,3))),x=3,y=60,parse=TRUE)
plot_F3C
#ggsave(filename='../Manuscript/Figures/F3/F3C_with_mean_1p5_4_Rsq.pdf',useDingbats=FALSE)

summary.lm(lm(mitotic_for_plot$spindle_length_poles_um~mitotic_for_plot$cell_diameter_um))$r.squared #0.2418697
summary.lm(lm(mitotic_for_plot$spindle_length_poles_um~log2(mitotic_for_plot$cell_diameter_um)))$r.squared
summary.lm(lm(means_F3C$spindle_length_poles_um~means_F3C$cell_diameter_um))$r.squared #0.5215238
summary.lm(lm(means_F3C$spindle_length_poles_um~log2(means_F3C$cell_diameter_um)))$r.squared #0.814689
summary.lm(lm(means_F3C$spindle_length_poles_um~log10(means_F3C$cell_diameter_um)))$r.squared #0.814689

################################################################################################
################################################################################################
################################################################################################
#S3
################################################################################################
################################################################################################
################################################################################################
#S3A
################################################################################################

meiotic_for_plot$spindle_length__asters_um
max(meiotic_for_plot$spindle_length__asters_um,na.rm=TRUE)
max(meiotic_for_plot$cell_diameter_um,na.rm=TRUE)

S3A_means<-create_means_DF(meiotic_for_plot,c("cell_diameter_um","spindle_length__asters_um"))

plot_S3A<-ggplot()
plot_S3A<-plot_S3A+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length__asters_um,colour=factor(organism)),size=1.5)
plot_S3A<-plot_S3A+ylim(c(0,65))+xlim(c(1,log2(1600)))
plot_S3A<-plot_S3A+geom_point(data=S3A_means,aes(x=log2(cell_diameter_um),y=spindle_length__asters_um,colour=factor(row.names(S3A_means))),size=6,alpha=0.6)
plot_S3A<-plot_S3A+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_S3A<-plot_S3A+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(S3A_means$spindle_length__asters_um~log2(S3A_means$cell_diameter_um)))$r.squared,3))),x=3,y=60,parse=TRUE)
plot_S3A
#ggsave(filename='../Manuscript/Figures/S3/S3A_with_mean_1p5_4_Rsq.pdf',useDingbats=FALSE)

################################################################################################
#S3B
################################################################################################
max(meiotic_for_plot$spindle_length_poles_um,na.rm=TRUE)
max(meiotic_for_plot$cell_diameter_um,na.rm=TRUE)

#FIX HEADER
vec_names<-names(means_F3B)
vec_names[3]<-'organism'
names(means_F3B)<-vec_names

row.names(means_F3B)
names(meiotic_for_plot)

meiotic[which(meiotic$organism=='danio_rerio'),]
original[which(original$organism=='danio_rerio'),]
meiotic_for_plot[which(meiotic_for_plot$organism=='danio_rerio'),]


centrosome_lst<-vector('list',length(row.names(means_F3B)))
for(i in 1:length(row.names(means_F3B))){
  print(row.names(means_F3B)[i])
  print(meiotic_for_plot[which(meiotic_for_plot$organism==row.names(means_F3B)[i]),]$centrosome)
  centrosome_lst[[i]]<-unique(meiotic_for_plot[which(meiotic_for_plot$organism==row.names(means_F3B)[i]),]$centrosome)
}

centrosome <- do.call("rbind",centrosome_lst)
means_F3B = cbind(means_F3B,data.frame(centrosome = centrosome))
#unique(meiotic_for_plot[which(meiotic_for_plot$organism=='xenopus_laevis'),]$centrosome)

#FIX COLORS
my_labels2<-c(my_labels,"0","1")
new_cols2<-c("#F8766D","#00B0F6","#FF6A98","#35A2FF","#C09B00","#E76BF3","#EA8331","#7CAE00","#9590FF","#39B600","#A3A500","#00BB4E","#FF62BC","#00BAE0","#00C1A3","#FA62DB","#00BF7D","#D89000","#00BAE0","#C77CFF",'#949494','#000000')
mapping_vector3<-setNames(object=as.character(new_cols2),my_labels2)
names(mapping_vector3)
mapping_vector3
)#,labels=names(mapping_vector2)

#PLOTTING
plot_S3B<-ggplot()
plot_S3B<-plot_S3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_S3B<-plot_S3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
plot_S3B
#plot_S3B<-plot_S3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(means_F3B$organism)),size=6,alpha=0.6) #WORKS
#plot_S3B
plot_S3B<-plot_S3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(centrosome)),size=6,alpha=0.7) 
plot_S3B
plot_S3B<-plot_S3B+scale_color_manual(values =mapping_vector3)#,labels=names(mapping_vector3))
plot_S3B
plot_S3B<-plot_S3B+annotate('text',label='0: no centrosome:',x=3,y=42)
plot_S3B
plot_S3B<-plot_S3B+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3B[means_F3B$centrosome=='0',]$spindle_length_poles_um~log2(means_F3B[means_F3B$centrosome=='0',]$cell_diameter_um)))$r.squared,3))),x=3,y=40,parse=TRUE)
plot_S3B
plot_S3B<-plot_S3B+annotate('text',label='1: with centrosome:',x=3,y=38)
plot_S3B<-plot_S3B+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3B[means_F3B$centrosome=='1',]$spindle_length_poles_um~log2(means_F3B[means_F3B$centrosome=='1',]$cell_diameter_um)))$r.squared,3))),x=3,y=36,parse=TRUE)
plot_S3B
ggsave(filename='../Manuscript/Figures/S3/S3B_with_mean_1p5_col_Rsq.pdf',useDingbats=FALSE)

#V2
plot_S3B<-ggplot()
plot_S3B<-plot_S3B+geom_point(data=meiotic_for_plot,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(centrosome)),size=1.5)
plot_S3B<-plot_S3B+ylim(c(0,45))+xlim(c(1,log2(1600)))
plot_S3B
#plot_S3B<-plot_S3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(means_F3B$organism)),size=6,alpha=0.6) #WORKS
#plot_S3B
plot_S3B<-plot_S3B+geom_point(data=means_F3B,aes(x=log2(cell_diameter_um),y=spindle_length_poles_um,colour=factor(centrosome)),size=6,alpha=0.7) 
plot_S3B
plot_S3B<-plot_S3B+scale_color_manual(values =mapping_vector3)#,labels=names(mapping_vector3))
plot_S3B
plot_S3B<-plot_S3B+annotate('text',label='0: no centrosome:',x=3,y=42)
plot_S3B
plot_S3B<-plot_S3B+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3B[means_F3B$centrosome=='0',]$spindle_length_poles_um~log2(means_F3B[means_F3B$centrosome=='0',]$cell_diameter_um)))$r.squared,3))),x=3,y=40,parse=TRUE)
plot_S3B
plot_S3B<-plot_S3B+annotate('text',label='1: with centrosome:',x=3,y=38)
plot_S3B<-plot_S3B+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(means_F3B[means_F3B$centrosome=='1',]$spindle_length_poles_um~log2(means_F3B[means_F3B$centrosome=='1',]$cell_diameter_um)))$r.squared,3))),x=3,y=36,parse=TRUE)
plot_S3B
ggsave(filename='../Manuscript/Figures/S3/S3B_with_mean_1p5_BW_Rsq.pdf',useDingbats=FALSE)


summary.lm(lm(means_F3B[means_F3B$centrosome=='0',]$spindle_length_poles_um~log2(means_F3B[means_F3B$centrosome=='0',]$cell_diameter_um)))$r.squared # 0.3128755

summary.lm(lm(means_F3B[means_F3B$centrosome=='1',]$spindle_length_poles_um~log2(means_F3B[means_F3B$centrosome=='1',]$cell_diameter_um)))$r.squared # 0.4484899

summary.lm(lm(meiotic_for_plot[meiotic_for_plot$centrosome=='0',]$spindle_length_poles_um~log2(meiotic_for_plot[meiotic_for_plot$centrosome=='0',]$cell_diameter_um)))$r.squared # 0.4966456

summary.lm(lm(meiotic_for_plot[meiotic_for_plot$centrosome=='1',]$spindle_length_poles_um~log2(meiotic_for_plot[meiotic_for_plot$centrosome=='1',]$cell_diameter_um)))$r.squared # 0.002705339




means_F3B[means_F3B$centrosome=='0',]

length(means_F3B$organism)
length(means_F3B$centrosome)
means_F3B$centrosome<-as.factor(means_F3B$centrosome)
levels(means_F3B$centrosome)
length(mapping_vector2)
length(unique(original$organism))

################################################################################################
#S3C
################################################################################################
meiotic_for_plot$spindle_length_poles_um
meiotic_for_plot$polar_body_diameter_1_um
meiotic_for_plot$polar_body_diameter_2_um
grep('polar_body_diameter_1_um',names(meiotic_for_plot))
grep('polar_body_diameter_2_um',names(meiotic_for_plot))

?apply
dim(meiotic_for_plot)
length(apply(meiotic_for_plot[,c(31,32)],1,mean,na.rm=TRUE))
meiotic_for_plot$polar_body_diameter_AVG=apply(meiotic_for_plot[,c(31,32)],1,mean,na.rm=TRUE)
class(meiotic_for_plot$polar_body_diameter_AVG)


S3C_means<-create_means_DF(meiotic_for_plot,c("polar_body_diameter_AVG","spindle_length_poles_um"))

plot_S3C<-ggplot()
plot_S3C<-plot_S3C+geom_point(data=meiotic_for_plot,aes(x=polar_body_diameter_AVG,y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_S3C
plot_S3C<-plot_S3C+ylim(c(0,45))+xlim(c(0,25))
plot_S3C
plot_S3C<-plot_S3C+geom_point(data=S3C_means,aes(x=polar_body_diameter_AVG,y=spindle_length_poles_um,colour=factor(row.names(S3C_means))),size=6,alpha=0.6)
plot_S3C<-plot_S3C+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_S3C
plot_S3C<-plot_S3C+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(S3C_means$spindle_length_poles_um~S3C_means$polar_body_diameter_AVG))$r.squared,3))),x=20,y=40,parse=TRUE)
plot_S3C
#ggsave(filename='../Manuscript/Figures/S3/S3C_with_mean_1p5_4_Rsq.pdf',useDingbats=FALSE)

################################################################################################
#S3D
################################################################################################
meiotic_for_plot$spindle_length_poles_um
meiotic_for_plot$genome
S3D_means<-create_means_DF(meiotic_for_plot,c("spindle_length_poles_um",'genome'))

plot_S3D<-ggplot()
plot_S3D<-plot_S3D+geom_point(data=meiotic_for_plot,aes(x=genome,y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_S3D
plot_S3D<-plot_S3D+ylim(c(0,45))+xlim(c(0,4000))
plot_S3D
plot_S3D<-plot_S3D+geom_point(data=S3D_means,aes(x=genome,y=spindle_length_poles_um,colour=factor(row.names(S3D_means))),size=6,alpha=0.6)
plot_S3D<-plot_S3D+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_S3D
plot_S3D<-plot_S3D+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(S3D_means$spindle_length_poles_um~S3D_means$genome))$r.squared,3))),x=2000,y=42,parse=TRUE)
plot_S3D
ggsave(filename='../Manuscript/Figures/S3/S3D_with_mean_1p5_4_Rsq.pdf',useDingbats=FALSE)

################################################################################################
#S3E
################################################################################################
meiotic_for_plot$spindle_length_poles_um
meiotic_for_plot$chromosomes
S3D_means<-create_means_DF(meiotic_for_plot,c("spindle_length_poles_um",'genome'))
class(meiotic_for_plot$chromosomes)
class(meiotic_for_plot$genome)
S3E_means<-create_means_DF(meiotic_for_plot,c("spindle_length_poles_um",'chromosomes'))
grep('chromosomes',names(meiotic_for_plot))
names(meiotic_for_plot)[5]
names(meiotic_for_plot)[11]

by(meiotic_for_plot$spindle_length_poles_um, meiotic_for_plot$organism,mean,na.rm=TRUE)
by(meiotic_for_plot$chromosomes, meiotic_for_plot$organism,mean,na.rm=TRUE)
names(by(meiotic_for_plot$chromosomes, meiotic_for_plot$organism,mean,na.rm=TRUE))

S3E_means<-cbind(by(meiotic_for_plot$spindle_length_poles_um, meiotic_for_plot$organism,mean,na.rm=TRUE),by(meiotic_for_plot$chromosomes, meiotic_for_plot$organism,mean,na.rm=TRUE))
S3E_means<-data.frame(S3E_means)
names(S3E_means)<-c("spindle_length_poles_um","chromosomes")

plot_S3E<-ggplot()
plot_S3E<-plot_S3E+geom_point(data=meiotic_for_plot,aes(x=chromosomes,y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_S3E
plot_S3E<-plot_S3E+ylim(c(0,45))+xlim(c(0,70))
plot_S3E
plot_S3E<-plot_S3E+geom_point(data=S3E_means,aes(x=chromosomes,y=spindle_length_poles_um,colour=factor(row.names(S3E_means))),size=6,alpha=0.6)
plot_S3E<-plot_S3E+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_S3E
plot_S3E<-plot_S3E+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(S3E_means$spindle_length_poles_um~S3E_means$chromosomes))$r.squared,3))),x=50,y=42,parse=TRUE)
plot_S3E
ggsave(filename='../Manuscript/Figures/S3/S3E_with_mean_1p5_4_Rsq.pdf',useDingbats=FALSE)

################################################################################################
#S3F
################################################################################################

meiotic_for_plot$spindle_length_poles_um
meiotic_for_plot$chromosomes
meiotic_for_plot$genome_over_chr_no=meiotic_for_plot$genome/meiotic_for_plot$chromosomes

by(meiotic_for_plot$spindle_length_poles_um, meiotic_for_plot$organism,mean,na.rm=TRUE)
by(meiotic_for_plot$chromosomes, meiotic_for_plot$organism,mean,na.rm=TRUE)
by(meiotic_for_plot$genome, meiotic_for_plot$organism,mean,na.rm=TRUE)
by(meiotic_for_plot$genome_over_chr_no,meiotic_for_plot$organism,mean,na.rm=TRUE)

S3F_means<-cbind(by(meiotic_for_plot$spindle_length_poles_um, meiotic_for_plot$organism,mean,na.rm=TRUE),by(meiotic_for_plot$chromosomes, meiotic_for_plot$organism,mean,na.rm=TRUE),by(meiotic_for_plot$genome, meiotic_for_plot$organism,mean,na.rm=TRUE),by(meiotic_for_plot$genome_over_chr_no,meiotic_for_plot$organism,mean,na.rm=TRUE))
S3F_means<-data.frame(S3F_means)
names(S3F_means)<-c("spindle_length_poles_um","chromosomes",'genome','genome_over_chr_no')

plot_S3F<-ggplot()
plot_S3F<-plot_S3F+geom_point(data=meiotic_for_plot,aes(x=genome_over_chr_no,y=spindle_length_poles_um,colour=factor(organism)),size=1.5)
plot_S3F
plot_S3F<-plot_S3F+ylim(c(0,45))+xlim(c(0,210))
plot_S3F
plot_S3F<-plot_S3F+geom_point(data=S3F_means,aes(x=genome_over_chr_no,y=spindle_length_poles_um,colour=factor(row.names(S3F_means))),size=6,alpha=0.6)
plot_S3F<-plot_S3F+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_S3F
plot_S3F<-plot_S3F+annotate('text',label=paste("R^2==",(signif(summary.lm(lm(S3F_means$spindle_length_poles_um~S3F_means$genome_over_chr_no))$r.squared,3))),x=150,y=42,parse=TRUE)
plot_S3F
ggsave(filename='../Manuscript/Figures/S3/S3F_with_mean_1p5_4_Rsq.pdf',useDingbats=FALSE)
