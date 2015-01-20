##### ANAPHASE
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
mitotic_anaphase<-read.csv('../SEPTEMBER_2014/mitotic_anaphase.csv')
mitotic_anaphase$organism<-as.character(mitotic_anaphase$organism)

head(mitotic_anaphase)
original<-read.csv('../SEPTEMBER_2014/original.csv')


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


plot_S1C<-ggplot()
plot_S1C<-plot_S1C+geom_point(data=mitotic_anaphase,aes(x=mitotic_anaphase$cell_diameter_um,y=mitotic_anaphase$spindle_length_poles_um,colour=factor(mitotic_anaphase$organism)),size=1.5)
plot_S1C<-plot_S1C+scale_color_manual(values = mapping_vector2,labels=names(mapping_vector2))
plot_S1C#+ylim(c(0,75))+xlim(c(0,500))

#there is something wrong with the max spindle pole length: FIX IT
#clean the problematic value (max spindle pole length)
ggplot()+geom_histogram(aes(x=mitotic_anaphase[which(mitotic_anaphase$organism_CAT=='6'),]$spindle_length_poles_um),binwidth=1) #clearly the 231 is an outlier
ggplot()+geom_histogram(aes(x=mitotic_anaphase[which(mitotic_anaphase$organism_CAT=='6'),]$cell_diameter_um),binwidth=1)

mitotic_anaphase[which(mitotic_anaphase$spindle_length_poles_um==max(mitotic_anaphase$spindle_length_poles_um,na.rm=TRUE)),]
mitotic_anaphase[which(mitotic_anaphase$spindle_length_poles_um==max(mitotic_anaphase$spindle_length_poles_um,na.rm=TRUE)),]$spindle_length_poles_um #231
which(mitotic_anaphase$spindle_length_poles_um==max(mitotic_anaphase$spindle_length_poles_um,na.rm=TRUE)) #row 272
mitotic_anaphase[272,]$spindle_length_poles_um<-mitotic_anaphase[272,]$spindle_length_poles_um/10

mitotic_anaphase[which(mitotic_anaphase$spindle_length_poles_um==max(mitotic_anaphase$spindle_length_poles_um,na.rm=TRUE)),]$cell_diameter_um

ggplot()+geom_histogram(aes(x=mitotic_anaphase[which(mitotic_anaphase$organism_CAT=='6'),]$spindle_length_poles_um),binwidth=1) #clearly the 231 is an outlier
ggplot()+geom_histogram(aes(x=mitotic_anaphase[which(mitotic_anaphase$organism_CAT=='6'),]$cell_diameter_um),binwidth=1)

#there is something wrong with the minimal cell diamter: FIX IT
ggplot()+geom_histogram(aes(x=mitotic_anaphase$cell_diameter_um),binwidth=10)
mitotic_anaphase[which(mitotic_anaphase$cell_diameter_um==min(mitotic_anaphase$cell_diameter_um,na.rm=TRUE)),]
ggplot()+geom_histogram(aes(x=mitotic_anaphase[which(mitotic_anaphase$organism_CAT=='3'),]$cell_diameter_um),binwidth=2)

#not sure where it came from: removing it
mitotic_anaphase<-mitotic_anaphase[which(mitotic_anaphase$cell_diameter_um!=min(mitotic_anaphase$cell_diameter_um,na.rm=TRUE)),]


plot_S1C<-ggplot()
plot_S1C<-plot_S1C+geom_point(data=mitotic_anaphase,aes(x=mitotic_anaphase$cell_diameter_um,y=mitotic_anaphase$spindle_length_poles_um,colour=factor(mitotic_anaphase$organism)),size=1.5)
plot_S1C<-plot_S1C+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_S1C#+ylim(c(0,75))+xlim(c(0,500))
##ggsave(filename='../Manuscript/Figures/S1/S1C_scatter_ALL_1p5.pdf',useDingbats=FALSE)

ma_no_Hs<-mitotic_anaphase[which(mitotic_anaphase$organism!="Homo_sapiens"),]
plotS1C_LOESS<-ggplot(data=ma_no_Hs,aes(x=ma_no_Hs$cell_diameter_um,y=ma_no_Hs$spindle_length_poles_um,colour=factor(ma_no_Hs$organism)))
plotS1C_LOESS<-plotS1C_LOESS+geom_point(alpha=0.5)
#plotS1D_LOESS<-plotS1D_LOESS+scale_colour_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plotS1C_LOESS<-plotS1C_LOESS+geom_smooth(na.rm=TRUE,alpha=0.2)
plotS1C_LOESS<-plotS1C_LOESS+stat_smooth(aes(fill=factor(ma_no_Hs$organism)))
plotS1C_LOESS<-plotS1C_LOESS+scale_colour_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plotS1C_LOESS<-plotS1C_LOESS+scale_fill_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plotS1C_LOESS
#ggsave(filename='../Manuscript/Figures/S1/S1C_LOESS_regressed.pdf',useDingbats=FALSE)




plotS1D<-ggplot()
plotS1D<-plotS1D+geom_point(data=mitotic_anaphase,aes(x=mitotic_anaphase$cell_diameter_um,y=mitotic_anaphase$spindle_length__asters_um,colour=factor(mitotic_anaphase$organism)),size=1.5)
plotS1D<-plotS1D+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plotS1D#+ylim(c(0,75))+xlim(c(0,500))
##ggsave(filename='../Manuscript/Figures/S1/S1D_scatter_ALL_1p5.pdf',useDingbats=FALSE)

mitotic_anaphase[which(mitotic_anaphase$organism!="Homo_sapiens"),]
?which

ma_no_Hs<-mitotic_anaphase[which(mitotic_anaphase$organism!="Homo_sapiens"),]
#rm(list=ls())
plotS1D_LOESS<-ggplot(data=ma_no_Hs,aes(x=ma_no_Hs$cell_diameter_um,y=ma_no_Hs$spindle_length__asters_um,colour=factor(ma_no_Hs$organism)))
plotS1D_LOESS<-plotS1D_LOESS+geom_point(alpha=0.5)
#plotS1D_LOESS<-plotS1D_LOESS+scale_colour_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plotS1D_LOESS<-plotS1D_LOESS+geom_smooth(na.rm=TRUE,alpha=0.2)
plotS1D_LOESS<-plotS1D_LOESS+stat_smooth(aes(fill=factor(ma_no_Hs$organism)))
plotS1D_LOESS<-plotS1D_LOESS+scale_colour_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plotS1D_LOESS<-plotS1D_LOESS+scale_fill_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plotS1D_LOESS
#ggsave(filename='../Manuscript/Figures/S1/S1D_LOESS_regressed.pdf',useDingbats=FALSE)





