###############################################################################################
##INNER ASTER DIAMETER AS A FUNCTION OF CELL SIZE
##final data prep in: A_Marina_spindle_zoo_part4.ipynb
###############################################################################################

library(ggplot2)

#setwd('/Volumes/Magdalena_NEW1/MARINA_PAPER/SEPTEMBER_2014')

setwd('/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo Data/R_and_Plots/')

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

inner_diam_mit_met=read.csv('../SEPTEMBER_2014/inner_aster_diam_cell_sizeDF_mit_met_Jan2015.csv')
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
#see file: A_Marina_spindle_zoo_part3.ipynb: use it to add means column to plotting data frame
head(inner_diam_mit_met)
inner_diam_mit_met_copy=inner_diam_mit_met[which(!inner_diam_mit_met$inner_aster_diam_AVG>50),]

#1
plot_Jan2015<-ggplot()
plot_Jan2015<-plot_Jan2015+geom_point(data=inner_diam_mit_met_copy,aes(x=inner_diam_mit_met_copy$cell_diameter_um,y=inner_diam_mit_met_copy$inner_aster_diam_AVG,colour=factor(inner_diam_mit_met_copy$organism)),size=1.5)
plot_Jan2015<-plot_Jan2015+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_Jan2015#+ylim(c(0,75))+xlim(c(0,500))
#ggsave(filename='../Manuscript/Figures/inner_diam_scatter_1p5.pdf',useDingbats=FALSE)

#2
inner_diam_mit_met_copy=inner_diam_mit_met[which(!inner_diam_mit_met$inner_aster_diam_AVG>50),]
plot_Jan2015<-ggplot()
plot_Jan2015<-plot_Jan2015+geom_point(data=inner_diam_mit_met_copy,aes(x=inner_diam_mit_met_copy$cell_diameter_um,y=inner_diam_mit_met_copy$inner_aster_diam_AVG,colour=factor(inner_diam_mit_met_copy$organism)),size=1.5)
plot_Jan2015<-plot_Jan2015+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_Jan2015+xlim(c(0,500))# +ylim(c(0,75))+xlim(c(0,500))
#ggsave(filename='../Manuscript/Figures/inner_diam_scatter_1p5_to500.pdf',useDingbats=FALSE)


#3
max(inner_diam_mit_met_copy[which(inner_diam_mit_met_copy$organism=='Strongylocentrotus_purpuratus'),]$inner_aster_diam_AVG)#[which(inner_diam_mit_met_copy$inner_aster_diam_AVG>20),]
#unique(inner_diam_mit_met_copy$organism)

max(inner_diam_mit_met_copy[which(inner_diam_mit_met_copy$organism=='Strongylocentrotus_purpuratus'),]$inner_aster_diam_AVG)#[which(inner_diam_mit_met_copy$inner_aster_diam_AVG>20),]

inner_diam_mit_met_copy[inner_diam_mit_met_copy$inner_aster_diam_AVG==max(inner_diam_mit_met_copy[which(inner_diam_mit_met_copy$organism=='Strongylocentrotus_purpuratus'),]$inner_aster_diam_AVG),]

inner_diam_mit_met_copy2=inner_diam_mit_met_copy[inner_diam_mit_met_copy$inner_aster_diam_AVG!=max(inner_diam_mit_met_copy[which(inner_diam_mit_met_copy$organism=='Strongylocentrotus_purpuratus'),]$inner_aster_diam_AVG),]

plot_Jan2015<-ggplot()
plot_Jan2015<-plot_Jan2015+geom_point(data=inner_diam_mit_met_copy2,aes(x=inner_diam_mit_met_copy2$cell_diameter_um,y=inner_diam_mit_met_copy2$inner_aster_diam_AVG,colour=factor(inner_diam_mit_met_copy2$organism)),size=1.5)
plot_Jan2015<-plot_Jan2015+scale_color_manual(values = mapping_vector2)#,labels=names(mapping_vector2))
plot_Jan2015+xlim(c(0,500))# +ylim(c(0,75))+xlim(c(0,500))
#ggsave(filename='../Manuscript/Figures/inner_diam_scatter_1p5_to500.pdf',useDingbats=FALSE)


