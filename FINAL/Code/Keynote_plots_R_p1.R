library(ggplot2) 

setwd('/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo\ Data/R_and_Plots')
dir()
list.files()
original<-read.csv('../SEPTEMBER_2014/original.csv')
original[1,]
################################################################################################
#MAPPING COLORS
################################################################################################

original$organism<-as.character(original$organism)
str(original$organism)
#customizing color scale
unique(original$organism_CAT) 
length(unique(original$organism_CAT)) #how many categories

#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

gg_color_hue <- function(n) {
  #function to generate colors 
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

categories=unique(original$organism_CAT)
n=length(unique(original$organism_CAT))
cols=gg_color_hue(n) #these are the colors, change assignment here 
#check the colors out:
plot(1:n, pch=16, cex=2, col=cols)
cols

labels=unique(original$organism)
mapping_data_frame=as.data.frame(cbind(cols,as.integer(categories),as.character(labels)))
str(original$organism_CAT)
str(mapping_data_frame)
names(mapping_data_frame)<-c('cols','organism_CAT','organism')
str(mapping_data_frame)
mapping_data_frame$organism_CAT<-as.character(mapping_data_frame$organism_CAT)
mapping_data_frame$organism<-as.character(mapping_data_frame$organism)
mapping_data_frame$cols<-as.character(mapping_data_frame$cols)
str(mapping_data_frame) #converted to character vectors

mapping_vector<-as.character(mapping_data_frame$cols)
names(mapping_vector)<-mapping_data_frame$organism_CAT
mapping_vector #use mapping vector as 'values'
names(mapping_vector)
mapping_vector_labs=setNames(as.character(mapping_data_frame$organism),mapping_data_frame$organism_CAT)
plot(1:20, pch=16, cex=2, col=mapping_vector)
mapping_vector_labs
#MODIFIED
new_cols<-c("#F8766D","#00B0F6","#FF6A98","#35A2FF","#C09B00","#E76BF3","#EA8331","#7CAE00","#9590FF","#39B600","#A3A500","#00BB4E","#FF62BC","#00BAE0","#00C1A3","#FA62DB","#00BF7D","#D89000","#00BAE0","#C77CFF")
mapping_vector2<-setNames(as.character(new_cols),mapping_data_frame$organism_CAT)
mapping_vector2
plot(1:20, pch=16, cex=2, col=mapping_vector2)

################################################################################################


#1
#mitosis: 
#cell diameter vs. mitotic spindle length (aster-to-aster) [all]
#spindle length(aster-to-aster) as a function of cell diameter

original$meiotic[1:5]
str(original$meiotic)
length(original$meiotic[original$meiotic==0]) #catergory of interest: 1456 rows
#substet the data frame
mitotic_df<-original[original$meiotic==0,]
str(mitotic_df)
str(mitotic_df$organism)
str(mitotic_df$organism_CAT)
#variables of interest: spindle_length__asters_um & cell_diameter_um

mitotic_df$alpha<-ifelse(mitotic_df$cell_diameter_um<=300,0.3,1)
#alpha=ifelse(mitotic_df$cell_diameter_um<=300,0.3,1)
plot_1<-ggplot()
plot_1<-plot_1+geom_point(data=mitotic_df,aes(x=mitotic_df$cell_diameter_um,y=mitotic_df$spindle_length__asters_um,colour=factor(mitotic_df$organism_CAT)),alpha=0.5)
plot_1+scale_colour_manual(values = mapping_vector2,labels=mapping_vector_labs)+labs(title='spindle length as a func of cell diameter - by organism')
#ggsave(filename = 'Y_spindle_length_AA_X_cell_diameter_stretched.pdf',width=11, height=8)


plot_1<-ggplot()
plot_1<-plot_1+geom_point(data=mitotic_df,aes(x=mitotic_df$cell_diameter_um,y=mitotic_df$spindle_length__asters_um,colour=factor(mitotic_df$organism_CAT)),alpha=0.5)
plot_1+scale_colour_manual(values = mapping_vector2,labels=mapping_vector_labs)+coord_fixed()+ylim(c(0,300))+labs(title='spindle length as a func of cell diameter - by organism')
#ggsave(filename = 'Y_spindle_length_AA_X_cell_diameter_scaled.pdf',width=11, height=8)


#alpha_scaling<-function(x){
#  ifelse()
#}

#ANAPHASE vs METAPHASE
plot_1<-ggplot()
plot_1<-plot_1+geom_point(data=mitotic_df,aes(x=mitotic_df$cell_diameter_um,y=mitotic_df$spindle_length__asters_um,colour=factor(mitotic_df$stage)),alpha=0.4)
plot_1+labs(title='spindle length as a func of cell diameter - by cell cycle stage')
#ggsave(filename = 'Y_spindle_length_AA_X_cell_diameter_cell_cycle_stretched.pdf',width=11, height=8)

################################################################################################
#2 mitosis pole to pole:
################################################################################################
plot_1<-ggplot()
plot_1<-plot_1+geom_point(data=mitotic_df,aes(x=mitotic_df$cell_diameter_um,y=mitotic_df$spindle_length_poles_um,colour=factor(mitotic_df$organism_CAT)),alpha=0.5)
plot_1+scale_colour_manual(values = mapping_vector2,labels=mapping_vector_labs)+labs(title='spindle length as a func of cell diameter - by organism')
#ggsave(filename = 'Y_spindle_length_PP_X_cell_diameter_stretched.pdf',width=11, height=8)


plot_1<-ggplot()
plot_1<-plot_1+geom_point(data=mitotic_df,aes(x=mitotic_df$cell_diameter_um,y=mitotic_df$spindle_length__asters_um,colour=factor(mitotic_df$organism_CAT)),alpha=0.5)
plot_1+scale_colour_manual(values = mapping_vector2,labels=mapping_vector_labs)+coord_fixed()+ylim(c(0,300))+labs(title='spindle length as a func of cell diameter - by organism')
#ggsave(filename = 'Y_spindle_length_AA_X_cell_diameter_scaled.pdf',width=11, height=8)


#alpha_scaling<-function(x){
#  ifelse()
#}

#ANAPHASE vs METAPHASE
plot_1<-ggplot()
plot_1<-plot_1+geom_point(data=mitotic_df,aes(x=mitotic_df$cell_diameter_um,y=mitotic_df$spindle_length__asters_um,colour=factor(mitotic_df$stage)),alpha=0.4)
plot_1+labs(title='spindle length as a func of cell diameter - by cell cycle stage')
#ggsave(filename = 'Y_spindle_length_AA_X_cell_diameter_cell_cycle_stretched.pdf',width=11, height=8)


