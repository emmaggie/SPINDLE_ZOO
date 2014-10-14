#RESOURCES:
#https://github.com/ckrogs/r_graphics_dlab/blob/master/r_graphics.md
library(ggplot2)

#library(RColorBrewer)
#display.brewer.all() #not enough levels

#INITIAL 
setwd('/Volumes/Magdalena_NEW1/MARINA_PAPER/SEPTEMBER_2014')
dir()

#################################################################################################################################
#ALL DATA
#################################
################################################################################################

original<-read.csv('original.csv')

#have a look
original[1,]
names(original)
str(original)

#plot 1: overview
asters_no_asters<-ggplot()
asters_no_asters+geom_point(data=original,aes(original$spindle_length__asters_um,original$spindle_length_poles_um,size=original$cell_diameter_um,colour=factor(original$organism_CAT)),alpha=0.3)
#################################
#RE-MAPPING COLORS
#this assignment will be used throughout the plotting so change it here for better contrast
#check colors for assignment here: http://html-color-codes.info/
#create named vector as input to scale_colour_manual : http://docs.ggplot2.org/0.9.3.1/scale_manual.html
#"integer"="color"
#################################

original$organism
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






merged_original<-merge(original,mapping_data_frame, by='organism_CAT')
merged_original[5:10,]
#This is but still not what I want
###########################################################
#SOLUTION 3:
###########################################################
#create named vector as input to scale_colour_manual : http://docs.ggplot2.org/0.9.3.1/scale_manual.html
#"integer"="color"
###########################################################

merged_original$organism_CAT<-as.character(merged_original$organism_CAT)

mapping_vector<-as.character(mapping_data_frame$cols)
names(mapping_vector)<-mapping_data_frame$organism_CAT
mapping_vector #use mapping vector as 'values'
names(mapping_vector)
mapping_vector_labs=setNames(as.character(mapping_data_frame$labels),mapping_data_frame$organism_CAT)
plot(1:20, pch=16, cex=2, col=mapping_vector)
mapping_vector_labs

###########################################################
#re-mapping colors: 
#see: file: mapping_colors.txt
###########################################################
mapping_data_frame$organism_CAT #names
new_cols<-c("#F8766D","#00B0F6","#FF6A98","#35A2FF","#C09B00","#E76BF3","#EA8331","#7CAE00","#9590FF","#39B600","#A3A500","#00BB4E","#FF62BC","#00BAE0","#00C1A3","#FA62DB","#00BF7D","#D89000","#00BAE0","#C77CFF")
mapping_vector2<-setNames(as.character(new_cols),mapping_data_frame$organism_CAT)
mapping_vector2

asters_no_asters<-ggplot()
asters_no_asters+geom_point(data=merged_original,aes(merged_original$spindle_length__asters_um,merged_original$spindle_length_poles_um,size=merged_original$cell_diameter_um,colour=merged_original$cols),alpha=0.3)#+scale_colour_manual(values=merged_original$cols)
#,colour=factor(original$organism_CAT)
asters_no_asters<-ggplot()
asters_no_asters+geom_point(data=merged_original,aes(merged_original$spindle_length__asters_um,merged_original$spindle_length_poles_um,size=merged_original$cell_diameter_um,colour=factor(merged_original$organism_CAT)),alpha=0.3)+scale_colour_manual(values=mapping_vector,labels=mapping_vector_labs)+xlim(c(0,200))+ylim(c(0,200))

#USE THIS ONE (SOLUTION 3)
asters_no_asters<-ggplot()
asters_no_asters+geom_point(data=merged_original,aes(merged_original$spindle_length__asters_um,merged_original$spindle_length_poles_um,size=merged_original$cell_diameter_um,colour=factor(merged_original$organism_CAT)),alpha=0.3)+scale_colour_manual(values=mapping_vector2,labels=mapping_vector_labs)+xlim(c(0,200))+ylim(c(0,200))+labs(title='all_data')
ggsave(filename = 'spindle_size_poles_vs_aster_sall_data_v2.pdf',width=11, height=8)


asters_no_asters_2<-ggplot()
asters_no_asters_2+geom_point(data=original,aes(original$spindle_length__asters_um,original$spindle_length_poles_um,size=original$cell_diameter_um,colour=factor(original$organism_CAT)),alpha=0.3)+xlim(c(0,150))+ylim(c(0,150))
#original$organism
asters_no_asters_2<-ggplot()
asters_no_asters_2+geom_point(data=merged_original,aes(merged_original$spindle_length__asters_um,merged_original$spindle_length_poles_um,size=merged_original$cell_diameter_um,colour=merged_original$cols),alpha=0.3)+xlim(c(0,150))+ylim(c(0,150))#+scale_colour_manual(values
ggsave(filename = 'spindle_size_poles_to_150_vs_asters_to_150.pdf')

asters_no_asters_3<-ggplot()
asters_no_asters_3+geom_point(data=original,aes(original$spindle_length__asters_um,original$spindle_length_poles_um,size=original$cell_diameter_um,colour=factor(original$organism_CAT)),alpha=0.5)+xlim(c(0,150))+ylim(c(0,100))
#original$organism
asters_no_asters_3<-ggplot()
asters_no_asters_3+geom_point(data=merged_original,aes(merged_original$spindle_length__asters_um,merged_original$spindle_length_poles_um,size=merged_original$cell_diameter_um,colour=merged_original$cols),alpha=0.3)+xlim(c(0,150))+ylim(c(0,100))+labs(title='all_spindles')
ggsave(filename = 'spindle_size_poles_to_100_vs_asters_to_150.pdf')
#################################################################################################################################
#MITOTIC ONLY METAPHASE ONLY
#################################################################################################################################


dir()
mitotic_metaphase<-read.csv('mitotic_metaphase.csv')



#USE THIS ONE (SOLUTION3):
asters_no_asters_mm<-ggplot()
asters_no_asters_mm+geom_point(data=mitotic_metaphase,aes(mitotic_metaphase$spindle_length__asters_um,mitotic_metaphase$spindle_length_poles_um,size=mitotic_metaphase$cell_diameter_um,colour=factor(mitotic_metaphase$organism_CAT)),alpha=0.3)+scale_colour_manual(values=mapping_vector2,labels=mapping_vector_labs)+xlim(c(0,200))+ylim(c(0,200))+labs(title='mitotic_metaphase')
ggsave(filename = 'spindle_size_poles_to_150_vs_asters_to_150_mitotic_metaphase_v2.pdf',width=11, height=8)



##RE-MAP COLORS:
merged_mitotic_metaphase<-merge(mitotic_metaphase,mapping_data_frame, by='organism_CAT')
#http://html-color-codes.info/

merged_mitotic_metaphase$cols[1:4]
str(merged_mitotic_metaphase$cols) #WRONG! it is a factor!
merged_mitotic_metaphase$cols<-as.character(merged_mitotic_metaphase$cols)
str(merged_mitotic_metaphase$cols) #now a character vector

plot(1:11,pch = 16,col=unique(merged_mitotic_metaphase$cols))
length(unique(merged_mitotic_metaphase$cols))
str(unique(merged_mitotic_metaphase$cols))
merged_mitotic_metaphase$cols
asters_no_asters_mm2<-ggplot()


asters_no_asters_mm2+geom_point(data=merged_mitotic_metaphase,aes(x=merged_mitotic_metaphase$spindle_length__asters_um,y=merged_mitotic_metaphase$spindle_length_poles_um,size=merged_mitotic_metaphase$cell_diameter_um, colour=as.character(merged_mitotic_metaphase$cols)),alpha=0.5)+xlim(c(0,150))+ylim(c(0,100))+labs(title='mitotic_metaphase_new')+scale_colour_manual(values=merged_mitotic_metaphase$cols) #still doesn't work


ggsave(filename = 'spindle_size_poles_to_150_vs_asters_to_150_mitotic_metaphase_v2.pdf')

#SOLUTION 3:
asters_no_asters_mm<-ggplot()
asters_no_asters_mm+geom_point(data=merged_mitotic_metaphase,aes(merged_mitotic_metaphase$spindle_length__asters_um,merged_mitotic_metaphase$spindle_length_poles_um,size=merged_mitotic_metaphase$cell_diameter_um,colour=factor(merged_mitotic_metaphase$organism_CAT)),alpha=0.3)+scale_colour_manual(values=mapping_vector,labels=mapping_vector_labs)+xlim(c(0,150))+ylim(c(0,100))+labs(title='mitotic_metaphase_new') #works



#MITOTIC ONLY ANAPHASE ONLY
mitotic_anaphase<-read.csv('mitotic_anaphase.csv')
asters_no_asters_ma<-ggplot()
asters_no_asters_ma+geom_point(data=mitotic_anaphase,aes(mitotic_anaphase$spindle_length__asters_um,mitotic_anaphase$spindle_length_poles_um,size=mitotic_anaphase$cell_diameter_um,colour=factor(mitotic_anaphase$organism_CAT)),alpha=0.5)+xlim(c(0,150))+ylim(c(0,100))+labs(title='mitotic_anaphase')
ggsave(filename = 'spindle_size_poles_to_150_vs_asters_to_150_mitotic_anaphase.pdf')

#USE THIS ONE (SOLUTION3):
asters_no_asters_ma<-ggplot()
asters_no_asters_ma+geom_point(data=mitotic_anaphase,aes(mitotic_anaphase$spindle_length__asters_um,mitotic_anaphase$spindle_length_poles_um,size=mitotic_anaphase$cell_diameter_um,colour=factor(mitotic_anaphase$organism_CAT)),alpha=0.3)+scale_colour_manual(values=mapping_vector2,labels=mapping_vector_labs)+xlim(c(0,200))+ylim(c(0,200))+labs(title='mitotic_anaphase')
ggsave(filename = 'spindle_size_poles_to_150_vs_asters_to_150_mitotic_anaphase_v2.pdf',width=11, height=8)



#SOLUTION 3
asters_no_asters_ma<-ggplot()
asters_no_asters_ma+geom_point(data=mitotic_anaphase,aes(mitotic_anaphase$spindle_length__asters_um,mitotic_anaphase$spindle_length_poles_um,size=mitotic_anaphase$cell_diameter_um,colour=factor(mitotic_anaphase$organism_CAT)),alpha=0.7)+xlim(c(0,150))+ylim(c(0,100))+labs(title='mitotic_anaphase')+scale_colour_manual(values=mapping_vector,labels=mapping_vector_labs)




#MEIOTIC ONLY METAPHASE
#MEIOTIC ONLY ANAPHASE




