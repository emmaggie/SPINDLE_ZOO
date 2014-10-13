#RESOURCES:
#https://github.com/ckrogs/r_graphics_dlab/blob/master/r_graphics.md
library(ggplot2)

setwd('/Volumes/Magdalena_NEW1/MARINA_PAPER/SEPTEMBER_2014')
dir()
original<-read.csv('original.csv')

#have a look
original[1,]
names(original)
str(original)

asters_no_asters<-ggplot()
asters_no_asters+geom_point(data=original,aes(original$spindle_length__asters_um,original$spindle_length_poles_um,size=original$cell_diameter_um,colour=factor(original$organism_CAT)),alpha=0.3)
original$organism
ggsave(filename = 'spindle_size_poles_vs_asters.pdf')

asters_no_asters_2<-ggplot()
asters_no_asters_2+geom_point(data=original,aes(original$spindle_length__asters_um,original$spindle_length_poles_um,size=original$cell_diameter_um,colour=factor(original$organism_CAT)),alpha=0.3)+xlim(c(0,150))+ylim(c(0,150))
#original$organism
ggsave(filename = 'spindle_size_poles_to_150_vs_asters_to_150.pdf')

asters_no_asters_3<-ggplot()
asters_no_asters_3+geom_point(data=original,aes(original$spindle_length__asters_um,original$spindle_length_poles_um,size=original$cell_diameter_um,colour=factor(original$organism_CAT)),alpha=0.5)+xlim(c(0,150))+ylim(c(0,100))
#original$organism
ggsave(filename = 'spindle_size_poles_to_100_vs_asters_to_150.pdf')



