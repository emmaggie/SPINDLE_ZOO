library(ggplot2)

setwd('/Volumes/Magdalena_NEW1/MARINA_PAPER/SEPTEMBER_2014')
dir()

original<-read.csv('original.csv')
original[1,]
spindle_length_width<-ggplot()
spindle_length_width+geom_point(data=original,aes(original$spindle_width_um,original$spindle_length_poles_um,size=original$cell_diameter_um,colour=original$chromosomes),alpha=0.3)
ggsave(filename = 'spindle_size_pole_vs_spindle_width_cell_size_chromosomes_all_data.pdf',width=11, height=8)

spindle_length_width<-ggplot()
spindle_length_width+geom_point(data=original,aes(original$spindle_width_um,original$spindle_length_poles_um,colour=original$chromosomes),alpha=0.3)
ggsave(filename = 'spindle_size_pole_vs_spindle_width_chromosomes_all_data.pdf',width=11, height=8)


spindle_length_width<-ggplot()
spindle_length_width+geom_point(data=original,aes(original$spindle_width_um,original$spindle_length_poles_um,size=original$cell_diameter_um,colour=original$genome),alpha=0.3)
ggsave(filename = 'spindle_size_pole_vs_spindle_width_cell_size_genome_all_data.pdf',width=11, height=8)

spindle_length_width<-ggplot()
spindle_length_width+geom_point(data=original,aes(original$spindle_width_um,original$spindle_length_poles_um,colour=original$genome),alpha=0.3)
ggsave(filename = 'spindle_size_pole_vs_spindle_width_genome_all_data.pdf',width=11, height=8)


