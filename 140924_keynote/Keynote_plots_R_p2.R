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
mitotic_metaphase<-read.csv('../SEPTEMBER_2014/mitotic_metaphase.csv')
smooth_data<-read.csv('../SEPTEMBER_2014/MitoticScaling_Binnedmeans_SD_reformatted.csv')
str(mitotic_metaphase)
smooth_data$organism<-gsub('marginalus', 'marginatus',as.character(smooth_data$organism))

original<-read.csv('../SEPTEMBER_2014/original.csv')
original[1,]
################################################################################################
################################################################################################
#MAPPING COLORS
################################################################################################
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
plot(1:20, pch=16, cex=2, col=mapping_vector) #saved as  "/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo\ Data/SEPTEMBER_2014/Rplot_original_color_scheme.pdf"
text(1:20,mapping_data_frame$organism,cex=0.5) #saved as "/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo\ Data/SEPTEMBER_2014/Rplot_original_color_scheme_w_names.pdf"
mapping_vector_labs
################################################################################################
#MODIFIED COLOR ASSIGNMENT TO CURRENT
################################################################################################


new_cols<-c("#F8766D","#00B0F6","#FF6A98","#35A2FF","#C09B00","#E76BF3","#EA8331","#7CAE00","#9590FF","#39B600","#A3A500","#00BB4E","#FF62BC","#00BAE0","#00C1A3","#FA62DB","#00BF7D","#D89000","#00BAE0","#C77CFF")
mapping_vector2<-setNames(as.character(new_cols),mapping_data_frame$organism_CAT)
mapping_vector2
plot(1:20, pch=16, cex=2, col=mapping_vector2)
#http://www.statmethods.net/advgraphs/axes.html
text(1:20,mapping_data_frame$organism,cex=0.5) #saved as "/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo\ Data/SEPTEMBER_2014/colors/Rplot_color_map_labeled.pdf"

################################################################################################
################################################################################################
################################################################################################
#PLOTTING
################################################################################################
################################################################################################
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
##ggsave(filename = 'Y_spindle_length_AA_X_cell_diameter_stretched.pdf',width=11, height=8)


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
##ggsave(filename = 'Y_spindle_length_AA_X_cell_diameter_cell_cycle_stretched.pdf',width=11, height=8)
head(mitotic_df)
################################################################################################
################################################################################################
#PLOTTING FIGURE 1A: y-spindle size pole-to-pole, x-cell diameter: METAPHASE MITOTIC: line: 13
################################################################################################
################################################################################################
plot_f1_v1<-ggplot()
plot_f1_v1<-plot_f1_v1+geom_point(data=mitotic_metaphase,aes(x=mitotic_metaphase$cell_diameter_um,y=mitotic_metaphase$spindle_length_poles_um,colour=factor(mitotic_metaphase$organism_CAT)))
plot_f1_v1<-plot_f1_v1+scale_color_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot_f1_v1+ylim(c(0,75))
#ggsave(filename='F1V1.pdf')

plot_f1_v2<-ggplot()
plot_f1_v2<-plot_f1_v2+geom_point(data=mitotic_metaphase,aes(x=mitotic_metaphase$cell_diameter_um,y=mitotic_metaphase$spindle_length_poles_um,colour=factor(mitotic_metaphase$organism_CAT)),size=1.3)
plot_f1_v2<-plot_f1_v2+scale_color_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot_f1_v2+ylim(c(0,75))
#ggsave(filename='F1V2.pdf')

plot_f1_v3<-ggplot()
plot_f1_v3<-plot_f1_v3+geom_point(data=mitotic_metaphase,aes(x=mitotic_metaphase$cell_diameter_um,y=mitotic_metaphase$spindle_length_poles_um,colour=factor(mitotic_metaphase$organism_CAT)),size=1.4)
plot_f1_v3<-plot_f1_v3+scale_color_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot_f1_v3+ylim(c(0,75))
#ggsave(filename='F1V3.pdf',useDingbats=FALSE)

plot_f1_v3<-ggplot()
plot_f1_v3<-plot_f1_v3+geom_point(data=mitotic_metaphase,aes(x=mitotic_metaphase$cell_diameter_um,y=mitotic_metaphase$spindle_length_poles_um,colour=factor(mitotic_metaphase$organism_CAT)),size=1.5)
plot_f1_v3<-plot_f1_v3+scale_color_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot_f1_v3+ylim(c(0,75))
#ggsave(filename='F1V3_1p5.pdf',useDingbats=FALSE)


plot_f1_v3_300<-ggplot()
plot_f1_v3_300<-plot_f1_v3_300+geom_point(data=mitotic_metaphase,aes(x=mitotic_metaphase$cell_diameter_um,y=mitotic_metaphase$spindle_length_poles_um,colour=factor(mitotic_metaphase$organism_CAT)),size=1.4)
plot_f1_v3_300<-plot_f1_v3_300+scale_color_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot_f1_v3_300+ylim(c(0,75))+xlim(c(0,300))
#ggsave(filename='F1V3_300.pdf')

plot_f1_v3_500<-ggplot()
plot_f1_v3_500<-plot_f1_v3_500+geom_point(data=mitotic_metaphase,aes(x=mitotic_metaphase$cell_diameter_um,y=mitotic_metaphase$spindle_length_poles_um,colour=factor(mitotic_metaphase$organism_CAT)),size=1.4)
plot_f1_v3_500<-plot_f1_v3_500+scale_color_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot_f1_v3_500+ylim(c(0,75))+xlim(c(0,500))
#ggsave(filename='F1V3_500.pdf',useDingbats=FALSE)

plot_f1_v3_500<-ggplot()
plot_f1_v3_500<-plot_f1_v3_500+geom_point(data=mitotic_metaphase,aes(x=mitotic_metaphase$cell_diameter_um,y=mitotic_metaphase$spindle_length_poles_um,colour=factor(mitotic_metaphase$organism_CAT)),size=1.5)
plot_f1_v3_500<-plot_f1_v3_500+scale_color_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot_f1_v3_500+ylim(c(0,75))+xlim(c(0,500))
#ggsave(filename='F1V3_500_1p5.pdf',useDingbats=FALSE)




plot_f1_v3_1000<-ggplot()
plot_f1_v3_1000<-plot_f1_v3_1000+geom_point(data=mitotic_metaphase,aes(x=mitotic_metaphase$cell_diameter_um,y=mitotic_metaphase$spindle_length_poles_um,colour=factor(mitotic_metaphase$organism_CAT)),size=1.4)
plot_f1_v3_1000<-plot_f1_v3_1000+scale_color_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot_f1_v3_1000+ylim(c(0,75))+xlim(c(0,1000))
#ggsave(filename='F1V3_1000.pdf')


################################################################################################
################################################################################################
#PLOTTING FIGURE 2: y-AVG: spindle size pole-to-pole, x-cell diameter: METAPHASE MITOTIC: line: 210
################################################################################################
################################################################################################
#V1: http://docs.ggplot2.org/current/geom_smooth.html
smooth_data<-read.csv('../SEPTEMBER_2014/MitoticScaling_Binnedmeans_SD_reformatted.csv')
getwd()
str(smooth_data)
head(smooth_data)
#change color scheme
#head(smooth_data[,-6])
str(smooth_data$organism)
mapping_vector_labs_2<-as.character(lapply(strsplit(mapping_vector_labs,'_'),function(x){x[2]}))
mapping_vector_labs_2<-setNames(mapping_vector_labs_2,seq(1,length(mapping_vector_labs_2)))
mapping_vector2_2<-setNames(mapping_vector2,mapping_vector_labs_2)

qplot(cell.diameter,y=sl.pole,data=smooth_data,colour=factor(smooth_data$organism))+scale_colour_manual(values = mapping_vector2_2)
#ggsave('S1_related_MitoticScaling_Binnedmeans_SD.pdf')
getwd()

model<-lm(sl.pole~cell.diameter+factor(organism),data=smooth_data)
grid<-with(smooth_data,expand.grid(cell.diameter=seq(min(cell.diameter),max(cell.diameter),length=length(smooth_data$cell.diameter)),organism=levels(factor(organism))))

grid$sl.pole<-stats::predict(model,newdata = grid)

err<-stats::predict(model,newdata=grid,se=TRUE)
grid$ucl<-err$fit+1.96*err$se.fit
grid$lcl<-err$fit-1.96*err$se.fit
head(grid)
qplot(cell.diameter,sl.pole,data=smooth_data,colour=factor(organism))+geom_smooth(aes(ymin=lcl,ymax=ucl),data=grid,stat='identity')

#WRONG!!! this is a linear model
################################################################################################
#V2: better, but not smoothened

head(smooth_data)
head(grid)
smooth_data$sl.pole.upper_sd<-smooth_data$sl.pole+smooth_data$SD.1
smooth_data$sl.pole.lower_sd<-smooth_data$sl.pole-smooth_data$SD.1

qplot(cell.diameter,sl.pole,data=smooth_data,colour=factor(organism))+geom_smooth(aes(ymin=smooth_data$sl.pole.lower_sd,ymax=smooth_data$sl.pole.upper_sd),data=smooth_data,stat='identity')

################################################################################################
#V3: http://docs.ggplot2.org/0.9.3/stat_smooth.html
#LOESS: http://en.wikipedia.org/wiki/Local_regression
plot<-ggplot(smooth_data,aes(cell.diameter, sl.pole,colour=factor(organism)))
plot+stat_smooth(aes(fill=factor(organism)))+xlim(c(0,500))+geom_point()
smooth_data[!smooth_data$organism=='elegans',]

plot<-ggplot(smooth_data,aes(cell.diameter, sl.pole,colour=factor(organism)))
plot+stat_smooth(aes(fill=factor(organism)))+xlim(c(0,500))+geom_point()
################################################################################################
################################################################################################
#V4: LOESS ON ALL
#http://stackoverflow.com/questions/13848854/use-glm-method-if-loess-method-returning-error
#need to remove the rows where there is not enough data points
#use the other data frame for filtering

head(mitotic_metaphase)

#############################################
#FILTER


str(unique(mitotic_metaphase$organism))
parse_cats<-strsplit(as.character(unique(mitotic_metaphase$organism)),"_")
parse_cats[[1]][2]
vector_for_filter<-unlist(lapply(parse_cats,function(x){x[2]}))

unique(smooth_data$organism)
vector_for_filter
#set operation
vector_for_filter %in% as.character(smooth_data$organism)

class(vector_for_filter[1])
class(unique(smooth_data$organism)[1])
vector_for_filter[vector_for_filter %in% as.character(smooth_data$organism)]
vector_for_filter[!vector_for_filter %in% as.character(smooth_data$organism)]

head(mitotic_metaphase[unlist(lapply(strsplit(as.character(mitotic_metaphase$organism),"_"),function(x){x[2]})) %in% unique(smooth_data$organism),])

filtered_mm<-mitotic_metaphase[unlist(lapply(strsplit(as.character(mitotic_metaphase$organism),"_"),function(x){x[2]})) %in% unique(smooth_data$organism),]
#############################################
#PLOT

#plot<-ggplot(mitotic_metaphase,aes(cell_diameter_um, mitotic_metaphase$spindle_length_poles_um,colour=factor(organism_CAT)))+geom_point()+geom_smooth(na.rm=TRUE)
#plot+stat_smooth(aes(fill=factor(organism_CAT)))+xlim(c(0,500))+geom_point()

plot<-ggplot(filtered_mm,aes(cell_diameter_um, filtered_mm$spindle_length_poles_um,colour=factor(organism_CAT)))
plot<-plot+geom_point(alpha=0.5)+geom_smooth(na.rm=TRUE,alpha=0.2)
plot<-plot+stat_smooth(aes(fill=factor(organism_CAT)))+xlim(c(0,500))
plot<-plot+scale_colour_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot+scale_fill_manual(values = mapping_vector2,labels=mapping_vector_labs)
#ggsave(filename='F1A_LOESS_regressed.pdf',useDingbats=FALSE)

plot<-ggplot(filtered_mm,aes(cell_diameter_um, filtered_mm$spindle_length_poles_um,colour=factor(organism_CAT)))
plot<-plot+geom_point(alpha=0.5)+geom_smooth(na.rm=TRUE,alpha=0.2)
plot<-plot+stat_smooth(aes(fill=factor(organism_CAT)))#+ylim(0,130)
plot<-plot+scale_colour_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot+scale_fill_manual(values = mapping_vector2,labels=mapping_vector_labs)
#ggsave(filename='S1A_LOESS_regressed.pdf',useDingbats=FALSE)

plot<-ggplot(filtered_mm,aes(cell_diameter_um, filtered_mm$spindle_length_poles_um,colour=factor(organism_CAT)))
plot<-plot+geom_point(alpha=0.5)+geom_smooth(na.rm=TRUE,alpha=0.2)
plot<-plot+stat_smooth(aes(fill=factor(organism_CAT)))+ylim(0,130)
plot<-plot+scale_colour_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot+scale_fill_manual(values = mapping_vector2,labels=mapping_vector_labs)
#ggsave(filename='S1A_y_rescaled_to S1B_LOESS_regressed.pdf',useDingbats=FALSE)

################################################################################################
################################################################################################
#V5: LOESS ON ALL:: ASTERS:: S1B
################################################################################################
################################################################################################
#http://stackoverflow.com/questions/13848854/use-glm-method-if-loess-method-returning-error
#need to remove the rows where there is not enough data points
#use the other data frame for filtering
rm(list=ls())
mitotic_metaphase<-read.csv('../SEPTEMBER_2014/mitotic_metaphase.csv')
smooth_data<-read.csv('../SEPTEMBER_2014/MitoticScaling_Binnedmeans_SD_reformatted.csv')
head(mitotic_metaphase)

#############################################
#FILTER

str(unique(mitotic_metaphase$organism))
parse_cats<-strsplit(as.character(unique(mitotic_metaphase$organism)),"_")
parse_cats[[1]][2]
vector_for_filter<-unlist(lapply(parse_cats,function(x){x[2]}))

unique(smooth_data$organism)
vector_for_filter
#set operation
vector_for_filter %in% as.character(smooth_data$organism)

class(vector_for_filter[1])
class(unique(smooth_data$organism)[1])
vector_for_filter[vector_for_filter %in% as.character(smooth_data$organism)]
vector_for_filter[!vector_for_filter %in% as.character(smooth_data$organism)]

head(mitotic_metaphase[unlist(lapply(strsplit(as.character(mitotic_metaphase$organism),"_"),function(x){x[2]})) %in% unique(smooth_data$organism),])

filtered_mm<-mitotic_metaphase[unlist(lapply(strsplit(as.character(mitotic_metaphase$organism),"_"),function(x){x[2]})) %in% unique(smooth_data$organism),]
head(filtered_mm)
#############################################
#PLOT
#plot<-ggplot(mitotic_metaphase,aes(cell_diameter_um, mitotic_metaphase$spindle_length_poles_um,colour=factor(organism_CAT)))+geom_point()+geom_smooth(na.rm=TRUE)
#plot+stat_smooth(aes(fill=factor(organism_CAT)))+xlim(c(0,500))+geom_point()

filtered_mm[!filtered_mm$organism=='mus_musculus',]$spindle_length__asters_um
filtered_mm$organism=='mus_musculus'
mapping_vector_labs=="mus_musculus" 
max(filtered_mm[!filtered_mm$organism=='mus_musculus',]$spindle_length__asters_um,na.rm=TRUE)

filtered_mm[filtered_mm[!filtered_mm$organism=='mus_musculus',]$spindle_length__asters_um==128.9,]$cell_diameter_um
filtered_mm[which(filtered_mm$spindle_length__asters_um==128.9),'cell_diameter_um']

plot2<-ggplot(data=filtered_mm[!filtered_mm$organism=='mus_musculus',],aes(x=cell_diameter_um, y=spindle_length__asters_um,colour=factor(organism_CAT)))
plot2<-plot2+geom_point(na.rm=TRUE,alpha=0.5) +geom_smooth(na.rm=TRUE,alpha=0.2)
plot2<-plot2+stat_smooth(aes(fill=factor(organism_CAT)))+ylim(0,130)
plot2<-plot2+scale_colour_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot2+scale_fill_manual(values = mapping_vector2,labels=mapping_vector_labs)
#ggsave(filename='S1B_LOESS_regressed.pdf',useDingbats=FALSE)


plot2_1p5<-ggplot()
plot2_1p5<-plot2_1p5+geom_point(data=filtered_mm[!filtered_mm$organism=='mus_musculus',],aes(x=cell_diameter_um,y=spindle_length__asters_um,colour=factor(organism_CAT)),size=1.5)
plot2_1p5<-plot2_1p5+scale_color_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot2_1p5+ylim(0,130)
#ggsave(filename='S1B_scatter_1p5.pdf',useDingbats=FALSE)

filtered_mm$spindle_length_poles_um
plot2_1p5<-ggplot()
plot2_1p5<-plot2_1p5+geom_point(data=filtered_mm,aes(x=cell_diameter_um,y=spindle_length_poles_um,colour=factor(organism_CAT)),size=1.5)
plot2_1p5<-plot2_1p5+scale_color_manual(values = mapping_vector2,labels=mapping_vector_labs)
plot2_1p5+ylim(0,130)
#ggsave(filename='../Manuscript/Figures/S1/S1A_scatter_1p5.pdf',useDingbats=FALSE)






