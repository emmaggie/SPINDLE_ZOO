###############################################################################################
#LOAD DATA AND LIBRARIES
###############################################################################################
library(ggplot2)

setwd("/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo Data/FINAL")
original<-read.csv('original.csv',stringsAsFactors=FALSE)
original.SQL<-read.csv('../SEPTEMBER_2014/df2_original_for_SQL.csv',stringsAsFactors=FALSE)

new_cols<-c("#F8766D","#00B0F6","#FF6A98","#35A2FF","#C09B00","#E76BF3","#EA8331","#7CAE00","#9590FF","#39B600","#A3A500","#00BB4E","#FF62BC","#00BAE0","#00C1A3","#FA62DB","#00BF7D","#D89000","#00BAE0","#C77CFF")
my_labels=unique(original$organism)
mapping_vector2<-setNames(object=as.character(new_cols),nm=my_labels)

original.SQL.met<-original.SQL[(original.SQL$stage == 'm' | original.SQL$stage=='mI' | original.SQL$stage=='mII'),]
original.SQL.met.mit<-original.SQL.met[which(original.SQL.met$meiotic==0),]
##########################################
###INITIAL
##########################################
setwd('/Volumes/Magdalena_NEW1/MARINA_PAPER/SEPTEMBER_2014')
dir()
list.files()
original<-read.csv('original.csv')
length(original$meiotic[original$meiotic==0]) #catergory of interest: 1456 rows
#substet the data frame
mitotic_df<-original[original$meiotic==0,]


###############################################################################################
#MODEL DATA:
###############################################################################################

##########################################
#1. SINGLE SPLIT
##########################################
my_model<-lm(mitotic_df$spindle_length__asters_um~mitotic_df$cell_diameter_um)
str(summary(my_model))
summary(my_model)$r.squared
#http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
summary.aov(my_model)
mitotic_df$cell_diameter_um<element

#http://stackoverflow.com/questions/2018178/finding-the-best-trade-off-point-on-a-curve
min.1<-min(original$cell_diameter_um,na.rm = TRUE)
max.1<-max(original$cell_diameter_um,na.rm = TRUE)
length(seq(from=min.1,to=max.1,by=10))

length(mitotic_df$cell_diameter_um)
length(mitotic_df$cell_diameter_um[mitotic_df$cell_diameter_um<500])

summary(lm(mitotic_df$spindle_length__asters_um[mitotic_df$cell_diameter_um<500] ~ mitotic_df$cell_diameter_um[mitotic_df$cell_diameter_um<500]))$r.squared

##########################################
#2. RANGE OF SPLITS
##########################################
my_range<-seq(from=min.1+10,to=max.1,by=10)

R_squares<-numeric(length(my_range))

for (i in 1:length(R_squares)){  
    R_squares[i]<-summary(lm(mitotic_df$spindle_length__asters_um[mitotic_df$cell_diameter_um<my_range[i]] ~ mitotic_df$cell_diameter_um[mitotic_df$cell_diameter_um<my_range[i]]))$r.squared   
}

##########################################
#3. WRAP INTO A FUNCTION TO BE ABLE TO WORK OVER FACTORS
##########################################
#https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
#http://adv-r.had.co.nz/Functions.html

get_vec_or_Rsq<-function(sp_length,c_diameter,vector_of_c_limits){
  
  R_squares<-numeric(length(vector_of_c_limits))
  
  for (i in 1:length(R_squares)){
    R_squares[i]<-summary(lm(sp_length[c_diameter<vector_of_c_limits[i]] ~ c_diameter[c_diameter<vector_of_c_limits[i]]))$r.squared
  } 
  
  return(R_squares)
}

get_vec_or_Rsq.LL<-function(sp_length,c_diameter,vector_of_c_limits){
  R_squares<-numeric(length(vector_of_c_limits))
  
  for (i in 1:length(R_squares)){
    R_squares[i]<-summary(lm(log2(sp_length[c_diameter<vector_of_c_limits[i]]) ~ log2(c_diameter[c_diameter<vector_of_c_limits[i]])))$r.squared
  } 
  
  return(R_squares)
}

##########################################
#4. ANALYSIS: INITIAL
##########################################
ASTERS<-max(get_vec_or_Rsq(mitotic_df$spindle_length__asters_um,mitotic_df$cell_diameter_um,my_range))
print(ASTERS)

POLES<-max(get_vec_or_Rsq(original.SQL.met.mit$spindle_length_poles_um,original.SQL.met.mit$cell_diameter_um,my_range))
print(POLES)
POLES.LL<-max(get_vec_or_Rsq.LL(original.SQL.met.mit$spindle_length_poles_um,original.SQL.met.mit$cell_diameter_um,my_range))


new_Rsq<-get_vec_or_Rsq(original.SQL.met.mit$spindle_length_poles_um,original.SQL.met.mit$cell_diameter_um,my_range)
new_Rsq.LL<-get_vec_or_Rsq.LL(original.SQL.met.mit$spindle_length_poles_um,original.SQL.met.mit$cell_diameter_um,my_range)


model_df<-as.data.frame(cbind(my_range,R_squares))
new_model_df<-as.data.frame(cbind(my_range,new_Rsq))

model_plot<-ggplot()
model_plot+geom_point(data=model_df,aes(x=my_range,y=R_squares))+labs(title="All mitotic spindles")+annotate("text",label='max Rsq: 0.7796717, cell diameter: 110 micrometers',x=280,y=0.82,size=4,colour='red',fontface='bold')

model_plot_line<-ggplot()
model_plot_line+geom_line(data=model_df,aes(x=my_range,y=R_squares),size=1)#+labs(title="All mitotic spindles")+annotate("text",label='max Rsq: 0.7796717, cell diameter: 110 micrometers',x=280,y=0.82,size=4,colour='red',fontface='bold')

model_plot_line.2<-ggplot()
model_plot_line.2+geom_line(data=new_model_df,aes(x=my_range,y=new_Rsq),size=1)

model_plot_line.2.LL<-ggplot()
model_plot_line.2+geom_line(data=as.data.frame(cbind(my_range,new_Rsq.LL)),aes(x=my_range,y=new_Rsq.LL),size=1)#+labs(title




ggsave(filename='X_cell_diam_Y_Rsq_all_mitotic_spindles.pdf', width=11, height=8)

model_df[model_df$R_squares==max(R_squares),]

names(model_df)
mitotic_df

#Let's add layer for metaphase and anaphase:
class(mitotic_df)
class(mitotic_df$stage)
is.factor(mitotic_df$stage)
mitotic_df$stage<-as.character(mitotic_df$stage)

class(mitotic_df$organism)
by(mitotic_df$spindle_length__asters_um,mitotic_df$organism,mean,na.rm=TRUE)

split_data<-split(mitotic_df[,c(5,6)],mitotic_df$organism)
length(split_data) #20 organisms
length(split_data$xenopus_tropicalis[[2]])

to_delete<-numeric(20)
for (i in 1:length(split_data)){
  #print(length(split_data[[i]]))
    if(length(split_data[[i]][[2]])==0){
      to_delete[i]<-i
  }
}
length(split_data) #20 organisms
idx_to_delete<-to_delete[! to_delete %in% 0]

for (idx in idx_to_delete){
  #print(split_data[[idx]])
  split_data[[idx]]<-NULL
}
length(split_data) #14 organisms

#list_of_results<-lapply(split_data,get_vec_or_Rsq,vector_of_c_limits=my_range)
for (animal in split_data){
  print(animal$cell_diameter_um)
}

names(mitotic_df)=='spindle_length__asters_um'
match('spindle_length__asters_um',names(mitotic_df))
match('cell_diameter_um',names(mitotic_df))


get_vec_or_Rsq(mitotic_df$spindle_length__asters_um,mitotic_df$cell_diameter_um,my_range)

?by
#http://plyr.had.co.nz/
mitotic_df$spindle_length__asters_um[mitotic_df$organism=='xenopus_laevis']

