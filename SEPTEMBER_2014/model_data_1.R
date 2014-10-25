#ADD LOADING DATA FUNCTION
library(ggplot2)

setwd('/Volumes/Magdalena_NEW1/MARINA_PAPER/SEPTEMBER_2014')
dir()
list.files()
original<-read.csv('original.csv')
original[1,]

original$meiotic[1:5]
str(original$meiotic)
length(original$meiotic[original$meiotic==0]) #catergory of interest: 1456 rows
#substet the data frame
mitotic_df<-original[original$meiotic==0,]
str(mitotic_df)
str(mitotic_df$organism)
str(mitotic_df$organism_CAT)



#MODEL DATA:
my_model<-lm(mitotic_df$spindle_length__asters_um~mitotic_df$cell_diameter_um)
str(summary(my_model))
summary(my_model)$r.squared
#http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
str(my_model)
summary.aov(my_model)
plot(my_model)
my_model$coefficients

?range
mitotic_df$cell_diameter_um<element
#http://stackoverflow.com/questions/2018178/finding-the-best-trade-off-point-on-a-curve
min.1<-min(original$cell_diameter_um,na.rm = TRUE)
max.1<-max(original$cell_diameter_um,na.rm = TRUE)
length(seq(from=min.1,to=max.1,by=10))

length(mitotic_df$cell_diameter_um)
length(mitotic_df$cell_diameter_um[mitotic_df$cell_diameter_um<500])

summary(lm(mitotic_df$spindle_length__asters_um[mitotic_df$cell_diameter_um<500] ~ mitotic_df$cell_diameter_um[mitotic_df$cell_diameter_um<500]))$r.squared
my_range<-seq(from=min.1+10,to=max.1,by=10)
length(my_range)
#numeric vector to store 

R_squares<-numeric(length(my_range))
for (i in 1:length(R_squares)){
#  for (element in my_range){
  #print(length(mitotic_df$cell_diameter_um[mitotic_df$cell_diameter_um<element]))
  
    R_squares[i]<-summary(lm(mitotic_df$spindle_length__asters_um[mitotic_df$cell_diameter_um<my_range[i]] ~ mitotic_df$cell_diameter_um[mitotic_df$cell_diameter_um<my_range[i]]))$r.squared
    #print(summary(lm(mitotic_df$spindle_length__asters_um[mitotic_df$cell_diameter_um<element] ~ mitotic_df$cell_diameter_um[mitotic_df$cell_diameter_um<element]))$r.squared)
}#}


#WRAP INTO A FUNCTION TO BE ABLE TO WORK OVER FACTORS
#https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
#http://adv-r.had.co.nz/Functions.html

get_vec_or_Rsq<-function(sp_length,c_diameter,vector_of_c_limits){
  R_squares<-numeric(length(vector_of_c_limits))
  
  for (i in 1:length(R_squares)){
    R_squares[i]<-summary(lm(sp_length[c_diameter<vector_of_c_limits[i]] ~ c_diameter[c_diameter<vector_of_c_limits[i]]))$r.squared
  } 
  
  return(R_squares)
}
get_vec_or_Rsq(mitotic_df$spindle_length__asters_um,mitotic_df$cell_diameter_um,my_range)


dim(model_df)
model_df<-as.data.frame(cbind(my_range,R_squares))

model_plot<-ggplot()
model_plot+geom_point(data=model_df,aes(x=my_range,y=R_squares))+labs(title="All mitotic spindles")+annotate("text",label='max Rsq: 0.7796717, cell diameter: 110 micrometers',x=280,y=0.82,size=4,colour='red',fontface='bold')
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

