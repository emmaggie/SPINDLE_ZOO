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

ggplot(data=original.SQL.met.mit)+geom_point(aes(y=log2(spindle_length_poles_um),x=log2(cell_diameter_um),colour=organism),alpha=0.9)+scale_color_manual(values=mapping_vector2,labels=names(mapping_vector2))+ylim(1,log2(2**7.2))

ggsave(ggplot(data=original.SQL.met.mit)+geom_point(aes(y=log2(spindle_length_poles_um),x=log2(cell_diameter_um),colour=organism),alpha=0.9)+scale_color_manual(values=mapping_vector2,labels=names(mapping_vector2))+ylim(1,log2(2**7.2)),filename='0_log_log_p2p_cd.pdf')

ggplot(data=original.SQL.met.mit)+geom_point(aes(y=log2(spindle_length__asters_um),x=log2(cell_diameter_um),colour=organism),alpha=0.9)+scale_color_manual(values=mapping_vector2,labels=names(mapping_vector2))+ylim(1,log2(2**7.2))

ggsave(ggplot(data=original.SQL.met.mit)+geom_point(aes(y=log2(spindle_length__asters_um),x=log2(cell_diameter_um),colour=organism),alpha=0.9)+scale_color_manual(values=mapping_vector2,labels=names(mapping_vector2))+ylim(1,log2(2**7.2)),filename='0_log_log_a2a_cd.pdf')

###############################################################################################
#DEFINE HELPER FUNCTIONS
###############################################################################################

mult_of_2<-function(x){
  #Tests for divisibility by 2.
  #
  #Args: x - value to be tested (numeric or integer)
  #
  #Returns x if x is divisible by 10. If not, returns first value larger than x, which is divisible by 10.
  
  if (ceiling(x) %% 2 == 0){
    return(x)
  }
  else {
    while (ceiling(x) %%2 !=0){
      x<-ceiling(x)+1
      if (x %% 2 == 0){
        return(x)
        break 
      }
    }
  }
}


break_number<-function(var_x){
  
  #USES mult_of_2()
  #1. initiate
  
  from=mult_of_2(min(var_x, na.rm=TRUE))
  to=mult_of_2(max(var_x, na.rm=TRUE))
  breaks<-seq(from=from, to=to,by=2)
  
  #2. check whether the marginal vectors 
  if ((sum(var_x<=min(breaks),na.rm=TRUE)>3) & (sum(var_x>=max(breaks),na.rm=TRUE)>3)){
    return(breaks)
  }
  
  if (sum(var_x<=min(breaks),na.rm=TRUE)<3){
    while (sum(var_x<=min(breaks),na.rm=TRUE)<3){
      from = from + 2
      breaks<-seq(from=from, to=to, by=2)
    }
  }
  
  if (sum(var_x>=max(breaks),na.rm=TRUE)<3){
    while (sum(var_x>=max(breaks),na.rm=TRUE)<3){
      to = to - 2
      breaks<-seq(from=from, to=to,by=2)
    }
  }
  return(ceiling(breaks))
}

piecewise_reg<-function(data_frame, x, y){
  breaks<-break_number(data_frame[,x])
  
  sigma<-vector(mode='numeric',length=length(breaks))
  AIC<-vector(mode='numeric',length=length(breaks))
  BIC<-vector(mode='numeric',length=length(breaks))
  for (i in 1:length(breaks)){
    model<-lm(data_frame[,y] ~ (data_frame[,x]<breaks[i])*data_frame[,x] + (data_frame[,x]>breaks[i])*data_frame[,x])
    sigma[i]<-summary(model)$sigma #c(sigma=model$sigma,AIC=AIC(model))
    AIC[i]<-AIC(model)#c(sigma=model$sigma,AIC=AIC(model))
    BIC[i]<-BIC(model)
  }
  result = list(breaks=breaks,sigma=sigma, AIC=AIC, BIC=BIC)
  return(result)
}

plotter<-function(list_of_models){
  #Saves batch generated plots   
  for (i in 1:length(list_of_models)){
    plot<-ggplot(data=as.data.frame(rs[[i]]))+geom_line(aes(x=breaks,y=ceiling(AIC)),size=1,col=mapping_vector2[names(rs)[[i]]])
    ggsave(filename=paste(i,'_PR_AIC_',names(rs)[[i]],'.pdf',sep=''),plot=plot)
  }
}

###############################################################################################
#ALL ORGANISMS
###############################################################################################

pr.cell_diam.sp_len.p2p=piecewise_reg(original.SQL.met.mit,'cell_diameter_um','spindle_length_poles_um')
ggplot(data=as.data.frame(pr.cell_diam.sp_len.p2p))+geom_line(aes(x=breaks,y=ceiling(AIC)),colour='black',size=1)
ggsave(ggplot(data=as.data.frame(pr.cell_diam.sp_len.p2p))+geom_line(aes(x=breaks,y=ceiling(AIC)),colour='black',size=1),filename='0_PR_AIC_ALL.pdf')



###############################################################################################
#INDIVIDUAL ORGANISMS
###############################################################################################
rows_per_org<-by(original.SQL.met.mit, original.SQL.met.mit$organism, function(x){dim(x)}[1])
rows_per_org[rows_per_org>6]
row.names(cbind(rows_per_org[rows_per_org>6]))
row.names(cbind(rows_per_org[rows_per_org<6]))
row.names(cbind(rows_per_org[rows_per_org>6]))

subset_doable=original.SQL.met.mit[original.SQL.met.mit$organism %in% row.names(cbind(rows_per_org[rows_per_org>6])),]
print(dim(subset_doable))
print(dim(original.SQL.met.mit[!original.SQL.met.mit$organism %in% row.names(cbind(rows_per_org[rows_per_org>6])),]))

by(subset_doable, subset_doable$organism, function(x) break_number(x[,'cell_diameter_um']))

rs<-by(subset_doable, subset_doable$organism, function(x) piecewise_reg(x,'cell_diameter_um','spindle_length_poles_um'))

plotter(rs)








