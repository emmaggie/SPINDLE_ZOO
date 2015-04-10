setwd("/Volumes/Magdalena_NEW1/ZOO_DATA/Zoo Data/FINAL")
original.SQL<-read.csv('df2_original_for_SQL.csv',stringsAsFactors=FALSE)
original.SQL.met<-original.SQL[(original.SQL$stage == 'm' | original.SQL$stage=='mI' | original.SQL$stage=='mII'),]
original.SQL.met.mit<-original.SQL.met[which(original.SQL.met$meiotic==0),]
mitotic_df<-original.SQL.met.mit



min.1<-min(mitotic_df$cell_diameter_um,na.rm = TRUE)
max.1<-max(mitotic_df$cell_diameter_um,na.rm = TRUE)
my_range<-seq(from=min.1+10,to=max.1,by=10)
print(my_range)

R_squares.asters<-numeric(length(my_range))
for (i in 1:length(R_squares)){  
  R_squares.asters[i]<-summary(lm(mitotic_df$spindle_length__asters_um[mitotic_df$cell_diameter_um<my_range[i]] ~ mitotic_df$cell_diameter_um[mitotic_df$cell_diameter_um<my_range[i]]))$r.squared
}
print(R_squares.asters)

R_squares.poles<-numeric(length(my_range))

for (i in 1:length(R_squares)){  
  R_squares.poles[i]<-summary(lm(mitotic_df$spindle_length_poles_um[mitotic_df$cell_diameter_um<my_range[i]] ~ mitotic_df$cell_diameter_um[mitotic_df$cell_diameter_um<my_range[i]]))$r.squared
}
print(R_squares.poles)

print(max(R_squares.asters))
print(max(R_squares.poles))
print(my_range[which(R_squares.asters==max(R_squares.asters))])
print(my_range[which(R_squares.poles==max(R_squares.poles))])

ggplot(data=as.data.frame(cbind(my_range,R_squares.asters)))+geom_line(aes(x=my_range,y=(R_squares.asters)),size=1)+xlab('cell size')+ylab('Rsq - spindle length - asters')
#ggsave(ggplot(data=as.data.frame(cbind(my_range,R_squares.asters)))+geom_line(aes(x=my_range,y=(R_squares.asters)),size=1)+xlab('cell size')+ylab('Rsq - spindle length - asters'),filename='RSq_asters.pdf')

ggsave(ggplot(data=as.data.frame(cbind(my_range,R_squares.poles)))+geom_line(aes(x=my_range,y=(R_squares.poles)),size=1)+xlab('cell size')+ylab('Rsq - spindle length - poles'),filename='RSq_poles.pdf')


