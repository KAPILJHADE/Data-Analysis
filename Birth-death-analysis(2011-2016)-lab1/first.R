library(tabulizer)
data <- extract_areas("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/CRS-2016.pdf",pages=11)
data=data[[1]]
data
colnames(data)=c('year','nve_l','nve_s','nve_d','c_b','c_d','per_b','per_d')
write.csv(data,"/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/data.csv",row.names=FALSE)