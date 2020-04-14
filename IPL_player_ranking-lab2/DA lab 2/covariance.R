library(igraph)
library(ggplot2)

#FINDING Corellation coefficient for all the player combinations and heatmap for it
data_for_covarience=read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab2/top10player_cv.csv", sep = ",", header = TRUE)
data_for_covarience
#cor(data_for_covarience$DA.Warner,data_for_covarience$DA.Warner,method="spearman")
k=cor(data_for_covarience[sapply(data_for_covarience, is.numeric)])
k
palette = colorRampPalette(c("green","white","red")) (20)
heatmap(k,col=palette,symm = TRUE)
