library(igraph)
library(ggplot2)
bat_data=read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab2/espn batsman.csv", sep = ",", header = TRUE)
#ipl_data
w0=-2
w1=1
w2=2
w3=3
w4=10
w5=8
w6=16
bat_data['rank_score1']=w6*bat_data$X100 + w5*bat_data$X50 + w4*bat_data$Ave + w3*bat_data$SR + w2*bat_data$X4s + w1*bat_data$X6s + w0*bat_data$X0
#Rank scaling done below
bat_data['rank_score1']=bat_data['rank_score1']/14
R1 <- bat_data[order(bat_data$rank_score1,decreasing=T),]
R1
R1[1:10,]
write.csv(R1[1:10,],"/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab2/top_10_batsman.csv",)



BOWLER_DATA=read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab2/espn bowler.csv", sep = ",", header = TRUE)
#BOWLER_DATA
BOWLER_DATA['WKTS/MAT']=BOWLER_DATA['Wkts']/BOWLER_DATA['Mat']
wb0=4     #8
wb1=8    #16
wb2=4     #8
wb3=5    #10
wb4=4     #8
  
BOWLER_DATA['rank_score2']=wb0*BOWLER_DATA$Mdns + wb1*BOWLER_DATA$X5 + wb2*BOWLER_DATA$X4 + wb3*BOWLER_DATA$'WKTS/MAT'- wb4*BOWLER_DATA$Econ
BOWLER_DATA['rank_score2']=BOWLER_DATA['rank_score2'] + 100
R2 <- BOWLER_DATA[order(BOWLER_DATA$rank_score2,decreasing=T),]
R2
R2[1:10,]
write.csv(R2[1:10,],"/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab2/top_10_bowler.csv")


#Interesting plots for top 10 batsmans
bat_analysis=read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab2/top_10_batsman.csv", sep = ",", header = TRUE)
bat_analysis
#plot(data$year,data$c_b, type="b", col="green", lwd=5, xlab="years", ylab="birth rate/nve_d rate",ylim=range(data$c_d,data$c_b))
#barplot(bat_analysis$Player,names.arg=Player,xlab="Average",y  lab="players",col="green",main="bar chart",border="red")
ggplot(bat_analysis,aes(x =Player,y=rank_score1))+ylab("Ranks") + geom_bar(stat = "identity",position = "dodge",fill="#FF0000") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Top 10 batsmens ranks")
ggplot(bat_analysis,aes(x =Player,y=Ave))+ylab("Average") + geom_bar(stat = "identity",position = "dodge",fill="#0000FF") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Top 10 batsmens Average")
ggplot(bat_analysis,aes(x =Player,y=rank_score1))+ylab("SR") + geom_bar(stat = "identity",position = "dodge",fill="#008000") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Top 10 batsmens strike rate")

#Interesting plots for top 10 batsmans
bol_analysis=read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab2/top_10_bowler.csv", sep = ",", header = TRUE)
bol_analysis
ggplot(bol_analysis,aes(x =Player,y=rank_score2))+ylab("Ranks") + geom_bar(stat = "identity",position = "dodge",fill="#FF0000") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Top 10 bowlers ranks")
ggplot(bol_analysis,aes(x =Player,y=Econ))+ylab("Economy") + geom_bar(stat = "identity",position = "dodge",fill="#0000FF") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Top 10 bowlerss Economy")
ggplot(bol_analysis,aes(x =Player,y=WKTS.MAT))+ylab("Wkts/match") + geom_bar(stat = "identity",position = "dodge",fill="#008000") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Top 10 bowlerss wkts/match")
