library(igraph)
library(ggplot2)

#Extracting scores of top 10 batsmens per match from deliveries.csv and write it to top10player_cv.csv
d = read.csv("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab2/deliveries.csv")
d
players_runs = d[,c('match_id','batsman','batsman_runs')]
players_runs

players = unique(players_runs$batsman)
match_id = unique(d$match_id)

batman = c('DA Warner','KL Rahul', 'MS Dhoni', 'J Bairstow', 'MP Stoinis', 'AD Russell', 'CH Gayle', 'HH Pandya', 'AB de Villiers','RR Pant')

#View(d)
s = c()
rs = c()
for(match in match_id){
  df = d[which(d$match_id == match),c('batsman','batsman_runs')]
  player_name = c()
  runs = c()
  for(p in unique(df$batsman)){
    
    if(p %in% batman){
      player_name = c(player_name,p)
      runs =c(runs , sum(df[which(df$batsman==p),'batsman_runs']))}
    print(match)
  }
  s = c(s,player_name )
  rs = c(rs,runs)
}
dfo = data.frame(s,rs)
dfo
total_run = c()
j = 1
for (pr in batman)
{
  
  run  = c()
  for(i in rownames(dfo)){
    if(dfo[i,1]== pr){
      run=c(run,dfo[i,2])
    }
    
  }
  total_run[[j]] = run
  j = j+1
}
batman
total_run
dfd = data.frame(matrix(ncol = 10,nrow=10))
dfd
colnames(dfd) <- batman
dfd
for(i in rownames(dfd)){
  i= as.numeric(i)
  for(j in 1:10){
    dfd[i,j] = total_run[[j]][i]
  }}
dfd
check = dfd
check
write.csv(check, file = "/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab2/top10player_cv.csv", row.names = c('match1','match2','match3','match4','match5','match6','match7','match8','match9','match10'))

#Finding Covariance for all the players and bar plot to analyse it
data_for_covarience=read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/DA LAB/lab2/top10player_cv.csv", sep = ",", header = TRUE)
summary(data_for_covarience)
cov =c(sd(data_for_covarience$DA.Warner)/mean(data_for_covarience$DA.Warner)*100,
       sd(data_for_covarience$KL.Rahul)/mean(data_for_covarience$KL.Rahul)*100,
       sd(data_for_covarience$MS.Dhoni)/mean(data_for_covarience$MS.Dhoni)*100,
       sd(data_for_covarience$J.Bairstow)/mean(data_for_covarience$J.Bairstow)*100,
       sd(data_for_covarience$MP.Stoinis)/mean(data_for_covarience$MP.Stoinis)*100,
       sd(data_for_covarience$AD.Russell)/mean(data_for_covarience$AD.Russell)*100,
       sd(data_for_covarience$CH.Gayle)/mean(data_for_covarience$CH.Gayle)*100,
       sd(data_for_covarience$HH.Pandya)/mean(data_for_covarience$HH.Pandya)*100,
       sd(data_for_covarience$AB.de.Villiers)/mean(data_for_covarience$AB.de.Villiers)*100,
       sd(data_for_covarience$RR.Pant)/mean(data_for_covarience$RR.Pant)*100
)
cov
batman
cov
gg = data.frame(batman,cov)
gg
ggplot(gg,aes(x =batman,y=cov))+ylab("Covariance") + geom_bar(stat = "identity",position = "dodge",fill = "#FFD700") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("More the covarience less is the consistency")

#individual covariences of each player
sd(data_for_covarience$DA.Warner)/mean(data_for_covarience$DA.Warner)*100
sd(data_for_covarience$KL.Rahul)/mean(data_for_covarience$KL.Rahul)*100
sd(data_for_covarience$MS.Dhoni)/mean(data_for_covarience$MS.Dhoni)*100
sd(data_for_covarience$J.Bairstow)/mean(data_for_covarience$J.Bairstow)*100
sd(data_for_covarience$MP.Stoinis)/mean(data_for_covarience$MP.Stoinis)*100
sd(data_for_covarience$AD.Russell)/mean(data_for_covarience$AD.Russell)*100
sd(data_for_covarience$CH.Gayle)/mean(data_for_covarience$CH.Gayle)*100
sd(data_for_covarience$HH.Pandya)/mean(data_for_covarience$HH.Pandya)*100
sd(data_for_covarience$AB.de.Villiers)/mean(data_for_covarience$AB.de.Villiers)*100
sd(data_for_covarience$RR.Pant)/mean(data_for_covarience$RR.Pant)*100
