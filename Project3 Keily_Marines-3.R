#Project 3
#Kylene Keily & Kaitlyn Marines

rm(list = ls())


colcounts=c(11,28,2,29,3,19)
file="http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf"
file2="gms.txt"
dates <- 1960:2010
df=data.frame("Date"=numeric(),"AwayTeam"=character(),"AwayScore"=numeric(),"HomeTeam"=character(),"HomeScore"=numeric(),"Location"=character(),"Season"=numeric())

for (i in 1:51){
  a<- read.fwf(paste(file,dates[i],file2, sep = ""),colcounts)
  b<- dates[i]
  season<- cbind(a,b)
  df=rbind(df,season)
}
colnames(df) <- c("Date","AwayTeam","AwayScore","HomeTeam","HomeScore","Location","Season") 

#predefine tie in main data frame
Tie<-which(df$AwayScore==df$HomeScore)
df$tie<-0
df$tie[Tie]<-1

Ties<-data.frame(df$tie,df$Season)
df1<- subset(df, select =-`Date`)
df2<- subset(df1, select =-`Location`)

#remove spaces in main dataframe
df2$AwayTeam<- gsub(" ","", df$`AwayTeam`)
df2$HomeTeam<- gsub(" ","", df$`HomeTeam`)

#unique ID
df2$homeseason<- with(df2, paste(df2$HomeTeam,df2$Season,sep=""))
df2$awayseason<- with(df2, paste(df2$AwayTeam,df2$Season,sep=""))
uni<-unique(c(df2$homeseason,df2$awayseason))

#counts number of games
numgames<-integer(length(uni))
for (i in 1:length(uni)) {
  df2$numgames[i]<-length(which(df2$homeseason==uni[i]))+length(which(df2$awayseason==uni[i]))
}

#drop division 2 teams
Q<- which(df2$numgames<6)
df3<- df2[-c(Q), ]

#drop tied games
tie<- which(df3$tie==1)
df4<- df3[-c(tie), ] 

#wins and losses (**loop is not working**)
listofdf<- list()
for (i in 2:51){
  for (k in 1:length(df4)){
    wincount = 0
    losscount = 0
    T<-listofdf[[i]]$Var1[k]
    for (j in 1:length(df4$Season)){
      if(df4$AwayTeam[df4$Season==dates[i]][j] == T){
        if(df4$AwayScore[df4$Season==dates[i]][j] < df4$HomeScore[df4$Season==dates[i]][j]){
          losscount = losscount + 1
        }
        if(df4$AwayScore[df1$Season==dates[i]][j] > df4$HomeScore[df4$Season==dates[i]][j]){
          wincount = wincount + 1
        }
      }
      if(df4$HomeTeam[df4$Season==dates[i]][j] == T){
        if(df4$HomeScore[df4$Season==dates[i]][j] < df4$AwayScore[df4$Season==dates[i]][j]){
          losscount = losscount + 1
        }
        if(df4$HomeScore[df4$Season==dates[i]][j] > df4$AwayScore[df4$Season==dates[i]][j]){
          wincount = wincount + 1
        }
      }
      losses = losscount
      listofdf[[i]]$losses[k] = losses
      wins = wincount
      listofdf[[i]]$wins[k] = wins
    }
  }
}

#save data frame
save(df4,file="Project3 Keily_Marines.rdata")

############
############
## PART 2 ##
############

rm(list=ls())

load("Project3 Keily_Marines.rdata")


colley<- function(year){
  df4$pksn<- 0
  for(i in 1:nrow(df)){
    if(df4$season[i]==year){
      df4$pksn[i]=1
    }
  }
  df4<- df4[df4$pksn==1,]
  #Colley Matrix
  n=nrow(df4)
  ColMatrix = matrix(data=NA, nrow=n, ncol=n)
  
  #off-diagonals
  for(i in 1:n){
    for(j in 1:length(df4$opponents)){
      ColMatrix[i,j]=ColMatrix[i,j]-1
    }
  }
  diag(ColMatrix)=2+df4$wins+df4$losses
  
  #solve
  bvector=vector()
  for(i in 1:n){
    bvector[i]= 1+((df4$wins[i]-df4$losses[i])/2)
  }
  
  #Ranking
  ranking<- solve(ColMatrix,bvector)
  solutions<-data.frame(df4$teams,ranking)
  Ranked<- solutions[order(-ranking),]
  View(Ranked)
}

