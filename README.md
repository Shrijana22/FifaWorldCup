# FifaWorldCup
Analyzing the data, Showing how various sampling methods can be used on your data. Drawing various random samples of the data and show the applicability of the Central Limit Theorem for this variable.
Import the data set into R.
setwd("/Users/) WorldCups=read.csv("WorldCups.csv")
winners<-table(WorldCups$Winner)
winners
barplot(winners)
barplot(winners[order(winners)], horiz=T)
barplot(winners[order(winners,decreasing=T)],
col = "cyan",las=3, ylim=c(0,8),xlab = " ", ylab = " ",
main="World Cup Winners By country") class(WorldCups)
sum(WorldCups$GoalsScored)
mean(WorldCups$GoalsScored)
median(WorldCups$GoalsScored)
sum(WorldCups$GoalsScored) 
range(WorldCups$GoalsScored)
var(WorldCups$GoalsScored)
sd(WorldCups$GoalsScored) 
fivenum(WorldCups$GoalsScored)
summary(WorldCups$GoalsScored)
IQR(WorldCups$GoalsScored) 
GS<-WorldCups$GoalsScored
hist(WorldCups$GoalsScored,probability = T,col="yellow", main="HistroGram of Goals Scored in WorldCup")
summary(GS)
boxplot(GS,horizontal = T) boxplot.stats(WorldCups$GoalsScored)
boxplot(GS,horizontal = T,main="BoxPlot of Number of Goal Scored",xlab="Number of Goals",
col=c("red"))
summary(WorldCups)
GS_out<-GS[GS<200]
length(GS_out)
boxplot(GS,horizontal = T,main="BoxPlot of Number of Goals Scored",xlab="Goals Scored",
col=c("red")) summary(GS)
summary(GS_out)
s<-cbind(c(WorldCups$GoalsScored),c(WorldCups$MatchesPlayed))
s
rownames(s) <- c("Italy","Uruguay","Italy","Uruguay","Germany FR","Brazil","Brazil", "England","Brazil","Germany FR","Argentina","Italy","Argentina","Germany
FR","Brazil" ,"France","Brazil","Italy","Spain","Germany")
colnames(s) <- c("GoalsScored", "MatchesPlayed")
dimnames(s) <- list(Winners=row.names(s), Sport=colnames(s)) margin.table(s,1)
margin.table(s, 2)
barplot(s, xlab = "Sport",
beside=TRUE, legend.text =TRUE, main = "Match Summary", col=c(#FF0099","#CCFF00","#33FF00",
"#00FF66","#FF9900","#FF99FF","#0066FF","#00FF4D"))
t(s)
barplot(t(s), xlab = "Winner",
beside = TRUE, legend.text = TRUE, main = "Match Summary", col=c("GREEN", "YELLOW","RED"))
#• Pick one variable with numerical data and examine the distribution of the data. par(mfrow=c(1,1))
hist(WorldCups$GoalsScored,xlim=c(50,200),
main="Histogram of Number of Goal Scored",xlab = "Goal Scored",col="green")
# Data can be examined by four ways # Describing Distributions
# SHAPE
# CENTER
# SPREAD AND
# OUTLIERS
# Shape of distribution
# This distribution is skewed to the right therefore this is right-tailed distribution.
# CENTER:The data seems to be centered around 120.
# SPREAD:From looking at the histogram, we can approximate the smallest observation (min), and the largest observation (max),
# So here the range is from 70 to 171 and thus approximate the range of the number of Goals Scored is 171-70=101.
#OUTLIERS:Outliers are observations that fall outside the overall pattern.So after looking at the boxplot there are no Outliers.
par(mfrow=c(2,2))
ss5<-sapply(0:500,function(x) mean(sample(WorldCups$GoalsScored,5))) hist(ss5,prob=TRUE, col = "red")
ss10<-sapply(0:500,function(x) mean(sample(WorldCups$GoalsScored,10))) hist(ss10,prob=TRUE, col = "green")
ss15<-sapply(0:500,function(x) mean(sample(WorldCups$GoalsScored,15))) hist(ss15,prob=TRUE, col = "blue")
s<-5000
n<-5 means<-rep(NA,s) for (i in 1:s){
d<-sample(WorldCups$GoalsScored,15)
means[i]<-mean(d)
}
hist(means) abline(v=mean((WorldCups$GoalsScored),ity=2,lwd=3,col= "blue"))
s<-srswor(nrow(WorldCups)/5,nrow(WorldCups))
hist(WorldCups[s==1,'GoalsScored'],cex=1.5,cex = 1.5,main=
'GOAL SCORED Simple Sampling',xlab = 'Number of Goals',xaxp=c(0,200,5))
z <- srswor(15,nrow(WorldCups)) z <- WorldCups[z != 0,] table(z$GoalsScored) prop.table(table(z$GoalsScored))
#symmetric sampling
N <- nrow(WorldCups)
n <- 15
k <- ceiling(N / n)
r <- sample(k,1)
k <- seq(r,by=k,length=n) k <- WorldCups[k,] table(k$GoalsScored) prop.table(k$GoalsScored)
hist(WorldCups$GoalsScored,cex=1.5,cex = 1.5,main=
'GOAL SCORED Systematic ',xlab = 'Number of Goals',xaxp=c(0,200,5))
# startified sampling
WorldCups<-WorldCups[order(WorldCups$GoalsScored),] freq<-table(WorldCups$GoalsScored) size<-20*freq/sum((freq))
size<-round(size)
size<-as.vector(size)
size <- size[size != 0] sample.s<-strata(WorldCups,stratanames =
"GoalsScored",size = size,method = "srswor",description=TRUE)
table(sample.s$GoalsScored) prop.table(table(sample.s$ID_unit))
hist(W orldCups[sample.s$ID_unit,'GoalsScored'],
xlab = 'Number of Goals',main="Goal Scored Stratified Sampling")



par(mfrow=c(3,3))
for(i in 1:length(unique(WorldCups$Winner))){
hist(WorldCups[WorldCups$Winner==unique(WorldCups$Winner)[i],'GoalsScored'], xaxs='i',yaxs='i',prob=T,main=unique(WorldCups$Winner)[i],)
}
