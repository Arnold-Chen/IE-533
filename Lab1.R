#download data
furl<-'http://www.stat.berkeley.edu/~statlabs/data/video.data'
download.file(furl,'video.data')
dataset<-read.table('video.data',head=TRUE,stringsAsFactors = FALSE)
summary(dataset)
library(ggplot2)
#0.Data clean
par(mfcol=(c(2,4)))
for(i in c(1:4,8,11,15)){
        y<-dataset[i]
        qqnorm(y[,1],main=paste('QQPlot of',names(y)))
        qqline(y[,1],col='red')        
}
#1.Estimate proportion
##Point proportion
play<-dataset$time[which(dataset$time!=99)]
playnum<-sum(play>0)
n<-length(play)
propplay<-playnum/n
###Visualization
play<-as.data.frame(play)
play$group<-ifelse(play$play>0,c('Yes'),c('No'))
bar <- ggplot(play, aes(x = factor(1), 
                        fill = group)) + geom_bar(width = 1,position = 'fill')
pie <- bar + coord_polar(theta = "y")+labs(title='Pie chart',x='',y='')
pie
##Interval proportion
SE<-sqrt(propplay*(1-propplay)/n)
E<-SE*qnorm(0.975)
c(-E,E)+propplay
#2.Compare time and how often
##Scatter plot
compr<-dataset[,c('time','freq')]
compr<-compr[which(compr$time!=99 & compr$freq!=99),]
a<-ggplot(compr,aes(freq,time))
a+geom_point(color='blue',alpha=0.3)
##Pie chart
compr$group<-ifelse(compr$time>0,c('Yes'),c('No'))
b<-ggplot(compr, aes(x=factor(1),fill=group))
bar <- b+ geom_bar(width = 1,position = 'fill')+facet_grid(. ~ freq)
pie <- bar + coord_polar(theta = "y")+labs(title='Pie chart',x='',y='')
pie
mytable<-table(compr$freq,compr$group)
mytable.p<-prop.table(mytable,1)
##Chi-test
chisq.test(mytable.p)
my#3.Average time
##Normal assumption
nm.ci<-function(x,alpha=0.95){
        aver<-mean(x)
        n<-length(x)
        std<-sd(x)
        error<-std*qnorm(0.5+alpha/2)/sqrt(n)
        c(-error,error)+aver
}
nm.ci(play$play)
for (i in c(1,2,3,4)){
        a<-nm.ci(compr$time[which(compr$freq==i)])
        a
        print(i)
        print(a)
}
##Chenk normality
graphics.off()
qqnorm(play$play)
qqline(play$play,col='red')
par(mfrow=c(1,4))
for (i in c(1,2,3,4)){
        y<-compr$time[compr$freq==i]
        qqnorm(y,main=paste0('Q-Q Plot for freq=',i))
        qqline(y,col='red')
}
##Non-normal interval
###Clean outliner
clean.compr<-compr[c(which(compr$freq==1 & compr$time<10),
                     which(compr$freq==2 & compr$time<10),
                     which(compr$freq==3 & compr$time<0.4),
                     which(compr$freq==4 & compr$time<0.4)),]
nnm.ci<-function(x,alpha=0.95){
        aver<-mean(x)
        n<-length(x)
        std<-sd(x)
        error<-std*qt(0.5+alpha/2,df=n-1)/sqrt(n)
        c(-error,error)+aver
}
nnm.ci(play$play[play<10])
for (i in c(1,2,3,4)){
        a<-nnm.ci(clean.compr$time[which(clean.compr$freq==i)])
        a
        print(i)
        print(a)
}
#4.Compare like and dislike
library(dplyr)
subdata<-dataset[c('like','sex','work','own','grade')]
subdata<-filter_all(subdata, all_vars(. !=99))
subdata<-filter(subdata,like!=1)
subdata$like<-as.numeric(subdata$like)
subdata$like<-ifelse(subdata$like==2,'Like',
       ifelse(subdata$like==3,'Like',
              ifelse(subdata$like==4,'Not like',
                     ifelse(subdata$like==5,'Not like',''))))
subdata$like<-as.factor(subdata$like)
subdata$sex<-ifelse(subdata$sex==0,'female','male')
subdata$work<-ifelse(subdata$work>0,'Yes','No')
subdata$own<-ifelse(subdata$own==0,'No','Yes')
subdata$grade<-ifelse(subdata$grade==4,'A','Not A')
for(i in c(2:5)){
        mytable<-table(subdata[[i]],subdata[[1]])
        mytable.p<-prop.table(mytable,1)
        Total<-as.vector(margin.table(mytable, 1))
        result<-cbind(mytable.p,Total)
        write.table(result,
                    file = 'result.txt',append=TRUE,quote = FALSE)
        t<-chisq.test(mytable)
        write.table(capture.output(t), 
                    file = 'result.txt',append=TRUE,quote = FALSE)
}
#5.Grade
grade<-dataset$grade[dataset$grade!=99]
grade<-ifelse(grade==4,'A',
              ifelse(grade==3,'B',
                     ifelse(grade==2,'C',ifelse(grade==1,'D','F'))))
mytable.p<-prop.table(table(grade))
mytable.p<-as.data.frame(mytable.p)
chisq.test(mytable$Freq,c(0.2,0.3,0.4))
mytable$grade<-as.character(mytable$grade)
newtable<-rbind(mytable,c('D/F',4))
chisq.test(newtable$Freq,c(0.2,0.3,0.4,0.1))
