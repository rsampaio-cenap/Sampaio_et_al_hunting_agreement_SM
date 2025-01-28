library(dplyr)
histograma<-hist(out$BUGSoutput$summary[,"Rhat"], main="Rhat", xlab="", las=1, nclass=20)

sumario<-summary(out$BUGSoutput$summary[,"Rhat"])

DF<-as.data.frame(out$BUGSoutput$summary)



####Species richness and aggrageted abundance
SR<-DF |> filter(grepl("SR", row.names(DF)))
SR<-mean(SR$Rhat)
AB<-DF |> filter(grepl("AB", row.names(DF)))
AB<-mean(AB$Rhat)
BI<-DF |> filter(grepl("BI", row.names(DF)))
BI<-mean(BI$Rhat)
##### Hyperparameter
mu.a1<-DF |> filter(grepl("mu.a1", row.names(DF)))
mu.a1<-mean(mu.a1$Rhat)
##### a2
mu.a2<-DF |> filter(grepl("mu.a2", row.names(DF)))
mu.a2<-mean(mu.a2$Rhat)
##### a3
mu.a3<-DF |> filter(grepl("mu.a3", row.names(DF)))
mu.a3<-mean(mu.a3$Rhat)
##### a1
a<-DF |> filter(grepl("a1", row.names(DF)))
a1<-mean(a$Rhat)
##### a2
a<-DF |> filter(grepl("a2", row.names(DF)))
a2<-mean(a$Rhat)
##### a3
a<-DF |> filter(grepl("a3", row.names(DF)))
a3<-mean(a$Rhat)

##### Detection
r<-DF |> filter(grepl("r1", row.names(DF)))
r1<-mean(r$Rhat)
##### r2
r<-DF |> filter(grepl("r2", row.names(DF)))
r2<-mean(r$Rhat)
##### r1
r<-DF |> filter(grepl("r3", row.names(DF)))
r3<-mean(r$Rhat)

##### Hyperparameter
r<-DF |> filter(grepl("mu.r1", row.names(DF)))
mu.r1<-mean(r$Rhat)
##### r2
r<-DF |> filter(grepl("mu.r2", row.names(DF)))
mu.r2<-mean(r$Rhat)
##### r1
r<-DF |> filter(grepl("mu.r3", row.names(DF)))
mu.r3<-mean(r$Rhat)


par<-c("a1","a2","a3",
       "mu.a1","mu.a2","mu.a3",
       "SR","AB","BI",
       "r1","r2","r3",
       "mu.r1","mu.r2","mu.r3")
mean<-round(c(a1,a2,a3,
              mu.a1,mu.a2,mu.a3,
              SR,AB,BI,
              r1,r2,r3,
              mu.r1,mu.r2,mu.r3),3)
rhat<-data.frame(par,mean)

