#Siulation Study_Finding the true value of E(E(Y(A=1)|v1,v2,w1,w2,a2,a3,u,r2,r3)|R1=0)
#scenario 1: d is independent of v
#investigate a1 treatment effect

args=commandArgs();
start=as.numeric(args[4]);
end=as.numeric(args[5]);
for (i in start:end){
N=3*10^(7);n=3*10^(4);clusterN=N/n
study_id<-c(rep(1:clusterN,each=n))
ind_id<-c(1:N)
v1_bar<-rnorm(clusterN,mean=0.3,sd=0.3)
v1<-rep(v1_bar,each=n)
#r1=1-->resistant to a1
r1<-rbinom(N,1,0.25)
r2<-rbinom(N,1,0.30)
r3<-rbinom(N,1,0.25)
#d1=1-->available study for a1
#assume every study has access to 2 different treatments, we force d1 to be 1, 
#and then we draw the second accessable one
library(faraway)
d1_bar <- rep(1,clusterN)
d2_bar <- rbinom(clusterN,1,ilogit(0.5+1.5*v1_bar))
d3_bar <- rbinom(clusterN,1,ilogit(1.5+0.3*v1_bar))
d1<-rep(d1_bar,each=n)
d2<-rep(d2_bar,each=n)
d3<-rep(d3_bar,each=n)

w1<-rnorm(N,mean=0.1*v1,sd=0.1)
#u:study-level radom unmeasured confounding 
u_bar<-rnorm(clusterN,mean=0.2*v1_bar+0.1,sd=0.5)
u<-rep(u_bar,each=n)


a1<-rep(1,N)
a2_full<-rbinom(N,1,ilogit(-1+1*v1+1.7*w1-0.15*r2))
a2<-ifelse(d2==1,a2_full,0)
a3_full<-rbinom(N,1,ilogit(-1.5+1.7*v1+1*w1-0.16*r3))
a3<-ifelse(d3==1,a3_full,0)

y0<-rbinom(N,1,ilogit(-2+3.5*v1+0.3*w1-0.1*u
                      +2.2*a1*(1-r1)+0.12*a2*(1-r2)+0.05*a3*(1-r3)))

data_full<-data.frame(ind_id,study_id,v1,r1,r2,r3,d1,d2,d3
                      ,w1,u,a1,a2,a3,y0)

t=mean(y0[r1==0])
}
devout=paste("/sb/home/guanbo/output/s2_T_3x7_3x4","_",start,"_",end,".csv", sep="")
write.csv(t, devout, row.names = F)

#total sample size:4*10^(7)
#cluster size:4*10^(4)
#value: 0.741600299 