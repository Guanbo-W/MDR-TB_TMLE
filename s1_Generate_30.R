# Generate the data--Simulation
# Scenario 1: D is independent
  N=9000;n=300;clusterN=N/n
  study_id<-c(rep(1:clusterN,each=n))
  ind_id<-c(1:N)
  v1_bar<-rnorm(clusterN,mean=0.3,sd=0.3)
  v1<-rep(v1_bar,each=n)
  #r1=1-->resistant to a1
  r1<-rbinom(N,1,0.25)
  r2<-rbinom(N,1,0.30)
  r3<-rbinom(N,1,0.25)
  D <-NULL
  for (i in 1:clusterN)
  {
    #d_index: how many medications are accessible
    d_index <- sample(x=c(1:3),size=1)
    d_zero <- c(0,0,0)
    #d: fill 1 into c(0,0,0), sample the positions of 1s with the number of accessible medications
    d <- replace(d_zero,sample(c(1:3),d_index), 1)
    D <- rbind(D, d)
  }
  d1_bar<-as.numeric(D[,1])
  d1<-rep(d1_bar,each=n)
  d2_bar<-as.numeric(D[,2])
  d2<-rep(d2_bar,each=n)
  d3_bar<-as.numeric(D[,3])
  d3<-rep(d3_bar,each=n)
  w1<-rnorm(N,mean=0.1*v1,sd=0.1)
  #u:study-level radom unmeasured confounding 
  u_bar<-rnorm(clusterN,mean=0.2*v1_bar+0.1,sd=0.5)
  u<-rep(u_bar,each=n)
  
  library(faraway)
  a1_full<-rbinom(N,1,ilogit(-0.75+2.4*v1+1.8*w1-0.1*r1))
  a1<-ifelse(d1==1,a1_full,0)
  a2_full<-rbinom(N,1,ilogit(-1+1*v1+1.7*w1-0.15*r2))
  a2<-ifelse(d2==1,a2_full,0)
  a3_full<-rbinom(N,1,ilogit(-1.5+1.7*v1+1*w1-0.16*r3))
  a3<-ifelse(d3==1,a3_full,0)
  
  y0<-rbinom(N,1,ilogit(-2+3.5*v1+0.3*w1-0.1*u
             +2.2*a1*(1-r1)+0.12*a2*(1-r2)+0.05*a3*(1-r3)))
  
  data_full<-data.frame(ind_id,study_id,v1,r1,r2,r3,d1,d2,d3
                        ,w1,u,a1,a2,a3,y0)
  BSD=data_full

  