seeds<-as.vector(read.table("sim_seed.txt"))
results<-NULL
for (i in 1:1000){
  set.seed(seeds[i,])
  source("Generate_S3.R")
#################### g1 correctly specified  ##########
data.g1.a1<-data.frame(BSD$a1,BSD$a2,BSD$a3,BSD$v1,BSD$w1,BSD$r1,BSD$d1)
g1.a1.glm<-glm(BSD.a1~BSD.v1+BSD.w1+BSD.a2+BSD.a3,family="binomial",data=data.g1.a1[which(BSD$r1==0),])
g1.a1.pre<-predict(g1.a1.glm,newdata=data.frame(BSD$v1,BSD$w1,BSD$a2,BSD$a3),type="response")
##################### g2 correctly specified  ###########
# g2.a1.glm<-glm(d1_bar~v1_bar,family="binomial")
# g2.a1.pre<-predict(g2.a1.glm,type="response")
g2.a1.pre<-rep(1,each=n)

g.a1.pre_all<-g1.a1.pre*g2.a1.pre
g.a1.pre<-g.a1.pre_all[which(BSD$r1==0 & BSD$d1==1)]
##################### Q correctly specified ###########
interaction_a2<-BSD$a2*(1-BSD$r2)
interaction_a3<-BSD$a3*(1-BSD$r3)
data.Q1.a1<-data.frame(BSD$y0,BSD$a1,BSD$a2,BSD$a3,BSD$v1,BSD$w1,BSD$r1,BSD$r2,BSD$r3,
                       BSD$study_id,interaction_a2,interaction_a3)[which(BSD$r1==0 & BSD$a1==1),]
newdata<-data.frame(BSD$a1,BSD$v1,BSD$w1,interaction_a2,interaction_a3)[which(BSD$r1==0 & BSD$d1==1),]  
Q1.a1.glm<-glm(BSD.y0~BSD.a1+BSD.v1+BSD.w1+interaction_a2+interaction_a3,family="binomial",data=data.Q1.a1)
Q1.a1.pre<-predict(Q1.a1.glm,newdata=newdata,type="response")
##################### TMLE #################
#(1)
y.a1 <- BSD$y0[BSD$r1==0 & BSD$d1==1]
h.a1<-as.numeric(as.logical(BSD$a1==1)[BSD$r1==0 & BSD$d1==1])/g.a1.pre
e.glm.a1<-glm(y.a1~-1+h.a1,offset=logit(Q1.a1.pre),family="binomial")
e.a1<- coef(e.glm.a1)["h.a1"]
#(2)
up.logitQ.a1<-logit(Q1.a1.pre)+(e.a1/g.a1.pre)
up.Q.a1<-ilogit(up.logitQ.a1)
#(3)
tau.Q.a1<-mean(up.Q.a1)
######### SD estimation ##############
diff.yQ.a1<- y.a1-up.Q.a1
ic_a1<-h.a1*diff.yQ.a1+up.Q.a1-tau.Q.a1
ic_a1.study<-split(ic_a1,study_id[BSD$r1==0 & BSD$d1==1])
var.san.a1<-sum(unlist(lapply(ic_a1.study,FUN=function(x) sum(x %*% t(x)))))/(length(BSD$a1[which(BSD$r1==0 & BSD$d1==1)])[1])^2
sd.san.a1<-sqrt(var.san.a1)
ci.san.a1<-c(tau.Q.a1-1.96*sqrt(var.san.a1),tau.Q.a1+1.96*sqrt(var.san.a1))
coverage<-as.numeric(as.logical(0.72>ci.san.a1[1])&0.72<ci.san.a1[2])
######### SD estimation without clustering ##############
diff.yQ.a1<- y.a1-up.Q.a1
ic_a1<-h.a1*diff.yQ.a1+up.Q.a1-tau.Q.a1
var.san.a1.w<-sum(ic_a1^2)/((length(BSD$a1[which(BSD$r1==0 & BSD$d1==1)])[1])^2)
sd.san.a1.w<-sqrt(var.san.a1.w)
ci.san.a1.w<-c(tau.Q.a1-1.96*sqrt(var.san.a1.w),tau.Q.a1+1.96*sqrt(var.san.a1.w))
coverage.w<-as.numeric(as.logical(0.72>ci.san.a1.w[1])&0.72<ci.san.a1.w[2])

result<-c(tau.Q.a1,sd.san.a1,sd.san.a1.w,coverage,coverage.w)
results<-rbind(results,result)
}
#mean(est)
mean(results[,1],na.rm=TRUE)
#Monte-Carlo SE
sd(results[,1],na.rm=TRUE)
#mean(SE clustered)
mean(results[,2],na.rm=TRUE)
#mean(SE iid)
mean(results[,3],na.rm=TRUE)
#clustered coverage
mean(results[,4],na.rm=TRUE)
#iid coverage
mean(results[,5],na.rm=TRUE)
