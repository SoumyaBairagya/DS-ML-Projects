#Consider 18 points data set referred in theory class. 
#Consider two test samples 1. P1(3,2) and P2(4.2,1.8). 
#Apply following algorithms to find the class of these test points.
#i.	NN
#ii.	KNN with K=3, 5 and 7.
#iii.	MKNN with K=3
#iv.	Radius based algorithm with radius as 1.5 units.
#1.	Consider the data set :
#  X1=(0.8,0.8,1); x2=(1.0,1.0,1); x3=(1.2,0.8,1) x4=(0.8,1.2,1); 
#x5=(1.2,1.2,1); x6=(4.0,3.0,2); x7=(3.8,2.8,2); x8=(4.2,2.8,2); 
#x9=(3.8,3.2,2); x10=(4.2,3.2,2); x11=(4.4,2.8,2); x12=(4.4,3.2,2); 
#x13=(3.2,0.4,3); x14=(3.2,0.7,3); x15=(3.8,0.5,3); x16=(3.5,1.0,3); 
#x17=(4.0,1.0,3); x18=(4.0,0.7,3)

df <- data.frame(X=c(0.8,1.0,1.2,0.8,1.2,4.0,3.8,4.2,3.8,4.2,4.4,4.4,3.2,3.2,3.8,3.5,4.0,4.0),
                 Y=c(0.8,1.0,0.8,1.2,1.2,3.0,2.8,2.8,3.2,3.2,2.8,3.2,0.4,0.7,0.5,1.0,1.0,0.7),
                 PClass=c(1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3))
P1 <- c(3,2)
P2 <- c(4.2,1.8)
distP1 <- sqrt((df$X-P1[1])^2 + (df$Y-P1[2])^2)
distP2 <- sqrt((df$X-P2[1])^2 + (df$Y-P2[2])^2)
d <- cbind(df, distP1)
e <- cbind(df, distP2)
dl <- d[order(d$distP1),]
dl2 <- e[order(e$distP2),]

#Nearest Neighbor ----
#FOR POINT P1
print(d[which(d$distP1==min(d$distP1)),])
cat("Class of P1(3,2) by NN is", d$PClass[which(d$distP1==min(d$distP1))])

#FOR POINT P2
cat("\n\n")
print(e[which(e$distP2==min(e$distP2)),])
cat("Class of P2(4.2,1.8) by NN is",e$PClass[which(e$distP2==min(e$distP2))])


#K-NN ----
#FOR POINT P1
#For K=3
dfm <- dl[1:3,]
lm1 <- length(which(dfm$PClass==1))
lm2 <- length(which(dfm$PClass==2))
lm3 <- length(which(dfm$PClass==3))
lm_count <- c(lm1,lm2,lm3)
cat("\n\nClass of P1(3,2) for K=3 by KNN is",dfm$PClass[which(lm_count==max(lm_count))])

#For K=5
dfn <- dl[1:5,]
ln1 <- length(which(dfn$PClass==1))
ln2 <- length(which(dfn$PClass==2))
ln3 <- length(which(dfn$PClass==3))
ln_count <- c(ln1,ln2,ln3)
cat("\nClass of P1(3,2) for K=5 by KNN is",dfn$PClass[which(ln_count==max(ln_count))])

#For K=7
dfo <- dl[1:7,]
lo1 <- length(which(dfo$PClass==1))
lo2 <- length(which(dfo$PClass==2))
lo3 <- length(which(dfo$PClass==3))
lo_count <- c(lo1,lo2,lo3)
cat("\nClass of P1(3,2) for K=7 by KNN is",dfo$PClass[which(lo_count==max(lo_count))])

#FOR POINT P2
#For K=3
dfm2 <- dl2[1:3,]
lm21 <- length(which(dfm2$PClass==1))
lm22 <- length(which(dfm2$PClass==2))
lm23 <- length(which(dfm2$PClass==3))
lm2_count <- c(lm21,lm22,lm23)
cat("\nClass of P2(4.2,1.8) for K=3 by KNN is",dfm2$PClass[which(lm2_count==max(lm2_count))])

#For K=5
dfn2 <- dl2[1:5,]
ln21 <- length(which(dfn2$PClass==1))
ln22 <- length(which(dfn2$PClass==2))
ln23 <- length(which(dfn2$PClass==3))
ln2_count <- c(ln21,ln22,ln23)
cat("\nClass of P2(4.2,1.8) for K=5 by KNN is",dfn2$PClass[which(ln2_count==max(ln2_count))])

#For K=7
dfo2 <- dl2[1:7,]
lo21 <- length(which(dfo2$PClass==1))
lo22 <- length(which(dfo2$PClass==2))
lo23 <- length(which(dfo2$PClass==3))
lo2_count <- c(lo21,lo22,lo23)
cat("\nClass of P2(4.2,1.8) for K=7 by KNN is",dfo2$PClass[which(lo2_count==max(lo2_count))])


#MKNN ----
#FOR POINT P1
#For K=3
#Consider dfm <- dl[1:3,]
dk1 <- max(dfm$distP1)
di1 <- min(dfm$distP1)
dj1 <- dfm$distP1
Wi1 <- (dk1-dj1[1])/(dk1-di1)
Wi2 <- (dk1-dj1[2])/(dk1-di1)
Wi3 <- (dk1-dj1[3])/(dk1-di1)
Wi <- c(Wi1,Wi2,Wi3)
im <- cbind(dfm,Wi)
P1sumclass1 <- sum(im$Wi[which(im$PClass==1)])
P1sumclass2 <- sum(im$Wi[which(im$PClass==2)])
P1sumclass3 <- sum(im$Wi[which(im$PClass==3)])
P1sumclass <- c(P1sumclass1,P1sumclass2,P1sumclass3)
cat("\nClass of P1 by MKNN is: ",im$PClass[P1sumclass[which(P1sumclass==max(P1sumclass))]])

#FOR POINT P2
#For K=3
#Consider dfm2 <- dl2[1:3,]
dk2 <- max(dfm2$distP2)
di2 <- min(dfm2$distP2)
dj2 <- dfm2$distP2
Wj1 <- (dk2-dj2[1])/(dk2-di2)
Wj2 <- (dk2-dj2[2])/(dk2-di2)
Wj3 <- (dk2-dj2[3])/(dk2-di2)
Wj <- c(Wj1,Wj2,Wj3)
jm <- cbind(dfm2,Wj)
P2sumclass1 <- sum(jm$Wj[which(jm$PClass==1)])
P2sumclass2 <- sum(jm$Wj[which(jm$PClass==2)])
P2sumclass3 <- sum(jm$Wj[which(jm$PClass==3)])
P2sumclass <- c(P2sumclass1,P2sumclass2,P2sumclass3)
cat("\nClass of P2 by MKNN is: ",jm$PClass[P2sumclass[which(P2sumclass==max(P2sumclass))]])

#RNN ----
#with radius as 1.5 units
#FOR POINT P1
#Consider dl
g1 <- dl[dl$distP1<1.5,]
g11 <- length(which(g1$PClass==1))
g12 <- length(which(g1$PClass==2))
g13 <- length(which(g1$PClass==3))
g1_count <- c(g11,g12,g13)
cat("\nClass of point P1(3,2) by RNN is: ",g1$PClass[which(g1_count==max(g1_count))])

#FOR POINT P2
#Consider dl2
g2 <- dl2[dl2$distP2<1.5,]
g21 <- length(which(g2$PClass==1))
g22 <- length(which(g2$PClass==2))
g23 <- length(which(g2$PClass==3))
g2_count <- c(g21,g22,g23)
cat("\nClass of point P2(4.2,1.8) by RNN is: ",g2$PClass[which(g2_count==max(g2_count))])
