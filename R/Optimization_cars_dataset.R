#optimization ----
d <- NULL
b1 <- 3.932
#b1 <- -50
#while(b1 <- 50) {

b0 <- -50
#maam took while(b0<30) in the session
while(b0<50) {
  e <- sum((cars$dist - b0 - b1*cars$speed)^2)
  df <- data.frame(beta=b0,error=e)
  d <- rbind(d,df)
  b0 = b0 + 0.5
}
  #b1 = b1 + 1
# }
library(ggplot2)
p <- ggplot(d,aes(beta,error)) + geom_point(size=0.1) + labs(title='Error Curve',x='b0',y='error')
print(p)
print(d)
optb0 <- d[which(d$error == min(d$error)),]
print(optb0)

#displaying the minimum point ----
xval <- optb0[1,1]
yval <- optb0[1,2]
pt <- c(xval,yval)
print(pt)