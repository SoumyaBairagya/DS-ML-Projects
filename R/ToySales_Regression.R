df1 <- read.csv("toy_sales_csv.csv",header = T)
df1

#SLR
l1 <- lm(Unitsales~Price,df1)
summary(l1)

library(ggplot2)
ggplot(df1,aes(x=Price,y=Unitsales))+geom_point()+geom_smooth(method="lm",formula= y~x,col="red")

pred1 <- predict(l1)
print(pred1)
error <- df1$Unitsales-pred1
print(error)

cbind(df1,pred1,error)

#MLR
l2 <- lm(Unitsales~Price+Adexp+Promexp,df1)
summary(l2)

pred2 <- predict(l2)
pred2

df = data.frame(Price = c(9.1, 8.1), Adexp = c(52, 50), Promexp = c(61, 60))
pred_mlr_toy_sales = predict(l2, df)
df_pred = cbind(df, pred_mlr_toy_sales)
cat('\nThe predicated values of these cases as follows:\n')
print(df_pred)
cat('\nTo get maximum number of unit sales, following case should be applied:\n')
print(df_pred[which(df_pred$pred_mlr_toy_sales == max(df_pred$pred_mlr_toy_sales)), c(1,2,3)])