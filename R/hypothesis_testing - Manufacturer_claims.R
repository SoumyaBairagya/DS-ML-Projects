#ABHA MARATHE1:13 PM
#Suppose the manufacturer claims that the mean lifetime of a ball bearing is 10000hours.
#The auditing team stated that the mean lifetime is less than what is claimed.
#On the basis of a randomly chosen sample of 50 ball bearings as given in the dataset, at 0.05 significance level, can we reject the claim of the manufacturer?
#What will be your interpretation if the significance level is made as 0.01?

m = 10000 
f1 <- read.csv("hypothesis_testing.csv", header = T)
x = mean(f1$Life_Hrs) 
n <- 50 #since given 50 samples

p=x/n
sd = sd(f1$Life_Hrs)
SE = sd/sqrt(50)

p <- pnorm(x,m,SE)
cat("Value of p:",p)


cat("\nFor Significance level 0.05")
if(p<0.05)
{
  cat("\nNull Hypothesis rejected since p-value is:",p)
}else
{
  cat("\nNull Hypothesis claimed with p-value:",p)
}

cat("\nFor Significance level 0.01")
if(p<0.01)
{
  cat("\nNull Hypothesis rejected since p-value is:",p)
}else
{
  cat("\nNull Hypothesis claimed with p-value:",p)
}
