#ABHA MARATHE1:38 PM
#The nutrition label on a bag of potato chips says that a one ounce
#(28 gram) serving of potato chips has 130 calories and contains ten grams of fat, with three grams
#of saturated fat. A random sample of 35 bags yielded a sample mean of 134 calories with a standard
#deviation of 17 calories. Is there evidence that the nutrition label does not provide an accurate
#measure of calories in the bags of potato chips

m = 130
n = 35
sd = 17
x = 134
se = sd/sqrt(n)
r <- pnorm(x,m,se,lower.tail = F)
p <- r*2
cat("Value of p:",p)

if(p<0.05)
{
  cat("\nReject null hypothesis with p:",p)
}else
{
  cat("\nDo not reject null hypothesis with p:",p)
}