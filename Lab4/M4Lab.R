# ST 517 - Module 4 Lab Reconn

# MLB


library(dplyr)
MLB <- select(MLB, R, HR, BB, SO)
MLB <- MLB[-c(4, 15, 26),]
write.csv(MLB, file = "MLB.csv")

lm(R ~ HR + BB + SO, data = MLB))

MLB <- read.csv("MLB.csv") # Tidied data saved for students


# Lab 3 =========================================


# Getting Started ============================
setwd("~/Desktop/ST 517/Week4/Lab")
Age <- read.csv("Age.csv", row.name = 1) # "row.name = 1" removes redundant index column
head(Age)
str(Age)

library(ggplot2)
qplot(x = HAge, y = WAge, data = Age) 

fit <- lm(WAge ~ HAge, data = Age)
summary(fit)
confint(fit)
predict(fit, interval = "confidence")

qplot(x = HAge, y = WAge, data = Age) + geom_smooth(method = "lm")

# "Goodness of Fit" stuff =========================

anova(lm(WAge ~ HAge, data = HWData), lm(WAge ~ HAge - 1, data = HWData))
summary(fit)
anova(fit)
sqrt(15.6)

# The .mystery solved!! ==========================
fit <- lm(WAge ~ HAge, data = HWData)
fortify(fit) # look at all the .variablename columns!
# ...this is what qplot is using when the data is not coming from a data frame;
# ...it uses this variable, from the data frame fortify creates internally
# ... the "." is not a function/verb, just a really uncommon start to a variable name, so the package author used it

head(fortify(fit))
  WAge HAge        .hat   .sigma     .cooksd  .fitted    .resid  .stdresid
1   43   49 0.007466122 3.954902 0.002524499 46.22485 -3.224846 -0.8192713
2   28   25 0.019626271 3.952574 0.008689425 24.35505  3.644952  0.9317243
3   30   40 0.006246783 3.913574 0.013043630 38.02367 -8.023672 -2.0371608
4   57   52 0.009413740 3.913195 0.019869959 48.95857  8.041429  2.0449304
5   52   58 0.015620735 3.958306 0.003038913 54.42602 -2.426020 -0.6188774
6   27   32 0.010985131 3.952162 0.005014647 30.73374 -3.733739 -0.9502414

qplot(HAge, .resid, data = fit)

# Use in LaTeX: for distributed iid... $\stackrel{iid}{\sim}$
