library(ltm)

dat = read.csv("test.csv")

cronbach.alpha(dat[ , c(11,12)])
cor(dat[ , c(11,12)])
