####Set up####
options(scipen = 999)

##load in JOL analyses
dat = read.csv("auto score test.csv")
colnames(dat)[11] = "Recall"

dat$JOL= as.numeric(as.character(dat$JOL))

summary(dat)

##add block
#dat$block = c(rep(1, 80), rep(2, 80))

##put recall and jols on same scale
dat$Recall = (dat$Recall * 100) 

##get number of subjects
length(unique(unlist(dat[c("Subject")])))

summary(dat$JOL)

dat$JOL[ dat$JOL > 100] = NA

dat = na.omit(dat)

##mean percent recall and mean JOL rating
m1 = tapply(dat$Recall, dat$Direction, mean)
m2 = tapply(dat$JOL, dat$Direction, mean)

m1;m2
