####set up####
dat = read.csv("scored 11_25.csv")
options(scipen = 999) ##turn off scientific notation

#load libraries and custom functions
library(ez)
library(mice)
library(reshape)

percentmiss = function(x){ sum(is.na(x)/length(x)) * 100}
source("pairwise_t.R")

####Data Screening####
summary(dat)
table(dat$ExperimentName)

#get n
length(unique(dat$Subject))

#fix listnum
dat$ListNum2 = rep(1:160)
dat$ListNum3 = rep('q')
dat$ListNum.final = paste(dat$ListNum3, dat$ListNum2, sep = "")

dat = dat[ , -c(13)]

#add block
dat$block = c(rep(1, times = 80), rep(2, times = 80))

#remove pairs that don't match well enough
dat2 = subset(dat,
             dat$percent_match <= 250)

nrow(dat2)/nrow(dat) #lost five percent of trials

#check for outliers
#First remove out of range JOLs
##out of range values
summary(dat2$JOL)
#we have 153 Missing JOLs

#need to remove missing values first
nomiss = na.omit(dat2)
nrow(nomiss)/nrow(dat2)

##going to cut out anyone three SDs above or below
##JOL
wide.dat = cast(nomiss, Subject ~ block, mean, value = 'JOL')
wide.dat$meanJOL = apply(wide.dat, 1, mean)
wide.dat$ZJOL = scale(wide.dat$meanJOL)

##Recall
wide.dat2 = cast(nomiss, Subject ~ block, mean, value = 'scored')
wide.dat2$meanRECALL = apply(wide.dat2, 1, mean)
wide.dat2$ZRECALL = scale(wide.dat2$meanRECALL)

##Combined Wide data
wide.final = cbind(wide.dat, wide.dat2)
wide.final = wide.final[ , -c(2:3, 6:8)]

#no zs above 3 or below -3

##checking distributions
hist(wide.dat$meanJOL)
hist(wide.final$meanRECALL)

####Get Descriptives####
summary(nomiss)

mean(nomiss$JOL)
mean(nomiss$scored)

tapply(nomiss$JOL,
       nomiss$Direction, mean)
tapply(nomiss$scored,
       nomiss$Direction, mean) * 100

nomiss$scored = nomiss$scored * 100

#combine by score type
long.dat = melt(nomiss, id = c("Subject", 'ExperimentName', "percent_match", "recall_cue", "Recall_Response",
                               "ListNum2", "ListNum.final", 'sorted_JOL_CUE', "sorted_JOL_TARGET",
                               "block", "JOL_RT", "Direction"))

summary(long.dat)

colnames(long.dat)[13] = "Task"
colnames(long.dat)[14] = "Score"

#get descriptives for one-way
tapply(long.dat$Score,
       long.dat$Direction, mean)

#run some ANOVAS!
#difference between JOLs and Recall
model = ezANOVA(data = long.dat,
        wid = Subject,
        within = Task,
        dv = Score,
        type = 3,
        detailed = T)

anovaLength = length(model$ANOVA)
model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

#difference within JOLs and recall across directions
model = ezANOVA(data = long.dat,
        wid = Subject,
        within = Direction,
        dv = Score,
        type = 3,
        detailed = T)

anovaLength = length(model$ANOVA)
model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

##2 (Task Type) X 4 (Associative Direction)
model = ezANOVA(data = long.dat,
                wid = Subject,
                within = .(Task, Direction),
                dv = Score,
                type = 3, detailed = T)

anovaLength = length(model$ANOVA)
model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

anovaLength = length(model$ANOVA)
model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

####Updated Post-hocs####
tapply(long.dat$Score,
       long.dat$Task, mean)
tapply(long.dat$Score,
       long.dat$Task, sd)

library(reshape)

#newdat = cast(long.dat, Subject ~ Task, mean)

means = tapply(long.dat$Score, list(long.dat$Direction, long.dat$Task), mean)
sds = tapply(long.dat$Score, list(long.dat$Direction, long.dat$Task), sd)

recall = subset(long.dat,
                long.dat$Task == "Recall")
jol = subset(long.dat,
             long.dat$Task == "JOL")

recall2 = cast(recall[ , -6], Subject ~ Direction, mean)
jol2 = cast(jol[ , -6], Subject ~ Direction, mean)

colnames(recall2)[3] = "f"
colnames(jol2)[3] = "f"

summary(jol2)
summary(recall2)

##get SEM
temp = t.test(jol2$f, recall2$f, paired = T, p.adjust.methods = "bonferroni")
p1 = round(temp$p.value, 3)
t1 = temp$statistic
SEM1 = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp =  t.test(jol2$B, recall2$B, paired = T)
p2 = round(temp$p.value, 3)
t2 = temp$statistic
SEM2 = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(jol2$S, recall2$S, paired = T)
p3 = round(temp$p.value, 3)
t3 = temp$statistic
SEM3 = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(jol2$U, recall2$U, paired = T)
p4 = round(temp$p.value, 3)
t4 = temp$statistic
SEM4 = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

output = matrix(NA, nrow = 4, ncol = 8)
colnames(output) = c("Direction", "Mean JOL", "Mean Recall",
                     "JOL SD", "Recall SD", "T value", "P value", "SEM")

output[1, ] = c("F", means[2,1], means[2,2], sds[2,1], sds[2,2], t1, p1, SEM1)
output[2, ] = c("B", means[1,1], means[1,2], sds[1,1], sds[1,2], t2, p2, SEM2)
output[3, ] = c("S", means[3,1], means[3,2], sds[3,1], sds[3,2], t3, p3, SEM3)
output[4, ] = c("U", means[4,1], means[4,2], sds[4,1], sds[4,2], t4, p4, SEM4)

#write.csv(output, file = "ex 3 post hocs.csv", row.names = FALSE)

#FIX POST HOCS FOR JOLS/RECALL ACROSS DIRECTION GROUPS
dat2 = cast(long.dat, Subject ~ Direction, mean)

mean(dat2$B)
mean(dat2$F)
mean(dat2$S)
mean(dat2$U)

sd(dat2$B)
sd(dat2$F)
sd(dat2$S)
sd(dat2$U)

temp = t.test(dat2$S, dat2$U, paired = T, p.adjust.methods = "bonferroni")
p1 = round(temp$p.value, 3)
t1 = temp$statistic
SEM1 = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

p1;t1;SEM1
