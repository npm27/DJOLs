####Setup####
library(reshape)
library(mice)
library(ez)
library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

options(scipen = 999) ##turn off scientific notation

percentmiss = function(x){ sum(is.na(x)/length(x)) * 100}

source("pairwise_t.R")

####Make the pooled data####
##read in the data files
dat1 = read.csv("final data.csv") ##Standard Instructions
dat2 = read.csv("Delayed final data 10 percent.csv") ##Delayed JOLs
dat3 = read.csv("T final data 10 percent.csv") ##Response Deadline
dat4 = read.csv("scored 11_25.csv")

summary(dat1);summary(dat2);summary(dat3);summary(dat4)

##get all the columns to match
#drop extra columns
dat2 = dat2[ , -1]
dat3 = dat3[ , -1]

dat2 = dat2[ , -4] #drop block column
dat3 = dat3[ , -c(4, 6:9)]

#reorder dat2 columns
dat2 = dat2[c(1,2,3,6,4,5)]

#reorder dat3 columns
dat3 = dat3[c(1,2,3,6,4,5)]

#fix dat 4
dat4 = na.omit(dat4)
dat4 = dat4[ , -c(5:10)]
dat4 = dat4[c(2,1,4,5,3)]

colnames(dat4)[2] = "Condition"
colnames(dat4)[4] = "Recall"
colnames(dat4)[3] = "Jol"

#add ex number columns
dat1$ex = rep(1)
dat2$ex = rep(2)
dat3$ex = rep(3)
dat4$ex = rep(4)

##Stick everything together
nomiss = rbind(dat1, dat2, dat3)
summary(nomiss)
nomiss = nomiss[ , -3] #Drop the list number column

#now add ex 4
nomiss = rbind(nomiss, dat4)

#Write the pooled data to a csv for later use
#write.csv(nomiss, file = "Pooled 11_26.csv", row.names = F)

####Analysis Time!####
colnames(nomiss)[3] = "JOL"

#Get recall on the same scale
nomiss$Recall = nomiss$Recall * 100

##melt the data
##check these names
long.dat = melt(nomiss, id = c("Subject", "Condition",
                               "Direction", "ex"))

summary(long.dat)

colnames(long.dat)[5] = "Task"
colnames(long.dat)[6] = "Score"

long.dat$ex = factor(long.dat$ex,
                     levels = c(1, 2, 3, 4),
                     labels = c("ex1", "ex2", "ex3", "ex4"))

long.dat$Score[long.dat$Score > 100] = 100
table(long.dat$Score)

summary(long.dat$Score)

##2 (Task Type) X 4 (Associative Direction)
model = ezANOVA(data = long.dat,
        wid = Subject,
        within = .(Task, Direction),
        dv = Score,
        type = 3,
        detailed = T)

tapply(long.dat$Score, list(long.dat$Direction, long.dat$Task), mean)

#threeway_model <- aov(Score ~ Task + ex + Direction + Score:Task + Task:ex + Score:ex
#          + Task:ex:Direction, data = long.dat)
#summary(threeway_model)

model = ezANOVA(data = long.dat,
                    dv = Score, wid = Subject, 
                    within = .(Task, Direction),
                    between = .(ex),
                    type = 3, detailed = T, return_aov = T)

model

model$ANOVA

anovaLength = length(model$ANOVA)
model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

####Post Hocs####
tapply(long.dat$Score, list(long.dat$Direction, long.dat$Task), mean)

tapply(long.dat$Score, list(long.dat$Task, long.dat$ex), mean)

pairwise.t.test.with.t.and.df(long.dat$Score, long.dat$ex,
                              paired = F, p.adjust.method = 'bonferroni')

####Post Hocs####
##post hocs
tapply(long.dat$Score, list(long.dat$Direction, long.dat$Task), mean)

Jol.t = pairwise.t.test.with.t.and.df(long.dat$Score, long.dat$Direction,
                                      paired = F, p.adjust.method = 'bonferroni')
Jol.t

summary(Jol.t)

print(Jol.t)[[5]] #T-values
print(Jol.t)[[6]] #df

R.t = pairwise.t.test.with.t.and.df(nomiss$Recall, nomiss$Direction,
                                    paired = F, p.adjust.method = 'none')
R.t

print(R.t)[[5]] #T-values
print(R.t)[[6]] #df

#differences across experiments
jol = subset(long.dat, long.dat$Task == "JOL")
recall_stuff = subset(long.dat, long.dat$Task == "Recall")

tapply(jol$Score, list(jol$Direction, jol$ex), mean)

tapply(long.dat$Score, list(long.dat$ex, long.dat$Task), mean)

pairwise.t.test.with.t.and.df(jol$Score,jol$ex,
                              paired = F, p.adjust.method = 'bonferroni')

x = tapply(long.dat$Score,
           list(long.dat$Subject, long.dat$ex), mean)

x = as.data.frame(x)

x = melt(x)
x = na.omit(x)

output1 = pairwise.t.test.with.t.and.df(x$value,x$variable,
                              paired = F, p.adjust.method = 'none')
output1[[5]]

x2 = subset(x,
            x$variable == "ex2")
x3 = subset(x,
            x$variable == "ex3")

temp = t.test(x2$value, x3$value, paired = F)
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

tapply(x$value, x$variable, mean)
tapply(x$value, x$variable, sd)

##SEM for forward and backward pairs
nomiss.f = subset(nomiss, nomiss$Direction == "F")
nomiss.b = subset(nomiss, nomiss$Direction == "B")

temp = t.test(nomiss.f$JOL, nomiss.f$Recall, paired = T)
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #.662
temp

####Bar Chart####
bar4 = ggplot(long.dat, aes(Direction, Score, fill = Task))
bar4 = bar4 +
  stat_summary(fun.y = mean, 
               geom = "bar",
               position = "dodge",
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2,
               color = "black") +
  scale_fill_manual("Task",
                    values = c("JOL" = "white",
                               "Recall" = "dimgray")) +
  cleanup +
  xlab("Direction") +
  ylab("Mean % JOL/Recall") +
  ylim(0,100)
bar4

##uSE THE NOMISS DATA TO MAKE POOLED CONF PLOTS
summary(nomiss)

#make direction subsets
f = subset(nomiss, nomiss$Direction == 'F')
b = subset(nomiss, nomiss$Direction == 'B')
s = subset(nomiss, nomiss$Direction == 'S')
u = subset(nomiss, nomiss$Direction == 'U')

#make binned subsets
#forward
f0 = subset(f, f$JOL == 0)
f0$bin = rep(0)
f10 = subset(f, f$JOL == 10)
f10$bin = rep(10)
f20 = subset(f, f$JOL == 20)
f20$bin = rep(20)
f30 = subset(f, f$JOL == 30)
f30$bin = rep(30)
f40 = subset(f, f$JOL == 40)
f40$bin = rep(40)
f50 = subset(f, f$JOL == 50)
f50$bin = rep(50)
f60 = subset(f, f$JOL == 60)
f60$bin = rep(60)
f70 = subset(f, f$JOL == 70)
f70$bin = rep(70)
f80 = subset(f, f$JOL == 80)
f80$bin = rep(80)
f90 = subset(f, f$JOL == 90)
f90$bin = rep(90)
f100 = subset(f, f$JOL == 100)
f100$bin = rep(100)

#backward
b0 = subset(b, b$JOL == 0)
b0$bin = rep(0)
b10 = subset(b, b$JOL == 10)
b10$bin = rep(10)
b20 = subset(b, b$JOL == 20)
b20$bin = rep(20)
b30 = subset(b, b$JOL == 30)
b30$bin = rep(30)
b40 = subset(b, b$JOL == 40)
b40$bin = rep(40)
b50 = subset(b, b$JOL == 50)
b50$bin = rep(50)
b60 = subset(b, b$JOL == 60)
b60$bin = rep(60)
b70 = subset(b, b$JOL == 70)
b70$bin = rep(70)
b80 = subset(b, b$JOL == 80)
b80$bin = rep(80)
b90 = subset(b, b$JOL == 90)
b90$bin = rep(90)
b100 = subset(b, b$JOL == 100)
b100$bin = rep(100)

#symmetrical
s0 = subset(s, s$JOL == 0)
s0$bin = rep(0)
s10 = subset(s, s$JOL == 10)
s10$bin = rep(10)
s20 = subset(s, s$JOL == 20)
s20$bin = rep(20)
s30 = subset(s, s$JOL == 30)
s30$bin = rep(30)
s40 = subset(s, s$JOL == 40)
s40$bin = rep(40)
s50 = subset(s, s$JOL == 50)
s50$bin = rep(50)
s60 = subset(s, s$JOL == 60)
s60$bin = rep(60)
s70 = subset(s, s$JOL == 70)
s70$bin = rep(70)
s80 = subset(s, s$JOL == 80)
s80$bin = rep(80)
s90 = subset(s, s$JOL == 90)
s90$bin = rep(90)
s100 = subset(s, s$JOL == 100)
s100$bin = rep(100)

#unrelated
u0 = subset(u, u$JOL == 0)
u0$bin = rep(0)
u10 = subset(u, u$JOL == 10)
u10$bin = rep(10)
u20 = subset(u, u$JOL == 20)
u20$bin = rep(20)
u30 = subset(u, u$JOL == 30)
u30$bin = rep(30)
u40 = subset(u, u$JOL == 40)
u40$bin = rep(40)
u50 = subset(u, u$JOL == 50)
u50$bin = rep(50)
u60 = subset(u, u$JOL == 60)
u60$bin = rep(60)
u70 = subset(u, u$JOL == 70)
u70$bin = rep(70)
u80 = subset(u, u$JOL == 80)
u80$bin = rep(80)
u90 = subset(u, u$JOL == 90)
u90$bin = rep(90)
u100 = subset(u, u$JOL == 100)
u100$bin = rep(100)

#put it all back together
f2 = rbind(f0,f10,f20,f30,f40,f50,f60,f70,f80,f90,f100)
b2 = rbind(b0,b10,b20,b30,b40,b50,b60,b70,b80,b90,b100)
s2 = rbind(s0,s10,s20,s30,s40,s50,s60,s70,s80,s90,s100)
u2 = rbind(u0,u10,u20,u30,u40,u50,u60,u70,u80,u90,u100)

combined = rbind(f2, b2, s2, u2)

#write.csv(combined, file = "pooled_conf binned 11_26.csv", row.names = F)
