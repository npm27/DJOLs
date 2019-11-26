#write a script to make set up for scoring easier
####set things up####
#load libraries
library(reshape)
library(dplyr)

#read in data
dat = read.csv("raw 10_8.csv")

summary(dat)

#subset the data by experiment version
A = subset(dat,
           dat$ExperimentName == "D_JOL A 2")
B = subset(dat,
           dat$ExperimentName == "D_JOL B 2")
C = subset(dat,
           dat$ExperimentName == "D_JOL C 2")
D = subset(dat,
           dat$ExperimentName == "D_JOL D 2")

####Start with version A!####
##First remove buffer trials
A = subset(A,
           A$ListNum > 5)

summary(A)

##Next, get things sorted
#By list number
A = A[order(A$ListNum), ]

#By sub ID
A = A[order(A$Subject), ]

#By Block
A = A[order(A$Running.Block.), ]

##Now separate jol and recall trials
A.recall = subset(A,
                  A$Procedure.Trial. == "recallproc1" | A$Procedure.Trial. == "recallproc2")
A.JOL = subset(A,
               A$Procedure.Trial. == "StudyProc3" | A$Procedure.Trial. == "StudyProc5")

##combine columns
A = cbind(A.JOL, A.recall)

#remove NA columns
A = A[ , -c(7:8)]

#subset on block
A1 = subset(A,
            A$Running.Block. == "Block1")
A2 = subset(A,
            A$Running.Block. == "Block2")

#rename columns
colnames(A1)[10] = "JOL"
colnames(A1)[11] = "JOL_RT"
colnames(A1)[27] = "Recall_prompt"
colnames(A1)[34] = "Response"
colnames(A1)[35] = "Response_RT"

#cut unused columns
A1 = A1[ , -c(12:17, 19:26, 28:33, 36:38)]

#now do the same for A2
#rename columns
colnames(A2)[12] = "JOL"
colnames(A2)[13] = "JOL_RT"
colnames(A2)[27] = "Recall_prompt"
colnames(A2)[36] = "Response"
colnames(A2)[37] = "Response_RT"

#cut unused columns
A2 = A2[ , -c(10:11, 14:17, 19:26, 28:35, 38)]

##put the two blocks back together
A = rbind(A1, A2)

####Now do the same for version B####
##First remove buffer trials
B = subset(B,
           B$ListNum > 5)

summary(B)

##Next, get things sorted
#By list number
B = B[order(B$ListNum), ]

#By sub ID
B = B[order(B$Subject), ]

#By Block
B = B[order(B$Running.Block.), ]

##Now separate jol and recall trials
B.recall = subset(B,
                  B$Procedure.Trial. == "recallproc1" | B$Procedure.Trial. == "recallproc2")
B.JOL = subset(B,
               B$Procedure.Trial. == "StudyProc3" | B$Procedure.Trial. == "StudyProc5")

##combine columns
B = cbind(B.JOL, B.recall)

#remove NB columns
B = B[ , -c(7:8)]

#subset on block
B1 = subset(B,
            B$Running.Block. == "Block1")
B2 = subset(B,
            B$Running.Block. == "Block2")

#rename columns
colnames(B1)[10] = "JOL"
colnames(B1)[11] = "JOL_RT"
colnames(B1)[27] = "Recall_prompt"
colnames(B1)[34] = "Response"
colnames(B1)[35] = "Response_RT"

#cut unused columns
B1 = B1[ , -c(12:17, 19:26, 28:33, 36:38)]

#now do the same for B2
#rename columns
colnames(B2)[12] = "JOL"
colnames(B2)[13] = "JOL_RT"
colnames(B2)[27] = "Recall_prompt"
colnames(B2)[36] = "Response"
colnames(B2)[37] = "Response_RT"

#cut unused columns
B2 = B2[ , -c(10:11, 14:17, 19:26, 28:35, 38)]

##put the two blocks back together
B = rbind(B1, B2)

####Now do the same for version C####
##First remove buffer trials
C = subset(C,
           C$ListNum > 5)

summary(C)

##Next, get things sorted
#By list number
C = C[order(C$ListNum), ]

#By sub ID
C = C[order(C$Subject), ]

#By Block
C = C[order(C$Running.Block.), ]

##Now separate jol and recall trials
C.recall = subset(C,
                  C$Procedure.Trial. == "recallproc1" | C$Procedure.Trial. == "recallproc2")
C.JOL = subset(C,
               C$Procedure.Trial. == "StudyProc3" | C$Procedure.Trial. == "StudyProc5")

##combine columns
C = cbind(C.JOL, C.recall)

#remove NA columns
C = C[ , -c(7:8)]

#subset on block
C1 = subset(C,
            C$Running.Block. == "Block1")
C2 = subset(C,
            C$Running.Block. == "Block2")

#rename columns
colnames(C1)[10] = "JOL"
colnames(C1)[11] = "JOL_RT"
colnames(C1)[27] = "Recall_prompt"
colnames(C1)[34] = "Response"
colnames(C1)[35] = "Response_RT"

#cut unused columns
C1 = C1[ , -c(12:17, 19:26, 28:33, 36:38)]

#now do the same for B2
#rename columns
colnames(C2)[12] = "JOL"
colnames(C2)[13] = "JOL_RT"
colnames(C2)[27] = "Recall_prompt"
colnames(C2)[36] = "Response"
colnames(C2)[37] = "Response_RT"

#cut unused columns
C2 = C2[ , -c(10:11, 14:17, 19:26, 28:35, 38)]

##put the two blocks back together
C = rbind(C1, C2)

####Now do the same for D####
##First remove buffer trials
D = subset(D,
           D$ListNum > 5)

summary(D)

##Next, get things sorted
#By list number
D = D[order(D$ListNum), ]

#By sub ID
D = D[order(D$Subject), ]

#By Block
D = D[order(D$Running.Block.), ]

##Now separate jol and recall trials
D.recall = subset(D,
                  D$Procedure.Trial. == "recallproc1" | D$Procedure.Trial. == "recallproc2")
D.JOL = subset(D,
               D$Procedure.Trial. == "StudyProc3" | D$Procedure.Trial. == "StudyProc5")

##combine columns
D = cbind(D.JOL, D.recall)

#remove NA columns
D = D[ , -c(7:8)]

#subset on block
D1 = subset(D,
            D$Running.Block. == "Block1")
D2 = subset(D,
            D$Running.Block. == "Block2")

#rename columns
colnames(D1)[10] = "JOL"
colnames(D1)[11] = "JOL_RT"
colnames(D1)[27] = "Recall_prompt"
colnames(D1)[34] = "Response"
colnames(D1)[35] = "Response_RT"

#cut unused columns
D1 = D1[ , -c(12:17, 19:26, 28:33, 36:38)]

#now do the same for B2
#rename columns
colnames(D2)[12] = "JOL"
colnames(D2)[13] = "JOL_RT"
colnames(D2)[27] = "Recall_prompt"
colnames(D2)[36] = "Response"
colnames(D2)[37] = "Response_RT"

#cut unused columns
D2 = D2[ , -c(10:11, 14:17, 19:26, 28:35, 38)]

##put the two blocks back together
D = rbind(D1, D2)

####Now put everything back together
#combined = rbind(A, B, C, D)
combined = rbind(C, D)

#Write to file
write.csv(combined, file = "processed 10_18.csv", row.names = FALSE)
