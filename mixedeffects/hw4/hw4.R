install.packages('lme4')
library (lme4)

install.packages('car')
library (car)

install.packages('psych')
library (psych)

install.packages('afex')
library (afex)

install.packages('pbkrtest')
library (pbkrtest)

install.packages('"parallel')
library (parallel)

cherry <- read.csv("C:\\Users\\s4600479\\Desktop\\CherryPit_large_N30Gender.csv", sep = ",")


#contrast to zero sum, because...
contrasts(cherry$gender) = contr.sum(2)

#A)for this hw we need to scale trials
cherry$numtrialc <- scale(cherry$num_trial, center = T, scale = F)

cherryg <- lmer(distance ~ numtrialc*gender + (1 + numtrialc| pid), data = cherry)
summary(cherrylargex1)
summary(cherrylargex2)
summary(cherryg)
#Compute Type 2 tests to obtain p values for the interaction effect and the two main effects via LRTs using the anova command.
cherrylarge <- lmer(distance ~ numtrialc*gender + (1 + numtrialc| pid), data = cherry)
cherrysmall <- lmer(distance ~ numtrialc + gender + (1 + numtrialc| pid), data = cherry)
#for main effects in type 2 errors
cherrylargex1 <- lmer(distance ~ numtrialc + gender + (1 + numtrialc| pid), data = cherry)
cherrysmallx1 <- lmer(distance ~ gender + (1 + numtrialc| pid), data = cherry)
#for main effects in type 2 errors
cherrylargex2 <- lmer(distance ~ numtrialc + gender + (1 + numtrialc| pid), data = cherry)
cherrysmallx2 <- lmer(distance ~ numtrialc + (1 + numtrialc| pid), data = cherry)

anova(cherrysmall, cherrylarge)
anova(cherrysmallx1, cherrylargex1)
anova(cherrysmallx2, cherrylargex2)

#Do the same using the KRmodcomp function.
cherryftest <- pbkrtest::KRmodcomp(cherrylarge, cherrysmall)
cherryftest
#main
cherryftestx1 <- pbkrtest::KRmodcomp(cherrylargex1, cherrysmallx1)
cherryftestx2 <- pbkrtest::KRmodcomp(cherrylargex2, cherrysmallx2)

cherryftestx1
cherryftestx2

#for PB test use multiple cores

#Multiple Cores with mixed()
# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 1))
#Run your mixed() command (LRT, KR, PB)
cherrygcl <- mixed(distance ~ gender*numtrialc + (1 + numtrialc | pid), type = 2, method = "LRT", data = cherry, cl = MyCluster)
#Each CPU runs a (sub)model! Can save a lot of time.
#Once you're done, stop the cluster
stopCluster(MyCluster)
#PBTEST
cherrypb <- pbkrtest::PBmodcomp(cherrylarge, cherrysmall)
summary(cherrypb)
#main
cherrypbx1 <- pbkrtest::PBmodcomp(cherrylargex1, cherrysmallx1)
summary(cherrypbx1)
cherrypbx2 <- pbkrtest::PBmodcomp(cherrylargex2, cherrysmallx2)
summary(cherrypbx2)


#Try to use anova to do Type 3 tests of the two main effects and the interaction
#for main effects in type 2 errors
#Compute Type 2 tests to obtain p values for the interaction effect and the two main effects via LRTs using the anova command.
cherrylargex3 <- lmer(distance ~ numtrialc*gender + (1 + numtrialc| pid), data = cherry)
cherrysmallx3 <- lmer(distance ~ numtrialc + gender + (1 + numtrialc| pid), data = cherry)
#for main effects in type 2 errors
cherrysmallx11 <- lmer(distance ~ gender + numtrialc:gender + (1 + numtrialc| pid), data = cherry)
summary(cherrysmallx1)
cherrysmallx21 <- lmer(distance ~ numtrialc + numtrialc:gender + (1 + numtrialc| pid), data = cherry)

anova(cherrysmallx3, cherrylargex3)
anova(cherrysmallx11, cherrylargex3)
anova(cherrysmallx21, cherrylargex3)

#K-R F with type 3 and Anova fn
car::Anova(cherrylarge, type = 3, test = "F")

#another way
cherrykr <- mixed(distance ~ gender*numtrialc + (1 + numtrialc |pid), data = cherry, type = 3, method = "KR")
# for output
anova(cherrykr)

#Type 3 tests to obtain p values for the interaction effect and the two main effects via LRTs
cherrylrt <- mixed(distance ~ gender*numtrialc + (1 + numtrialc |pid), data = cherry, type = 3, method = "LRT")
# for output
anova(cherrylrt)

# Type 3 tests to obtain p values for the interaction effect and the two main effects via KR F tests
#another way
cherrykr <- mixed(distance ~ gender*numtrialc + (1 + numtrialc |pid), data = cherry, type = 3, method = "KR")
# for output
anova(cherrykr)

#Type 3 tests to obtain p values for the interaction effect and the two main effects via PB tests

cherrypbt <- mixed(distance ~ gender*numtrialc + (1 + numtrialc |pid), data = cherry, type = 3, method = "PB")
# for output
anova(cherrypbt)

############2####################
politenessdata<- read.csv("D:/mijn documenten/Nijmegen/mixedeffects/politeness_data.csv", sep = ",")

view(politenessdata)

summary(politenessdata)

which(is.na(politenessdata$frequency))

boxplot(frequency ~ attitude*gender, col=c("white","lightgray"),politenessdata)

lmer(frequency ~ attitude, data=politenessdata)

politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politenessdata)

politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politenessdata)

summary(politeness.model)

politeness.null = lmer(frequency ~ gender + (1|subject) + (1|scenario), data=politenessdata, REML=FALSE)

politeness.model = lmer(frequency ~ attitude +
                          gender + (1|subject) + (1|scenario),
                        data=politenessdata, REML=FALSE)

anova(politeness.null,politeness.model)

politeness.int = lmer(frequency ~ attitude *
                        gender + (1|subject) + (1|scenario),
                      data=politenessdata, REML=FALSE)

anova(politeness.model, politeness.int)

coef(politeness.model)

politeness.model = lmer(frequency ~ attitude +
                          gender + (1+attitude|subject) +
                          (1+attitude|scenario),
                        data=politenessdata,
                        REML=FALSE)

coef(politeness.model)

politeness.null = lmer(frequency ~ gender +
                         (1+attitude|subject) + (1+attitude|scenario),
                       data=politenessdata, REML=FALSE)

anova(politeness.null,politeness.model)

all.res=numeric(nrow(politenessdata))
for(i in 1:nrow(politnessdata)){politeness.int = lmer(frequency ~ attitude * gender + (1|subject) + (1|scenario), data=politenessdata, REML=FALSE), POP[-i,])
all.res[i]=fixef(politeness.int)[3]}

