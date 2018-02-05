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

install.packages('lattice')
library (lattice)

install.packages('parallel')
library (parallel)

install.packages('lsmeans')
library (lsmeans)

chick <- ChickWeight

options(contrasts=c("contr.sum", "contr.poly"))
chick$dietf <-as.factor(paste("Diet", chick$Diet, sep = '_'))
chick$id <-as.factor(paste("ID", chick$Chick, sep = '_'))
chick$timec <- scale(chick$Time, center = T, scale = F)
#(a) Check the data frame to see whether there are any missing values.
which(is.na(chick))
summary(chick)
describe(chick)
is.na(chick)
as.data.frame(table(chick$Time)) # five missing values.. chicks died
contrasts(chick$dietf)
#(b) Figure out and write down the grouping factor(s) of your model #chick
#the above is just confirming what theoretically makes sense
#(c) Figure out and write down the fixed-effects structure of your model
#dietf and timec... we want to know how things change in time and how diet affects the weight
#(d) Use crosstables to figure out for each of your fixed effects (don't forget interaction(s) whether you can model it as random slope or not.
with(chick, table(dietf, id)) #between subject factor because diest stays the same only one for each pp
with(chick, table(timec, dietf, id))
with(chick, table(timec, id))#time as a slope becasue its continues you need 3 or more points.. and it-s repeated
#(e) Based on steps (a) to (d) , write down the 'maximal model' you want to use to answer the research questions above
chickmax <- lmer(weight ~ dietf*timec + (1 + timec| id), data = chick)
summary(chickmax)
#desnity plot reiduals
dplot <- lattice::densityplot(resid(chickmax, scaled = TRUE))
#qqplot
qqPlot(resid(chickmax, scaled = TRUE))
#proportion of residuals
sum(abs(resid(chickmax, scaled = TRUE)) > 3)/ length(resid(chickmax))
sum(abs(resid(chickmax, scaled = TRUE)) > 2.5)/ length(resid(chickmax))
sum(abs(resid(chickmax, scaled = TRUE)) > 2)/ length(resid(chickmax))
#fitted vs residuals hetero
plot(chickmax, type = c('p', 'smooth'))
#Determine p values for the fixed effects using bootstrapped Likelihood Ratio Tests
# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 1))
#Run your mixed() command (LRT, KR, PB)
chicklrt <- mixed(weight ~ dietf*timec + (1 + timec| id), type = 3, method = "PB", data = chick, cl = MyCluster, args.test = list(nsim = 500, cl = MyCluster))
#Each CPU runs a (sub)model! Can save a lot of time.
#Once you're done, stop the cluster
stopCluster(MyCluster)
summary(chicklrt)
#check p-values
chicklrt$tests

########post-hocs########
#Which diets differ from each other with respect to the chicks weight in the middle of the experiment?
chickpost <- lsmeans(chickmax, pairwise ~ dietf)
chickpost
#In which diets is there significant evidence of change in weight over time?
lstrends(chickmax, "dietf", var = "timec")
#Which diets differ from each other significantly in the change in weight over time?
pairs(lstrends(chickmax, "dietf", var ="timec"))
#to report?
describeBy(chick$weight, list(chick$dietf))

#(j) Do follow-up models comparing Diets 1 and 3
# selecting data
#exclude variables
chick2 <- subset(chick, Diet == "1" | Diet == "3" , select = c(weight, Chick, Time, Diet)) 
describe(chick2)
chick2$timec <- scale(chick2$Time, center = T, scale = F)
chick2$id <-as.factor(paste("ID", chick2$Chick, sep = '_'))
chick2$dietf <-as.factor(paste("Diet", chick2$Diet, sep = '_'))
chickmax2 <- mixed(weight ~ dietf*timec + (1 + timec| id), data = chick2)
summary(chickmax2)

# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 2))
#Run your mixed() command (LRT, KR, PB)
chicklrt2 <- mixed(weight ~ dietf*timec + (1 + timec| id), type = 3, method = "PB", data = chick2, cl = MyCluster, args.test = list(nsim = 500, cl = MyCluster))
#Each CPU runs a (sub)model! Can save a lot of time.
#Once you're done, stop the cluster
stopCluster(MyCluster)
summary(chicklrt2)
#check p-values
chicklrt2$tests
######################
## these models dont have diet as an IV, because it's just one level
#(k) Do follow-up models to investigate whether there is a significant change in weight over time:
#in Diet 1
#exclude variables
contrast()
chick3 <- subset(chick, Diet == "1" , select = c(weight, Chick, Time, Diet)) 
describe(chick3)
chick3$timec <- scale(chick3$Time, center = T, scale = F)
chick3$id <-as.factor(paste("ID", chick3$Chick, sep = '_'))
chickmax3 <- mixed(weight ~ timec + (1 + timec| id), data = chick3)
summary(chickmax3)

# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 2))
#Run your mixed() command (LRT, KR, PB)
chicklrt3 <- mixed(weight ~ timec + (1 + timec| id), type = 3, method = "PB", data = chick3, cl = MyCluster, args.test = list(nsim = 500, cl = MyCluster))
#Each CPU runs a (sub)model! Can save a lot of time.
#Once you're done, stop the cluster
stopCluster(MyCluster)
summary(chicklrt3)
#check p-values
chicklrt3$tests
#####################
#in Diet 3
#exclude variables
chick4 <- subset(chick, Diet == "3" , select = c(weight, Chick, Time, Diet)) 
describe(chick4)
chick4$timec <- scale(chick4$Time, center = T, scale = F)
chick4$id <-as.factor(paste("ID", chick4$Chick, sep = '_'))
chickmax4 <- mixed(weight ~ timec + (1 + timec| id), data = chick4)
summary(chickmax4)

# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 2))
#Run your mixed() command (LRT, KR, PB)
chicklrt4 <- mixed(weight ~ timec + (1 + timec| id), type = 3, method = "PB", data = chick4, cl = MyCluster, args.test = list(nsim = 500, cl = MyCluster))
#Each CPU runs a (sub)model! Can save a lot of time.
#Once you're done, stop the cluster
stopCluster(MyCluster)
summary(chicklrt4)
#check p-values
chicklrt4$tests

citation("lsmeans")
sessionInfo()
