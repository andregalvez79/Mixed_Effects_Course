##### necessary packages #####

install.packages('lme4')
library (lme4)

install.packages('car')
library (car)

install.packages('psych')
library (psych)

install.packages('lsmeans')
library (lsmeans)

install.packages('afex')
library (afex)

install.packages('pbkrtest')
library (pbkrtest)

install.packages('lattice')
library (lattice)

install.packages('parallel')
library (parallel)

install.packages("plotrix")
library(plotrix)

install.packages('ggplot2')
library (ggplot2)

#loading data
invmain <- read.csv("C://Users//André//Google Drive//Master//period 3//mixedeffects//takehome//th16_mainfile.csv", sep = ",")
invmaj <- read.csv("C://Users//André//Google Drive//Master//period 3//mixedeffects//takehome//th16_majors.csv", sep = ",")

summary(invmain)
summary(invmaj)

#merging dataframes

invest <- merge(invmain, invmaj, by="pp_code")
summary(invest)

#setting contrast
options(contrasts=c("contr.sum", "contr.poly"))
contrasts(invest$f_major)

#questions

#trees of data
sizetree(invest[, c('pp_code', 'f_major')])
sizetree(invest[1:30, c('pp_code', 'sustainability')]) #within 12 times
sizetree(invest[1:30, c('pp_code', 'exp_profit')])#within 12 times

####################questions#############
#What is the relationship between sustainability and willingness to invest:
#Is there evidence for a significant linear relationship?
with(invest, densityplot(willingness_invest))
with(invest, densityplot(~willingness_invest | sustainability))
ggplot(data = invest, aes(x = pp_code, y = willingness_invest)) + geom_point() + geom_smooth()
densityplot(~ willingness_invest | sustainability, group = f_major, auto.key = TRUE, data = invest)
xyplot(willingness_invest ~ exp_profit | f_major, data = invest, type = c('g', 'p', 'r'))
xyplot(willingness_invest ~ sustainability | pp_code, data = invest, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
xyplot(willingness_invest ~ exp_profit | pp_code, data = invest, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])

investm1 <- lmer(willingness_invest ~ sustainability + (1 + sustainability|pp_code), data = invest)
summary(investm1)

########## p values & CI
##CI
# bootstrap
confint(investm1, method = 'boot', .progress="txt", level= .95, nsim = 500 , PBargs=list(style=3))
##p values
#Determine p values for the fixed effects using bootstrapped Likelihood Ratio Tests
# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 2))
#Run your mixed() command (LRT, KR, PB)
investm1lrt <- mixed(willingness_invest ~ sustainability + (1 + sustainability|pp_code), type = 3, method = "PB", data = invest, cl = MyCluster, args.test = list(nsim = 500, cl = MyCluster))
stopCluster(MyCluster)
summary(investm1lrt)
#check p-values
investm1lrt$tests


#########################
#Is there evidence for a significant quadratic relationship?
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(investm1, type = c('p', 'smooth'))
#not really
boxplot(invest$sustainability)
invest$sustain <- poly(invest$sustainability, 2)[, 1]
invest$sustain2 <- poly(invest$sustainability, 2)[, 2]
plot(invest$sustain, xlab = "sustainability (1 to 4)", xaxt = 'n')
plot(invest$sustain2, xlab = "sustainability (1 to 4)", xaxt = 'n')
mean(invest$sustain)
sd(invest$sustain)
mean(invest$sustain2)
sd(invest$sustain2)

investm2 <- lmer(willingness_invest ~ sustain + sustain2 + (1 + sustain + sustain2|pp_code), data = invest)
summary(investm2)

########## p values 
#Determine p values for the fixed effects using bootstrapped Likelihood Ratio Tests
# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 2))
#Run your mixed() command (LRT, KR, PB)
investm2lrt <- mixed(willingness_invest ~ sustain + sustain2 + (1 + sustain + sustain2|pp_code), type = 3, method = "PB", data = invest, cl = MyCluster, args.test = list(nsim = 500, cl = MyCluster))
stopCluster(MyCluster)
summary(investm2lrt)
#check p-values
investm2lrt$tests
plot(investm2, type = c('p', 'smooth'))

######################### 
#Is there evidence that the linear relationship differs across majors?
with(invest, densityplot(~willingness_invest | f_major))
ggplot(data = invest, aes(x = f_major, y = willingness_invest)) + geom_point() + geom_smooth()
densityplot(~ willingness_invest | sustainability, group = f_major, auto.key = TRUE, data = invest)
xyplot(willingness_invest ~ sustainability | f_major, data = invest, type = c('g', 'p', 'r'))

investm3 <- lmer(willingness_invest ~ sustain*f_major + (1 + sustain|pp_code), data = invest)
summary(investm3)

########## p values ##p values
#Determine p values for the fixed effects using bootstrapped Likelihood Ratio Tests
# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 2))
#Run your mixed() command (LRT, KR, PB)
investm3lrt <- mixed(willingness_invest ~ sustain*f_major + (1 + sustain|pp_code), type = 3, method = "PB", data = invest, cl = MyCluster, args.test = list(nsim = 500, cl = MyCluster))
stopCluster(MyCluster)
summary(investm3lrt)
#check p-values
investm3lrt$tests

########post-hocs######## how to do pairwise comparisons without lsmeans or glht?
#Which majors differ from each other with respect to the sustainability of the investment?
pairs(lstrends(investm3, "f_major", var ="sustain"))
#business from ecology, and business from physics
############## follow up models
######first for business ecology
#exclude variables
invest2 <- subset(invest, f_major == "Ecology" | f_major == "Business" , select = c(sustainability, f_major, pp_code, willingness_invest)) 
describe(invest2)
invest2$sustain <- poly(invest2$sustainability, 2)[, 1]
invest2$f_major <- droplevels(invest2$f_major)
contrasts(invest2$f_major)
investpost1 <- mixed(willingness_invest ~ f_major*sustain + (1 + sustain|pp_code), data = invest2)
summary(investpost1)

# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 2))
#Run your mixed() command (LRT, KR, PB)
investlrtpost1 <- mixed(willingness_invest ~ f_major*sustain + (1 + sustain|pp_code), type = 3, method = "PB", data = invest2, cl = MyCluster, args.test = list(nsim = 500, cl = MyCluster))
#Each CPU runs a (sub)model! Can save a lot of time.
#Once you're done, stop the cluster
stopCluster(MyCluster)
summary(investlrtpost1)
#check p-values
investlrtpost1$tests


#######for business and physics
#exclude variables
invest3 <- subset(invest, f_major == "Physics" | f_major == "Business" , select = c(sustainability, f_major, pp_code, willingness_invest)) 
describe(invest3)
invest3$sustain <- poly(invest3$sustainability, 2)[, 1]
invest3$f_major <- droplevels(invest3$f_major)
contrasts(invest3$f_major)
investpost2 <- mixed(willingness_invest ~ f_major*sustain + (1 + sustain|pp_code), data = invest3)
summary(investpost2)

# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 2))
#Run your mixed() command (LRT, KR, PB)
investlrtpost2 <- mixed(willingness_invest ~ f_major*sustain + (1 + sustain|pp_code), type = 3, method = "PB", data = invest3, cl = MyCluster, args.test = list(nsim = 500, cl = MyCluster))
#Each CPU runs a (sub)model! Can save a lot of time.
#Once you're done, stop the cluster
stopCluster(MyCluster)
summary(investlrtpost2)
#check p-values
investlrtpost2$tests


###########################
#Is there evidence that the quadratic relationship differs across majors?

investm4 <- lmer(willingness_invest ~ f_major * (sustain + sustain2) + (1 + sustain + sustain2|pp_code), data = invest)

#Model diagnostics:
#A densityplot of the scaled model residuals
dplot <- lattice::densityplot(resid(investm4, scaled = TRUE))
#A q-q plot of the scaled residuals; use qqPlot() from the package car
qqPlot(resid(investm4, scaled = TRUE))
#Compute the proportion of residuals larger than +/- 2, 2.5, 3. Are any of these numbers problematic?
#larger than |± 2| (should be around 5%)larger than |± 2.5| (should be around 1%) Every case with a residual > |± 3| could be anutlier
sum(abs(resid(investm4, scaled = TRUE)) > 3)/ length(resid(investm4))
sum(abs(resid(investm4, scaled = TRUE)) > 2.5)/ length(resid(investm4))
sum(abs(resid(investm4, scaled = TRUE)) > 2)/ length(resid(investm4))
#A boxplot showing for each participant the distribution of the scaled residuals. Are there any participants with problematic data points? If so, what are these participants' participant IDs?
plot(investm4, pp_code ~ resid(.))
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(investm4, type = c('p', 'smooth'))	
#A scatter plot showing the observed vs. the fitted values
fitobs <- car::scatterplot(fitted(investm4) ~ invest$willingness_invest, boxplots =FALSE, smoother = FALSE, xlim = c(-55, 55), ylim = c(-55, 55))
summary(investm4)

########post-hocs########
#Which majors differ from each other with respect to the sustainability of the investment?
pairs(lstrends(investm4, "f_major", var ="sustain"))
pairs(lstrends(investm4, "f_major", var ="sustain2"))
invest4 <- subset(invest, f_major == "Physics" | f_major == "Business" , select = c(sustainability, f_major, pp_code, willingness_invest)) 
describe(invest4)
invest4$sustain <- poly(invest4$sustainability, 2)[, 1]
invest4$f_major <- droplevels(invest4$f_major)
contrasts(invest4$f_major)
investpost3 <- mixed(willingness_invest ~ f_major * (sustain + sustain2) + (1 + sustain + sustain2|pp_code), data = invest4)
summary(investpost3)

# Create the cluster
n_cores <- detectCores()
MyCluster <- makeCluster(rep("localhost", n_cores - 2))
#Run your mixed() command (LRT, KR, PB)
investlrtpost3 <- mixed(willingness_invest ~ f_major*sustain + (1 + sustain|pp_code), type = 3, method = "PB", data = invest3, cl = MyCluster, args.test = list(nsim = 500, cl = MyCluster))
#Each CPU runs a (sub)model! Can save a lot of time.
#Once you're done, stop the cluster
stopCluster(MyCluster)
summary(investlrtpost3)
#check p-values
investlrtpost3$tests

#######################
#Is there evidence for a significant linear relationship 
#between expected profit and willingness to invest
with(invest, boxplot(willingness_invest ~ exp_profit))
ggplot(data = invest, aes(x = exp_profit, y = willingness_invest)) + geom_point() + geom_smooth()
densityplot(~ willingness_invest | pp_code, group = f_major, auto.key = TRUE, data = invest)
xyplot(willingness_invest ~ pp_code | exp_profit, data = invest, type = c('g', 'p', 'r'))
sizetree(invest[1:30, c('pp_code', 'exp_profit')])#within 12 times

investm5 <- lmer(willigness_invest ~ exp_profit + (1 + exp_profit|pp_code), data = invest)
###########################
#Is there evidence for a significant quadratic relationship?
plot(cherryg, type = c('p', 'smooth'))	#con el modelo anterior

with(invest, densityplot(willingness_invest))
with(invest, densityplot(~willingness_invest | exp_profit))

invest$eprof <- poly(invest$exp_profit, 2)[, 1]
invest$eprof2 <- poly(invest$exp_profit, 2)[, 2]
plot(invest$eprof, xlab = "expected profit (1 to 3) ", xaxt = 'n')
plot(invest$eprof2, xlab = "sustainability (1 to 3)", xaxt = 'n')

investm6 <- lmer(willigness_invest ~ eprof + eprof2 + (1 + eprof + eprof2|pp_code), data = invest)
#########################
#Is there evidence that the linear relationship differs across majors?

densityplot(~ willingness_invest | exp_profit, group = f_major, auto.key = TRUE, data = invest)
xyplot(willingness_invest ~ exp_profit | f_major, data = invest, type = c('g', 'p', 'r'))

investm7 <- lmer(willigness_invest ~ eprof*f_majors + (1 + eprofit|pp_code), data = invest)

########post-hocs########
#Which diets differ from each other with respect to the chicks weight in the middle of the experiment?
chickpost <- lsmeans(chickmax, pairwise ~ dietf)
chickpost
#In which diets is there significant evidence of change in weight over time?
lstrends(chickmax, "dietf", var = "timec")
#Which diets differ from each other significantly in the change in weight over time?
pairs(lstrends(chickmax, "dietf", var ="timec"))
### follow up models


############################
#Is there evidence that the quadratic relationship differs across majors?

investm7 <- lmer(willigness_invest ~ f_major * (eprofit + eprofit2) + (1 + eprofit + eprofit2|pp_code), data = invest

#Model diagnostics:
#A densityplot of the scaled model residuals
dplot <- lattice::densityplot(resid(cherryg, scaled = TRUE))
#A q-q plot of the scaled residuals; use qqPlot() from the package car
qqPlot(resid(cherryg, scaled = TRUE))
#Compute the proportion of residuals larger than +/- 2, 2.5, 3. Are any of these numbers problematic?
#larger than |± 2| (should be around 5%)larger than |± 2.5| (should be around 1%) Every case with a residual > |± 3| could be anutlier
sum(abs(resid(cherryg, scaled = TRUE)) > 3)/ length(resid(cherryg))
sum(abs(resid(cherryg, scaled = TRUE)) > 2.5)/ length(resid(cherryg))
sum(abs(resid(cherryg, scaled = TRUE)) > 2)/ length(resid(cherryg))
#A boxplot showing for each participant the distribution of the scaled residuals. Are there any participants with problematic data points? If so, what are these participants' participant IDs?
plot(cherryg, pid ~ resid(.))
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(cherryg, type = c('p', 'smooth'))	
#A scatter plot showing the observed vs. the fitted values; use the relevant function from the package car.
fitobs <- car::scatterplot(fitted(cherryg) ~ cherry$distance, boxplots =FALSE, smoother = FALSE, xlim = c(0, 30), ylim = c(0, 30))

########post-hocs########
#Which diets differ from each other with respect to the chicks weight in the middle of the experiment?
chickpost <- lsmeans(chickmax, pairwise ~ dietf)
chickpost
#In which diets is there significant evidence of change in weight over time?
lstrends(chickmax, "dietf", var = "timec")
#Which diets differ from each other significantly in the change in weight over time?
pairs(lstrends(chickmax, "dietf", var ="timec"))
### follow up models

#######################
#Is there evidence that the linear relationship between sustainability and willingness to
invest is moderated by the linear predictor of expected profit?

investm8 <- lmer(willigness_invest ~ sustain*eprofit + (1 + eprofit*sustain|pp_code), data = invest

########################
#Is there evidence that this (potential) moderation effect differs across majors?

investm9 <- lmer(willigness_invest ~ sustain*eprofit*f_major + (1 + eprofit*sustain|pp_code), data = invest)

#Model diagnostics:
#A densityplot of the scaled model residuals
dplot <- lattice::densityplot(resid(cherryg, scaled = TRUE))
#A q-q plot of the scaled residuals; use qqPlot() from the package car
qqPlot(resid(cherryg, scaled = TRUE))
#Compute the proportion of residuals larger than +/- 2, 2.5, 3. Are any of these numbers problematic?
#larger than |± 2| (should be around 5%)larger than |± 2.5| (should be around 1%) Every case with a residual > |± 3| could be anutlier
sum(abs(resid(cherryg, scaled = TRUE)) > 3)/ length(resid(cherryg))
sum(abs(resid(cherryg, scaled = TRUE)) > 2.5)/ length(resid(cherryg))
sum(abs(resid(cherryg, scaled = TRUE)) > 2)/ length(resid(cherryg))
#A boxplot showing for each participant the distribution of the scaled residuals. Are there any participants with problematic data points? If so, what are these participants' participant IDs?
plot(cherryg, pid ~ resid(.))
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(cherryg, type = c('p', 'smooth'))	
#A scatter plot showing the observed vs. the fitted values; use the relevant function from the package car.
fitobs <- car::scatterplot(fitted(cherryg) ~ cherry$distance, boxplots =FALSE, smoother = FALSE, xlim = c(0, 30), ylim = c(0, 30))

########post-hocs########
#Which diets differ from each other with respect to the chicks weight in the middle of the experiment?
chickpost <- lsmeans(chickmax, pairwise ~ dietf)
chickpost
#In which diets is there significant evidence of change in weight over time?
lstrends(chickmax, "dietf", var = "timec")
#Which diets differ from each other significantly in the change in weight over time?
pairs(lstrends(chickmax, "dietf", var ="timec"))
### follow up models


#####################################################
in case
###convergence error not solved .... daamn! preguntar...solo para CI... p values si calcula
densityplot(invest$sustainability) #no variability?

scale(invest$sustain, center=F, scale=T)
scale(invest$sustain2, center=F, scale=T)
invest$sustain10<- invest$sustain*100
invest$sustain20<- invest$sustain2*100
mean(invest$sustain10)
sd(invest$sustain10)
mean(invest$sustain20)
sd(invest$sustain20)

