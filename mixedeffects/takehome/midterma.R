##### necessary packages #####

install.packages('lme4')
library (lme4)

install.packages("coefplot")
library(coefplot)

install.packages("effects")
library(effects)

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

install.packages("influence.ME")
library(influence.ME)

#loading data modify the path
invmain <- read.csv("C://Users//André//Google Drive//Master//period 3//mixedeffects//takehome//th16_mainfile.csv", sep = ",")
invmaj <- read.csv("C://Users//André//Google Drive//Master//period 3//mixedeffects//takehome//th16_majors.csv", sep = ",")

summary(invmain)
summary(invmaj)

#merging dataframes

invest <- merge(invmain, invmaj, by="pp_code")
#generating a trial variable
invest$trial <- as.factor(paste(invest$sustainability, invest$exp_profit, sep = "_"))
summary(invest)

#setting contrast
options(contrasts=c("contr.sum", "contr.poly"))
contrasts(invest$f_major)

#####understanding data
#trees of data for visual inspection of possible random slopes
sizetree(invest[, c('pp_code', 'f_major')])
sizetree(invest[1:30, c('pp_code', 'sustainability')]) #within 12 times
sizetree(invest[1:30, c('pp_code', 'exp_profit')])#within 12 times
sizetree(invest[1:30, c('trial', 'exp_profit')])#within 12 times
sizetree(invest[1:30, c('trial', 'exp_profit')])#within 12 times

#another way which is sometimes confusing
with(invest, table(sustainability, exp_profit))
with(invest, table(f_major, sustainability))
with(invest, table(f_major, exp_profit))
with(invest, table(sustainability, pp_code))
with(invest, table(exp_profit, pp_code))
with(invest, table(f_major, pp_code))
with(invest, table(pp_code, sustainability, exp_profit))
with(invest, table(sustainability, trial))
with(invest, table(exp_profit, trial))
with(invest, table(f_major, trial))


#some(many)plots of the data to get an idea of whats gonna happen in the model
with(invest, densityplot(willingness_invest))
with(invest, densityplot(~willingness_invest | sustainability))
ggplot(data = invest, aes(x = pp_code, y = willingness_invest)) + geom_point() + geom_smooth()
ggplot(data = invest, aes(x = trial, y = willingness_invest)) + geom_point() + geom_smooth()
densityplot(~ willingness_invest | sustainability, group = f_major, auto.key = TRUE, data = invest)
xyplot(willingness_invest ~ exp_profit | f_major, data = invest, type = c('g', 'p', 'r'))
xyplot(willingness_invest ~ sustainability | pp_code, data = invest, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
xyplot(willingness_invest ~ exp_profit | pp_code, data = invest, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
xyplot(willingness_invest ~ trial | pp_code, data = invest, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])

with(invest, densityplot(~willingness_invest | f_major))
xyplot(willingness_invest ~ trial | f_major, data = invest, type = c('g', 'p', 'r'))
with(invest, boxplot(willingness_invest ~ exp_profit))

densityplot(~ willingness_invest | pp_code, group = f_major, auto.key = TRUE, data = invest)
densityplot(~ willingness_invest | trial, group = f_major, auto.key = TRUE, data = invest)
xyplot(willingness_invest ~ pp_code | exp_profit, data = invest, type = c('g', 'p', 'r'))
sizetree(invest[1:30, c('pp_code', 'exp_profit')])#within 12 times
scatterplot(willingness_invest ~ sustainability, smooth = TRUE, boxplots =FALSE, data = invest)
xyplot(willingness_invest ~ trial | f_major, data = invest, group = pp_code, type = c('g', 'r'))

ggplot(invest, aes(sustainability, willingness_invest) ) + geom_smooth( aes(linetype = f_major), method = "lm") +geom_point()+ labs(title="Willingness to Invest and Sustainability Levels by Major", x="Sustainability Levels", y="Willingness to Invest")
ggplot(invest, aes(f_major, willingness_invest)) + geom_point() + geom_boxplot()

#because we are considering to use a quadratic term we use the function poly
invest$sustain <- poly(invest$sustainability, 2)[, 1]
invest$sustain2 <- poly(invest$sustainability, 2)[, 2]
plot(invest$sustain, xlab = "sustainability (1 to 4)", xaxt = 'n')
plot(invest$sustain2, xlab = "sustainability (1 to 4)", xaxt = 'n')
invest$eprof <- poly(invest$exp_profit, 2)[, 1]
invest$eprof2 <- poly(invest$exp_profit, 2)[, 2]
plot(invest$eprof, xlab = "expected profit (1 to 3) ", xaxt = 'n')
plot(invest$eprof2, xlab = "sustainability (1 to 3)", xaxt = 'n')
mean(invest$sustain)
sd(invest$sustain)
mean(invest$sustain2)
sd(invest$sustain2)
#we multiply by ten beacuse small numbers may cause ocnvergence problems... which they did
invest$sustain10 <- invest$sustain*10
invest$sustain20 <- invest$sustain2*10
invest$eprof10 <- invest$eprof*10
invest$eprof20 <- invest$eprof2*10

#max model specification
maxmodel <- lmer(willingness_invest ~ f_major * (sustain10 + sustain20) + f_major * (eprof10 + eprof20) + f_major * sustain10 * eprof10 + (1 + eprof10*sustain10 + sustain20 + eprof20| pp_code) + (1 + f_major|trial), data = invest, control = lmerControl(optCtrl = list(maxfun = 1e+9)))
summary(maxmodel)
contrasts(invest$f_major)
#just checking which otpimizer works best to converge the model
my_allFit <- allFit(maxmodel, maxfun = 10e+9)
ok_fits <- sapply(my_allFit, is, "merMod")
ok_fits

#Model diagnostics:
#A densityplot of the scaled model residuals
dplot <- lattice::densityplot(resid(maxmodel, scaled = TRUE))
#A q-q plot of the scaled residuals; use qqPlot() from the package car
qqPlot(resid(maxmodel, scaled = TRUE))
#Compute the proportion of residuals larger than +/- 2, 2.5, 3. Are any of these numbers problematic?
#larger than |± 2| (should be around 5%)larger than |± 2.5| (should be around 1%) Every case with a residual > |± 3| could be anutlier
sum(abs(resid(maxmodel, scaled = TRUE)) > 3)/ length(resid(maxmodel))*100
sum(abs(resid(maxmodel, scaled = TRUE)) > 2.5)/ length(resid(maxmodel))*100
sum(abs(resid(maxmodel, scaled = TRUE)) > 2)/ length(resid(maxmodel))*100
#A boxplot showing for each participant the distribution of the scaled residuals. Are there any participants with problematic data points? If so, what are these participants' participant IDs?
plot(maxmodel, pp_code ~ resid(.))
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(maxmodel, type = c('p', 'smooth'))	
#A scatter plot showing the observed vs. the fitted values
fitobs <- car::scatterplot(fitted(maxmodel) ~ invest$willingness_invest, boxplots =FALSE, smoother = FALSE, xlim = c(-55, 55), ylim = c(-55, 55))
summary(maxmodel)
#DFBETAS n COOKS not necessary... takes long time
influ <- influence(maxmodel, "pp_code")
cooks.distance(influ)
dfbetas(influ)
plot(influ, which = 'cook')
plot(influ, which = 'dfbetas')


########## p values ##p values
#Determine p values for the fixed effects using bootstrapped Likelihood Ratio Tests
#Run your mixed() command (LRT, KR, PB)
investmlrt <- mixed(willingness_invest ~ f_major * (sustain10 + sustain20) + f_major * (eprof10 + eprof20) + f_major * sustain10 * eprof10 + (1 + eprof10*sustain10 + sustain20 + eprof20| pp_code) + (1 + f_major|trial), type = 3, method = "LRT", data = invest)
summary(investmlrt)
#check p-values
investmlrt$tests


#### this follow up models are another wy to test interaction efects with factor variables.
#an easy way is to use pairwise comparisons and commands
########post-hocs######## how to do pairwise comparisons without lsmeans or glht?
#Which majors differ from each other with respect to the sustainability of the investment?
##############################################################################borrar el pairs
############## follow up models
######first for business ecology
#exclude variables
invest2 <- subset(invest, f_major == "Ecology" | f_major == "Business" , select = c(sustainability, f_major, pp_code, willingness_invest, trial, exp_profit)) 
describe(invest2)
invest2$sustain <- poly(invest2$sustainability, 2)[, 1]
invest2$sustain2 <- poly(invest2$sustainability, 2)[, 2]
invest2$eprof <- poly(invest2$exp_profit, 2)[, 1]
invest2$eprof2 <- poly(invest2$exp_profit, 2)[, 2]
invest2$f_major <- droplevels(invest2$f_major)

scale(invest2$sustain, center=T, scale=T)
scale(invest2$sustain2, center=T, scale=T)
scale(invest2$eprof, center=T, scale=T)
scale(invest2$eprof2, center=T, scale=T)

invest2$sustain10 <- invest2$sustain*10
invest2$sustain20 <- invest2$sustain2*10
invest2$eprof10 <- invest2$eprof*10
invest2$eprof20 <- invest2$eprof2*10

contrasts(invest2$f_major)
#cheking if obs are ok for parameter number.. beacuause i had convergenece problems
nrow(invest2)
length(getME(maxmodel, "theta"))
length(fixef(maxmodel))
18+22 #almost 10 obs per parameter
#checking other possible issues
summary(investpost1)
#no SD or Var near 0

#doesnt work with mixed...
previous_est <- getME(investpost1, c("theta","fixef"))
m2 <- update(investpost1, start = previous_est, control = glmerControl(optCtrl = list(maxfun =1e+9)))
#outliers?
nrow(subset(invest2, abs(scale(eprof2)) >3))
nrow(subset(invest2, abs(scale(sustain2)) >3))
#model with mixed in order to run it for pvalues
investpost1 <- mixed(willingness_invest ~ f_major * (sustain10 + sustain20) + f_major * (eprof10 + eprof20) + f_major * sustain10 * eprof10 + (1 + eprof10 + sustain10 + sustain20 + eprof20| pp_code) + (1 + f_major|trial), data = invest2, control = lmerControl(optCtrl = list(maxfun = 1e+9), optimizer="bobyqa"))
#convergence issues... 
"solved by remove the highest random slope and test the effects for this model (eprof10:sustain10) as ben bolker suggests in forum and like barr etal suggests.
summary(investpost1)

#Run your mixed() command (LRT, KR, PB)
investlrtpost1 <- mixed(willingness_invest ~ f_major * (sustain10 + sustain20) + f_major * (eprof10 + eprof20) + f_major * sustain10 * eprof10 + (1 + eprof10 + sustain10 + sustain20 + eprof20| pp_code) + (1 + f_major|trial), type = 3, method = "LRT", data = invest2)
summary(investlrtpost1)
#check p-values
investlrtpost1$tests

#to get some means of the effects of the factor variable major
describeBy(invest2$willingness_invest, group=invest2$f_major)
describeBy(invest2$willingness_invest, list(invest2$f_major,invest2$sustainability))


#########second business from physics

invest3 <- subset(invest, f_major == "Physics" | f_major == "Business" , select = c(sustainability, f_major, pp_code, willingness_invest, trial, exp_profit)) 
describe(invest3)
invest3$sustain <- poly(invest3$sustainability, 2)[, 1]
invest3$sustain2 <- poly(invest3$sustainability, 2)[, 2]
invest3$eprof <- poly(invest3$exp_profit, 2)[, 1]
invest3$eprof2 <- poly(invest3$exp_profit, 2)[, 2]
invest3$f_major <- droplevels(invest3$f_major)

scale(invest3$sustain, center=T, scale=T)
scale(invest3$sustain2, center=T, scale=T)
scale(invest3$eprof, center=T, scale=T)
scale(invest3$eprof2, center=T, scale=T)

invest3$sustain10 <- invest3$sustain*10
invest3$sustain20 <- invest3$sustain2*10
invest3$eprof10 <- invest3$eprof*10
invest3$eprof20 <- invest3$eprof2*10

contrasts(invest3$f_major)

nrow(invest3)
length(getME(maxmodel, "theta"))
length(fixef(maxmodel))
18+22 #almost 10 obs per parameter

summary(investpost3)
#no SD or Var near 0

#outliers?
nrow(subset(invest3, abs(scale(eprof2)) >3))
nrow(subset(invest3, abs(scale(sustain2)) >3))

investpost2 <- mixed(willingness_invest ~ f_major * (sustain10 + sustain20) + f_major * (eprof10 + eprof20) + f_major * sustain10 * eprof10 + (1 + eprof10 + sustain10 + sustain20 + eprof20| pp_code) + (1 + f_major|trial), data = invest3, control = lmerControl(optCtrl = list(maxfun = 1e+9), optimizer="bobyqa"))

#convergence issues... 
"solved by remove the highest random slope and test the effects for this model (eprof10:sustain10) as ben bolker suggest.
summary(investpost2)

#Run your mixed() command (LRT, KR, PB)
investlrtpost2 <- mixed(willingness_invest ~ f_major * (sustain10 + sustain20) + f_major * (eprof10 + eprof20) + f_major * sustain10 * eprof10 + (1 + eprof10 + sustain10 + sustain20 + eprof20| pp_code) + (1 + f_major|trial), type = 3, method = "LRT", data = invest3)
summary(investlrtpost2)
#check p-values
investlrtpost2$tests


describeBy(invest3$willingness_invest, group=invest3$f_major)
describeBy(invest3$willingness_invest, list(invest3$f_major,invest3$sustainability))


#########third ecology from physics
invest4 <- subset(invest, f_major == "Physics" | f_major == "Ecology" , select = c(sustainability, f_major, pp_code, willingness_invest, trial, exp_profit)) 
describe(invest4)
invest4$sustain <- poly(invest4$sustainability, 2)[, 1]
invest4$sustain2 <- poly(invest4$sustainability, 2)[, 2]
invest4$eprof <- poly(invest4$exp_profit, 2)[, 1]
invest4$eprof2 <- poly(invest4$exp_profit, 2)[, 2]
invest4$f_major <- droplevels(invest4$f_major)

scale(invest4$sustain, center=T, scale=T)
scale(invest4$sustain2, center=T, scale=T)
scale(invest4$eprof, center=T, scale=T)
scale(invest4$eprof2, center=T, scale=T)

invest4$sustain10 <- invest4$sustain*10
invest4$sustain20 <- invest4$sustain2*10
invest4$eprof10 <- invest4$eprof*10
invest4$eprof20 <- invest4$eprof2*10

contrasts(invest4$f_major)

summary(investpost4)
#no f_major slope seems near to 0 eliminating f_major as a slope

#outliers?
nrow(subset(invest4, abs(scale(eprof2)) >3))
nrow(subset(invest4, abs(scale(sustain2)) >3))

investpost3 <- mixed(willingness_invest ~ f_major * (sustain10 + sustain20) + f_major * (eprof10 + eprof20) + f_major * sustain10 * eprof10 + (1 + eprof10 * sustain10 + sustain20 + eprof20| pp_code) + (1|trial), data = invest4, control = lmerControl(optCtrl = list(maxfun = 1e+9), optimizer="bobyqa"))

#convergence issues... 
summary(investpost3)

#Run your mixed() command (LRT, KR, PB)
investlrtpost3 <- mixed(willingness_invest ~ f_major * (sustain10 + sustain20) + f_major * (eprof10 + eprof20) + f_major * sustain10 * eprof10 + (1 + eprof10 * sustain10 + sustain20 + eprof20| pp_code) + (1|trial), type = 3, method = "LRT", data = invest4)
summary(investlrtpost3)
#check p-values
investlrtpost3$tests

describeBy(invest4$willingness_invest, group=invest4$f_major)
describeBy(invest4$willingness_invest, list(invest4$f_major,invest4$sustainability))



#########fourth only business
#here i rewrite the prevoius substet... no reason in particular just a mistake i didn't corrected.
invest4 <- subset(invest, f_major == "Business" , select = c(sustainability, f_major, pp_code, willingness_invest, trial, exp_profit)) 
describe(invest4)
invest4$sustain <- poly(invest4$sustainability, 2)[, 1]
invest4$sustain2 <- poly(invest4$sustainability, 2)[, 2]
invest4$eprof <- poly(invest4$exp_profit, 2)[, 1]
invest4$eprof2 <- poly(invest4$exp_profit, 2)[, 2]
invest4$f_major <- droplevels(invest4$f_major)

scale(invest4$sustain, center=T, scale=T)
scale(invest4$sustain2, center=T, scale=T)
scale(invest4$eprof, center=T, scale=T)
scale(invest4$eprof2, center=T, scale=T)

invest4$sustain10 <- invest4$sustain*10
invest4$sustain20 <- invest4$sustain2*10
invest4$eprof10 <- invest4$eprof*10
invest4$eprof20 <- invest4$eprof2*10

contrasts(invest4$f_major)

summary(investpost4)
#no SD or Var near 0

#outliers?
nrow(subset(invest4, abs(scale(eprof2)) >3))
nrow(subset(invest4, abs(scale(sustain2)) >3))

investpost4 <- mixed(willingness_invest ~ (sustain10 + sustain20) + (eprof10 + eprof20) + sustain10:eprof10 + (1 + eprof10 + sustain10 + sustain20 + eprof20| pp_code) + (1 |trial), data = invest4, control = lmerControl(optCtrl = list(maxfun = 1e+9), optimizer="bobyqa"))

#convergence issues... 
"solved by remove the highest random slope and test the effects for this model (eprof10:sustain10) as ben bolker suggest.
summary(investpost4)

#Run your mixed() command (LRT, KR, PB)
investlrtpost4 <- mixed(willingness_invest ~ (sustain10 + sustain20) + (eprof10 + eprof20) + sustain10:eprof10 + (1 + eprof10 + sustain10 + sustain20 + eprof20| pp_code) + (1 |trial), type = 3, method = "LRT", data = invest4)
summary(investlrtpost4)
#check p-values
investlrtpost4$tests



#########fifth only ecology

invest5 <- subset(invest, f_major == "Ecology" , select = c(sustainability, f_major, pp_code, willingness_invest, trial, exp_profit)) 
describe(invest5)
invest5$sustain <- poly(invest5$sustainability, 2)[, 1]
invest5$sustain2 <- poly(invest5$sustainability, 2)[, 2]
invest5$eprof <- poly(invest5$exp_profit, 2)[, 1]
invest5$eprof2 <- poly(invest5$exp_profit, 2)[, 2]
invest5$f_major <- droplevels(invest5$f_major)

scale(invest5$sustain, center=T, scale=T)
scale(invest5$sustain2, center=T, scale=T)
scale(invest5$eprof, center=T, scale=T)
scale(invest5$eprof2, center=T, scale=T)

invest5$sustain10 <- invest5$sustain*10
invest5$sustain20 <- invest5$sustain2*10
invest5$eprof10 <- invest5$eprof*10
invest5$eprof20 <- invest5$eprof2*10

contrasts(invest5$f_major)

summary(investpost5)
#no SD or Var near 0

#outliers?
nrow(subset(invest5, abs(scale(eprof2)) >3))
nrow(subset(invest5, abs(scale(sustain2)) >3))

investpost5 <- mixed(willingness_invest ~ (sustain10 + sustain20) + (eprof10 + eprof20) + sustain10 : eprof10 + (1 + eprof10 + sustain10 + sustain20 + eprof20| pp_code) + (1 |trial), data = invest5, control = lmerControl(optCtrl = list(maxfun = 1e+9), optimizer="bobyqa"))

#convergence issues... 
"solved by remove the highest random slope and test the effects for this model (eprof10:sustain10) as ben bolker suggest.
summary(investpost5)

#Run your mixed() command (LRT, KR, PB)
investlrtpost5 <- mixed(willingness_invest ~ (sustain10 + sustain20) + (eprof10 + eprof20) + sustain10 : eprof10 + (1 + eprof10 + sustain10 + sustain20 + eprof20| pp_code) + (1 |trial), type = 3, method = "LRT", data = invest5)
summary(investlrtpost5)
#check p-values
investlrtpost5$tests



#########sixth only physics

invest6 <- subset(invest, f_major == "Physics" , select = c(sustainability, f_major, pp_code, willingness_invest, trial, exp_profit)) 
describe(invest6)
invest6$sustain <- poly(invest6$sustainability, 2)[, 1]
invest6$sustain2 <- poly(invest6$sustainability, 2)[, 2]
invest6$eprof <- poly(invest6$exp_profit, 2)[, 1]
invest6$eprof2 <- poly(invest6$exp_profit, 2)[, 2]
invest6$f_major <- droplevels(invest6$f_major)

scale(invest6$sustain, center=T, scale=T)
scale(invest6$sustain2, center=T, scale=T)
scale(invest6$eprof, center=T, scale=T)
scale(invest6$eprof2, center=T, scale=T)

invest6$sustain10 <- invest6$sustain*10
invest6$sustain20 <- invest6$sustain2*10
invest6$eprof10 <- invest6$eprof*10
invest6$eprof20 <- invest6$eprof2*10

contrasts(invest6$f_major)

summary(investpost6)
#no SD or Var near 0

#outliers?
nrow(subset(invest6, abs(scale(eprof2)) >3))
nrow(subset(invest6, abs(scale(sustain2)) >3))

investpost6 <- mixed(willingness_invest ~ (sustain10 + sustain20) + (eprof10 + eprof20) + sustain10 * eprof10 + (1 + eprof10 + sustain10 + sustain20 + eprof20| pp_code) + (1 |trial), data = invest6, control = lmerControl(optCtrl = list(maxfun = 1e+9), optimizer="bobyqa"))

#convergence issues... 
"solved by remove the highest random slope and test the effects for this model (eprof10:sustain10) as ben bolker suggest.
summary(investpost6)

#Run your mixed() command (LRT, KR, PB)
investlrtpost6 <- mixed(willingness_invest ~ (sustain10 + sustain20) + (eprof10 + eprof20) + sustain10 * eprof10 + (1 + eprof10 + sustain10 + sustain20 + eprof20| pp_code) + (1 |trial), type = 3, method = "LRT", data = invest6)
summary(investlrtpost6)
#check p-values
investlrtpost6$tests

############some result plots

#interaction effect
#but I already have one
ggplot(data = invest, aes(x=sustainability, y = willingness_invest, color =
f_major)) + geom_point() + stat_smooth(method = 'lm')

#####interaction with raw data
#calculate means for each type x treatment 
means=by(invest$willingness_invest,list(invest$f_major, invest$sustainability),mean) 

# a function for calculating standard error 
se=function(x) sqrt(var(x)/length(x)) 

#now calculate standard error for each type x treatment 
ses=by(invest$willingness_invest,list(invest$f_major, invest$sustainability),se) 

#plot means as interaction plot; type="b" means plot both symbols and lines; pch=c(21,19) are the two symbol types; ylim is the y axis minimum & maximum values; las=1 makes the y-axis numbers horizontal 
interaction.plot(invest$sustainability,invest$f_major,invest$willingness_invest,type="b",pch=c(-25,25), ylim=c(-25,25),las=1,ylab="Willingness to Invest",xlab="Sustainability Levels") 

#now add the standard error lines (means plus/minus standard error)keep the plot open 
lines(c(1,1),c(means[1]-ses[1],means[1]+ses[1]), col="red") 
lines(c(1,1),c(means[2]-ses[2],means[2]+ses[2]), col="blue")
lines(c(1,1),c(means[3]-ses[3],means[3]+ses[3]), col="green") 
lines(c(2,2),c(means[4]-ses[4],means[4]+ses[4]), col="red") 
lines(c(2,2),c(means[5]-ses[5],means[5]+ses[5]), col="blue")
lines(c(2,2),c(means[6]-ses[6],means[6]+ses[6]), col="green")
lines(c(3,3),c(means[7]-ses[7],means[7]+ses[7]), col="red")
lines(c(3,3),c(means[8]-ses[8],means[8]+ses[8]), col="blue")
lines(c(3,3),c(means[9]-ses[9],means[9]+ses[9]), col="green")
lines(c(4,4),c(means[10]-ses[10],means[10]+ses[10]), col="red")
lines(c(4,4),c(means[11]-ses[11],means[11]+ses[11]), col="blue")
lines(c(4,4),c(means[12]-ses[12],means[12]+ses[12]), col="green")

#coeficient plot with SE
coefplot(maxmodel)

#cuadratic effect
scatterplot(invest$willingness_invest ~ invest$sustainability, smoother
= loessLine, jitter = list(x = 0, y = 1), boxplots = FALSE)

##model based means effects plot
plot(effect("f_major:sustain10:eprof10", maxmodel), multiline =TRUE, ci.style = 'bars')

#######moderation effect
#just because
#MAXMODEL NOT RESTRICTED MODEL
summary(maxmodel)
investmlrt$tests
#model for restricted.
#first interaction between sustain and expected profit, then 3 way interaction
#restricted model
minmodel <- lmer(willingness_invest ~ f_major * (sustain10 + sustain20) + f_major * (eprof10 + eprof20) + f_major * sustain10 + eprof10 + (1 + eprof10*sustain10 + sustain20 + eprof20| pp_code) + (1 + f_major|trial), data = invest, control = lmerControl(optCtrl = list(maxfun = 1e+9)))
summary(minmodel)
#is there a significative difference in the variance explained when the model has the moderation effect?
#we already know that the moderation is not significative(except for physics), but it's good to test it.
anova(minmodel, maxmodel)

#dont forget to cite
citation()#ya
citation('lme4')#ya
citation("coefplot")#ya
citation("effects")#ya
citation('car')#ya
citation('psych')#ya
citation('lsmeans')#ya
citation('afex')#ya
citation('pbkrtest')#ya
citation('lattice')#ya
citation('parallel')#ya
citation("plotrix")#ya
citation('ggplot2')#ya
citation("influence.ME")#ya
