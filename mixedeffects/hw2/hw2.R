install.packages('lme4')
library (lme4)

install.packages('car')
library (car)

cherry <- read.csv("C:\\Users\\s4600479\\Desktop\\CherryPit_large_N30Gender.csv", sep = ",")

#contrast to zero sum, because...
contrasts(cherry$gender) = contr.sum(2)

#is a gender effect in how far participants can spit their cherry pits.
#a) Write down the lmer syntax 


cherryg <- lmer(distance ~ gender + (1 | pid), data = cherry)

#b) Create some plots to understand the data

#A boxplot showing distance as a function of gender
install.packages('ggplot2')
library(ggplot2)
box <- qplot(gender, distance,	data=cherry,	geom="boxplot")
#A densityplot showing a separate curve for females and males.
density <- ggplot(cherry, aes(x=distance)) + xlim(-10,40) + geom_density(aes(group=gender, colour=gender, fill=gender), alpha=0.3)

#c) run model
cherryg <- lmer(distance ~ gender + (1 | pid), data = cherry)
#summary
summary(cherryg)
#residuals are in the sclae no more than +-3, number of participants and the whole matrix was used
#What is the variance associated with the participant code? 25.33
#What is the residual variance? 25.29

#Compute the ICC as discussed during class
#first run with intercept only
cherrygint <- lmer(distance ~ (1 | pid), data = cherry)
#use the intercept variance of random effects = 25.33
#the the normal one
cherryg <- lmer(distance ~ gender + (1 | pid), data = cherry)
#Divide the variance associated with the grouping factor(25.33) by the sum of this and the residual variance (25.29)
# ICC= 25.33/(25.33+25.29) = 0.5003951
25.33/(25.33+25.29)
#Use this ICC estimate to compute the "effective sample size" using the formula they give in the Aarts et al. (2014) paper (hint: they give it in Figure 2d)
#Neff = Ntotal/1+(n=number of trials per participant per cluster = ntotal/amount of obs per participant = 150/30 -1)*ICC
150/(1+(5-1)*0.5003951)

#	What is the estimate for the intercept? What does that number mean?
summary(cherryg)
#the estimate is 15.981, this means that the overall spitting distance of a person is 15.98
#What is the estimate for the gender effect? What does that number mean?
#3.621, females spit 3.621 distance more than the average of 15.981, and males spit 3.621 less than the average of 15.981
#average distance for females (determined by contrast)
3.621+15.981
#and the average distance for males
-3.621+15.981
install.packages("pastecs")
library(pastecs)
by(cherry$distance, cherry$gender, mean)
stat.desc(cherry$distance)

#d)Model diagnostics: Create the following plots
# A densityplot of the scaled model residuals
install.packages("lattice")
library(lattice)
dplot <- lattice::densityplot(resid(cherryg, scaled = TRUE))
#A q-q plot of the residuals; you can simply use qqmath(MyModel) or you can use qqPlot() from the package car
qqmath(cherryg)
#compute the numbers standardize residuals... run without the / is the number, with the / is the proportion
sum(abs(resid(cherryg, scaled = TRUE)) > 3)/ length(resid(cherryg))
sum(abs(resid(cherryg, scaled = TRUE)) > 2.5)/ length(resid(cherryg))
sum(abs(resid(cherryg, scaled = TRUE)) > 2)/ length(resid(cherryg))

#A boxplot showing for each participant the distribution of the scaled residuals
plot(cherryg, pid ~ resid(., scaled = TRUE))
#A scatter plot showing the fitted vs. residuals values
plot(cherryg, type = c('p', 'smooth'))
#A scatter plot showing the observed vs. the fitted values
plot(cherry$distance, fitted(cherryg))
#Add a regression line to the previous scatter plot
car::scatterplot(fitted(cherryg) ~ cherry$distance, boxplots =FALSE, smoother = FALSE, xlim = c(0, 32), ylim = c(0, 30))
#compute the correlation and this Pseudo-R2
cor(cherry$distance, fitted(cherryg)) ^ 2
# e) confidence intervals with wald
confint(cherryg, level = 0.95, method = 'Wald')
confint(cherryg, level = 0.99, method = 'Wald')
confint(cherryg, level = 0.999, method = 'Wald')
# profile CI
cherryprofci <- profile(cherryg, level = 0.95, signames= FALSE)
confint(cherryprofci, level = 0.95)
confint(cherryprofci, level = 0.99)
confint(cherryprofci, level = 0.999)
#bootstraps
confint(cherryg, method = 'boot', .progress="txt", level= .95, nsim = 1000 , PBargs=list(style=3))
confint(cherryg, method = 'boot', .progress="txt", level= .99, nsim = 1000 , PBargs=list(style=3))
confint(cherryg, method = 'boot', .progress="txt", level= .999, nsim = 10000 , PBargs=list(style=3))


###########2############

feather <- read.csv("C://Users//André//Google Drive//Master//period 3//mixedeffects//hw2//FeatherContest_13Feb2016.csv", sep = ",")
contrasts(feather$smoking_status) = contr.sum(2)

#a) Write down the lmer syntax 

featherg <- lmer(distance ~ smoking_status + (1 | pid), data = feather)

#b)A densityplot showing a separate curve for females and males.
plot(density(feather$distance))
densityf <- ggplot(feather, aes(x=distance)) + xlim(-4,6) + geom_density(aes(group=smoking_status, colour=smoking_status, fill=smoking_status), alpha=0.3)


#c) run model 

featherg <- lmer(distance ~ smoking_status + (1 | pid), data = feather)

#d)Model diagnostics: Create the following plots
# A densityplot of the scaled model residuals
install.packages("lattice")
library(lattice)
dplot <- lattice::densityplot(resid(featherg, scaled = TRUE))
dplot

#A q-q plot of the residuals; you can simply use qqmath(MyModel) or you can use qqPlot() from the package car
qqmath(featherg)

#A scatter plot showing the fitted vs. residuals values
plot(featherg, type = c('p', 'smooth'))

#A scatter plot showing the observed vs. the fitted values
plot(feather$distance, fitted(featherg))

#Add a regression line to the previous scatter plot
car::scatterplot(fitted(featherg) ~ feather$distance, boxplots =FALSE, smoother = FALSE, xlim = c(-3, 6), ylim = c(-3, 6))

#compute the numbers standardize residuals... run without the / is the number, with the / is the proportion
sum(abs(resid(featherg, scaled = TRUE)) > 3)/ length(resid(featherg))
sum(abs(resid(featherg, scaled = TRUE)) > 2.5)/ length(resid(featherg))
sum(abs(resid(featherg, scaled = TRUE)) > 2)/ length(resid(featherg))

#summary
summary(featherg)
#residuals are in the sclae no more than +-3, number of participants and the whole matrix was used
#What is the variance associated with the participant code? 1.2119
#What is the residual variance? .5352

#What is the estimate for the intercept? What does that number mean?
summary(cherryg)
#the estimate is 1.3945, this means that the overall distance of blowing a feather is 1.39
#What is the estimate, standard error, and t value for the smoking status effect? What does that number mean? note: el -1 es non smokers, el 1 es smokers. arriba females debe ser 1 y -1 males
#the estimate = .1433, SE = .1179, t = 1.215, non-smokers blow the feather .1433 distance more than the average of 1.39, and smokers blow the feather .1433 less than the average of 1.39
#average distance for non-smokers(1) (determined by contrast)
.1433+1.3945
#and the average distance for smokers (-1)
-.1433+1.3945

#should be the same
install.packages("pastecs")
library(pastecs)
by(feather$distance, feather$smoking_status, mean)


# e) confidence intervals 

# profile CI
featherprofci <- profile(featherg, level = 0.95, signames= FALSE)
confint(featherprofci, level = 0.95)
confint(featherprofci, level = 0.99)
confint(featherprofci, level = 0.999)

#bootstraps to use different types different method
install.packages("boot")
library (boot)

#Create a function (here to return fixed effexts)
FUN_bootMer <- function(fit) {return(fixef(fit)) }

#create the dataframe necessary to bootstrap
featherboot <- bootMer(featherg, FUN_bootMer,nsim = 10000, type = "parametric", .progress ="txt", PBargs = list(style = 3))

#then bootstrap the dataframe index=2 because 1 is intercept.
featherboot95 <- boot.ci(featherboot, index= 2, conf = 0.95, type=c("norm", "basic", "perc"))
featherboot99 <- boot.ci(featherboot, index= 2, conf = 0.99, type=c("norm", "basic", "perc"))
featherboot999 <- boot.ci(featherboot, index= 2, conf = 0.999, type=c("norm", "basic", "perc"))

#check the bootsrapped objects
featherboot95 
featherboot99 
featherboot999

#(f) Smokers vs. non-smokers conclusion



