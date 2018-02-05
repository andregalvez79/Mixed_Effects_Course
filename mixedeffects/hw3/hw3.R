install.packages('lme4')
library (lme4)

install.packages('car')
library (car)

install.packages('ggplot2')
library (ggplot2)

install.packages('lattice')
library (lattice)

install.packages('psych')
library (psych)

cherry <- read.csv("C:\\Users\\s4600479\\Desktop\\CherryPit_large_N30Gender.csv", sep = ",")
cherry <- read.csv("C://Users//André//Google Drive//Master//period 3//mixedeffects//hw3//CherryPit_large_N30Gender.csv", sep = ",")


#contrast to zero sum, because...
contrasts(cherry$gender) = contr.sum(2)
#1.	Is there a significant trial effect? If so, in which direction does it go 
#(i.e., do participants reach better or worse distances over the course of the 5 trials)?
with(cherry, boxplot(distance ~ trial))
ggplot(data = cherry, aes(x = trial, y = distance)) + geom_point() + geom_smooth()
xyplot(distance ~ trial | pid, data= cherry, type = c('g', 'p', 'r'))

#A)for this hw we need to scale trials
cherry$numtrialc <- scale(cherry$num_trial, center = T, scale = F)
#note:	Only within-subject effects can be (and should be) modeled as random slopes

#B)A densityplot showing distance as a function of trial and gender, with each trial being shown in a separate panel, and each gender being represented in a different color. 
densityplot(~ distance | trial, group = gender, auto.key = TRUE, data = cherry)

#One panel per participant, ordered by participant code and showing trial on the x axis and distance on the y axis.
xyplot(distance ~ num_trial | pid, data = cherry, type = c('g', 'p', 'r'))

#The same as the previous one, but ordered by intercept in descending order. 
xyplot(distance ~ num_trial | pid, data = cherry, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])

#The same as the previous one, but ordered by trial slope in ascending order.
xyplot(distance ~ num_trial | pid, data = cherry, type = c('g', 'p', 'r'), index.cond = function(x,y) coef(lm(y ~ x))[1])

#An xyplot with 1 panel for females and one panel for males, showing each one regression line
xyplot(distance ~ num_trial | gender, data = cherry, type = c('g', 'p', 'r'))

#1 panel for females and one panel for males. However, a separate regression line per participant is shown (and participants are shown in different colors).
xyplot(distance ~ num_trial | gender, data = cherry, group = pid, type = c('g', 'r'))

#c)Run your lmer model and do the following things:
#Thoroughly inspect your summary() for the model:

cherryg <- lmer(distance ~ numtrialc*gender + (1 + numtrialc| pid), data = cherry)
summary(cherryg)
#What is the variance associated with the participant intercepts? 27.374
#What is the variance associated with the random slopes? 3.127
#What is the random correlation? What does that mean? -.18, it is the covariance between random intercept (participant) and random slope (trial)
#What is the residual variance? 15.095  
#What is the estimate for the intercept? What does that number mean?It means that the average distance of this sample is 15.098 distance.
#What is the estimate for the Gender effect? What does that number mean? 3.62148 It means that females spit 4.03 distance further than the average, and men spit 4.03 distance less than the average. Females spit further than most males.
#What is the estimate for the trial effect? What does that number mean? -0.07861 It means that in average participants get worst (less distance) as the trials advance.
#What is the estimate for the interaction effect? What does that number mean? 1.12061 It means that females spit a longer distance every new trial.
#what do you think is the average distance of female participants on trial 2? 18.4 (Plug in the relevant numbers into the regression equations; for such computations, you can ignore the random effects and use only the fixed effect estimates.)
#What do you think is the average distance of male participants on trial 4? 11.14
#Verify these numbers by using describeBy from the library psych to get the respective raw means.
install.packages('psych')
library (psych)
describeBy(cherry$distance, list(cherry$gender,cherry$num_trial))
#Were your guesses based on the intercepts and slope correct or not? If not, why not? yes, it was correct

#(d) Model diagnostics: Create the following plots
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
#Two caterpillar plots showing the random effects: One caterpillar plot should show on the y axis each participant with equal spacing; the other should show them spaced according to standard normal quantiles.
dotplot(ranef(cherryg, condVar = TRUE))
qqmath(ranef(cherryg, condVar = TRUE))
#Compute the correlation between the observed and fitted values and the pseudo-R2 based on that correlation. Are these numbers different from the model you ran in your last homework? If it has changed, why do you think it has changed the way it has changed?
cor(cherry$distance, fitted(cherryg)) ^ 2

#e) confidence intervals

# bootstrap
confint(cherryg, method = 'boot', .progress="txt", level= .90, nsim = 1000 , PBargs=list(style=3))
confint(cherryg, method = 'boot', .progress="txt", level= .95, nsim = 1000 , PBargs=list(style=3))
#profile long time
cherryprof1 <- profile(cherryg, signames= FALSE)
confint(cherryprof1, level = 0.90)
confint(cherryprof1, level = 0.95)
#############################2##########################
#sleepstudy data inside lme4 (Reactions, Days, Subject)

sleep <-sleepstudy


#scale days, to make the intercept represent the middle of the experiment...and 1 unit change in Y is a change of 1 in x (change of 1 in RT is change in one day) 
sleep$daysc <- scale(sleepstudy$Days, center=T, scale=F)
#factor days
sleep$daysf <-as.factor(paste("Day", sleepstudy$Days, sep = '_'))
sleep$dayscf <-as.factor(paste("Day", sleepstudy$daysc, sep = '_'))
#factor subject
sleep$pidf <-as.factor(paste("pid", sleep$Subject, sep = '_'))

#lmer syntax
sleepg <- lmer(Reaction ~ daysc + (1 +  daysc|pidf), data = sleep)
summary(sleepg)
#what do you think is the average response time on day 3 (trial 4) (298.508-10.467)= 288
#check correspondance
unique(cbind(sleep$Days, sleep$daysc))
#check
describeBy(sleep$Reaction, sleep$Days)
#mean 282.99
#profile long time
sleepprofci <- profile(sleepg, signames= FALSE)
confint(sleepprofci, level = 0.95)


