######
#1
install.packages('Matrix', repos='http://cran.us.r-project.org')
packageVersion('Matrix') 
install.packages('pbkrtest', repos='http://cran.us.r-project.org')
packageVersion('pbkrtest') 
install.packages("lme4", repos = c("http://lme4.r-forge.r-project.org/repos", getOption("repos")))
packageVersion('lme4') 
library(lme4)

#sample model
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(fm1)
#maybe for levels? itll come later
?predict.merMod
###########
#2
#lm tutorial
pitch = c(233,204,242,130,112,142)
sex = c(rep("female",3),rep("male",3))
my.df = data.frame(sex,pitch)
xmdl = lm(pitch ~ sex, my.df)
summary(xmdl)
#was	significant	(F(1,4)=46.61,	p<0.01).	(.)"
mean(my.df[my.df$sex=="female",]$pitch)
#this or by
age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
my.df = data.frame(age,pitch)
xmdl = lm(pitch ~ age, my.df)
summary(xmdl)
#center for average age in intercept instead of 0
my.df$age.c = my.df$age - mean(my.df$age)
xmdl = lm(pitch ~ age.c, my.df)
summary(xmdl)
#plot linearity assumption
plot(fitted(xmdl),residuals(xmdl))
#heteroscedasticity assumption in plot
plot(rnorm(100),rnorm(100))
#normality in residuals plots
hist(residuals(xmdl))
qqnorm(residuals(xmdl))
#dfbetas
dfbeta(xmdl)
#	DFbetas	and	look	for	values	that	are different	by	
#at	least	half	of	the	absolute	value	of	the	slope
#independence of observations
##########
#3
#read http://www.nature.com/neuro/journal/v17/n4/pdf/nn.3648.pdf
###############
#4

# recreate plots
install.packages("lattice")
library(lattice)
install.packages("ggplot2")
library(ggplot2)
cherry <- read.csv("C:\\Users\\s4600479\\Desktop\\CherryPit_small_N5.csv", sep = ",")

#dotplot
plot_1 <- qplot(pid,  distance,  data	=	cherry,	geom=c("point"))
plot_1

#density plot
densityplot(~distance|pid,data=cherry,
            xlab="distance (meters)",
            main="distance of cherry by participant")

#xyplot
xyplot(distance ~ trial | pid, cherry, grid = TRUE)
