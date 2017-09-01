install.packages("mvabund")
library(mvabund)

inverts <- read.csv(file.choose())
head(inverts)

inverts_spp <- mvabund(inverts[,4:15])
head(inverts_spp)

par(mar=c(2,10,2,2)) # adjusts the margins
boxplot(inverts[,4:15],horizontal = TRUE,las=2, main="Abundance")
meanvar.plot(inverts_spp) #check this - is this not v. low variance?
plot(inverts_spp~inverts$site, cex.axis=0.6, cex=0.7)

#The model syntax below fits our response variable (the mvabund object 
#inverts_spp with the counts of 11 species) to the predictor variable site 
mod1 <- manyglm(inverts_spp ~ inverts$site, family="poisson")

#need to check model assumptions - plot of residuals
plot(mod1) #is this wrong? Check with Fred (for poisson family).

inverts$temp <- as.factor(inverts$temp)

#Think we have fan-shape, changing family to negative_binomial
mod2 <- manyglm(inverts_spp ~ inverts$temp, family="negative_binomial")
plot(mod2)

#TODO go through assumption checking properly

anova(mod2)

#So P is low, but not THAT low, how high should LRT value be?

#see which species are more likely to be found at each habitat -
#run univariate tests for each species separately.

anova(mod2, p.uni="adjusted") #don't quite get this - multivariate still 
#says effect but nearly all univariate tests don't?

#can we add temp? Temp is the site though?
#if there is a diff, does that mean unexplained reasons?
mod3 <- manyglm(inverts_spp ~ inverts$site*inverts$temp, 
                family="negative_binomial")
anova(mod3)

#so difference between site, but not temp? Eh?

summary.manyglm(mod2)
