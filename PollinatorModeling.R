## Callin Switzer
## 20 Jan 2016
## start of pollinator modeling

# Questions that we want to ask
# 1)	Is there preference for color (lb vs dr, lb vs db, lb vs lr)?
# 2)	Does the preference for color change with context of array (drum vs drum, drum vs drum with cusp, cusp vs. drum)?
# 3)	Do the two pollinators vary in their preferences? 


# load packages:
ipak <- function(pkg){
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
     if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
     sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", "lme4", "car", "multcomp")
ipak(packages)


polDS <- read.csv('PollinatorShortFormat.csv')
colnames(polDS)

# drop Light blue vs. Cusp
polDS <- droplevels(polDS[polDS$array != "Light Blue",])

# write.csv(x = polDS, "PollShortFormat_NoLB.csv", row.names = FALSE)



polDS$context <- relevel(polDS$context, ref = "DvD")
polDS$pol <- relevel(polDS$pol, ref = "SKIP")

# mod1
pol_DR <- polDS[polDS$array == 'Dark Blue', ]
m1 <- glmer(cbind(visits.LB, visits.other) ~ context *  pol + (1|polID), family = binomial, pol_DR )
summary(m1)

levels(pol_DR$context)

m2 <- update(m1, .~. - context :  pol)
summary(m2)

anova(m1, m2)

pol_DR$pref <- pol_DR$visits.LB / (pol_DR$visits.LB + pol_DR$visits.other)
pol_DR$totalVisits <- (pol_DR$visits.LB + pol_DR$visits.other)

theme_set(theme_classic())

ggplot(pol_DR, aes(x = pol, y = pref)) + 
     geom_boxplot() + 
     facet_wrap(~context) + 
     geom_point(position = position_jitter())

lm1 <- lm(pref ~ pol * context, weights =totalVisits, data = pol_DR) 
summary(lm1)

pol_DR$predictions = predict(lm1)

ggplot(pol_DR, aes(x = pol, y = predictions)) + 
     geom_boxplot() + 
     facet_wrap(~context)

ggplot(pol_DR, aes(x = context, y = predictions, col = context)) + 
     geom_boxplot() + 
     facet_wrap(~pol)


table(pol_DR$context, pol_DR$pol)


table(pol_DR$visits.LB, pol_DR$pol)
table(pol_DR$visits.other, pol_DR$pol)
colnames(polDS)

# full model
m1 <- glmer(cbind(visits.LB, visits.other) ~ context *  pol  *  array + (1 |polID) + (1|date), family = binomial, control = glmerControl(optimizer = 'bobyqa'), data =  polDS )
summary(m1)

# check for overdispersion
overdisp_fun <- function(model) {
     ## number of variance parameters in 
     ##   an n-by-n variance-covariance matrix
     vpars <- function(m) {
          nrow(m)*(nrow(m)+1)/2
     }
     model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
     rdf <- nrow(model.frame(model))-model.df
     rp <- residuals(model,type="pearson")
     Pearson.chisq <- sum(rp^2)
     prat <- Pearson.chisq/rdf
     pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
     c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(m1) # double check this later

m2 <- update(m1, .~.  - context :  pol  :  array)
anova(m1, m2)


# TO LOOK UP: can you drop a two-way interaction with a three way interaction
# TO DO: interpret coefficients, and make contrasts for each of the six boxes

# hist((polDS$visits.other + polDS$visits.LB), xlim = c(0, 10), breaks = 30)

# dataset with more than 2 total visits
# dsGT2 <- polDS[(polDS$visits.other + polDS$visits.LB) > 2, ]
polDS$threeWayPreds <- predict(m1, type = 'response')


aa <- ggplot(polDS, aes(x = context, y = threeWayPreds, col = context)) + 
     geom_boxplot() + 
     facet_grid( array~ pol )
aa

ggsave(filename = "polPrefPredictions.pdf", width = 11, height = 8)


table(polDS$array)

# TODO: find where the three way interactions

polDS$threeWay <- with(polDS, interaction(context, pol, array, sep = "x"))
m5 <- update(m1, .~ + threeWay + (1|date) + (1|polID))
summary(m5)

nrow(summary(m5)$coefficients)

l2 <- glht(m5, linfct = mcp(threeWay = "Tukey"))
t1 <- summary(l2, test = adjusted(type = "none"))
write.table(t1, file = 'test.txt')


par(mai = c(2,2,2,2))
x11()
graphics.off()
pdf("bigContrasts.pdf", width= 11, height = 70)
par(mai = c(1,5,1,1))
plot(l2)
dev.off()
?par

# try to do contrasts
summary(m1)


# difference between ses = 2 and ses =3 when female = 0
K <- matrix(c(0, 0, 0, 1, -1, 0, 0, 0,0,0,0,0,0,0,0,0,0,0), nrow = 2)
t <- glht(m1, linfct = K)
plot(t)
summary(t)


# difference between 
summary(m1)

# Next Steps: 
# Make plot with raw data (weighted proportions w/ bootstrap CI's)
# type out the interpretation of coefficients
# model diagnostics
# get a summary table of the contrasts we want
# read -- see if we can test two-way interactions in the presence of a 3-way interaction
# check to make sure the contrasts are right -- see if models are the same

deviance(m1)/m1$df.residual # slightly overdispersed, but not too bad



