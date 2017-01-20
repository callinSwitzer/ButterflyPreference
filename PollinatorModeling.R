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


# full model
m1 <- glmer(cbind(visits.LB, visits.other) ~ context *  pol  *  array + (1|polID), family = binomial, control = glmerControl(optimizer = 'bobyqa'), data =  polDS )
summary(m1)


m2 <- update(m1, .~. - context :  pol  :  array)
summary(m2)

anova(m1, m2)

polDS$threeWayPreds <- predict(m1, type = 'response')

ggplot(polDS, aes(x = context, y = threeWayPreds, col = context)) + 
     geom_boxplot() + 
     facet_grid(~ pol + array, margins = c(2,4))


table(polDS$array)


