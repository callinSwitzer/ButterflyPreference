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
m1 <- glmer(cbind(visits.LB, visits.other) ~ context *  pol  *  array + (1 |polID) + (1|date), 
            family = binomial, control = glmerControl(optimizer = 'bobyqa'), data =  polDS )
summary(m1)


# check to see if we need to keep random effect
m2 <- update(m1, .~. - (1|polID))
anova(m1, m2) # keep random effect of polID

m3 <- update(m1, .~. - (1|date))
anova(m1, m3) # keep random effect of date

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

overdisp_fun(m1) # looks like no evidence of overdispersion

# here's another way to check for overdispersion
residDev <- sum(residuals(m1, type = 'deviance')^2) # calculate residual deviance
# this ratio should be about 1 -- larger than 1 suggests overdispersion
residDev / df.residual(m1) 

# check to see if we need to keep three way interaction
m2 <- update(m1, .~.  - context :  pol  :  array)
anova(m1, m2)


# model diagnostics -- resudial vs. fitted values
plot(m1) # no severe outliers

# model diagnostics for random effects -- should be approx normally distributed
qqnorm(resid(m1), main = "")
qqline(resid(m1)) # not too bad

# QQPlot for group-level effects -- ID
qqnorm(ranef(m1)$polID[[1]], main="Normal Q-Q plot for random effects")
qqline(ranef(m1)$polID[[1]])

# QQPlot for group-level effects -- date
qqnorm(ranef(m1)$date[[1]], main="Normal Q-Q plot for random effects")
qqline(ranef(m1)$date[[1]]) 




# hist((polDS$visits.other + polDS$visits.LB), xlim = c(0, 10), breaks = 30)
# dataset with more than 2 total visits
# dsGT2 <- polDS[(polDS$visits.other + polDS$visits.LB) > 2, ]

# get predictions from model with 3-way interactions
polDS$threeWayPreds <- predict(m1, type = 'response')


aa <- ggplot(polDS, aes(x = context, y = threeWayPreds, col = context)) + 
     geom_boxplot() + 
     facet_grid( array~ pol )
aa

# ggsave(filename = "polPrefPredictions.pdf", width = 11, height = 8)


table(polDS$array)


# creat new variable for three way interaction, which will allow us to use
# post-hoc tests
polDS$threeWay <- with(polDS, interaction(context, pol, array, sep = "x"))
polDS$threeWay <- relevel(polDS$threeWay, ref = "DvDxSKIPxDark Blue")

# rerun the model with the new three-way interaction
m5 <- glmer(cbind(visits.LB, visits.other) ~ threeWay + (1|date) + (1|polID), 
            control = glmerControl(optimizer = 'bobyqa'),
            family = binomial, data = polDS)
summary(m5)

# make sure m5 and m1 are the same
# try to do contrasts with the model I 
# artificially constructed the 3-way contrasts on
summary(m1)
summary(m5) # coefficients are different, because some reflevels are different

# same deviance
deviance(m1)
deviance(m5)

# predictions are pretty much the same -- within 0.00001
sum(abs(predict(m1) - predict(m5))  > 0.00001)


# use m5 to calculate multiple comparisons
l2 <- glht(m5, linfct = mcp(threeWay = "Tukey"))
t1 <- summary(l2, test = adjusted(type = "none"))


# put summary of contrasts into dataframe
contrastDF <- data.frame(estimate = t1$test$coefficients, se = t1$test$sigma, TStat = t1$test$tstat, pval = t1$test$pvalues)
contrastDF

# remove whitespace
row.names(contrastDF) <- gsub(x = row.names(contrastDF), pattern = " ", replacement = "")

gps <- t(as.data.frame(strsplit(x = row.names(contrastDF), split = '[x]|[-]')))
row.names(gps) <- NULL

cdf <- cbind(gps, contrastDF)
colnames(cdf)[1:6] <- c("context1", "pol1", "array1", 'context2', 'pol2', 'array2')



# subset contrast DF, so we get only the ones that interest us
cdf <- cdf[as.character(cdf$array1) == as.character(cdf$array2) &
                cdf$pol1 == cdf$pol2 , ]

cdf <- cdf[order(cdf$pol1, cdf$array1), ]

# remove extra columns
cdf[, c("context1", "pol1", "array1", 'context2', 'pol2', 'array2')] <- NULL

# round p-values for table
cdf$pval <- round(cdf$pval, digits = 6)

# write table to file
# write.csv(cdf, file = "Contrasts.csv")







par(mai = c(2,2,2,2))
x11()
graphics.off()
pdf("bigContrasts.pdf", width= 11, height = 70)
par(mai = c(1,5,1,1))
plot(l2)
dev.off()
?par


# Next Steps: 
# Make plot with raw data (weighted proportions w/ bootstrap CI's)
# type out the interpretation of coefficients
# interpret coefficients


# Done
# Confirmed that the two 3-way interaction models are the same
# wrote table (.csv) of relevant contrasts
# learned that if we have a 3-way interaction, we must also include all 2-way interactions 
# I think we can't test two-way interactions in the presence of a 3-way interaction

# how to calculate overdispersion for regular GLM
# deviance(m1)/m1$df.residual # slightly overdispersed, but not too bad



