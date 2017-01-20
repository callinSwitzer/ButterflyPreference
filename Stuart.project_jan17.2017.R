# Callin Switzer
# 20 Jan 2016
# Update to previously written code by
# Heather and Stuart


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

packages <- c("ggplot2", "lme4", "car")
ipak(packages)


# Heather's update:
#Update on "Stuart" analysis. January,16, 2017
#Note: ALB is light blue. I put an A in front so that model would deliver preference in comparison to LB visits (alphabetical)
#Same goes for the "context" ADvD. 

#Data
#I kept skippers and battus separate in these analyses because that way it was easier for me to isolate the array type I was interested in. 
#I am also including the combined version here. 


# Load data
#battus
battus.2 <- read.csv("Swallowtails.2.csv", header=TRUE, sep=",", dec = ".")

#skippers
skip2 <- read.csv("skipper.2.csv", header=TRUE, sep=",", dec = ".")

#combined data. Including this combined version but did not use it for the analyses below.
all<-read.csv("Batt.skip.Stuart.csv",header=TRUE, sep=",", dec = ".")


#__________________________________________________________________
#Battus
batLR <- battus.2[battus.2$Array_type == "LR",]
colnames(batLR)

batLR <- batLR[,c('Year', 'Array_type', 'ALB_Count', 'Context', 'other_Count', 'Color_other', 'total.visits')]
car::scatterplotMatrix(batLR[c(1,3,5,7)])


#1. LR vs LB array
batarray1<-glm(cbind(ALB_Count,other_Count) ~ Context , family = binomial, data = battus.2[battus.2$Array_type == "LR",])
summary(batarray1)

# model diagnostics
par(mfrow = c(2,3))
plot(batarray1, which = 1:6)

# deviance residuals
plot(batarray1$fitted.values, residuals(batarray1, type="deviance"),
     xlab="fitted probabilities",
     ylab="deviance residuals")


#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      0.5162     0.1469   3.514 0.000441 ***
#ContextDvCusp   -2.4941     0.2708  -9.211  < 2e-16 ***
#ContextDvDcusp  -0.7137     0.1790  -3.987 6.68e-05 ***

#there is a significant preference for LR over LB in both the Dvcusp and the dvdcusp compared to preference in DVD. 


#2. DR vs LB array
batarray2<-glm(cbind(ALB_Count,other_Count) ~ Context , family = binomial, data = battus.2[battus.2$Array_type == "DR",])
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      0.5524     0.1403   3.937 8.26e-05 ***
#ContextDvCusp   -2.1298     0.2884  -7.384 1.54e-13 ***
#ContextDvDcusp  -0.4322     0.1637  -2.640  0.00828 ** 

#there is a significant preference for DR over LB in both the DvCusp as well as the DvDcusp compared to the DVD. 


#3. DB vs LB array

batarray3<-glm(cbind(ALB_Count,other_Count) ~ Context , family = binomial, data = battus.2[battus.2$Array_type == "DB",])
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      0.2649     0.1204   2.201   0.0278 *  
# ContextDvCusp   -1.7948     0.2242  -8.005 1.19e-15 ***
# ContextDvDcusp  -0.7550     0.1681  -4.490 7.11e-06 ***
# 
# There is a significant preference for DB over LB in both the DvCusp and the DvDwith cusp compared to the DVD context. 


#------------------------------------------------------------------------------
#Skippers

#1. LR vs LB array

skiparray1<-glm(cbind(ALB_count,other_Count) ~ Context, family = binomial, data = skip2[skip2$Array_type == "LR",])
#Estimate Std. Error z value Pr(>|z|)
#(Intercept)     0.28768    0.21183   1.358    0.174
#ContextDvCusp   0.07878    0.26950   0.292    0.770
#ContextDvDcusp -0.34956    0.29351  -1.191    0.234

#no difference between preference for LR and LB across contexts for skipper. There is obviously a difference between the two with cusp (looking at the graph), so I may want to do another contrast there. 


#2. DR vs LB array

skiparray2<-glm(cbind(ALB_count,other_Count) ~ Context , family = binomial, data = skip2[skip2$Array_type == "DR",])
                    #Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        2.3026     0.5244   4.391 1.13e-05 ***
#  ContextDvCusp   -1.0876     0.5684  -1.914   0.0557 .  
#ContextDvDcusp     -0.4394     0.6351  -0.692   0.4891  

#there is a significantly greater preference for DR in the DVCusp context compared to the and DVD context 


#3. DB vs LB array

skiparray3<-glm(cbind(ALB_count,other_Count) ~ Context , family = binomial, data = skip2[skip2$Array_type == "DB",])
summary(skiparray3)
                    #Estimate Std. Error z value Pr(>|z|)    
  #(Intercept)      0.7510     0.1872   4.011 6.03e-05 ***
#  ContextDvCusp   -0.8631     0.2591  -3.331 0.000866 ***
#  ContextDvDcusp   0.3476     0.2704   1.285 0.198633    

#There is  higly significant preference for DB over LB in the DvCusp context compared to the DVD context. 



