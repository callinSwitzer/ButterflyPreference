## Callin Switzer
## 20 Jan 2016
## Merge pollinator datasets to include date

library(reshape2)
ds1 <- read.csv("Batt.skip.Stuart.2017.csv",header=TRUE, sep=",", dec = ".")
nrow(ds1)

# read in long dataset
ds2 <- read.csv("poldata3yr.csv")
head(ds2)


library(plyr)
ds2$pol <- mapvalues(ds2$pol, from = c(" BAT"), to = "BAT")

ds2[ds2$ind == 1040, ]

# convert to short format
ds2s <- reshape(ds2, 
                timevar = 'color',
                idvar = c("ind", "context", "date", "array", "pol"),
                direction = "wide")

colnames(ds2s)
ds2s$visits.other <- rowSums(ds2s[, c("visits.DB","visits.DR","visits.LR", "visits.drmLB")], na.rm = TRUE)

ds2s[ds2s$ind == 1040, ]

ds2s[is.na(ds2s)] = 0

head(ds2s)
nrow(ds2s)

ds3s <- ds2s[ds2s$visits.LB != 0 | ds2s$visits.other != 0,]
nrow(ds3s)
as.character(unique(ds3s$pol))

ds3s[, c(6,7,8, 10)] <- NULL

# write.csv(ds3s, file = "PollinatorShortFormat.csv", row.names = FALSE)


# TODO, later: compare the two datasets to see the 627 vs 683 rows
head(ds1)


# make unique id column for pollinator
ds3s$polID <- paste(ds3s$date, ds3s$ind, ds3s$pol, sep = "_")



#
