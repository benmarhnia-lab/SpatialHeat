
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Documents - Kristen’s MacBook Pro/EnviroResearchArmin")
#matchedDataset is the resulting dataset after running the matching procedure
matchedData <- read.csv("hw_min99_3.csv", header = T) # Take matched dataset

#Get aggregated hospitalization counts
d <- aggregate(matchedData$diffMean, by = list(zipcode = matchedData$patzip), FUN = mean)
names(d) <- c("id", "Y")
library(dplyr)
population <- read.csv("2010Census_DemoProfile.csv")
population$Total.population = as.numeric(gsub(",", "", population$Total.population))
d <- left_join(d, population, by =c("id" = "zipnum"))

d$ZCTA5CE10 <- d$id
mod = lm(Y~Total.population,data = d)
resids=d$Y - predict(mod, d)
d$Y = resids

#spBayes

library("spBayes")
library(MBA)
#library(geoR)
library(fields)
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(lattice)
library(gstat) 
library(coda)
coords <- read.csv("~/Downloads/zip_gps.csv", header = T)
coords <- coords[which(coords$ZCTA5 >= 90001),]

coords$ZCTA5CE10 <- coords$ZCTA5
names(d)
bayesDF<- merge(d, coords, by = c("ZCTA5CE10"))
bayesDF = bayesDF[complete.cases(bayesDF), ]
#Assumes Isotropy 

coord = as.matrix(bayesDF[,c("FinalLat", "FinalLon")])
n.samples = 1000
bef.sp<-spLM(Y ~ 1, data = bayesDF, coords = coord, starting = list("phi" = 2, "sigma.sq" = 0.4, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.10, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 3), "sigma.sq.IG" = c(1, 1)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)

bayesDF$w.hat.mu = w.hat.mu
bayesDF$w.hat.sd = w.hat.sd

results = data.frame(bayesDF$Y, bayesDF$ZCTA5CE10, bayesDF$w.hat.mu, bayesDF$w.hat.sd)

write.csv(results, "bayesDF_rel_min99_3.csv")

#Interpolated Surface

#y.residuals = residuals(lm(Y ~ Total.population, data = bayesDF))
#par(mfrow = c(1,2))
#surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est

#image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
# par(mfrow = c(1,1))
# surf <- mba.surf(cbind(bayesDF[,c(23:24)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# z.lim = range(surf[[3]], na.rm = T)
# par(mfrow = c(1,1))
# image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")
# 
# library(tigris)
# library("rgeos")
# caloutline <- states() %>% filter_state("California")
# #caloutline <- st_as_sf(caloutline)
# plot(caloutline$geometry, add = T)
# 


# #Relative Scale
# 
# bayesDF_var = bayesDF
# coordinates(bayesDF_var) <- ~FinalLat + FinalLon
# TheVariogram=variogram(Y ~ Total.population, data=bayesDF_var)
# plot(TheVariogram)
# TheVariogramModel <- vgm(psill=0.05, model="Sph", nugget=0.0, range=1)
# plot(TheVariogram, model=TheVariogramModel) 
# #Assumes Isotropy Clearly that doesn't follow here
# #Tau..sq = 0.0
# #sg.sq = 0.05
# #range = 1
# n.samples = 10000
# bayesDF=bayesDF[!is.na(bayesDF$population), ]
# bef.sp<-spLM(Y ~ population, data = bayesDF, coords = as.matrix(bayesDF[,c("FinalLat", "FinalLon")]), starting = list("phi" = 1, "sigma.sq" = 0.05, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.02, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 1.5), "sigma.sq.IG" = c(2, 0.05)), cov.model = "spherical", n.samples = n.samples)
# round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
# burn.in <- floor(0.75*n.samples)
# bef.sp <- spRecover(bef.sp, start = burn.in)
# 
# beta.samples <- bef.sp$p.beta.recover.samples
# w.samples = bef.sp$p.w.recover.samples
# 
# w.hat.mu = apply(w.samples, 1, mean)
# w.hat.sd = apply(w.samples, 1, sd)
# 
# 
# #Interpolated Surface
# 
# y.residuals = residuals(lm(Y ~ population, data = d))
# par(mfrow = c(1,2))
# surf.z <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# z.lim = range(surf.z[[3]], na.rm = T)
# image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
# 
# par(mfrow = c(1,1))
# surf.rel <- mba.surf(cbind(bayesDF[,c(7:8)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# image.plot(surf.rel, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")
# 
# library(tigris)
# library("rgeos")
# caloutline <- states() %>% filter_state("California")
# 
# plot(caloutline, add = T)


matchedData <- read.csv("hw_min99_2.csv", header = T) # Take matched dataset

#Get aggregated hospitalization counts
d <- aggregate(matchedData$diffMean, by = list(zipcode = matchedData$patzip), FUN = mean)
names(d) <- c("id", "Y")
library(dplyr)
population <- read.csv("2010Census_DemoProfile.csv")
population$Total.population = as.numeric(gsub(",", "", population$Total.population))
d <- left_join(d, population, by =c("id" = "zipnum"))

d$ZCTA5CE10 <- d$id
mod = lm(Y~Total.population,data = d)
resids=d$Y - predict(mod, d)
d$Y = resids

#spBayes

library("spBayes")
library(MBA)
#library(geoR)
library(fields)
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(lattice)
library(gstat) 
library(coda)
coords <- read.csv("~/Downloads/zip_gps.csv", header = T)
coords <- coords[which(coords$ZCTA5 >= 90001),]

coords$ZCTA5CE10 <- coords$ZCTA5
names(d)
bayesDF<- merge(d, coords, by = c("ZCTA5CE10"))
bayesDF = bayesDF[complete.cases(bayesDF), ]
#Assumes Isotropy 

coord = as.matrix(bayesDF[,c("FinalLat", "FinalLon")])
n.samples = 1000
bef.sp<-spLM(Y ~ 1, data = bayesDF, coords = coord, starting = list("phi" = 2, "sigma.sq" = 0.4, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.10, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 3), "sigma.sq.IG" = c(1, 1)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)

bayesDF$w.hat.mu = w.hat.mu
bayesDF$w.hat.sd = w.hat.sd

results = data.frame(bayesDF$Y, bayesDF$ZCTA5CE10, bayesDF$w.hat.mu, bayesDF$w.hat.sd)

write.csv(results, "bayesDF_rel_min99_2.csv")

#Interpolated Surface

#y.residuals = residuals(lm(Y ~ Total.population, data = bayesDF))
#par(mfrow = c(1,2))
#surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est

#image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
# par(mfrow = c(1,1))
# surf <- mba.surf(cbind(bayesDF[,c(23:24)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# z.lim = range(surf[[3]], na.rm = T)
# par(mfrow = c(1,1))
# image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")
# 
# library(tigris)
# library("rgeos")
# caloutline <- states() %>% filter_state("California")
# #caloutline <- st_as_sf(caloutline)
# plot(caloutline$geometry, add = T)



#######min 99 1 day#########
matchedData <- read.csv("hw_min99.csv", header = T) # Take matched dataset

#Get aggregated hospitalization counts
d <- aggregate(matchedData$diffMean, by = list(zipcode = matchedData$patzip), FUN = mean)
names(d) <- c("id", "Y")
library(dplyr)
population <- read.csv("2010Census_DemoProfile.csv")
population$Total.population = as.numeric(gsub(",", "", population$Total.population))
d <- left_join(d, population, by =c("id" = "zipnum"))

d$ZCTA5CE10 <- d$id
mod = lm(Y~Total.population,data = d)
resids=d$Y - predict(mod, d)
d$Y = resids

#spBayes

library("spBayes")
library(MBA)
#library(geoR)
library(fields)
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(lattice)
library(gstat) 
library(coda)
coords <- read.csv("~/Downloads/zip_gps.csv", header = T)
coords <- coords[which(coords$ZCTA5 >= 90001),]

coords$ZCTA5CE10 <- coords$ZCTA5
names(d)
bayesDF<- merge(d, coords, by = c("ZCTA5CE10"))
bayesDF = bayesDF[complete.cases(bayesDF), ]
#Assumes Isotropy 

coord = as.matrix(bayesDF[,c("FinalLat", "FinalLon")])
n.samples = 1000
bef.sp<-spLM(Y ~ 1, data = bayesDF, coords = coord, starting = list("phi" = 2, "sigma.sq" = 0.4, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.10, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 3), "sigma.sq.IG" = c(1, 1)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)

bayesDF$w.hat.mu = w.hat.mu
bayesDF$w.hat.sd = w.hat.sd

results = data.frame(bayesDF$Y, bayesDF$ZCTA5CE10, bayesDF$w.hat.mu, bayesDF$w.hat.sd)

write.csv(results, "bayesDF_rel_min99_1.csv")

#Interpolated Surface

#y.residuals = residuals(lm(Y ~ Total.population, data = bayesDF))
#par(mfrow = c(1,2))
#surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est

#image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
# par(mfrow = c(1,1))
# surf <- mba.surf(cbind(bayesDF[,c(23:24)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# z.lim = range(surf[[3]], na.rm = T)
# par(mfrow = c(1,1))
# image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")
# 
# library(tigris)
# library("rgeos")
# caloutline <- states() %>% filter_state("California")
# #caloutline <- st_as_sf(caloutline)
# plot(caloutline$geometry, add = T)




####min 975 3 day #########
matchedData <- read.csv("hw_min975_3.csv", header = T) # Take matched dataset

#Get aggregated hospitalization counts
d <- aggregate(matchedData$diffMean, by = list(zipcode = matchedData$patzip), FUN = mean)
names(d) <- c("id", "Y")
library(dplyr)
population <- read.csv("2010Census_DemoProfile.csv")
population$Total.population = as.numeric(gsub(",", "", population$Total.population))
d <- left_join(d, population, by =c("id" = "zipnum"))

d$ZCTA5CE10 <- d$id
mod = lm(Y~Total.population,data = d)
resids=d$Y - predict(mod, d)
d$Y = resids

#spBayes

library("spBayes")
library(MBA)
#library(geoR)
library(fields)
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(lattice)
library(gstat) 
library(coda)
coords <- read.csv("~/Downloads/zip_gps.csv", header = T)
coords <- coords[which(coords$ZCTA5 >= 90001),]

coords$ZCTA5CE10 <- coords$ZCTA5
names(d)
bayesDF<- merge(d, coords, by = c("ZCTA5CE10"))
bayesDF = bayesDF[complete.cases(bayesDF), ]
#Assumes Isotropy 

coord = as.matrix(bayesDF[,c("FinalLat", "FinalLon")])
n.samples = 1000
bef.sp<-spLM(Y ~ 1, data = bayesDF, coords = coord, starting = list("phi" = 2, "sigma.sq" = 0.4, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.10, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 3), "sigma.sq.IG" = c(1, 1)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)

bayesDF$w.hat.mu = w.hat.mu
bayesDF$w.hat.sd = w.hat.sd

results = data.frame(bayesDF$Y, bayesDF$ZCTA5CE10, bayesDF$w.hat.mu, bayesDF$w.hat.sd)

write.csv(results, "bayesDF_rel_min975_3.csv")

#Interpolated Surface

#y.residuals = residuals(lm(Y ~ Total.population, data = bayesDF))
#par(mfrow = c(1,2))
#surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est

#image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
# par(mfrow = c(1,1))
# surf <- mba.surf(cbind(bayesDF[,c(23:24)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# z.lim = range(surf[[3]], na.rm = T)
# par(mfrow = c(1,1))
# image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")
# 
# library(tigris)
# library("rgeos")
# caloutline <- states() %>% filter_state("California")
# 
# #caloutline <- st_as_sf(caloutline)
# plot(caloutline$geometry, add = T)

#####min 975 2 Day ######
matchedData <- read.csv("hw_min975_2.csv", header = T) # Take matched dataset
d <- aggregate(matchedData$diffMean, by = list(zipcode = matchedData$patzip), FUN = mean)
names(d) <- c("id", "Y")
library(dplyr)
population <- read.csv("2010Census_DemoProfile.csv")
population$Total.population = as.numeric(gsub(",", "", population$Total.population))
d <- left_join(d, population, by =c("id" = "zipnum"))

d$ZCTA5CE10 <- d$id
mod = lm(Y~Total.population,data = d)
resids=d$Y - predict(mod, d)
d$Y = resids

#spBayes

library("spBayes")
library(MBA)
#library(geoR)
library(fields)
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(lattice)
library(gstat) 
library(coda)
coords <- read.csv("~/Downloads/zip_gps.csv", header = T)
coords <- coords[which(coords$ZCTA5 >= 90001),]

coords$ZCTA5CE10 <- coords$ZCTA5
names(d)
bayesDF<- merge(d, coords, by = c("ZCTA5CE10"))
bayesDF = bayesDF[complete.cases(bayesDF), ]
#Assumes Isotropy 

coord = as.matrix(bayesDF[,c("FinalLat", "FinalLon")])
n.samples = 1000
bef.sp<-spLM(Y ~ 1, data = bayesDF, coords = coord, starting = list("phi" = 2, "sigma.sq" = 0.4, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.10, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 3), "sigma.sq.IG" = c(1, 1)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)

bayesDF$w.hat.mu = w.hat.mu
bayesDF$w.hat.sd = w.hat.sd

results = data.frame(bayesDF$Y, bayesDF$ZCTA5CE10, bayesDF$w.hat.mu, bayesDF$w.hat.sd)

write.csv(results, "bayesDF_rel_min975_2.csv")

#Interpolated Surface

#y.residuals = residuals(lm(Y ~ Total.population, data = bayesDF))
#par(mfrow = c(1,2))
#surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est

#image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
# par(mfrow = c(1,1))
# surf <- mba.surf(cbind(bayesDF[,c(23:24)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# z.lim = range(surf[[3]], na.rm = T)
# par(mfrow = c(1,1))
# image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")
# 
# library(tigris)
# library("rgeos")
# caloutline <- states() %>% filter_state("California")
# #caloutline <- st_as_sf(caloutline)
# plot(caloutline$geometry, add = T)

#### min 975 1 day ######
matchedData <- read.csv("hw_min975.csv", header = T) # Take matched dataset
d <- aggregate(matchedData$diffMean, by = list(zipcode = matchedData$patzip), FUN = mean)
names(d) <- c("id", "Y")
library(dplyr)
population <- read.csv("2010Census_DemoProfile.csv")
population$Total.population = as.numeric(gsub(",", "", population$Total.population))
d <- left_join(d, population, by =c("id" = "zipnum"))

d$ZCTA5CE10 <- d$id
mod = lm(Y~Total.population,data = d)
resids=d$Y - predict(mod, d)
d$Y = resids

#spBayes

library("spBayes")
library(MBA)
#library(geoR)
library(fields)
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(lattice)
library(gstat) 
library(coda)
coords <- read.csv("~/Downloads/zip_gps.csv", header = T)
coords <- coords[which(coords$ZCTA5 >= 90001),]

coords$ZCTA5CE10 <- coords$ZCTA5
names(d)
bayesDF<- merge(d, coords, by = c("ZCTA5CE10"))
bayesDF = bayesDF[complete.cases(bayesDF), ]
#Assumes Isotropy 

coord = as.matrix(bayesDF[,c("FinalLat", "FinalLon")])
n.samples = 1000
bef.sp<-spLM(Y ~ 1, data = bayesDF, coords = coord, starting = list("phi" = 2, "sigma.sq" = 0.4, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.10, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 3), "sigma.sq.IG" = c(1, 1)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)

bayesDF$w.hat.mu = w.hat.mu
bayesDF$w.hat.sd = w.hat.sd

results = data.frame(bayesDF$Y, bayesDF$ZCTA5CE10, bayesDF$w.hat.mu, bayesDF$w.hat.sd)

write.csv(results, "bayesDF_rel_min975_1.csv")

#Interpolated Surface

#y.residuals = residuals(lm(Y ~ Total.population, data = bayesDF))
#par(mfrow = c(1,2))
#surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est

#image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
# par(mfrow = c(1,1))
# surf <- mba.surf(cbind(bayesDF[,c(23:24)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# z.lim = range(surf[[3]], na.rm = T)
# par(mfrow = c(1,1))
# image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")
# 
# library(tigris)
# library("rgeos")
# caloutline <- states() %>% filter_state("California")
# #caloutline <- st_as_sf(caloutline)
# plot(caloutline$geometry, add = T)

#### min 95 3 day#####
matchedData <- read.csv("hw_min95_3.csv", header = T) # Take matched dataset
d <- aggregate(matchedData$diffMean, by = list(zipcode = matchedData$patzip), FUN = mean)
names(d) <- c("id", "Y")
library(dplyr)
population <- read.csv("2010Census_DemoProfile.csv")
population$Total.population = as.numeric(gsub(",", "", population$Total.population))
d <- left_join(d, population, by =c("id" = "zipnum"))

d$ZCTA5CE10 <- d$id
mod = lm(Y~Total.population,data = d)
resids=d$Y - predict(mod, d)
d$Y = resids

#spBayes

library("spBayes")
library(MBA)
#library(geoR)
library(fields)
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(lattice)
library(gstat) 
library(coda)
coords <- read.csv("~/Downloads/zip_gps.csv", header = T)
coords <- coords[which(coords$ZCTA5 >= 90001),]

coords$ZCTA5CE10 <- coords$ZCTA5
names(d)
bayesDF<- merge(d, coords, by = c("ZCTA5CE10"))
bayesDF = bayesDF[complete.cases(bayesDF), ]
#Assumes Isotropy 

coord = as.matrix(bayesDF[,c("FinalLat", "FinalLon")])
n.samples = 1000
bef.sp<-spLM(Y ~ 1, data = bayesDF, coords = coord, starting = list("phi" = 2, "sigma.sq" = 0.4, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.10, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 3), "sigma.sq.IG" = c(1, 1)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)

bayesDF$w.hat.mu = w.hat.mu
bayesDF$w.hat.sd = w.hat.sd

results = data.frame(bayesDF$Y, bayesDF$ZCTA5CE10, bayesDF$w.hat.mu, bayesDF$w.hat.sd)

write.csv(results, "bayesDF_rel_min95_3.csv")

#Interpolated Surface

#y.residuals = residuals(lm(Y ~ Total.population, data = bayesDF))
#par(mfrow = c(1,2))
#surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est

#image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
# par(mfrow = c(1,1))
# surf <- mba.surf(cbind(bayesDF[,c(23:24)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# z.lim = range(surf[[3]], na.rm = T)
# par(mfrow = c(1,1))
# image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")
# 
# library(tigris)
# library("rgeos")
# caloutline <- states() %>% filter_state("California")
# #caloutline <- st_as_sf(caloutline)
# plot(caloutline$geometry, add = T)

##### min 95, 2 day######
matchedData <- read.csv("hw_min95_2.csv", header = T) # Take matched dataset
d <- aggregate(matchedData$diffMean, by = list(zipcode = matchedData$patzip), FUN = mean)
names(d) <- c("id", "Y")
library(dplyr)
population <- read.csv("2010Census_DemoProfile.csv")
population$Total.population = as.numeric(gsub(",", "", population$Total.population))
d <- left_join(d, population, by =c("id" = "zipnum"))

d$ZCTA5CE10 <- d$id
mod = lm(Y~Total.population,data = d)
resids=d$Y - predict(mod, d)
d$Y = resids

#spBayes

library("spBayes")
library(MBA)
#library(geoR)
library(fields)
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(lattice)
library(gstat) 
library(coda)
coords <- read.csv("~/Downloads/zip_gps.csv", header = T)
coords <- coords[which(coords$ZCTA5 >= 90001),]

coords$ZCTA5CE10 <- coords$ZCTA5
names(d)
bayesDF<- merge(d, coords, by = c("ZCTA5CE10"))
bayesDF = bayesDF[complete.cases(bayesDF), ]
#Assumes Isotropy 

coord = as.matrix(bayesDF[,c("FinalLat", "FinalLon")])
n.samples = 1000
bef.sp<-spLM(Y ~ 1, data = bayesDF, coords = coord, starting = list("phi" = 2, "sigma.sq" = 0.4, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.10, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 3), "sigma.sq.IG" = c(1, 1)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)

bayesDF$w.hat.mu = w.hat.mu
bayesDF$w.hat.sd = w.hat.sd

results = data.frame(bayesDF$Y, bayesDF$ZCTA5CE10, bayesDF$w.hat.mu, bayesDF$w.hat.sd)

write.csv(results, "bayesDF_rel_min95_2.csv")

#Interpolated Surface

#y.residuals = residuals(lm(Y ~ Total.population, data = bayesDF))
#par(mfrow = c(1,2))
#surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est

#image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
# par(mfrow = c(1,1))
# surf <- mba.surf(cbind(bayesDF[,c(23:24)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# z.lim = range(surf[[3]], na.rm = T)
# par(mfrow = c(1,1))
# image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")
# 
# library(tigris)
# library("rgeos")
# caloutline <- states() %>% filter_state("California")
# #caloutline <- st_as_sf(caloutline)
# plot(caloutline$geometry, add = T)

###### min 95, 1 day #######
matchedData <- read.csv("hw_min95.csv", header = T) # Take matched dataset

#Get aggregated hospitalization counts
d <- aggregate(matchedData$diffMean, by = list(zipcode = matchedData$patzip), FUN = mean)
names(d) <- c("id", "Y")
library(dplyr)
population <- read.csv("2010Census_DemoProfile.csv")
population$Total.population = as.numeric(gsub(",", "", population$Total.population))
d <- left_join(d, population, by =c("id" = "zipnum"))

d$ZCTA5CE10 <- d$id
mod = lm(Y~Total.population,data = d)
resids=d$Y - predict(mod, d)
d$Y = resids

#spBayes

library("spBayes")
library(MBA)
#library(geoR)
library(fields)
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(lattice)
library(gstat) 
library(coda)
coords <- read.csv("~/Downloads/zip_gps.csv", header = T)
coords <- coords[which(coords$ZCTA5 >= 90001),]

coords$ZCTA5CE10 <- coords$ZCTA5
names(d)
bayesDF<- merge(d, coords, by = c("ZCTA5CE10"))
bayesDF = bayesDF[complete.cases(bayesDF), ]
#Assumes Isotropy 

coord = as.matrix(bayesDF[,c("FinalLat", "FinalLon")])
n.samples = 1000
bef.sp<-spLM(Y ~ 1, data = bayesDF, coords = coord, starting = list("phi" = 2, "sigma.sq" = 0.4, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.10, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 3), "sigma.sq.IG" = c(1, 1)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)

bayesDF$w.hat.mu = w.hat.mu
bayesDF$w.hat.sd = w.hat.sd

results = data.frame(bayesDF$Y, bayesDF$ZCTA5CE10, bayesDF$w.hat.mu, bayesDF$w.hat.sd)

write.csv(results, "bayesDF_rel_min95_1.csv")

#Interpolated Surface

#y.residuals = residuals(lm(Y ~ Total.population, data = bayesDF))
#par(mfrow = c(1,2))
#surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est

#image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
# par(mfrow = c(1,1))
# surf <- mba.surf(cbind(bayesDF[,c(23:24)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
# z.lim = range(surf[[3]], na.rm = T)
# par(mfrow = c(1,1))
# image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")
# 
# library(tigris)
# library("rgeos")
# caloutline <- states() %>% filter_state("California")
# #caloutline <- st_as_sf(caloutline)
# plot(caloutline$geometry, add = T)

