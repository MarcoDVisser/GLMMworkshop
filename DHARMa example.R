
# For more information and functionalities visit https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html



# from CRAN
# install.packages("DHARMa")

# development version
# devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
#                          dependencies = T, build_vignettes = T)


library(DHARMa)
library(lme4)
library(effects)



# Data generation ---------------------------------------------------------


# I simulated data so that we know about the true model
testData = createData(sampleSize = 300, overdispersion = 1,
                      fixedEffects = c(1, -1), quadraticFixedEffects = c(-2, 0),
                      family = poisson())
head(testData)

myFavouriteSpecies = data.frame(count = testData$observedResponse,
                                temperature = testData$Environment1,
                                precipitation = testData$Environment2,
                                plot = testData$group)
# save(myFavouriteSpecies, file = "myFavoriteSpecies.Rdata")




# First model attempt -----------------------------------------------------


# load("myFavoriteSpecies.Rdata")
fittedModel <- glmer(count ~ temperature  + precipitation + (1|plot),
                     family = "poisson", data = myFavouriteSpecies)

# there seem to be effects...
summary(fittedModel)
plot(allEffects(fittedModel))


# but residuals look bad
simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput)


# plot residuals against predictor
par(mfrow = c(1, 2))
plotResiduals(myFavouriteSpecies$temperature, simulationOutput$scaledResiduals, xlab = "temp")
plotResiduals(myFavouriteSpecies$precipitation, simulationOutput$scaledResiduals, xlab = "precip")




# Improved model ----------------------------------------------------------


# include a quadratic term for temperature
fittedModel <- glmer(count ~ temperature + I(temperature^2) + precipitation + (1|plot),
                     family = "poisson", data = myFavouriteSpecies)

# there is clearly a quadratic term needed
summary(fittedModel)
plot(allEffects(fittedModel))


# residuals look much better
simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput)


# plot residuals against predictor also look better
par(mfrow = c(1, 2))
plotResiduals(myFavouriteSpecies$temperature, simulationOutput$scaledResiduals, xlab = "temp")
plotResiduals(myFavouriteSpecies$precipitation, simulationOutput$scaledResiduals, xlab = "precip")


# test for typical problems are also fine: no overdispersion, no zero inflation detected
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)





