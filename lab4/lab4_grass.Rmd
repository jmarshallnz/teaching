#' ---
#' title:  227.215 Biostats, Grass platemeter measurements
#' author: Jonathan Marshall
#' date:   11 August 2015
#' ---

#' ## Introduction
#' 
#' In this lab we're looking at grass measured using platemeters.
#' We read in data on grass measurements and take a look at the summary.
grass <- read.csv("http://www.massey.ac.nz/~jcmarsha/227215/data/grass.csv")
summary(grass)

#' Plot of the meter readings versus farm, paddock and person are below.
plot(metread ~ farm, data=grass)
plot(metread ~ paddock, data=grass)
plot(metread ~ person, data=grass)
#' These plots suggest a small effect of farm, with larger effects of paddock and person.
#' 
#' A linear model for farm is as follows
mod1 <- lm(metread ~ farm, data=grass)
anova(mod1)
#' From this there is little evidence for a difference in the grass length between farms. Perhaps once accounting
#' for the variation between people we'll be able to see a difference between the farms?
mod2 <- lm(metread ~ farm + person, data=grass)
anova(mod2)
#' Yes, there is now a difference between the farms. The summary table will show us what those differences are
summary(mod2)
#' It looks like farm C has higher readings than farm A and B.

#' The problem with this model is two fold:
#' 
#' 1. We can't use it for future measurements unless the same people are doing the measuring, as we'll need the
#' individual 'person' effect, and we won't know that for future people.
#' 
#' 2. If we **don't** include the people in the model (to get around problem 1) then the residuals won't be
#' independent, so any conclusions from the model may be wrong. This is because repeat measures were taken by
#' the same people, so we'd expect measures taken by the same person to be more similar to each other than to
#' measures taken by other people (due to the person effect).
#' 
#' Ideally we'll want to account for the variation between people while not actually having to estimate a
#' separate mean for each person. This will account for the lack of independence in the residuals, and also
#' allow us to predict for new people measuring in the future.
#' 
#' We do this by incorporating a **random effect**.
#'
#' Load in the nlme library
library(nlme)
#' Fit a model incorporating a random effect for person
mixed <- lme(metread ~ farm, random=~1|person, data=grass)
anova(mixed)
#' We see here that farm is significant, just like it was for the linear model with person added in above, but now we no longer
#' have a separate line in the anova table for person as we're not estimating means for each person.
#' 
#' The summary table then shows us how the farms differ, as well as telling us about the effect of person.
summary(mixed)
#' From here we see that again farm C is largest (positive coefficient) while farm B is smallest, with farm A in-between. Notice none of the P-values
#' except the intercept are significant, while we know there are differences across the farms from the anova table. This is
#' because the largest difference is between farms B and C which we don't see in the summary table (only B vs A and C vs A).
#' 
#' Notice that the summary also gives us some information about the standard deviation of the random effects. This (1.49) is a measure
#' of the between-person variation, while the residual value (2.42) is a measure of within-group variation.
#' 
#' This suggests there's more variation in the measures done by the same person compared with the measures done by different people. Obviously
#' plate meter measuring is highly variable!
