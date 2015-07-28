#' ---
#' title:  227.215 Biostats, Lamb birthweights
#' author: Jonathan Marshall
#' date:   4 August 2015
#' ---

#' ## Introduction
#' 
#' In this lab we're looking at lamb birthweight and how it relates to ewe nutrition and whether the ewe was shorn.
#' Read in the lamb birthweight data
births <- read.csv("http://www.massey.ac.nz/~jcmarsha/227215/data/birthweight.csv")
head(births)

#' ## Exploratory data analysis
#' 
#' Plot of birthweight versus rank, sex, feed and shorn
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(BirthWeight ~ Rank, data=births)
plot(BirthWeight ~ Sex, data=births)
plot(BirthWeight ~ Feed, data=births)
plot(BirthWeight ~ Shorn, data=births)
#' These plots suggest reasonable effects of rank and sex, but less clear effects of feed and shorn.

#' The birthweight looks to be distributed reasonably symmetric within each level of the other factors, suggesting
#' that the residuals (represented by the within-group variation) are close to being normally distributed.
#' 
#' The spread is pretty similar as well, suggesting the constant variance assumption will be met.
#'
#' The linearity assumption is OK as we're dealing with grouped data so any 'trends' are just differences in group means
#' which means residuals within each group are always centered at 0 (remember: trend is residuals is due to the residuals not
#' being centered at zero across all values of the covariates.)
#' 
#' The independence assumption can't be assessed by this plot. The way the data were collected the ewes were randomly assigned
#' to treatment group and shorn group. The twin weights are likely to be correlated though, so perhaps the independence of residuals
#' may not quite hold there.
#'
#' ## Statistical modelling
#' 
#' A linear model assessing whether shearing the ewe is important for lamb birthweight. The linear model equation would be
#' $$
#' \mathsf{Birthweight} = \alpha + \beta z
#' $$
#' where $z$ is an indicator variable with $z=1$ when the ewe was shorn, and $z=0$ when unshorn. This gives 
#' $\mathsf{Birthweight}=\alpha$ when unshorn, and $\mathsf{Birthweight} = \alpha + \beta$ when shorn, so $\beta$
#' will be the difference between unshorn and shorn.
lm1 <- lm(BirthWeight ~ Shorn, data=births)
anova(lm1)
summary(lm1)
#' The P-value of 0.14 from the ANOVA table suggests shorn is not important - i.e. there's insufficient evidence from this model
#' to suggest a difference in birthweight of lambs to shorn and unshorn ewes.

#' A linear model assessing whether ewe nutrition is important for lamb birthweight. The linear model equation would be
#' $$
#' \mathsf{Birthweight} = \alpha + \beta_{HM} z_{HM} + \beta_{MH} z_{MH} + \beta_{MM} z_{MM}
#' $$
#' where $z_{HM}$ is an indicator variable with $z_{HM}=1$ when the ewe was in the $HM$ treatment group, and $z_{HM}=0$ otherwise.
#' This gives $\mathsf{Birthweight}=\alpha$ when the ewe is in feed group HH, $\mathsf{Birthweight}=\alpha + \beta_{HM}$ when
#' the ewe is in feed group HM and so on. Thus, $\beta_{HM}$ represents the difference in lamb birth weight between those ewes
#' in feed group HH (the base-line) and those in feed-group HM.
lm2 <- lm(BirthWeight ~ Feed, data=births)
anova(lm2)
summary(lm2)
#' The P-value of 0.11 from the ANOVA table suggests insufficient evidence for a difference in feed treatment on lamb birthweight.
#' Interestingly, there does seem to be a significant difference between the HM feed group and the baseline (HH) in the summary
#' table, but as the overall effect of feed is not significant, we shouldn't take this as being important (the summary table is
#' assessing multiple hypotheses at once).

#' A linear model containing all variables is below.
lm3 <- lm(BirthWeight ~ Sex + Rank + Feed + Shorn, data=births)
anova(lm3)
summary(lm3)
#' Based on the ANOVA table, we see all terms are significant, so all are important for birthweight.
#' 
#' The summary table suggests that
#' 
#'  - male lambs are on average 386g heavier than females.
#'
#'  - twin lambs are just over 1 kg lighter than singles.
#'  
#'  - The HM feed treatment differs from the HH feed treatment, with ewes on the HM treatment being lighter by 450g than
#'    those on the HH treatment. The others seem similar to HH treatment.
#'  
#'  - Ewes that were shorn tend to have heavier lambs (by about 240g).
#'
#' In earlier models both feed and shearing weren't significant. Now they are. The reason is that we've accounted for a
#' bunch of the variation in the data by include Sex and Rank which we **know** affect birthweight. By accounting for
#' some of the variation, we've reduced the remaining residual variation, thus being able to pick out the smaller between-group
#' variation due to feed and shearing.

new_data <- data.frame(Sex='Male', Rank='Twin', Shorn='No', Feed='HM')
predict(lm3, new_data, interval="prediction")
#' we can see a male twin born to an unshorn ewe on the HM feed treatment would be 4.45kg on average,
#' and we'd expect to be between 2.84 and 6.06kg 95% of the time.

new_data <- data.frame(Sex='Female', Rank='Single', Shorn='No', Feed='MM')
predict(lm3, new_data, interval="confidence")
#' we can see that female singles born to unshorn ewes on the MM feed treatment would be 5.56kg on avaerage,
#' and we're 95% confident that the average is between 5.23 and 5.90kg.

par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(lm3)
#' We can see from the model diagnostics that
#'  - There is no trend in the residuals vs fitted plot (or scale-location) suggesting
#'    the assumption of linearity is OK (residuals have mean 0 regardless of covariates).
#' 
#'  - The residuals seem to have constant variation in the residual vs fitted plot (or scale-location)
#'  suggesting the equal-variance assumption is satisifed.
#'  
#'  - The Normal-QQ plot suggests the residuals are mostly normal, only veering away at the lower tail.
#'  
#'  - There doesn't seem to be any residuals that have high influence (all points are inside the Cook's
#'  distance bands, as we can't even see the bands on the plot).
#'  
#'  - We can't assess the independence assumption with these plots. We'd be a little concerned that some of the
#'  residuals may not be independent (each pair of twins are likely similar). Ideally we'd take this into
#'  account in our model, but this is a bit beyond our current knowledge! Another way to 'fix' this, at the expense
#'  of lower power would be to remove one of the twins (at random) from the data-set and re-fit the model.
#'  
#'  From this, we'd conclude the model assumptions are likely to hold, so our conclusions are likely OK.