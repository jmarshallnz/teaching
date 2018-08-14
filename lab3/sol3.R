#' ---
#' title:  227.215 Biostats, Lamb birthweights
#' author: Jonathan Marshall
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
#'
#' It's possible that we can't see much difference in the feed treatment as there is
#' too much within-treatment variation due to the other variables like rank and sex.
#' 
#' Plotting subsets show us this might be right
par(mfrow=c(2,2), mar=c(3,3,2,2))
plot(BirthWeight ~ Feed, data=subset(births, Rank == 'Twin'), main="Twins")
plot(BirthWeight ~ Feed, data=subset(births, Rank == 'Single'), main="Singles")
plot(BirthWeight ~ Feed, data=subset(births, Sex == 'Male'), main="Males")
plot(BirthWeight ~ Feed, data=subset(births, Sex == 'Female'), main="Females")

#' Using `ggplot2` gives nicer results. The default is to do a scatterplot
#' which isn't much use, but using `geom='boxplot'` converts to boxplots.
library(ggplot2)
qplot(Feed, BirthWeight, data=births)
qplot(Feed, BirthWeight, data=births, geom='boxplot')

#' The main advantage of using ggplot is that you can facet over multiple
#' plots quite easily. e.g. to facet based on rank or sex. Note that the scales
#' are the same for all facets, so they're more easily comparable than the
#' technique with `subset` used above.
qplot(Feed, BirthWeight, data=births, geom='boxplot') + facet_wrap(~Rank)
qplot(Feed, BirthWeight, data=births, geom='boxplot') + facet_wrap(~Sex)

#' We can also facet on two variables at once, producing a grid of sex (rows)
#' by rank (columns)
qplot(Feed, BirthWeight, data=births, geom='boxplot') + facet_grid(Sex~Rank)

#' Our conclusion is there is an effect of feed, but it's only really noticeable
#' once we first account for rank and sex. The HM feed treatment seems to result
#' in the lowest weight lambs.
#'
#' ## Statistical modelling
#' 
##' A linear model assessing whether ewe nutrition is important for lamb birthweight. The linear model equation would be
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
#' assessing multiple hypotheses at once, and when you do that you have a higher chance of a false positive. In this case, the false
#' positive is due to the HM and HH groups being the most distant possible pair we could look at: All other pairs of treatments are
#' closer together than this one. It's not really a surprise that if we only look at the most distinct groups that there's
#' evidence for a difference: We're being biased if we consider this is important while ignoring all the other pairs that show no difference!)
#'
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

#' We can visualise the model fit (essentially visualising what the summary table is telling us) with `visreg`
library(visreg)
par(mfrow=c(2,2), mar=c(4,4,2,2))
visreg(lm3)
#' Notice this tells us the same thing as we got from the summary table: The HM group is lowest with the others
#' somewhat similar to the HH group. Males are heavier, as are singles, and the shearing treatment seems to have
#' a positive effect on weight. The blue lines are the model fit, and the grey bars are uncertainty (95\%).
#'
#' ## Interactions and model diagnostics
#' 
#' Adding an interaction between Shorn and Rank allows us to assess if the effect
#' of shearing differs between those having twins and those having singles.
lm4 <- lm(BirthWeight ~ Sex + Rank + Feed + Shorn + Rank:Shorn, data=births)
anova(lm4)
#' The `Rank:Shorn` variable is not significant from the anova table, so we'd
#' conclude there isn't much evidence for the effect of shearing differing
#' between singles and twins. We can see this in the visualisation as well
visreg(lm4, "Shorn", by="Rank")
#' Notice the Yes vs No differences are about the same in each plot, especially
#' when considering their uncertainties. This confirms what the anova table was
#' telling us. You can also do an overlay plot for this which shows the same thing
visreg(lm4, "Shorn", by="Rank", overlay=TRUE)
#' Model diagnostics are shown below
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(lm4)
#' We can see from the model diagnostics that
#'  - There is no trend in the residuals vs fitted plot suggesting
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
#'  of lower power would be to remove one of the twins (at random) from the data-set and re-fit the model,
#'  but the problem is we'd be ignoring the within-ewe (between-twin) variation. Ignoring variation is usually
#'  a bad thing to do unless we know in advance that it's not important! In this case, between-twin variation
#'  would be important to know: If the twins are quite different, you'd want to know as maybe the feeding or
#'  shearing treatments don't apply equally to the twins (though I'm not sure how this would happen from a
#'  biological perspective!)
#'  
#'  From this, we'd conclude the model assumptions are likely to hold, with the possible exception of
#'  independence. Our conclusions from the model are probably OK.
