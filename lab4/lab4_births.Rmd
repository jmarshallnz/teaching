#' ---
#' title:  227.215 Biostats, Lamb birthweights
#' author: Jonathan Marshall
#' date:   30 August 2016
#' ---

#' ## Introduction
#' 
#' In this lab we're looking at lamb birthweight and how it relates to ewe nutrition and whether the ewe was shorn.
#' Read in the lamb birthweight data
births <- read.csv("http://www.massey.ac.nz/~jcmarsha/227215/data/birthweight.csv")
head(births)

#' ## Assessing whether the experiment is balanced
#' 
#' Each row in the data represents a lamb, so there are multiple rows for any ewe that has twins.
#' 
#' We start by pulling out just the columns that apply to the ewes, storing the result.
ewe_columns = births[, c("DamID", "Feed", "Shorn")]
nrow(ewe_columns)
#' Next, we run `unique` on it to pull out only the unique rows, storing the result.
ewes = unique(ewe_columns)
nrow(ewes)
#' Notice we only have 134 now, instead of the 204 we had previously. We can now do the table:
table(ewes$Feed, ewes$Shorn)
#' The experiment is almost perfectly balanced (same number of ewes in each treatment combination) with
#' just two groups being 1 ewe different to the others.
#' 
#' The advantage of a balanced experiment is that the order of the factors in a linear model doesn't affect
#' the p-values given in the ANOVA table. It's not a huge deal, but if you can make the experiment balanced
#' it can help.
#' 
#' ## Exploratory data analysis
#' 
#' Plot of birthweight versus rank, sex, feed and shorn
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(BirthWeight ~ Rank, data=births)
plot(BirthWeight ~ Sex, data=births)
plot(BirthWeight ~ Feed, data=births)
plot(BirthWeight ~ Shorn, data=births)
#' These plots suggest reasonable effects of rank and sex, but less clear effects of feed and shorn.

#' Recall that the linear model assumptions are in terms of the residuals. We can often evaluate whether
#' the hold directly for a linear model containing factor (categorical) variables, as the model is just
#' fitting a mean in each group, which will usually be about where the median is (particularly if the
#' assumptions hold!) and thus the distribution of observations within each group (i.e. within-group variation)
#' is the distribution of the residuals. The model is accounting only for between-group variation.
#' 
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
#' to suggest a difference in birthweight of lambs to shorn and unshorn ewes. At this point we don't need to bother looking
#' at the summary table: the summary table tells us about where the difference in groups lie, but we know there's no difference
#' from the ANOVA table.

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
#' assessing multiple hypotheses at once, and when you do that you have a higher chance of a false positive. In this case, the false
#' positive is due to the HM and HH groups being the most distant possible pair we could look at: All other pairs of treatments are
#' closer together than this one. It's not really a surprise that if we only look at the most distinct groups that there's
#' evidence for a difference: We're being biased if we consider this is important while ignoring all the other pairs that show no difference!)

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

par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(lm3)
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
#'  From this, we'd conclude the model assumptions are likely to hold, so our conclusions are likely OK.
#'  
#' ## Lab 4
#'  
#' We start by assessing whether the effect of shearing differs between twins and singles. We do this by including
#' an **interaction** term in the linear model.

mod_rank <- lm(BirthWeight ~ Sex + Rank + Feed + Shorn + Shorn:Rank, data=births)
anova(mod_rank)

#' We can see here that the interaction term between `Shorn` and `Rank` is not significant, so there is no evidence
#' of a differential effect of shearing and rank.

summary(mod_rank)

#' From the summary table, we see we have a new row for `ShornYes:RankTwin`. This corresponds to the additional effect
#' of shearing on the twin group over and above the singles group. The value of -0.22 suggests that the effect of shearing
#' on twins is to reduce the weight by 220g compared with the effect of shearing overall (which increases the weight by 395g).
#' Thus, we can conclude that the effect of shearing is to increase singles by 395g and twins by 395-220 = 175g.
#' 
#' This difference (220g) is not significant (We know that from the anova table), so we can conclude that
#' shearing effects the birthweight of both singles and twins similarly.
#'
plot(BirthWeight ~ interaction(Shorn, Rank), data=births)
#' This plot shows that the effect of the ewe being `Shorn` is about the same
#' to both singles and twins - with twins being a little less pronounced. This is in agreement with the
#' anova and summary table.

library(visreg)
visreg(mod_rank, "Shorn", by="Rank")
#' This shows the model fit (average birthweight) of the two possibilities for the shorn variable, divided
#' into two groups by rank. We see the same thing as the summary table: the difference in shorn vs unshorn
#' is less for twins, but the difference isn't much compared to the uncertainty (grey bars).
#' 
#' Adding `overlay=TRUE` gives
visreg(mod_rank, "Shorn", by="Rank", overlay=TRUE)
#' This perhaps shows it a little better: You can see that the singles increase more under the shearing
#' treatment, but there's quite a lot of uncertainty.
#' 
#' Our conclusion would be that the interaction term isn't really needed here as the differential effect
#' just isn't very strong.

#' Repeating the above for feed gives
mod_feed <- lm(BirthWeight ~ Sex + Rank + Feed + Shorn + Feed:Rank, data=births)
anova(mod_feed)
summary(mod_feed)

#' From the anova we see that the interaction isn't significant, so there's no suggestion that the effects of feed are different for singles
#' than twins. Notice we have 3 additional terms in the summary table now. `RankTwin:FeedHM` represents the effect of FeedHM on twins over and
#' above the effect of FeedHM on singles. We can see that the HM feed results in singles that are 390g lighter, and twins that are 390+92=482g lighter.
#' However, once again this difference is not significant (anova table).

#' Visualising this with `visreg` gives
visreg(mod_feed, "Feed", by="Rank", overlay=TRUE)
#' We can see the efect of feed is pretty similar for both singles and twins, particularly once uncertainty is
#' taken into account. This is in agreement with the ANOVA results. If the interaction term was significant
#' we'd expect to be able to see clear differences in the pattern of Feed effects in the two groups.
#' 
#' Note that we can also plot them the other way around
visreg(mod_feed, "Rank", by="Feed", overlay=TRUE)
#' This is a bit harder to see what is happening though, but the colour patterns are fairly similar 
#' in each group so the feed treatments affect singles and twins similarly (or at least we don't have
#' enough evidence to conclude otherwise.)
