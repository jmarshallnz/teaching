#' 227.215 Biostats.
#'
#' ## Lab 1
#' 
#' In this notebook we'll take another look at data on Moroccan donkeys that we looked at
#' in semester 1.
donkey <- read.csv("http://www.massey.ac.nz/~jcmarsha/227215/data/donkey.csv")

#' Take a look at it
head(donkey)

#' A pairs plot of all measurement variables (columns 3 through 7)
plot(donkey[,3:7])

#' Plot of body weight versus heart girth
plot(Bodywt ~ Heartgirth, data=donkey)

#' fit a linear model to body weight in terms of heart girth
lm1 <- lm(Bodywt ~ Heartgirth, data=donkey)
summary(lm1)

#' The equation is $\mathsf{Bodywt} = -194.5 + 2.83 \times \mathsf{Heartgirth}$.
#'
#' The P-value for `Heartgirth` is really tiny. The null hypothesis being tested is whether this is 0 in the population. If it was
#' zero in the population, it would be really unlikely that we'd get the value of 2.83 in our sample. Thus we'd conclude that
#' it is unlikely to be zero in the population! Therefore, there's evidence that there is a relationship between body weight
#' and heart girth.
#'
#' The $R^2$ value is 0.8, which means that 80% of the variation in body weight is explained by the linear model (i.e. once
#' we know what the heart girth is.) This means there's only about 20% of residual variation (variation about the line).
#'
#' Using the equation, we can work out the average body weight for donkeys that have heartgirth equal to 110cm by substitution 110
#' into the equation above:
-194.5 + 2.83 * 110
#' Our best guess is 116.8 kg.
#'
#' Using R to do this instead via the predict function we have
new_data <- data.frame(Heartgirth = 110)
predict(lm1, new_data)

#' We can give an interval around this as well:
predict(lm1, new_data, interval="confidence")
predict(lm1, new_data, interval="prediction")

#' The first gives us a 95% confidence interval for the **average** donkey weight for donkeys whose heartgirth is 110cm.
#' i.e. we're 95% confident that average donkey weight is between 115.6kg and 117.8kg for those donkeys whose heartgirth is 110cm.
#' The second gives us a 95% prediction interval for the weight of **individual** donkeys whose heartgirth is 110cm.
#' i.e. we're 95% confident that a donkey with heartgirth 110cm will have a weight between 95.4kg and 138.0kg.
#' (95% of individual donkeys will be in this range).
#'
#' ## Lab 2
#' 
#' Diagnostic plots for the linear model.
par(mfrow=c(2,2), mar=c(4,4,2,2)) # creates a 2x2 plot and makes the margins a bit smaller
plot(lm1)
#' From these plots we see
#'  - There is a curvy trend present on the residual vs fitted plots. This suggests the linearity assumption
#'  doesn't hold. This is quite serious as it means our estimates will be biased, so should be fixed.
#'  
#'  - There is also a 'fanning' in the residual vs fitted plot (increasing vertical scatter from left to right) 
#'  suggesting that the residuals don't have constant variance.
#'  
#'  - The normal Q-Q plot shows no problems as the points mostly lie along the line - the residuals appear to be normally distributed.
#'  
#'  - There doesn't seem to be any points outside the cook's distance of 0.5, so no outliers to be concerned about.
#'  
#'  We should fix up the lack of linearity and constant variance. A usual fix is a log transform to the independent variable
#'  and maybe also to the dependent variables.

#' Let's try modelling the log of body weight versus log heartgirth to see if we
#' get a better model.
lm2 <- lm(log(Bodywt) ~ log(Heartgirth), data=donkey)
summary(lm2)
#' The new model formula is
#' 
#' $$
#' \log(\mathsf{Bodywt}) = -7.39 + 2.58 \log(\mathsf{Heartgirth})
#' $$
#' 
#' If we exponentiate both sides to get rid of the logarithm we get
#' 
#' $$
#' \mathsf{Bodywt} = 0.0006 \mathsf{Heartgirth}^2.58
#' $$

par(mfrow=c(2,2), mar=c(4,4,2,2)) # creates a 2x2 plot and makes the margins a bit smaller
plot(lm2, cex=0.8)
#' Model diagnostics look much better here. The trend and fanning in residuals vs fitted is gone, so linearity and constant variance
#' hold, normality is still OK, and no points have high influence.
#'
#' Comparing the model summaries, we see the new model has a higher $R^2$ than the previous one. So not only does it actually meet
#' the model assumptions, but it's a better fit as well.
#'
#' We can do a prediction for a donkey with heart girth 110cm as follows
new_data <- data.frame(Heartgirth=110)
exp(predict(lm2, new_data, interval="confidence"))
exp(predict(lm2, new_data, interval="prediction"))
#' These are a both a little bit tighter than the equivalent intervals for the previous model, which is due to the lower residual
#' variance (as the $R^2$ is higher)
#' 
#' ## Visualising the model
#' 
#' Visualise the model fit using the `visreg` package
library(visreg)
visreg(lm1)
#'
#' We can see that the model fit (blue line) and uncertainty around it (grey band) is a pretty good representation
#' of the trend within the middle of the heartgirth range, but underestimates the weight of donkeys at either end of
#' the heartgirth range. This is because the trend is really curved, so the straight line fit doesn't really work.

#' Visualise the second model
visreg(lm2)
#' We can see that we have a curved relationship here, but the vertical axis is on the log scale rather than
#' the normal scale. Nonetheless, you can see that the points (which represent where the data are) are captured quite
#' well by the trend line, indicating we are modelling the trend quite well.
#' 
#' Use an exponential transformation to visualise the data and model on the natural scale
visreg(lm2, trans=exp, partial=TRUE, ylab="Body weight (kg)")
#' The above plot shows the data on the natural scale and better allows us to see the actual shape of the trend
#' which is slightly curved up. Notice the trend now captures (i.e. goes through) the values at the lower and upper
#' heartgirths now. We'd expect our predictions for animals with such heartgirths to be considerably better.
#' 
#' ## Multivariable model
#'
#' A linear model containing both heart and umbilical girths.
lm3 <- lm(Bodywt ~ Heartgirth + Umbgirth, data=donkey)
summary(lm3)
#' The effect of heartgirth has changed as we're now also accounting for umbilical girth. Thus, each row represents the effect
#' of the variable **after adjusting for other variables**. The P-value is thus testing: "Is this variable important to body weight
#' after accounting for other variables?" This is different to "Is this variable important"!
#'
#' Our conclusion would be that heartgirth and umbilical girth are both important for the bodyweight of the donkey after accounting for
#' the other, as both P-values are significant.

lm4 <- lm(Bodywt ~ Heartgirth + Umbgirth + Length + Height, data=donkey)
summary(lm4)
#' After adjusting for heart girth, umbilical girth and length, there is no evidence that Height is important for body-weight as the
#' P-value is quite large (the effect size of 0.25 kg/cm of Height could arise by chance about 6% of the time, so may not be genuine).

#' A linear model with just Height in it
lm5 <- lm(Bodywt ~ Height, data=donkey)
summary(lm5)
#' We see Height is related to body weight. The difference is that in the previous model we'd accounted for a lot of the variation
#' in bodyweight already by using heart and umbilical girths and length. Over and above those, the height didn't provide additional
#' useful information. But absent of the other measures, height is ofcourse useful!

#' Visualising the model.
par(mfrow=c(2,2), mar=c(4,4,2,2))
visreg(lm4)
#' We can see the relationship with `Heartgirth` is the strongest, and with `Height` is weak - a flat line could fit within the uncertainty
#' bands. Notice that the y-coordinates of the points don't correspond precisely to the actual body weights here: They differ from picture to
#' picture. Instead, they're representations of the data points after accounting for the other 3 variables that are not present in each plot.
#' i.e. in the heart girth plot, they represent where the data points are after accounting for the umbilical girth, length and height.
#' 
#' Model diagnostics
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(lm4)
#' We can see from the plot that it looks like we still have curvature and perhaps also non-constant variance. A log-transform of
#' the body-weight variable (and perhaps some of the others) would be advised!
