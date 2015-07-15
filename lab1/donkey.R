#' 227.215 Biostats.

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

#' The P-value for `Heartgirth` is really tiny. The null hypothesis being tested is whether this is 0 in the population. If it was
#' zero in the population, it would be really unlikely that we'd get the value of 2.83 in our sample. Thus we'd conclude that
#' it is unlikely to be zero in the population! Therefore, there's evidence that there is a relationship between body weight
#' and heart girth.

#' The $R^2$ value is 0.8, which means that 80% of the variation in body weight is explained by the linear model (i.e. once
#' we know what the heart girth is.) This means there's only about 20% of residual variation (variation about the line).

#' Using the equation, we can work out the average body weight for donkeys that have heartgirth equal to 110cm by substitution 110
#' into the equation above:
-194.5 + 2.83 * 110
#' Our best guess is 116.8 kg.

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
