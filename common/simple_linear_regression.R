#' Generate some data for a simple regression example plot
gen_simple_regression_data <- function() {
  set.seed(2015)
  x <- rnorm(100, 3, 1)
  y <- 0.5 + 0.5*x + rnorm(100, 0, 0.3)
  data.frame(x=x, y=y)
}

#' Plot some data with slope and intercept labelled
plot_slope_intercept <- function(data, cex=1.5, tikz=FALSE) {
  par(mar=c(2,1.2,1,1), cex=cex)
  plot(y ~ x, data=data, col="#00000020", xlim=c(0, max(x)+0.2), ylim=c(0, max(y)+0.2), pch=19, xlab="", ylab="", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
  line <- lm(y ~ x, data=data) 
  abline(line, lwd=2)
  xv <- c(1,5)
  yv <- predict(line, data.frame(x=xv))
  lines(xv, rep(yv[1], 2), lty="dotted")
  lines(rep(xv[2], 2), yv, lty="dotted")
  text(mean(xv), yv[1], "run", adj=c(0.5,1.2), col="grey30")
  text(xv[2], mean(yv), "rise", adj=c(-0.2,0.5), col="grey30")
  alpha_text <- ifelse(tikz, "$\\alpha$", expression(alpha))
  beta_text <- ifelse(tikz, "$\\beta = \\frac{\\textrm{rise}}{\\textrm{run}}$", expression(beta==over(rise,run)))
  mtext(alpha_text, side=2, line=0.5, at = coef(line)[1], las=1, cex=cex)
  text(4, yv[1]-0.5, beta_text)
  axis(1, at=0, line=0)
}

#' Plot some data with confidence and prediction intervals shown
plot_intervals <- function(data, cex=1.5) {
  par(mar=rep(0,4), cex=cex)
  plot(y ~ x, data=data, col="#00000050", xlim=c(min(x)-0.25, max(x)+0.25), ylim=c(min(x)-0.25, max(y)+0.25), pch=19, xlab="", ylab="", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
  xv <- seq(0,6,0.01)
  line <- lm(y ~ x, data=example)
  yv_c <- predict(line, data.frame(x=xv), interval="confidence")
  yv_p <- predict(line, data.frame(x=xv), interval="prediction")
  polygon(c(xv,rev(xv)),c(yv_p[,2], rev(yv_p[,3])), col="#00000020", border=NA)
  polygon(c(xv,rev(xv)),c(yv_c[,2], rev(yv_c[,3])), col="#00000040", border=NA)
  abline(coef(line), lwd=2)
  legend("bottomright", fill=c("#00000060", "#00000020"), legend=c("Confidence interval", "Prediction interval"), bty="n")
}