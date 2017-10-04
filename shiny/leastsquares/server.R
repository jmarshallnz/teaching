library(shiny)

# generate our data - this never changes
set.seed(2015)
x <- rnorm(100, 3, 1)
y <- 0.5 + 0.5*x + rnorm(100, 0, 0.3)
line <- lm(y ~ x)
mx <- mean(x)
my <- mean(y)
vy <- var(y)

shinyServer(function(input, output, session) {

  observe({
    if(input$fit != 0) {
      updateSliderInput(session, "beta", value=as.numeric(coef(line)[2])) #round(,2))
    }
  })
  
  output$plot <- renderPlot({

    # compute intercept and model fit
    alpha <- my - input$beta*mx
    yhat <- alpha + input$beta*x

    square_col <- function(x, max_x = 1.5) {
      x <- abs(x)
      x <- ifelse(x > max_x, max_x, x)
      strength <- (x/max_x)^2
      colorFunc <- colorRamp(c("#b0b0b0", "#FF0000"), space='Lab')
      rgb(colorFunc(strength), maxColorValue = 255)
    }

    # plot the points, residuals as lines, model fit and (mean(x),mean(y))
    par(mar=c(2,2,0,0), cex=2)
    plot(NULL, xlim=range(x), ylim=c(min(y)-0.5, max(y)+0.5), xaxt="n", yaxt="n", xlab="", ylab="")
    segments(x, yhat, x, ifelse(y > yhat, y-0.05, y+0.05), col=square_col(y-yhat), lwd=2)
    points(x, y, col="#00000050", pch=19, xlab="", ylab="", xaxt="n", yaxt="n")
    points(mx, my, pch=19)
    abline(alpha, input$beta, lwd=2)
    axis(1, at=mx, labels = expression(bar(x)))
    axis(2, at=my, labels = expression(bar(y)), las=1)
    # variance of the residuals
    sigma2 <- sum((y - yhat)^2)
    text(max(x), min(y)-0.5, bquote(SS[Residuals] == .(round(sigma2,4))), adj=c(1,0))
  })

})
