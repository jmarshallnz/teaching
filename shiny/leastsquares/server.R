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

    # plot the points, residuals as lines, model fit and (mean(x),mean(y))
    par(mar=c(2,2,0,0), cex=2)
    plot(NULL, xlim=range(x), ylim=c(min(y)-0.5, max(y)+0.5), xaxt="n", yaxt="n", xlab="", ylab="")
    segments(x, yhat, x, ifelse(y > yhat, y-0.05, y+0.05), col="#00000040")
    points(x, y, col="#00000050", pch=19, xlab="", ylab="", xaxt="n", yaxt="n")
    points(mx, my, pch=19)
    abline(alpha, input$beta, lwd=2)
    axis(1, at=mx, labels = expression(bar(x)))
    axis(2, at=my, labels = expression(bar(y)), las=1)
    # variance of the residuals
    sigma2 <- var(y - yhat)
    text(max(x), min(y)-0.5, bquote(sigma[Residuals]^2 == .(round(sigma2,4))), adj=c(1,0))
  })

})
