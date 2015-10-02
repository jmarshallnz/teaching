library(shiny)

# generate our data - this never changes
set.seed(2015)
n = 100
x = runif(n, 1, 8)
e = rnorm(n, 0, 0.3)

get_data = function(curve=0, hetero=0, r) {

  y = 0.5*x^(abs(curve)+1) + r + 0.5*(exp(r)*x^2)^(abs(curve)+1)*hetero
  if (curve < 0) {
    m = lm(y ~ x)
    y = fitted(m)-resid(m)
  }
  y - mean(y)
  y - min(y) + 1
}

shinyServer(function(input, output, session) {

  data = reactiveValues(r = e)

  observeEvent(input$newdata, {
    data$r = rnorm(n, 0, 0.3)
  })

  val = reactive({
    y = get_data(input$curve, input$hetero, data$r)
    xf = if (input$transform == 1) log(x) else x
    yf = if (input$transform == 1) log(y) else y
    m  = if (input$transform == 2) lm(yf ~ poly(xf, 2)) else lm(yf ~ xf)
    list(y = y, m = m)
  })

  output$data <- renderPlot({

    # plot the points, residuals as lines, model fit and (mean(x),mean(y))
    par(mar=c(1,1,0,0), cex=2)
    plot(NULL, xlim=range(x), ylim=range(val()$y), xaxt="n", yaxt="n", xlab="", ylab="")
    points(x, val()$y, col="#00000050", pch=19)
    xf = seq(max(0, min(x)-1), max(x)+1, length.out=100)
    xm = if (input$transform == 1) log(xf) else xf
    yf = predict(val()$m, data.frame(xf=xm))
    if (input$transform == 1)
      yf = exp(yf)
    lines(xf, yf, col="#00000080", lwd=2)
    text(max(x), min(val()$y), "Data", col="#00000040", adj=c(1,0))
  })

  output$resid <- renderPlot({
    par(mar=c(1,0,0,1.5), cex=2)
    r = resid(val()$m)
    yr = max(max(r), -min(r))
    qqnorm(r, main="", pch=19, col="#00000050", xaxt="n")
    qqline(r, lty="dotted")
    text(-qnorm(0.5/100), min(r), "Residuals", col="#00000040", adj=c(1,0))
  })
})
