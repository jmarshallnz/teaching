library(shiny)

y0 = 50
n  = 20

# computes jitter according to the distribution of y
dist_jitter <- function(y, jitter=0.2) {
  d <- density(y)
  j <- rnorm(length(y), 0, d$y / max(d$y))
  (j - mean(j))/sd(j) * jitter
}

get_data <- function(y0, a, b, ab, n) {

  # group assignment
  A <- rep(0:1, each=2*n)
  B <- rep(rep(0:1, each=n),2)

  # residual, within-group variation
  r <- matrix(rnorm(n*4),4,n)
  r <- as.numeric(t(r - rowMeans(r)))

  # y value
  y = rep(y0, 4*n)

  # jitter for plotting
  x = factor(A + 2*B)
  jitter <- do.call(c, tapply(r, x, dist_jitter, jitter=0.07))
  data.frame(y, r, A, B, jitter=jitter)
}

shinyServer(function(input, output, session) {

  dat <- reactiveValues(data=get_data(y0,0,0,0,n))

  observeEvent(input$new_data, {
    # generate some new data
    dat$data <- get_data(y0, input$effectA, input$effectB, input$effectAB, n)
  })

  val <- reactive({
    data <- dat$data #get_data(y0, input$effectA, input$effectB, input$effectAB, n)

    # get rid of unused groups, and normalise residuals per group
    x <- data$A + 2*data$B
    data$r <- data$r - rep(tapply(data$r, x, mean), each=n)
    data$y <- data$y + input$effectA*data$A + input$effectB*data$B + (input$effectAB-input$effectA-input$effectB)*data$A*data$B + data$r*input$resid*5

    list(x=x+1, y=data$y, jitter=data$jitter, lm1=lm(y ~ A + B, data=data), lm2=lm(y ~ A + B + A:B, data=data))
  })

  output$data <- renderPlot({
    # plot the points and model fit
    par(mar=c(3,0,0,0), cex=2)
    plot(NULL, xlim=c(0.5, 4+0.5), ylim=c(40,60) ,xaxt="n", yaxt="n", xlab="", ylab="")
    print(val()$x)
    print(val()$y)
    segments(-1,40:60,5,40:60,"grey80")
    segments(-1,50,5,50,"black","dashed")
    points(as.numeric(val()$x) + val()$jitter, val()$y, col="#00000050", pch=19, xlab="", ylab="", xaxt="n", yaxt="n")
    axis(side=1, at=1:4, labels=c("Control", "Drug A", "Drug B", "Both A&B"))
    # model fit...
    fit_width <- 0.1
    ci_width  <- 0.05
    new_data <- expand.grid(A=0:1, B=0:1)
    if (input$interaction) {
      ci1 <- predict(val()$lm1, new_data, interval="confidence")
      points(1:4-fit_width, ci1[,1], pch=19, cex=1.5, col="red")
      segments(1:4-fit_width, ci1[,2], 1:4-fit_width, ci1[,3], lwd=2, col="red")
      segments(1:4-fit_width-ci_width, ci1[,2], 1:4-fit_width+ci_width, ci1[,2], lwd=2, col="red")
      segments(1:4-fit_width-ci_width, ci1[,3], 1:4-fit_width+ci_width, ci1[,3], lwd=2, col="red")
      ci2 <- predict(val()$lm2, new_data, interval="confidence")
      points(1:4+fit_width, ci2[,1], pch=19, cex=1.5, col="blue")
      segments(1:4+fit_width, ci2[,2], 1:4+fit_width, ci2[,3], lwd=2, col="blue")
      segments(1:4+fit_width-ci_width, ci2[,2], 1:4+fit_width+ci_width, ci2[,2], lwd=2, col="blue")
      segments(1:4+fit_width-ci_width, ci2[,3], 1:4+fit_width+ci_width, ci2[,3], lwd=2, col="blue")
    } else {
      ci1 <- predict(val()$lm1, new_data, interval="confidence")
      points(1:4, ci1[,1], pch=19, cex=1.5, col="red")
      segments(1:4, ci1[,2], 1:4, ci1[,3], lwd=2, col="red")
      segments(1:4-ci_width, ci1[,2], 1:4+ci_width, ci1[,2], lwd=2, col="red")
      segments(1:4-ci_width, ci1[,3], 1:4+ci_width, ci1[,3], lwd=2, col="red")
    }
  })

})
