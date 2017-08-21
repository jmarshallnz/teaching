library(shiny)
library(ggplot2)
library(Cairo)
library(visreg)

y0 = 50
n  = 40

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

  r <- r - rep(tapply(r, x, mean), each=n)

  data.frame(y, r, A, B, jitter=jitter)
}

shinyServer(function(input, output, session) {

  dat <- reactiveValues(data=get_data(y0,0,0,0,n))

  observeEvent(input$new_data, {
    # generate some new data
    dat$data <- get_data(y0, input$effectA, input$effectB, input$effectAB, n)
  })

  output$data <- renderPlot({
    # plot the points and model fit
    par(mar=c(3,0,0,0), cex=2)
    data <- dat$data
#    data$y <- data$y + input$effectA*data$A + input$effectB*data$B + (input$effectAB-input$effectA-input$effectB)*data$A*data$B + data$r*input$resid*5
    data$y <- data$y + input$effectA*data$A + input$effectB*data$B + (input$effectAB)*data$A*data$B + data$r*input$resid*5
    data$A <- factor(data$A, levels=0:1, labels=c("Control", "Drug A"))
    data$B <- factor(data$B, levels=0:1, labels=c("Control", "Drug B"))
    if (!input$show_model) {
      # show the data
      ggplot(data) + geom_boxplot(aes(x=A, y=y)) + scale_y_continuous(name = "Effect", limits=c(40,60)) + xlab("") + facet_wrap(~B) + theme_bw(base_size=15)
    } else {
      if (input$interaction) {
        lm2 <- lm(y ~ A*B, data=data)
        print(visreg(lm2, "A", by="B", gg=TRUE, points.par=c(size=1.5)) + scale_y_continuous(name = "Effect", limits=c(40,60)) + xlab("") + theme_bw(base_size=15))
      } else {
        lm1 <- lm(y ~ A+B, data=data)
        print(visreg(lm1, "A", by="B", gg=TRUE, points.par=c(size=1.5)) + scale_y_continuous(name = "Effect", limits=c(40,60)) + xlab("") + theme_bw(base_size=15))
      }
    }
  })

})
