library(shiny)

get_data <- function(n) {
  x <- jitter(1:n)
  r <- rnorm(n)
  data.frame(x=x-mean(x), r=r-mean(r))
}

# sub-sample stuff
set.seed(2015)
subsample <- list()
subsample[[100]] <- 1:100
for (n in 99:2)
  subsample[[n]] <- sort(sample(subsample[[n+1]], n, replace=FALSE))

shinyServer(function(input, output, session) {

  dat <- reactiveValues(data=get_data(100))

  observeEvent(input$new_data, {
    # generate some new data
    dat$data <- get_data(100)
  })

  val <- reactive({
    data <- dat$data[ subsample[[input$n]], ]
    data$y <- input$strength / 50 * data$x + data$r * input$resid
    # subtract off the slope of this model
    c <- coef(lm(y ~ x, data=data))
    data$y <- data$y - (c[2] - input$strength/50) * data$x - c[1]
    list(x=data$x, y=data$y, lm=lm(y ~ x, data=data))
  })

  output$data <- renderPlot({
    # plot the points and model fit
    par(mar=c(1,0,0,0), cex=2)
    plot(NULL, xlim=range(dat$data$x), ylim=c(-2,2), xaxt="n", yaxt="n", xlab="", ylab="")
    points(val()$x, val()$y, col="#00000050", pch=19, xlab="", ylab="", xaxt="n", yaxt="n")
    abline(coef(val()$lm), lwd=3, col="red")
  })

  output$fdist <- renderPlot({

    # plot the fdistribution for the null
    f <- seq(0.1,20,by=0.1)
    y <- df(f, 1, input$n-2)
    max_y <- 0.5

    x1 <- seq(qf(0.95, 1, input$n-2), 20, by=0.1)
    y1 <- df(x1, 1, input$n-2)

    par(mar=c(1,0,0,0), cex=2)
    plot(NULL, xlim=range(f), ylim=c(0,max_y), xaxt="n", yaxt="n", xlab="", ylab="", type="l", xaxs="i", yaxs="i")
    polygon(c(x1[1], x1, x1[length(x1)]), c(0, y1, 0), col="grey80", border=NA)
    lines(f, y)

    # mark a line at our f-statistic
    fhat <- summary(val()$lm)$fstatistic[1]
    abline(v=fhat, col="red")
    text(19, max_y*0.95, bquote(F == .(round(fhat,2))), adj=c(1,1), col="red")
    text(19, max_y*0.85, bquote(P == .(sprintf("%0.4f", 1-pf(fhat, 1, input$n-2)))), adj=c(1,1), col="grey60")
    text(19, 0.03, "F distribution", col="grey80", adj=c(1,0))
  })
})
