library(shiny)

get_x <- function(n) {
  x <- jitter(1:n)
  x - mean(x)
}

get_r <- function(n) {
  r <- rnorm(n)
  r - mean(r)
}

# sub-sample stuff
set.seed(2015)
subsample <- list()
subsample[[100]] <- 1:100
for (n in 99:2)
  subsample[[n]] <- sort(sample(subsample[[n+1]], n, replace=FALSE))

shinyServer(function(input, output, session) {

  dat <- reactiveValues(x=get_x(100), r=get_r(100))

  observeEvent(input$new_data, {
    # generate some new data
    dat$x <- get_x(100)
    dat$r <- get_r(100)
  })

  val <- reactive({
    x <- dat$x[ subsample[[input$n]] ]
    r <- dat$r[ subsample[[input$n]] ]
    y <- input$strength / 50 * x+ r * input$resid
    # subtract off the slope of this model
    c <- coef(lm(y ~ x))
    y <- y - (c[2] - input$strength/50) * x - c[1]
    list(x=x, y=y, lm=lm(y ~ x))
  })

  output$data <- renderPlot({
    # plot the points and model fit
    par(mar=c(1,0,0,0), cex=2)
    plot(NULL, xlim=range(dat$x), ylim=c(-2,2), xaxt="n", yaxt="n", xlab="", ylab="")
    points(val()$x, val()$y, col="#00000050", pch=19, xlab="", ylab="", xaxt="n", yaxt="n")
    abline(coef(val()$lm), col="#00000080", lwd=2)
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
