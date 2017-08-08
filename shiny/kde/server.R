library(shiny)

# generate our data
get_data <- function(n) {
  n_m <- sample(1:4, 1)
  nn <- as.numeric(rmultinom(1, n, prob=rep(1/n_m, n_m)))
  mu <- rep(rnorm(n_m, sd=3), times=nn)
  r <- rnorm(n) + mu
  r
}

shinyServer(function(input, output, session) {

  dat <- reactiveValues(data=get_data(100))

  observeEvent(input$new, {
    # generate some new data
    dat$data <- get_data(100)
  })

  observeEvent(input$default_bw, {
      updateSliderInput(session, "bandwidth", value=density(dat$data)$bw)
  })

  output$plot <- renderPlot({

    x <- dat$data
    d <- density(x, bw=input$bandwidth)

    par(mar=c(4,4,0,0.5))
    plot(NULL, xlim=range(x), ylim=c(-0.02,0.5), xlab="Data", main="", ylab="Frequency")
    points(x, rep(-0.02,length(x)), pch=4)
    lines(d$x, d$y, col="black", lwd="3")
  })

})
