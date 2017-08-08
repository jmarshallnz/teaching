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

  dat <- reactiveValues(data=get_data(200))

  observeEvent(input$new, {
    # generate some new data
    dat$data <- get_data(200)
  })

  observeEvent(input$default_bw, {
    updateSliderInput(session, "bins", value=length(hist(dat$data, plot=FALSE)$counts))
    updateSliderInput(session, "bin_start", value=0.5)
  })

  output$plot <- renderPlot({

    x <- dat$data

    par(mar=c(4,4,0,1))
    bin_width <- diff(range(x))/(input$bins-1)
    dx <- bin_width*(1-input$bin_start)
    breaks <- seq(min(x)-dx, max(x)+bin_width-dx, length.out=input$bins+1)
    hist(x, breaks=breaks, xlim=c(min(x)-bin_width, max(x)+bin_width), col='grey60', main="", xlab="Data")
  })

})
