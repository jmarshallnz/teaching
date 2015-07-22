library(shiny)

# computes jitter according to the distribution of y
dist_jitter <- function(y, jitter=0.2) {
  d <- density(y)
  j <- rnorm(length(y), 0, d$y / max(d$y))
  (j - mean(j))/sd(j) * jitter
}

get_data <- function(g, n) {

  # group assignment
  x <- as.factor(rep(1:g, each=n))

  # residual, within-group variation
  r <- matrix(rnorm(n*g),g,n)
  r <- as.numeric(t(r - rowMeans(r)))

  # between-group variation
  g <- rnorm(g)
  g <- rep(g - mean(g), each=n)

  # jitter for plotting
  jitter <- do.call(c, tapply(r, x, dist_jitter, jitter=0.07))

  data.frame(x, r, g, jitter=jitter)
}

# sub-sample stuff
max_groups  <- 10
max_samples <- 100

set.seed(2015)
subsample <- list()
subsample[[max_samples]] <- 1:max_samples
for (n in (max_samples-1):min_samples)
  subsample[[n]] <- sort(sample(subsample[[n+1]], n, replace=FALSE))

get_sample <- function(g, n) {
  rep(subsample[[n]], g) + rep((1:g-1) * max_samples, each=n)
}

shinyServer(function(input, output, session) {

  dat <- reactiveValues(data=get_data(max_groups,max_samples))

  observeEvent(input$new_data, {
    # generate some new data
    cat("new_data hit\n")
    dat$data <- get_data(max_groups, max_samples)
  })

  val <- reactive({
    cat("val computed\n")

    data <- dat$data[ get_sample(input$g, input$n),]
    
    # get rid of unused groups, and normalise residuals per group
    data <- droplevels(data)
    data$r <- data$r - rep(tapply(data$r, data$x, mean), each=input$n)

    # compute y
    data$y <- input$strength * data$g + input$resid * data$r

    print(summary(lm(y ~ x, data=data)))
    print(anova(lm(y ~ x, data=data)))
    print(tapply(data$y, data$x, mean))
    
    # TODO: Normalise the between-group variation to what we want it to be???
#    c <- coef(lm(y ~ x))
#    y <- y - (c[2] - input$strength/50) * x - c[1]
    list(x=data$x, y=data$y, jitter=data$jitter, lm=lm(y ~ x, data=data))
  })

  output$data <- renderPlot({
    # plot the points and model fit
    cat("data plot rendered\n")
    par(mar=c(1,0,0,0), cex=2)
    plot(NULL, xlim=c(0.5, input$g+0.5), ylim=c(-2,2), xaxt="n", yaxt="n", xlab="", ylab="")
    points(as.numeric(val()$x) + val()$jitter, val()$y, col="#00000050", pch=19, xlab="", ylab="", xaxt="n", yaxt="n")

    # model fit...
    fit_width <- 0.2
    ci_width  <- 0.15
    new_data <- data.frame(x=factor(1:input$g))
    ci <- predict(val()$lm, new_data, interval="confidence")
#    rect(1:input$g-ci_width, ci[,2], 1:input$g+ci_width, ci[,3], col="#ff000040", border=NA)
    segments(1:input$g-fit_width, ci[,1], 1:input$g+fit_width, ci[,1], lwd=3, col="red")
  })

  output$fdist <- renderPlot({

    # plot the fdistribution for the null
    f <- seq(0.1,20,by=0.1)
    y <- df(f, input$g-1, input$n-input$g)
    max_y <- 0.5

    x1 <- seq(qf(0.95, input$g-1, input$n-input$g), 20, by=0.1)
    y1 <- df(x1, input$g-1, input$n-input$g)

    par(mar=c(1,0,0,0), cex=2)
    plot(NULL, xlim=c(0,20), ylim=c(0,max_y), xaxt="n", yaxt="n", xlab="", ylab="", type="l", xaxs="i", yaxs="i")
    polygon(c(x1[1], x1, x1[length(x1)]), c(0, y1, 0), col="grey80", border=NA)
    lines(f, y)

    # mark a line at our f-statistic
    fhat <- summary(val()$lm)$fstatistic[1]
    abline(v=fhat, col="red")
    text(19, max_y*0.95, bquote(F == .(round(fhat,2))), adj=c(1,1), col="red")
    text(19, max_y*0.85, bquote(P == .(sprintf("%0.4f", 1-pf(fhat, input$g-1, input$n-input$g)))), adj=c(1,1), col="grey60")
    text(19, 0.03, "F distribution", col="grey80", adj=c(1,0))
  })
})
