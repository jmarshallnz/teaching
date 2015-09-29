library(shiny)

# read in heartgirth data

donkey <- read.csv("http://www.massey.ac.nz/~jcmarsha/227215/data/donkey.csv")
s = summary(lm(Bodywt ~ Heartgirth, data=donkey))
dx = 0.8

shinyServer(function(input, output, session) {

  output$plot <- renderPlot({
    # plot the heartgirth data

    zones = matrix(c(2,1,3), ncol=3, byrow=T)
    layout(zones, widths=c(1,6,1), heights=c(1,1,1))

    par(mar=c(3,3,0,1), cex=1.5)
    plot(Bodywt ~ Heartgirth, data=donkey, col="#00000050", pch=19, xlab="", ylab="", ylim=c(50,230))
    # generate some bands
    rect(input$x - dx, 0, input$x + dx, 250, col="#FF000030", border=NA)

    # now plot the marginals
    h = hist(donkey$Bodywt, breaks=seq(50,230,by=10), plot=FALSE)

    par(mar=c(3,0,0,0))
    plot(NULL, type="n", ylim =c(50,230), xlim=c(0,1), axes=FALSE)
    d = density(donkey$Bodywt, bw=5, from=0, to=300)
    polygon(1-c(0,d$y,0)*40, c(50,d$x,230), border=NA, col="#0000005F")
    text(0.5, 230, "Total\nVariation", adj=c(0.5,1))

    wt = donkey$Bodywt[donkey$Heartgirth >= input$x - dx & donkey$Heartgirth <= input$x + dx]
    par(mar=c(3,0,0,0))
    plot(NULL, type="n", ylim =c(50,230), xlim=c(0,1), axes=FALSE)
    if (length(wt) > 0) {
      d = density(wt, bw=7, from=0, to=300)
      polygon(c(0,d$y,0)*15, c(50,d$x,230), border=NA, col="#FF00005F")
    }
    text(0.5, 230, "Variation\ngiven\nHeartgirth", adj=c(0.5,1))
    #    h = hist(wt, breaks=seq(50,230,by=10), plot=FALSE)
    #    barplot(h$counts, space = 0, axes = FALSE, horiz=TRUE, border=NA)
    
    #    rect(input$x + (-3:2), 0, input$x + (-2:3), 250, col=paste0("#000000", c(seq(20,40,by=10),seq(40,20,by=-10))), border=NA)
    # TODO: Add reactive shit...
#    
  })

  output$var_resid = renderUI({
    # grab out just the data in the vicinity of the heartgirth
    withMathJax(helpText(sprintf("$$\\begin{aligned} R^2 &= \\frac{\\textsf{Variation explained}}{\\textsf{Total variation}} = \\frac{\\textsf{Total Variation - Variation given Heartgirth}}{\\textsf{Total Variation}}\\\\ &= \\frac{%2.f - %2.f}{%2.f} = %2.1f \\%% \\end{aligned}$$", var(donkey$Bodywt), s$sigma^2, var(donkey$Bodywt), s$r.squared*100)))
  })
})
