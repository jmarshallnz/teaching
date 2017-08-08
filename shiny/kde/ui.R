library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(width=8, plotOutput("plot")),
    column(width=4, wellPanel(
      sliderInput("bandwidth", label="Bandwidth", min=0.05, max=2, value=0.4, step=0.01),
      actionButton("default_bw", "'Optimal' Bandwidth"),
      actionButton("new", "New Dataset")
    ))
  )
))
