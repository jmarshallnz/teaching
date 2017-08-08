library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(width=8, plotOutput("plot")),
    column(width=4, wellPanel(
      sliderInput("bins", label="Number of Bins", min=1, max=50, value=10, step=1),
      sliderInput("bin_start", label="Bin start", min=0, max=1, value=0.5, step=0.1),
      actionButton("default_bw", "'Optimal' Bins"),
      actionButton("new", "New Dataset")
    ))
  )
))
