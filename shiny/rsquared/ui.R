library(shiny)

shinyUI(fluidPage(
  # Show a plot of the generated distribution
  fluidRow(
    column(width=12, plotOutput("plot"))
  ),
  fluidRow(
    column(width=4, sliderInput("x", "Heartgirth", min=80, max=140, value=100, step=1)),
    column(width=8, htmlOutput("var_resid"))
  )
))
