library(shiny)

shinyUI(fluidPage(
  # Show a plot of the generated distribution
  fluidRow(
    column(width=6, plotOutput("data")),
    column(width=6, plotOutput("fdist"))
  ),
  fluidRow(
    column(width=3, sliderInput("n", "Sample size", min=10, max=100, value=30, step=1)),
    column(width=3, sliderInput("strength", "Strength of relationship", min=-1, max=1, value=0, step=0.05)),
    column(width=3, sliderInput("resid", "Residual variation", min=0.01, max=1, value=0.5, step=0.01)),
    column(width=3, br(),
                    actionButton("new_data", "New data set"))
  )
))
