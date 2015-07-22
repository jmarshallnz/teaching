library(shiny)

shinyUI(fluidPage(
  # Show a plot of the generated distribution
  fluidRow(
    column(width=6, plotOutput("data")),
    column(width=6, plotOutput("fdist"))
  ),
  fluidRow(
    column(width=2, sliderInput("g", "Groups", min=2, max=10, value=4, step=1)),
    column(width=2, sliderInput("n", "Samples/group", min=5, max=100, value=10, step=1)),
    column(width=3, sliderInput("strength", "Variation between", min=0, max=1, value=0, step=0.01)),
    column(width=3, sliderInput("resid", "Variation within", min=0.01, max=1, value=0.5, step=0.01)),
    column(width=2, br(),
                    actionButton("new_data", "New data set"))
  )
))
