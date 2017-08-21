library(shiny)

shinyUI(fluidPage(
  # Show a plot of the generated distribution
  fluidRow(
    column(width=8, plotOutput("data", height="500px")),
    column(width=4, sliderInput("effectA", "Drug A", min=-2.5, max=2.5, value=0, step=0.1),
           sliderInput("effectB", "Drug B", min=-2.5, max=2.5, value=0, step=0.1),
           sliderInput("effectAB", "Drugs A and B combined", min=-5, max=5, value=0, step=0.1),
           sliderInput("resid", "Variation within", min=0.01, max=1, value=0.5, step=0.01),
           actionButton("new_data", "New data set"),
           checkboxInput("show_model", "Show model fit", value=FALSE),
           checkboxInput("interaction", "Include Interaction", value=FALSE)
    )
  )
))
