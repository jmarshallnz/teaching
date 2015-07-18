library(shiny)

shinyUI(fluidPage(
  # Show a plot of the generated distribution
  fluidRow(
    column(width=6, plotOutput("data", click = "plot_click")),
    column(width=6, plotOutput("cooks"))
  )
))
