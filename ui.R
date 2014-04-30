library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Variance inflation factor sandbox"), 
  sidebarPanel(
    sliderInput("r2", "Covariate R-squared:", 
                min=0, max=.99, value=0),
    sliderInput("resid_var", "Residual variance:", 
                min=1, max=10, value=1), 
    sliderInput("var_x1", "Variance of X1:", 
                min=1, max=10, value=1),
    sliderInput("var_x2", "Variance of X2:", 
                min=1, max=10, value=1)
    ),
  mainPanel(plotOutput("plot")
            )
  ))