#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib) # for css stuff...
#library(DT) # editable data tables
#library(ggplot2)

options(shiny.mathjax.url = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js")

# Define UI for application that draws a histogram
ui <- page_fillable(

  # Application title
  titlePanel("CSC 2025 effort discounting demo"),
  tags$head(tags$script('<script type="text/javascript" id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>')),

  # Sidebar with a slider input for number of bins
  layout_sidebar(
    border = FALSE,
    fillable = FALSE,
    sidebar = sidebar(
      #tags$label("'k' param"), # workaround for weird scrolling when updating datatable (https://stackoverflow.com/questions/47048303/r-shiny-how-to-stop-sliderinput-label-click-from-causing-scroll-to-top-of-page)
      sliderInput("k_param",
        #label = NULL,
        label = "'k' parameter",
        min = 0,
        max = 2,
        value = 0.5,
        step = 0.01
      ),
      sliderInput("beta",
        label = "'beta' parameter",
        min = 0,
        max = 5,
        value = 1,
        step = 0.1
      ),
      sliderInput("effort",
        label = "Effort level",
        min = 2,
        max = 6,
        value = 2,
        # step = 0.1),
        step = 1
      ),
      sliderInput("reward",
        label = "Reward level",
        min = 2,
        max = 6,
        value = 2,
        # step = 0.1)
        step = 1
      )
    ),

    # Main page content
    withMathJax(),
    selectInput("func_form",
      "Select functional form",
      choices = list("Linear" = "Linear", "Parabolic" = "Parabolic", "Hyperbolic" = "Hyperbolic"),
      selected = "Parabolic"
    ),
    withMathJax(uiOutput("sv_expression")),
    uiOutput("softmax_expression"),

    # uiOutput("sv_print"),
    # uiOutput("softmax_print"),
    uiOutput("math_output"),
    fluidRow(
      column(6, plotOutput("effort_plot")),
      column(6, plotOutput("softmax_plot"))
    )
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  softmax <- function(SV) {
    exp(SV * input$beta) / (exp(SV * input$beta) + exp(1 * input$beta))
  }

  SV_compute <- function() {
    if (input$func_form == "Linear") {
      SV <- input$reward - (input$k_param * input$effort)
    } else if (input$func_form == "Parabolic") {
      SV <- input$reward - (input$k_param * (input$effort^2))
    } else if (input$func_form == "Hyperbolic") {
      SV <- input$reward / (1 + input$k_param * input$effort)
    }
  }

  # Render the SV expression
  output$sv_expression <- renderUI({
    withMathJax(
      if (input$func_form == "Linear") {
        sprintf("$$SV = R - kE$$")
      } else if (input$func_form == "Parabolic") {
        sprintf("$$SV = R - kE^2$$")
      } else if (input$func_form == "Hyperbolic") {
        sprintf("$$SV = \\frac{R}{1+kE}$$")
      }
    )
  })

  # Render the softmax formula
  output$softmax_expression <- renderUI({
    withMathJax(
      sprintf("$$Pr(work) = \\frac{e^{SV*\\beta}}{e^{SV*\\beta} + e^{1*\\beta}}$$")
    )
  })

  # Render the maths output of the SV and softmax formulas
  output$math_output <- renderUI({
    SV <- SV_compute()
    soft_out <- softmax(SV)
    k_param <- input$k_param
    beta <- input$beta
    effort <- input$effort
    reward <- input$reward

    if (input$func_form == "Linear") {
      sv_text <- sprintf("$$%.2f = %i - %.2f*%i$$", SV, reward, k_param, effort)
    } else if (input$func_form == "Parabolic") {
      sv_text <- sprintf("$$%.2f = %i - %.2f*%i^2$$", SV, reward, k_param, effort)
    } else if (input$func_form == "Hyperbolic") {
      sv_text <- sprintf("$$%.2f = \\frac{%i}{1 + %.2f*%i}$$", SV, reward, k_param, effort)
    }

    # print the maths using MathJax
    withMathJax(
      sv_text,
      sprintf("$$%.3f = \\frac{e^{%.2f*%.2f}}{e^{%.2f*%.2f} + e^{1*%.2f}}$$", soft_out, SV, beta, SV, beta, beta)
    )
  })

  create_curve <- function() {
    k_param <- input$k_param
    effort <- input$effort

    if (input$func_form == "Linear") {
      curve(-x * k_param, from = 2, to = 6, ylim = c(-15, -0), xlab = "Effort level", ylab = "SV effort")
      points(x = effort, y = -(effort * k_param), col = "red", bg = "red", pch = 21)
    } else if (input$func_form == "Parabolic") {
      curve(-((x^2) * k_param), from = 2, to = 6, ylim = c(-50, 0), xlab = "Effort level", ylab = "SV effort")
      points(x = effort, y = -((effort^2) * k_param), col = "red", bg = "red", pch = 21)
    } else if (input$func_form == "Hyperbolic") {
      curve(1 / (1 + x * k_param), from = 2, to = 6, ylim = c(0, 1), xlab = "Effort level", ylab = "SV effort")
      points(x = effort, y = (1 / (1 + k_param * effort)), col = "red", bg = "red", pch = 21)
    }
  }

  output$effort_plot <- renderPlot(
    create_curve()
  )

  output$softmax_plot <- renderPlot({
    curve(softmax(x), from = -7, to = 7, xlab = "SV", ylab = "Pr(Work)", ylim = c(0, 1))
    points(SV_compute(), softmax(SV_compute()), col = "red", bg = "red", pch = 21)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
