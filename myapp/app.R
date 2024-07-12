#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib) #for css stuff...
library(DT) #editable data tables
library(ggplot2)

# Define UI for application that draws a histogram
ui <- page_fillable(

  # Application title
  titlePanel("Summer school test"),
  


  # Sidebar with a slider input for number of bins
  layout_sidebar(
    border = FALSE,
    fillable = FALSE,
    sidebar = sidebar(
      tags$label("k param"), #workaround for weird scrolling when updating datatable (https://stackoverflow.com/questions/47048303/r-shiny-how-to-stop-sliderinput-label-click-from-causing-scroll-to-top-of-page)
      sliderInput("k_param",
                  label=NULL,
                  min = 0,
                  max = 3,
                  value = 1,
                  step = 0.01),
      sliderInput("beta",
                  "beta",
                  min = 0,
                  max = 10,
                  value = 1,
                  step = 0.1),
      sliderInput("effort",
                  "Effort level",
                  min = 1,
                  max = 5, 
                  value = 1,
                  #step = 0.1),
                  step = 1),
      sliderInput("reward",
                  "Reward level",
                  min = 1,
                  max = 5,
                  value = 1,
                  #step = 0.1)
                  step = 1)
    ),
    
    #Main page content
    selectInput("func_form",
                "Select functional form",
                choices = list("Linear" = "Linear", "Parabolic" = "Parabolic", "Hyperbolic" = "Hyperbolic"),
                selected = "Parabolic"
    ),
    uiOutput("sv_expression"),
    uiOutput("softmax_expression"),
    
    #uiOutput("sv_print"),
    #uiOutput("softmax_print"),
    uiOutput("math_output"),
    
    plotOutput("softmax_plot"),
    
    plotOutput("effort_plot"),
    
    sprintf("testing including datatable. Should be possible make it editable and tied to the figures above"),
    DTOutput("table")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  softmax <- function(SV) {
    exp(SV * input$beta) / (exp(SV * input$beta) + exp(1 * input$beta))
  }
  
  SV_compute <- function() {
    if (input$func_form == "Linear") {
      SV <- input$reward - (input$k_param*input$effort)
    } else if (input$func_form == "Parabolic") {
      SV <- input$reward - (input$k_param*(input$effort^2))
    } else if (input$func_form == "Hyperbolic") {
      SV <- input$reward / (1 + input$k_param*input$effort)
    }
  }
  
  
  testtable = data.frame(
    ID = "101",
    reward_level = rep(2:6, times=5),
    effort_level = rep(2:6, each=5)
  )
  
  
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
  
  output$softmax_expression <- renderUI({
    withMathJax(
      sprintf("$$P(work) = \\frac{e^{SV*\\beta}}{e^{SV*\\beta} + e^{1*\\beta}}$$")
    )
  })
  
  output$math_expressions <- renderUI({
    if (input$func_form == "Linear") {
      sv_expr = sprintf("\\(SV = R - kE\\)")
    } else if (input$func_form == "Parabolic") {
      sv_expr = sprintf("$$SV = R - kE^2$$")
    } else if (input$func_form == "Hyperbolic") {
      sv_expr = sprintf("$$SV = \\frac{R}{1+kE}$$")
    }
    
    withMathJax(
      sv_expr, #SV formula
      sprintf("$$P(work) = \\frac{e^{SV*\\beta}}{e^{SV*\\beta} + e^{1*\\beta}}$$") #softmax formula
    )
    
  })
  
   
  # output$sv_print <- renderUI({
  #   if (input$func_form == "Linear") {
  #     sv_text = sprintf("$$%.2f = %i - (%.2f)%i$$", SV_compute(), input$reward, input$k_param, input$effort)
  #   } else if (input$func_form == "Parabolic") {
  #     sv_text = sprintf("$$%.2f = %i - (%.2f)(%i^2)$$", SV_compute(), input$reward, input$k_param, input$effort)
  #   } else if (input$func_form == "Hyperbolic") {
  #     sv_text = sprintf("$$%.2f = \\frac{%i}{1 + (%.2f)%i}$$", SV_compute(), input$reward, input$k_param, input$effort)
  #   }  
  #   withMathJax(sv_text)  
  # })
  
  # output$softmax_print <- renderUI({
  #     soft_out <- softmax(SV_compute())
  #     SV <- SV_compute()
  #     beta <- input$beta
  #     sv_text = sprintf("$$%.2f = %i - (%.2f)%i$$", SV_compute(), input$reward, input$k_param, input$effort)
  #     
  #     withMathJax(
  #       
  #       sprintf("$$%.3f = \\frac{e^{%.2f*%.2f}}{e^{%.2f*%.2f} + e^{1*%.2f}}$$", soft_out, SV, beta, SV, beta, beta)
  #     )
  # })
  
  output$math_output <- renderUI({
    
    SV <- SV_compute()
    soft_out <- softmax(SV)
    k_param <- input$k_param
    beta <- input$beta
    effort <- input$effort
    reward <-input$reward
    
    if (input$func_form == "Linear") {
      sv_text = sprintf("$$%.2f = %i - (%.2f)%i$$", SV, reward, k_param, effort)
    } else if (input$func_form == "Parabolic") {
      sv_text = sprintf("$$%.2f = %i - (%.2f)(%i^2)$$", SV, reward, k_param, effort)
    } else if (input$func_form == "Hyperbolic") {
      sv_text = sprintf("$$%.2f = \\frac{%i}{1 + (%.2f)%i}$$", SV, reward, k_param, effort)
    }
    
    withMathJax(
      sv_text,
      sprintf("$$%.3f = \\frac{e^{%.2f*%.2f}}{e^{%.2f*%.2f} + e^{1*%.2f}}$$", soft_out, SV, beta, SV, beta, beta)
    )
  })
  
  output$softmax_plot <- renderPlot({
    
    # include choices based on softmax? would need to be recalculated
    # each time, which could become expensive...
    # might be a way to preallocate to workaround (set a seed and load all possibilities?)
    # choice = as.numeric(soft_out > runif(length(soft_out)))

    curve(softmax(x), from=-6, to=6)
    points(SV_compute(), softmax(SV_compute()), col = "red", bg = "red", pch = 21)
  })

  create_curve <- function() {
    k_param = input$k_param
    effort = input$effort
    
    if (input$func_form == "Linear") {
      curve(-x*k_param, from=1, to=5, ylim=c(-15,-0))
    } else if (input$func_form == "Parabolic") {
      curve(-((x^2)*k_param), from=1, to=5, ylim=c(-50,0))
      points(effort, -((effort^2)*k_param), col="red", bg="red", pch=21)
    } else if (input$func_form == "Hyperbolic") {
      curve(1/(1+x*k_param), from=1, to=5, ylim=c(0,1))
    }
  }
  
  output$effort_plot <- renderPlot({
    create_curve()
  })
  
  output$table <- renderDT({
    testtable$SV <- testtable$reward_level - (testtable$effort_level*input$k_param)
    testtable$SV = round(testtable$SV, digits=4)
    testtable
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
