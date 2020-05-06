library(shiny)
library(tidyverse)

ui <- fluidPage(
  #####PLOTS#####
  fluidRow(
    # sample plot
    column(
      6,
      plotOutput("sampleplot")
    ),
    # phase diagram
    column(
      6,
      plotOutput("phasediag")
    )
  ),
  #####INPUTS#####
  # fixed inputs
  fluidRow(
    column(
      2,
      numericInput("timesteps", "# time steps",
                   value = 75, min = 0)
    ),
    column(
      2,
      numericInput("popsize", "Population size",
                   value = 10000, min = 0)
    ),
    column(
      2,
      numericInput("threshold", "Threshold",
                   value = 5000, min = 0)
    ),
    column(
      2,
      numericInput("gamma", "Gamma",
                   value = 0.1, min = 0, max = 1)
    )
  ),
  # table heading
  fluidRow(
    column(
      2,
      ""
    ),
    column(
      2,
      "min"
    ),
    column(
      2,
      "max"
    ),
    column(
      2,
      "step"
    ),
    column(
      2,
      "sample plot"
    )
  ),
  # beta row
  fluidRow(
    column(
      2,
      "beta"
    ),
    column(
      2,
      numericInput("minbeta", NULL,
                   value = 0.25, min = 0, max = 1)
    ),
    column(
      2,
      numericInput("maxbeta", NULL,
                   value = 0.75, min = 0, max = 1)
    ),
    column(
      2,
      numericInput("stepbeta", NULL,
                   value = 0.1, min = 0, max = 1)
    ),
    column(
      2,
      numericInput("samplebeta", NULL,
                   value = 0.6, min = 0, max = 1)
    )
  ),
  # I(0) row
  fluidRow(
    column(
      2,
      "I(0)"
    ),
    column(
      2,
      numericInput("minI", NULL,
                   value = 100, min = 0)
    ),
    column(
      2,
      numericInput("maxI", NULL,
                   value = 1000, min = 0)
    ),
    column(
      2,
      numericInput("stepI", NULL,
                   value = 100, min = 0)
    ),
    column(
      2,
      numericInput("sampleI", NULL,
                   value = 50, min = 0)
    )
  ),
  #initial values
  fluidRow(
    column(
      2,
      htmlOutput("sampleInitS")
    ),
    column(
      2,
      htmlOutput("sampleInitI")
    ),
    column(
      2,
      htmlOutput("sampleInitR")
    )
  )
)

server <- function(input, output, session) {
  # initial values
  #FIX: instead of literals, filter out initial values from sample_vals()
  output$sampleInitI <- renderUI({
    HTML(paste(
      "<b><big>I(0) = ", input$sampleI, "</big></b>",
      sep = ""
    ))
  })
  output$sampleInitS <- renderUI({
    HTML(paste(
      "<b><big>S(0) = ", input$popsize - input$sampleI, "</big></b>",
      sep = ""
    ))
  })
  output$sampleInitR <- renderUI({
    HTML(paste(
      "<b><big>R(0) = ", 0, "</big></b>",
      sep = ""
    ))
  })
  
  #####SAMPLE PLOT OUTPUT#####
  # setup tibble containing all S-I-R data (and time-dependent beta)
  sample_vals <- reactive({
    ans <- tibble(Time = 1:input$timesteps,
                  S = numeric(input$timesteps),
                  I = numeric(input$timesteps),
                  R = numeric(input$timesteps),
                  beta = rep(input$samplebeta, input$timesteps))
    
    # set initial values
    ans$R[1] <- 0
    ans$I[1] <- input$sampleI
    ans$S[1] <- input$popsize - input$sampleI
    
    # calculate stuff (rather inefficiently done rn - should replace w/ purrr at some point)
    for (i in 2:input$timesteps) {
      # delta for this iteration
      del_S <- round(-(ans$beta[i - 1] * ans$I[i - 1] * ans$S[i - 1]) / input$popsize,
                     digits = 0)
      del_R <- round(input$gamma * ans$I[i - 1],
                     digits = 0)
      del_I <- -del_R - del_S
      
      # store in tibble
      ans$S[i] <- ans$S[i - 1] + del_S
      ans$I[i] <- ans$I[i - 1] + del_I
      ans$R[i] <- ans$R[i - 1] + del_R
    }
    
    ans %>%
      gather(key = "Variable", value = "Value",
             -c(Time, beta))
  })
  
  output$sampleplot <- renderPlot({
    # ggplot(sample_vals(), aes(x = Time)) +
    #   geom_line(aes(y = S), color = "red") +
    #   geom_line(aes(y = I), color = "blue") +
    #   geom_line(aes(y = R), color = "black") +
    #   geom_hline(yintercept = input$threshold, linetype = "dashed") +
    #   xlab("Time") + ylab("# people") +
    #   ggtitle("Plot with sample parameters") +
    #   theme(panel.background = element_blank(),
    #         panel.grid = element_blank(),
    #         axis.line = element_line(colour = "black"))
    
    sample_vals() %>%
      ggplot(aes(x = Time, y = Value, color = Variable)) + 
      geom_line() +
      scale_color_manual(values = c("red", "blue", "black")) +
      geom_hline(yintercept = input$threshold, linetype = "dashed") +
      xlab("Time") + ylab("# people") +
      ggtitle("Sample Parameter Plot") +
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_line(colour = "black"))
  })
  
  
}

shinyApp(ui, server)
