library(shiny)
library(tidyverse)

#####UTILITY FUNCTIONS#####
# for one set of parameters, calculate S-I-R time series
get_sir_data <- function(initS, initI, initR,
                         func_beta, gamma, timesteps) {
  # total population
  popsize <- initS + initI + initR
  
  # setup tibble
  ans <- tibble(Time = 1:timesteps,
                S = numeric(timesteps),
                I = numeric(timesteps),
                R = numeric(timesteps),
                beta = rep(func_beta, timesteps))
  
  # set initial values
  ans$R[1] <- initR
  ans$I[1] <- initI
  ans$S[1] <- initS
  
  # calculate stuff (rather inefficiently done rn - should replace w/ purrr at some point)
  for (i in 2:timesteps) {
    # delta for this iteration
    # print(paste(
    #   i, class(ans$beta[i-1]), class(ans$I[i-1]), class(ans$S[i-1]), class(popsize)
    # ))
    del_S <- round(-(ans$beta[i - 1] * ans$I[i - 1] * ans$S[i - 1]) / popsize,
                   digits = 0)
    # print(2)
    del_R <- round(gamma * ans$I[i - 1],
                   digits = 0)
    del_I <- -del_R - del_S
    
    # store in tibble
    ans$S[i] <- ans$S[i - 1] + del_S
    ans$I[i] <- ans$I[i - 1] + del_I
    ans$R[i] <- ans$R[i - 1] + del_R
  }
  
  # return answer in ggplottable format
  ans %>%
    gather(key = "Variable", value = "Value",
           -c(Time, beta))
}

#####USER INTERFACE#####
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
                   value = 1000, min = 0)
    ),
    column(
      2,
      numericInput("threshold", "Threshold",
                   value = 600, min = 0)
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

#####SERVER#####
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
    get_sir_data(initS = input$popsize - input$sampleI,
                 initI = input$sampleI,
                 initR = 0,
                 func_beta = input$samplebeta,
                 gamma = input$gamma,
                 timesteps = input$timesteps)
  })
  
  output$sampleplot <- renderPlot({
    sample_vals() %>%
      ggplot(aes(x = Time, y = Value, color = Variable)) + 
      geom_line() +
      scale_color_manual(values = c("red", "blue", "black")) +
      geom_hline(yintercept = input$threshold, linetype = "dashed") +
      xlab("Time") + ylab("# people") +
      ggtitle("Sample Parameter Plot") +
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.title = element_blank(),
            legend.position = c(0.9, 0.5))
  })
  
  #####PHASE DIAGRAM#####
  # calculations
  phase_vals <- reactive({
    # setup tibble
    beta <- seq(from = input$minbeta, to = input$maxbeta, by = input$stepbeta)
    I<- seq(from = input$minI, to = input$maxI, by = input$stepI)
    all_combos <- as_tibble(expand.grid(beta, I))
    
    # replace w/ purrr and tidyverse compatible functions at some point
    all_combos$Outcome <- vapply(X = 1:nrow(all_combos),
                                 FUN.VALUE = character(1),
                                 FUN = function(i) {
                                   # curr params
                                   curr_beta <- all_combos[[i, 1]]
                                   curr_I <- all_combos[[i, 2]]
                                   
                                   # get df
                                   curr_vals <- get_sir_data(
                                     initS = input$popsize - curr_I,
                                     initI = curr_I,
                                     initR = 0,
                                     func_beta = curr_beta,
                                     gamma = input$gamma,
                                     timesteps = input$timesteps
                                   )
                                   
                                   # check if threshold has been passed
                                   only_I <- filter(curr_vals, Variable == "I")
                                   
                                   if (sum(only_I$Value > input$threshold) > 0) {
                                     return("EXCEEDED")
                                   } else {
                                     return("NOT EXCEEDED")
                                   }
                                 })
    
    all_combos
  })
  
  # plot results
  output$phasediag <- renderPlot({
    phase_vals() %>%
      ggplot(aes(x = Var1, y = Var2, fill = Outcome)) +
      geom_tile(color = "black") +
      xlab("Beta") +
      ylab("I(0)") +
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_line(color = "black"))
  })
}

shinyApp(ui, server)

# NOTE:
#   discrete, deterministic
#   not validated yet (quite possibly bugs present)
