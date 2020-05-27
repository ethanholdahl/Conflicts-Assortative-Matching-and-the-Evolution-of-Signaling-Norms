
## Load and install the packages
library("tidyverse", "shiny", "stringr")
theme_set(theme_minimal())


# Define server logic
function(input, output, session) {
  
  observeEvent(input$vHH, {
    vHH = input$vHH
    updateSliderInput(session, "vHL", min = 2, max = vHH -2)
  })
  
  observeEvent(input$vHL, {
    vHL = input$vHL
    updateSliderInput(session, "vLL", min = 1, max = vHL -1)
  })
  
  observeEvent(c(input$vHL, input$vHH, input$vLL), {
    vHL = input$vHL
    vHH = input$vHH
    vLL = input$vLL
    updateSliderInput(session, "vLH", min = 1, max = vHH+vLL-vHL-1)
  })
  
  observeEvent(c(input$A, input$vHH, input$vHL, input$vLH, input$vLL), {
    vHL = input$vHL
    vHH = input$vHH
    vLH = input$vLH
    vLL = input$vLL
    A = input$A
    updateSliderInput(session, "A", label = paste(HTML("Costliness parameter A such that K(A) = "), vLH-vLL+A*(vHH+vLL-vHL-vLH)))
  })

  evo = function(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, A, time){
    H_N = (1-ratio_h)*ratio
    H_S = ratio_h*ratio
    L_N = (1-ratio_l)*(1-ratio)
    L_S = ratio_l*(1-ratio)
    pop = data.frame(H_N,H_S,L_N,L_S)
    K = vLH-vLL+A*(vHH+vLL-vLH-vHL)
    SHH = vHH - K
    SHL = vHL - K
    SLH = vLH - K
    SLL = vLL - K
    payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
    for(i in 1:time){
      H_N = as.numeric(tail(pop,1)[1])
      H_S = as.numeric(tail(pop,1)[2])
      L_N = as.numeric(tail(pop,1)[3])
      L_S = as.numeric(tail(pop,1)[4])
      S_H = if(H_S+L_S>0){
        H_S/(H_S+L_S)} else {0}
      S_L = 1-S_H
      N_H = if(H_N+L_N>0){
        H_N/(H_N+L_N)} else {0}
      N_L = 1-N_H
      H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
      H_S_P = H_S*(S_H*as.numeric(payoffs[5])+S_L*as.numeric(payoffs[6]))
      L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
      L_S_P = L_S*(N_H*as.numeric(payoffs[7])+N_L*as.numeric(payoffs[8]))
      Total_P = H_N_P+H_S_P+L_N_P+L_S_P
      H_N = H_N_P/Total_P
      H_S = H_S_P/Total_P
      L_N = L_N_P/Total_P
      L_S = L_S_P/Total_P
      pop1 = data.frame(H_N,H_S,L_N,L_S)
      pop = rbind(pop, pop1)
    }
    pop = pop %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 0:time) %>%
      gather("High_No_Signal", "High_Signal", "Low_No_Signal", "Low_Signal", key = Type, value = Proportion)
  }
  
  evo_elect = function(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, A, time){
    H_N = (1-ratio_h)*ratio
    H_S = ratio_h*ratio
    L_N = (1-ratio_l)*(1-ratio)
    L_S = ratio_l*(1-ratio)
    pop = data.frame(H_N,H_S,L_N,L_S)
    K = vLH-vLL+A*(vHH+vLL-vLH-vHL)
    SHH = vHH - K
    SHL = vHL - K
    SLH = vLH - K
    SLL = vLL - K
    payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
    for(i in 1:time){
      H_N = as.numeric(tail(pop,1)[1])
      H_S = as.numeric(tail(pop,1)[2])
      L_N = as.numeric(tail(pop,1)[3])
      L_S = as.numeric(tail(pop,1)[4])
      H = H_N+H_S
      L = L_N+L_S
      L_S = 0
      L_N = L
      SHH = as.numeric(payoffs[5])
      vH = H*as.numeric(payoffs[1])+L*as.numeric(payoffs[2])
      if(SHH>vH){
        H_S = H
        H_N = 0
      } else {
        H_S = 0
        H_N = H
      }
      S_H = if(H_S+L_S>0){
        H_S/(H_S+L_S)} else {0}
      S_L = 1-S_H
      N_H = if(H_N+L_N>0){
        H_N/(H_N+L_N)} else {0}
      N_L = 1-N_H
      H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
      H_S_P = H_S*(S_H*as.numeric(payoffs[5])+S_L*as.numeric(payoffs[6]))
      L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
      L_S_P = L_S*(N_H*as.numeric(payoffs[7])+N_L*as.numeric(payoffs[8]))
      Total_P = H_N_P+H_S_P+L_N_P+L_S_P
      H_N = H_N_P/Total_P
      H_S = H_S_P/Total_P
      L_N = L_N_P/Total_P
      L_S = L_S_P/Total_P
      pop1 = data.frame(H_N,H_S,L_N,L_S)
      pop = rbind(pop, pop1)
    }
    pop = pop %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 0:time) %>%
      gather("High_No_Signal", "High_Signal", "Low_No_Signal", "Low_Signal", key = Type, value = Proportion)
  }
  
  pop_evo_elect = reactive({
    evo_elect(input$ratio, input$ratio_h, input$ratio_l, input$vHH, input$vHL, input$vLH, input$vLL, input$A, input$time)
  })
  
  output$TE = renderPlot({
  ggplot(data = pop_evo_elect(), aes(x = t, y = Proportion, color = Type)) +
    geom_line()
  })
  
  pop_evo = reactive({
    evo(input$ratio, input$ratio_h, input$ratio_l, input$vHH, input$vHL, input$vLH, input$vLL, input$A, input$time)
  })

  output$PA = renderPlot({
    ggplot(data = pop_evo(), aes(x = t, y = Proportion, color = Type)) +
      geom_line()
    })
}
  