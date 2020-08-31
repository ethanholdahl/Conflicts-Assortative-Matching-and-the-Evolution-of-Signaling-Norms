
## Load and install the packages
library("tidyverse", "shiny", "stringr")
theme_set(theme_minimal())


# Define server logic
function(input, output, session) {
  
  observeEvent(input$vHH, {
    vHH = input$vHH
    updateSliderInput(session, "vHL", min = .01, max = vHH -.02)
  })
  
  observeEvent(input$vHL, {
    vHL = input$vHL
    updateSliderInput(session, "vLL", min = 0, max = vHL -.01)
  })
  
  observeEvent(c(input$vHL, input$vHH, input$vLL), {
    vHL = input$vHL
    vHH = input$vHH
    vLL = input$vLL
    updateSliderInput(session, "vLH", min = 0, max = vHH+vLL-vHL-.01)
  })
  
  observeEvent(c(input$A, input$vHH, input$vHL, input$vLH, input$vLL), {
    vHL = input$vHL
    vHH = input$vHH
    vLH = input$vLH
    vLL = input$vLL
    K = input$K
    updateSliderInput(session, "K", min = round(max(0, vLH-vLL),2), max = round(vHH-vHL,2))
  })
  
  observeEvent(input$time, {
    time = input$time
    updateSliderInput(session, "start", min = 1, max = time)
  })

  evo = function(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, K, time, pop_grow){
    H_N = (1-ratio_h)*ratio
    H_S = ratio_h*ratio
    L_N = (1-ratio_l)*(1-ratio)
    L_S = ratio_l*(1-ratio)
    pop = data.frame(H_N,H_S,L_N,L_S)
    SHH = max(vHH - K, 0)
    SHL = max(vHL - K, 0)
    SLH = max(vLH - K, 0)
    SLL = max(vLL - K, 0)
    payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
    expected_payoffs = data.frame()
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
      L_S_P = L_S*(S_H*as.numeric(payoffs[7])+S_L*as.numeric(payoffs[8]))
      if (pop_grow == "Fixed population"){
        Total_P = H_N_P+H_S_P+L_N_P+L_S_P
        expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_P), High_Signal = H_S_P/(H_S*Total_P), Low_No_Signal = L_N_P/(L_N*Total_P), Low_Signal = L_S_P/(L_S*Total_P))
        H_N = H_N_P/Total_P
        H_S = H_S_P/Total_P
        L_N = L_N_P/Total_P
        L_S = L_S_P/Total_P
      }
      if (pop_grow == "Unbounded exponential growth") {
        expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
        H_N = H_N_P
        H_S = H_S_P
        L_N = L_N_P
        L_S = L_S_P
      }
      pop1 = data.frame(H_N,H_S,L_N,L_S)
      pop = rbind(pop, pop1)
      expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
    }
    
    growth = pop[2:(time+1),]-pop[1:time,]
    growth = growth %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 1:time,
             Signal = High_Signal + Low_Signal,
             No_Signal = High_No_Signal + Low_No_Signal) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth")
    
    pop = pop %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 0:time) %>%
      gather("High_No_Signal", "Low_No_Signal", "High_Signal", "Low_Signal", key = Type, value = "Population")
    
    expected_payoffs = expected_payoffs %>%
      mutate(t = 1:time) %>%
      gather("High_No_Signal", "Low_No_Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth_Rate")
    list(pop, expected_payoffs, growth)
  }
  
  evo_elect = function(ratio, vHH, vHL, vLH, vLL, K, time){
    SHH = max(vHH - K, 0)
    SHL = max(vHL - K, 0)
    SLH = max(vLH - K, 0)
    SLL = max(vLL - K, 0)
    payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
    H = ratio
    L = 1-ratio
    vH = H*vHH+L*vHL
    if(SHH>vH){
      S = 1
    } else {
      S = 0
    }
    pop = data.frame(H,L,S)
    for(i in 1:time){
      H = as.numeric(tail(pop,1)[1])
      L = as.numeric(tail(pop,1)[2])
      vH = H*as.numeric(payoffs[1])+L*as.numeric(payoffs[2])
      if(SHH>vH){
        S = 1
        H_P = H*SHH
      } else {
        S = 0
        H_P = H*vH
      }
      L_P = L*((1-S)*(H*vLH+L*vLL)+S*vLL)
      Total_P = H_P+L_P
      H = H_P/Total_P
      L = L_P/Total_P
      pop1 = data.frame(H,L,S)
      pop = rbind(pop, pop1)
    }
    pop = pop %>%
      rename(High_Type = H,
             Low_Type = L,
             Signal = S) %>%
      mutate(t = 0:time) %>%
      gather("High_Type", "Low_Type", key = Type, value = Proportion)
    pop$Signal[pop$Type=="Low_Type"]=0
    pop$Signal = as.factor(pop$Signal)
    pop
  }
  
  evo_apart = function(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow){
    H_N = ratio
    H_S = ratio
    L_N = 1-ratio
    L_S = 1-ratio
    pop = data.frame(H_N,H_S,L_N,L_S)
    SHH = max(vHH - K, 0)
    SHL = max(vHL - K, 0)
    SLH = max(vLH - K, 0)
    SLL = max(vLL - K, 0)
    payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
    expected_payoffs = data.frame()
    group_payoffs = data.frame()
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
      L_S_P = L_S*(S_H*as.numeric(payoffs[7])+S_L*as.numeric(payoffs[8]))
      if (pop_grow == "Fixed population"){
        Total_N_P = H_N_P+L_N_P
        Total_S_P = H_S_P+L_S_P
        expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_N_P), High_Signal = H_S_P/(H_S*Total_S_P), Low_No_Signal = L_N_P/(L_N*Total_N_P), Low_Signal = L_S_P/(L_S*Total_S_P))
        H_N = H_N_P/Total_N_P
        H_S = H_S_P/Total_S_P
        L_N = L_N_P/Total_N_P
        L_S = L_S_P/Total_S_P
      }
      if (pop_grow == "Unbounded exponential growth") {
        expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
        H_N = H_N_P
        H_S = H_S_P
        L_N = L_N_P
        L_S = L_S_P
      }
      pop1 = data.frame(H_N,H_S,L_N,L_S)
      pop = rbind(pop, pop1)
      group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
      group_payoffs = rbind(group_payoffs, group_payoffs1)
      expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
    }
    growth = pop[2:(time+1),]-pop[1:time,]
    growth = growth %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 1:time,
             Signal = High_Signal + Low_Signal,
             No_Signal = High_No_Signal + Low_No_Signal) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth")
    
    pop = pop %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 0:time,
             Signal = High_Signal + Low_Signal,
             No_Signal = High_No_Signal + Low_No_Signal) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Population")
    
    expected_payoffs = cbind(expected_payoffs,group_payoffs)
    expected_payoffs = expected_payoffs %>%
      mutate(t = 1:time) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth_Rate")
    list(pop, expected_payoffs, growth)
  }
  
  evo_join = function(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow, join_scenario){
    H_N = ratio
    H_S = ratio
    L_N = 1-ratio
    L_S = 1-ratio
    pop = data.frame(H_N,H_S,L_N,L_S)
    SHH = max(vHH - K, 0)
    SHL = max(vHL - K, 0)
    SLH = max(vLH - K, 0)
    SLL = max(vLL - K, 0)
    payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
    expected_payoffs = data.frame()
    group_payoffs = data.frame()
    for(i in 1:ceiling(time/2)){
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
      L_S_P = L_S*(S_H*as.numeric(payoffs[7])+S_L*as.numeric(payoffs[8]))
      if (pop_grow == "Fixed population"){
        Total_N_P = H_N_P+L_N_P
        Total_S_P = H_S_P+L_S_P
        expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_N_P), High_Signal = H_S_P/(H_S*Total_S_P), Low_No_Signal = L_N_P/(L_N*Total_N_P), Low_Signal = L_S_P/(L_S*Total_S_P))
        H_N = H_N_P/Total_N_P
        H_S = H_S_P/Total_S_P
        L_N = L_N_P/Total_N_P
        L_S = L_S_P/Total_S_P
      }
      if (pop_grow == "Unbounded exponential growth") {
        expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
        H_N = H_N_P
        H_S = H_S_P
        L_N = L_N_P
        L_S = L_S_P
      }
      pop1 = data.frame(H_N,H_S,L_N,L_S)
      pop = rbind(pop, pop1)
      group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
      group_payoffs = rbind(group_payoffs, group_payoffs1)
      expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
    }
    if (join_scenario == "Merge"){
      if (pop_grow == "Fixed population"){
        pop1 = pop1/2
        pop = rbind(pop,pop1)
        group_payoffs1 = group_payoffs1/2
        group_payoffs = rbind(group_payoffs, group_payoffs1)
        expected_payoffs1 = expected_payoffs1/2
        expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        for(i in (ceiling(time/2)+1):time){
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
          L_S_P = L_S*(S_H*as.numeric(payoffs[7])+S_L*as.numeric(payoffs[8]))
          Total_P = H_N_P+L_N_P+H_S_P+L_S_P
          expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_P), High_Signal = H_S_P/(H_S*Total_P), Low_No_Signal = L_N_P/(L_N*Total_P), Low_Signal = L_S_P/(L_S*Total_P))
          H_N = H_N_P/Total_P
          H_S = H_S_P/Total_P
          L_N = L_N_P/Total_P
          L_S = L_S_P/Total_P
          pop1 = data.frame(H_N,H_S,L_N,L_S)
          pop = rbind(pop, pop1)
          group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
          group_payoffs = rbind(group_payoffs, group_payoffs1)
          expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        }
      }
      if (pop_grow == "Unbounded exponential growth"){
        pop = rbind(pop,pop1)
        group_payoffs = rbind(group_payoffs, group_payoffs1)
        expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        for(i in (ceiling(time/2)+1):time){
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
          L_S_P = L_S*(S_H*as.numeric(payoffs[7])+S_L*as.numeric(payoffs[8]))
          expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
          H_N = H_N_P
          H_S = H_S_P
          L_N = L_N_P
          L_S = L_S_P
          pop1 = data.frame(H_N,H_S,L_N,L_S)
          pop = rbind(pop, pop1)
          group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
          group_payoffs = rbind(group_payoffs, group_payoffs1)
          expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        }
      }
    }
    if (join_scenario == "Fight"){
      if (pop_grow == "Fixed population"){
        pop1 = pop1/2
        pop = rbind(pop,pop1)
        group_payoffs1 = group_payoffs1/2
        group_payoffs = rbind(group_payoffs, group_payoffs1)
        expected_payoffs1 = expected_payoffs1/2
        expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        for(i in (ceiling(time/2)+1):time){
          H_N = as.numeric(tail(pop,1)[1])
          H_S = as.numeric(tail(pop,1)[2])
          L_N = as.numeric(tail(pop,1)[3])
          L_S = as.numeric(tail(pop,1)[4])
          #Fight where each kills .5 of other group
          S_D = (H_S+L_S)*.5
          N_D = (H_N+L_N)*.5
          S_H = if(H_S+L_S>0){
            H_S/(H_S+L_S)} else {0}
          S_L = 1-S_H
          N_H = if(H_N+L_N>0){
            H_N/(H_N+L_N)} else {0}
          N_L = 1-N_H
          H_N = max(H_N-S_D*N_H,0)
          H_S = max(H_S-N_D*S_H,0)
          L_N = max(L_N-S_D*N_L,0)
          L_S = max(L_S-N_D*S_L,0)
          #Reproduce
          S_H = if(H_S+L_S>0){
            H_S/(H_S+L_S)} else {0}
          S_L = 1-S_H
          N_H = if(H_N+L_N>0){
            H_N/(H_N+L_N)} else {0}
          N_L = 1-N_H
          H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
          H_S_P = H_S*(S_H*as.numeric(payoffs[5])+S_L*as.numeric(payoffs[6]))
          L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
          L_S_P = L_S*(S_H*as.numeric(payoffs[7])+S_L*as.numeric(payoffs[8]))
          Total_P = H_N_P+L_N_P+H_S_P+L_S_P
          expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_P), High_Signal = H_S_P/(H_S*Total_P), Low_No_Signal = L_N_P/(L_N*Total_P), Low_Signal = L_S_P/(L_S*Total_P))
          H_N = H_N_P/Total_P
          H_S = H_S_P/Total_P
          L_N = L_N_P/Total_P
          L_S = L_S_P/Total_P
          pop1 = data.frame(H_N,H_S,L_N,L_S)
          pop = rbind(pop, pop1)
          group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
          group_payoffs = rbind(group_payoffs, group_payoffs1)
          expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        }
      }
      if (pop_grow == "Unbounded exponential growth"){
        pop = rbind(pop,pop1)
        group_payoffs = rbind(group_payoffs, group_payoffs1)
        expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        for(i in (ceiling(time/2)+1):time){
          H_N = as.numeric(tail(pop,1)[1])
          H_S = as.numeric(tail(pop,1)[2])
          L_N = as.numeric(tail(pop,1)[3])
          L_S = as.numeric(tail(pop,1)[4])
          #Fight where each kills .5 of other group
          S_D = (H_S+L_S)*.5
          N_D = (H_N+L_N)*.5
          S_H = if(H_S+L_S>0){
            H_S/(H_S+L_S)} else {0}
          S_L = 1-S_H
          N_H = if(H_N+L_N>0){
            H_N/(H_N+L_N)} else {0}
          N_L = 1-N_H
          H_N = max(H_N-S_D*N_H,0)
          H_S = max(H_S-N_D*S_H,0)
          L_N = max(L_N-S_D*N_L,0)
          L_S = max(L_S-N_D*S_L,0)
          #Reproduce
          S_H = if(H_S+L_S>0){
            H_S/(H_S+L_S)} else {0}
          S_L = 1-S_H
          N_H = if(H_N+L_N>0){
            H_N/(H_N+L_N)} else {0}
          N_L = 1-N_H
          H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
          H_S_P = H_S*(S_H*as.numeric(payoffs[5])+S_L*as.numeric(payoffs[6]))
          L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
          L_S_P = L_S*(S_H*as.numeric(payoffs[7])+S_L*as.numeric(payoffs[8]))
          expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
          H_N = H_N_P
          H_S = H_S_P
          L_N = L_N_P
          L_S = L_S_P
          pop1 = data.frame(H_N,H_S,L_N,L_S)
          pop = rbind(pop, pop1)
          group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
          group_payoffs = rbind(group_payoffs, group_payoffs1)
          expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        }
      }
    }
    growth = pop[2:(time+2),]-pop[1:(time+1),]
    growth[ceiling(time/2)+1,]=NA
    growth = growth %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 1:(time+1),
             Signal = High_Signal + Low_Signal,
             No_Signal = High_No_Signal + Low_No_Signal) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth")
    
    pop = pop %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 0:(time+1),
             Signal = High_Signal + Low_Signal,
             No_Signal = High_No_Signal + Low_No_Signal) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Population")
    
    expected_payoffs = cbind(expected_payoffs,group_payoffs)
    expected_payoffs[ceiling(time/2)+1,]=NA
    expected_payoffs = expected_payoffs %>%
      mutate(t = 1:(time+1)) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth_Rate")
    list(pop, expected_payoffs, growth)
  }
  
  evo_apart_high = function(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow){
    H_N = ratio
    H_S = ratio
    L_N = 1-ratio
    L_S = 1-ratio
    pop = data.frame(H_N,H_S,L_N,L_S)
    SHH = max(vHH - K, 0)
    SHL = max(vHL - K, 0)
    SLH = max(vLH - K, 0)
    SLL = max(vLL - K, 0)
    payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
    expected_payoffs = data.frame()
    group_payoffs = data.frame()
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
      H_S_P = SHH*H_S
      L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
      L_S_P = vLL*L_S
      if (pop_grow == "Fixed population"){
        Total_N_P = H_N_P+L_N_P
        Total_S_P = H_S_P+L_S_P
        expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_N_P), High_Signal = H_S_P/(H_S*Total_S_P), Low_No_Signal = L_N_P/(L_N*Total_N_P), Low_Signal = L_S_P/(L_S*Total_S_P))
        H_N = H_N_P/Total_N_P
        H_S = H_S_P/Total_S_P
        L_N = L_N_P/Total_N_P
        L_S = L_S_P/Total_S_P
      }
      if (pop_grow == "Unbounded exponential growth") {
        expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
        H_N = H_N_P
        H_S = H_S_P
        L_N = L_N_P
        L_S = L_S_P
      }
      pop1 = data.frame(H_N,H_S,L_N,L_S)
      pop = rbind(pop, pop1)
      group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
      group_payoffs = rbind(group_payoffs, group_payoffs1)
      expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
    }
    growth = pop[2:(time+1),]-pop[1:time,]
    growth = growth %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 1:time,
             Signal = High_Signal + Low_Signal,
             No_Signal = High_No_Signal + Low_No_Signal) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth")
    
    pop = pop %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 0:time,
             Signal = High_Signal + Low_Signal,
             No_Signal = High_No_Signal + Low_No_Signal) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Population")
    
    expected_payoffs = cbind(expected_payoffs,group_payoffs)
    expected_payoffs = expected_payoffs %>%
      mutate(t = 1:time) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth_Rate")
    list(pop, expected_payoffs, growth)
  }
  
  evo_join_high = function(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow, join_scenario, beta, start){
    H_N = ratio
    H_S = ratio
    L_N = 1-ratio
    L_S = 1-ratio
    pop = data.frame(H_N,H_S,L_N,L_S)
    SHH = max(vHH - K, 0)
    SHL = max(vHL - K, 0)
    SLH = max(vLH - K, 0)
    SLL = max(vLL - K, 0)
    payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
    expected_payoffs = data.frame()
    group_payoffs = data.frame()
    if (start != 1){
      for(i in 1:(start-1)){
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
        H_S_P = SHH*H_S
        L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
        L_S_P = vLL*L_S
        if (pop_grow == "Fixed population"){
          Total_N_P = H_N_P+L_N_P
          Total_S_P = H_S_P+L_S_P
          expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_N_P), High_Signal = H_S_P/(H_S*Total_S_P), Low_No_Signal = L_N_P/(L_N*Total_N_P), Low_Signal = L_S_P/(L_S*Total_S_P))
          H_N = H_N_P/Total_N_P
          H_S = H_S_P/Total_S_P
          L_N = L_N_P/Total_N_P
          L_S = L_S_P/Total_S_P
        }
        if (pop_grow == "Unbounded exponential growth") {
          expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
          H_N = H_N_P
          H_S = H_S_P
          L_N = L_N_P
          L_S = L_S_P
        }
        pop1 = data.frame(H_N,H_S,L_N,L_S)
        pop = rbind(pop, pop1)
        group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
        group_payoffs = rbind(group_payoffs, group_payoffs1)
        expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
      }
    }
    if (join_scenario == "Merge"){
      if (pop_grow == "Fixed population"){
        for(i in start:time){
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
          H_S_P = H_S*SHH
          L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
          L_S_P = L_S*vLL
          Total_P = H_N_P+L_N_P+H_S_P+L_S_P
          expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_P), High_Signal = H_S_P/(H_S*Total_P), Low_No_Signal = L_N_P/(L_N*Total_P), Low_Signal = L_S_P/(L_S*Total_P))
          H_N = H_N_P/Total_P
          H_S = H_S_P/Total_P
          L_N = L_N_P/Total_P
          L_S = L_S_P/Total_P
          pop1 = data.frame(H_N,H_S,L_N,L_S)
          pop = rbind(pop, pop1)
          group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
          group_payoffs = rbind(group_payoffs, group_payoffs1)
          expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        }
      }
      if (pop_grow == "Unbounded exponential growth"){
        for(i in start:time){
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
          H_S_P = H_S*SHH
          L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
          L_S_P = L_S*vLL
          expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
          H_N = H_N_P
          H_S = H_S_P
          L_N = L_N_P
          L_S = L_S_P
          pop1 = data.frame(H_N,H_S,L_N,L_S)
          pop = rbind(pop, pop1)
          group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
          group_payoffs = rbind(group_payoffs, group_payoffs1)
          expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        }
      }
    }
    if (join_scenario == "Fight"){
      if (pop_grow == "Fixed population"){
        for(i in start:time){
          H_N = as.numeric(tail(pop,1)[1])
          H_S = as.numeric(tail(pop,1)[2])
          L_N = as.numeric(tail(pop,1)[3])
          L_S = as.numeric(tail(pop,1)[4])
          #Fight where each kills beta of other group
          S_D = (H_S+L_S)*beta
          N_D = (H_N+L_N)*beta
          S_H = if(H_S+L_S>0){
            H_S/(H_S+L_S)} else {0}
          S_L = 1-S_H
          N_H = if(H_N+L_N>0){
            H_N/(H_N+L_N)} else {0}
          N_L = 1-N_H
          H_N = max(H_N-S_D*N_H,0)
          H_S = max(H_S-N_D*S_H,0)
          L_N = max(L_N-S_D*N_L,0)
          L_S = max(L_S-N_D*S_L,0)
          #Reproduce
          S_H = if(H_S+L_S>0){
            H_S/(H_S+L_S)} else {0}
          S_L = 1-S_H
          N_H = if(H_N+L_N>0){
            H_N/(H_N+L_N)} else {0}
          N_L = 1-N_H
          H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
          H_S_P = H_S*SHH
          L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
          L_S_P = L_S*vLL
          Total_P = H_N_P+L_N_P+H_S_P+L_S_P
          expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_P), High_Signal = H_S_P/(H_S*Total_P), Low_No_Signal = L_N_P/(L_N*Total_P), Low_Signal = L_S_P/(L_S*Total_P))
          H_N = H_N_P/Total_P
          H_S = H_S_P/Total_P
          L_N = L_N_P/Total_P
          L_S = L_S_P/Total_P
          pop1 = data.frame(H_N,H_S,L_N,L_S)
          pop = rbind(pop, pop1)
          group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
          group_payoffs = rbind(group_payoffs, group_payoffs1)
          expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        }
      }
      if (pop_grow == "Unbounded exponential growth"){
        for(i in start:time){
          H_N = as.numeric(tail(pop,1)[1])
          H_S = as.numeric(tail(pop,1)[2])
          L_N = as.numeric(tail(pop,1)[3])
          L_S = as.numeric(tail(pop,1)[4])
          #Fight where each kills beta of other group
          S_D = (H_S+L_S)*beta
          N_D = (H_N+L_N)*beta
          S_H = if(H_S+L_S>0){
            H_S/(H_S+L_S)} else {0}
          S_L = 1-S_H
          N_H = if(H_N+L_N>0){
            H_N/(H_N+L_N)} else {0}
          N_L = 1-N_H
          H_N = max(H_N-S_D*N_H,0)
          H_S = max(H_S-N_D*S_H,0)
          L_N = max(L_N-S_D*N_L,0)
          L_S = max(L_S-N_D*S_L,0)
          #Reproduce
          S_H = if(H_S+L_S>0){
            H_S/(H_S+L_S)} else {0}
          S_L = 1-S_H
          N_H = if(H_N+L_N>0){
            H_N/(H_N+L_N)} else {0}
          N_L = 1-N_H
          H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
          H_S_P = H_S*SHH
          L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
          L_S_P = L_S*vLL
          expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
          H_N = H_N_P
          H_S = H_S_P
          L_N = L_N_P
          L_S = L_S_P
          pop1 = data.frame(H_N,H_S,L_N,L_S)
          pop = rbind(pop, pop1)
          group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
          group_payoffs = rbind(group_payoffs, group_payoffs1)
          expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
        }
      }
    }
    growth = pop[2:(time+1),]-pop[1:(time),]
    growth = growth %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 1:time,
             Signal = High_Signal + Low_Signal,
             No_Signal = High_No_Signal + Low_No_Signal) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth")
    
    pop = pop %>%
      rename(High_No_Signal = H_N,
             High_Signal = H_S,
             Low_No_Signal = L_N,
             Low_Signal = L_S) %>%
      mutate(t = 0:time,
             Signal = High_Signal + Low_Signal,
             No_Signal = High_No_Signal + Low_No_Signal) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Population")
    
    expected_payoffs = cbind(expected_payoffs,group_payoffs)
    expected_payoffs = expected_payoffs %>%
      mutate(t = 1:time) %>%
      gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth_Rate")
    list(pop, expected_payoffs, growth)
  }
  
  regions = function(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow, beta, start){
    #create a few graphs showing which group will survive the competition under different paramaters
    Klow = round(max(vLH-vLL,0),2)
    Khigh = round(vHH-vHL,2)
    Krange = seq(from = Klow, to = Khigh, by = .01)
    startrange = seq(from = 1, to = time, by = 1)
    betarange = seq(from = .01, to = 1, by = .01)
    SKregion = tibble(start = rep(startrange, length(Krange)), K = rep(Krange, each = length(startrange)), result = "")
    BKregion = tibble(beta = rep(betarange, length(Krange)), K = rep(Krange, each = length(betarange)), result = "")
    SBregion = tibble(start = rep(startrange, length(betarange)), beta = rep(betarange, each = length(startrange)), result = "")
    for(Kvar in Krange){
      #KSregion and BKregion first
      j = match(Kvar,Krange)
      H_N = ratio
      H_S = ratio
      L_N = 1-ratio
      L_S = 1-ratio
      pop = data.frame(H_N,H_S,L_N,L_S)
      SHH = max(vHH - Kvar, 0)
      SHL = max(vHL - Kvar, 0)
      SLH = max(vLH - Kvar, 0)
      SLL = max(vLL - Kvar, 0)
      payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
      if (time != 1){
        for(i in 1:(time-1)){
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
          H_S_P = SHH*H_S
          L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
          L_S_P = vLL*L_S
          if (pop_grow == "Fixed population"){
            Total_N_P = H_N_P+L_N_P
            Total_S_P = H_S_P+L_S_P
            H_N = H_N_P/Total_N_P
            H_S = H_S_P/Total_S_P
            L_N = L_N_P/Total_N_P
            L_S = L_S_P/Total_S_P
          }
          if (pop_grow == "Unbounded exponential growth") {
            H_N = H_N_P
            H_S = H_S_P
            L_N = L_N_P
            L_S = L_S_P
          }
          pop1 = data.frame(H_N,H_S,L_N,L_S)
          pop = rbind(pop, pop1)
        }
      }
      for(startvar in startrange){
        #SKregion
        i = match(startvar,startrange)
        popstart = pop[startvar,]
        stop = 0
        while(stop == 0){
          if (pop_grow == "Fixed population"){
            H_N = as.numeric(tail(popstart,1)[1])
            H_S = as.numeric(tail(popstart,1)[2])
            L_N = as.numeric(tail(popstart,1)[3])
            L_S = as.numeric(tail(popstart,1)[4])
            #Fight where each kills beta of other group
            S_D = (H_S+L_S)*beta
            N_D = (H_N+L_N)*beta
            if((H_S+L_S)<=N_D){
              if((H_N+L_N)<=S_D & S_D>N_D){
                #tiebreak goes to N
                SKregion$result[(j-1)*length(startrange)+i] = "S"
                stop = 1
              } else {
                SKregion$result[(j-1)*length(startrange)+i] = "N"
                stop = 1
              }
            }
            if((H_N+L_N)<=S_D){
              SKregion$result[(j-1)*length(startrange)+i] = "S"
              stop = 1
            }
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N = max(H_N-S_D*N_H,0)
            H_S = max(H_S-N_D*S_H,0)
            L_N = max(L_N-S_D*N_L,0)
            L_S = max(L_S-N_D*S_L,0)
            #Reproduce
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
            H_S_P = H_S*SHH
            L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
            L_S_P = L_S*vLL
            Total_P = H_N_P+L_N_P+H_S_P+L_S_P
            H_N = H_N_P/Total_P
            H_S = H_S_P/Total_P
            L_N = L_N_P/Total_P
            L_S = L_S_P/Total_P
            popstart1 = data.frame(H_N,H_S,L_N,L_S)
            popstart = rbind(popstart, popstart1)
          }
          if (pop_grow == "Unbounded exponential growth"){
            H_N = as.numeric(tail(popstart,1)[1])
            H_S = as.numeric(tail(popstart,1)[2])
            L_N = as.numeric(tail(popstart,1)[3])
            L_S = as.numeric(tail(popstart,1)[4])
            #Fight where each kills beta of other group
            S_D = (H_S+L_S)*beta
            N_D = (H_N+L_N)*beta
            if((H_S+L_S)<=N_D){
              if((H_N+L_N)<=S_D & S_D>N_D){
                #tiebreak goes to N
                SKregion$result[(j-1)*length(startrange)+i] = "S"
                stop = 1
              } else {
                SKregion$result[(j-1)*length(startrange)+i] = "N"
                stop = 1
              }
            }
            if((H_N+L_N)<=S_D){
              SKregion$result[(j-1)*length(startrange)+i] = "S"
              stop = 1
            }
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N = max(H_N-S_D*N_H,0)
            H_S = max(H_S-N_D*S_H,0)
            L_N = max(L_N-S_D*N_L,0)
            L_S = max(L_S-N_D*S_L,0)
            #Reproduce
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
            H_S_P = H_S*SHH
            L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
            L_S_P = L_S*vLL
            H_N = H_N_P
            H_S = H_S_P
            L_N = L_N_P
            L_S = L_S_P
            popstart1 = data.frame(H_N,H_S,L_N,L_S)
            popstart = rbind(popstart, popstart1)
          }
        }
      }
      for(betavar in betarange){
        #BKregion
        i = match(betavar, betarange)
        popstart = pop[start,]
        stop = 0
        while(stop == 0){
          if (pop_grow == "Fixed population"){
            H_N = as.numeric(tail(popstart,1)[1])
            H_S = as.numeric(tail(popstart,1)[2])
            L_N = as.numeric(tail(popstart,1)[3])
            L_S = as.numeric(tail(popstart,1)[4])
            #Fight where each kills beta of other group
            S_D = (H_S+L_S)*betavar
            N_D = (H_N+L_N)*betavar
            if((H_S+L_S)<=N_D){
              if((H_N+L_N)<=S_D & S_D>N_D){
                #tiebreak goes to N
                BKregion$result[(j-1)*length(betarange)+i] = "S"
                stop = 1
              } else {
                BKregion$result[(j-1)*length(betarange)+i] = "N"
                stop = 1
              }
            }
            if((H_N+L_N)<=S_D){
              BKregion$result[(j-1)*length(betarange)+i] = "S"
              stop = 1
            }
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N = max(H_N-S_D*N_H,0)
            H_S = max(H_S-N_D*S_H,0)
            L_N = max(L_N-S_D*N_L,0)
            L_S = max(L_S-N_D*S_L,0)
            #Reproduce
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
            H_S_P = H_S*SHH
            L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
            L_S_P = L_S*vLL
            Total_P = H_N_P+L_N_P+H_S_P+L_S_P
            H_N = H_N_P/Total_P
            H_S = H_S_P/Total_P
            L_N = L_N_P/Total_P
            L_S = L_S_P/Total_P
            popstart1 = data.frame(H_N,H_S,L_N,L_S)
            popstart = rbind(popstart, popstart1)
          }
          if (pop_grow == "Unbounded exponential growth"){
            H_N = as.numeric(tail(popstart,1)[1])
            H_S = as.numeric(tail(popstart,1)[2])
            L_N = as.numeric(tail(popstart,1)[3])
            L_S = as.numeric(tail(popstart,1)[4])
            #Fight where each kills beta of other group
            S_D = (H_S+L_S)*betavar
            N_D = (H_N+L_N)*betavar
            if((H_S+L_S)<=N_D){
              if((H_N+L_N)<=S_D & S_D>N_D){
                #tiebreak goes to N
                BKregion$result[(j-1)*length(betarange)+i] = "S"
                stop = 1
              } else {
                BKregion$result[(j-1)*length(betarange)+i] = "N"
                stop = 1
              }
            }
            if((H_N+L_N)<=S_D){
              BKregion$result[(j-1)*length(betarange)+i] = "S"
              stop = 1
            }
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N = max(H_N-S_D*N_H,0)
            H_S = max(H_S-N_D*S_H,0)
            L_N = max(L_N-S_D*N_L,0)
            L_S = max(L_S-N_D*S_L,0)
            #Reproduce
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
            H_S_P = H_S*SHH
            L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
            L_S_P = L_S*vLL
            H_N = H_N_P
            H_S = H_S_P
            L_N = L_N_P
            L_S = L_S_P
            popstart1 = data.frame(H_N,H_S,L_N,L_S)
            popstart = rbind(popstart, popstart1)
          }
        }
      }
    }
    ##SBregion
    H_N = ratio
    H_S = ratio
    L_N = 1-ratio
    L_S = 1-ratio
    pop = data.frame(H_N,H_S,L_N,L_S)
    SHH = max(vHH - K, 0)
    SHL = max(vHL - K, 0)
    SLH = max(vLH - K, 0)
    SLL = max(vLL - K, 0)
    payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
    if (time != 1){
      for(i in 1:(time-1)){
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
        H_S_P = SHH*H_S
        L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
        L_S_P = vLL*L_S
        if (pop_grow == "Fixed population"){
          Total_N_P = H_N_P+L_N_P
          Total_S_P = H_S_P+L_S_P
          H_N = H_N_P/Total_N_P
          H_S = H_S_P/Total_S_P
          L_N = L_N_P/Total_N_P
          L_S = L_S_P/Total_S_P
        }
        if (pop_grow == "Unbounded exponential growth") {
          H_N = H_N_P
          H_S = H_S_P
          L_N = L_N_P
          L_S = L_S_P
        }
        pop1 = data.frame(H_N,H_S,L_N,L_S)
        pop = rbind(pop, pop1)
      }
    }
    for(startvar in startrange){
      i = match(startvar, startrange)
      for(betavar in betarange){
        j = match(betavar, betarange)
        popstart = pop[startvar,]
        stop = 0
        while(stop == 0){
          if (pop_grow == "Fixed population"){
            H_N = as.numeric(tail(popstart,1)[1])
            H_S = as.numeric(tail(popstart,1)[2])
            L_N = as.numeric(tail(popstart,1)[3])
            L_S = as.numeric(tail(popstart,1)[4])
            #Fight where each kills beta of other group
            S_D = (H_S+L_S)*betavar
            N_D = (H_N+L_N)*betavar
            if((H_S+L_S)<=N_D){
              if((H_N+L_N)<=S_D & S_D>N_D){
                #tiebreak goes to N
                SBregion$result[(j-1)*length(startrange)+i] = "S"
                stop = 1
              } else {
                SBregion$result[(j-1)*length(startrange)+i] = "N"
                stop = 1
              }
            }
            if((H_N+L_N)<=S_D){
              SBregion$result[(j-1)*length(startrange)+i] = "S"
              stop = 1
            }
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N = max(H_N-S_D*N_H,0)
            H_S = max(H_S-N_D*S_H,0)
            L_N = max(L_N-S_D*N_L,0)
            L_S = max(L_S-N_D*S_L,0)
            #Reproduce
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
            H_S_P = H_S*SHH
            L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
            L_S_P = L_S*vLL
            Total_P = H_N_P+L_N_P+H_S_P+L_S_P
            H_N = H_N_P/Total_P
            H_S = H_S_P/Total_P
            L_N = L_N_P/Total_P
            L_S = L_S_P/Total_P
            popstart1 = data.frame(H_N,H_S,L_N,L_S)
            popstart = rbind(popstart, popstart1)
          }
          if (pop_grow == "Unbounded exponential growth"){
            H_N = as.numeric(tail(popstart,1)[1])
            H_S = as.numeric(tail(popstart,1)[2])
            L_N = as.numeric(tail(popstart,1)[3])
            L_S = as.numeric(tail(popstart,1)[4])
            #Fight where each kills beta of other group
            S_D = (H_S+L_S)*betavar
            N_D = (H_N+L_N)*betavar
            if((H_S+L_S)<=N_D){
              if((H_N+L_N)<=S_D & S_D>N_D){
                #tiebreak goes to N
                SBregion$result[(j-1)*length(startrange)+i] = "S"
                stop = 1
              } else {
                SBregion$result[(j-1)*length(startrange)+i] = "N"
                stop = 1
              }
            }
            if((H_N+L_N)<=S_D){
              SBregion$result[(j-1)*length(startrange)+i] = "S"
              stop = 1
            }
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N = max(H_N-S_D*N_H,0)
            H_S = max(H_S-N_D*S_H,0)
            L_N = max(L_N-S_D*N_L,0)
            L_S = max(L_S-N_D*S_L,0)
            #Reproduce
            S_H = if(H_S+L_S>0){
              H_S/(H_S+L_S)} else {0}
            S_L = 1-S_H
            N_H = if(H_N+L_N>0){
              H_N/(H_N+L_N)} else {0}
            N_L = 1-N_H
            H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
            H_S_P = H_S*SHH
            L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
            L_S_P = L_S*vLL
            H_N = H_N_P
            H_S = H_S_P
            L_N = L_N_P
            L_S = L_S_P
            popstart1 = data.frame(H_N,H_S,L_N,L_S)
            popstart = rbind(popstart, popstart1)
          }
        }
      }
    }
    list(SKregion, BKregion, SBregion)
  }
  
  pop_evo_elect = reactive({
    evo_elect(input$ratio, input$vHH, input$vHL, input$vLH, input$vLL, input$K, input$time)
  })
  
  pop_evo_electS = reactive({
    pop_evo_elect() %>%
      mutate(Type = "High_Type_Signaling")%>%
      filter(Signal == 1)
  })
  
  output$TE = renderPlot({
    ggplot(data = pop_evo_elect(), aes(x = t, y = Proportion, color = Type)) +
      geom_line()+
      geom_line(data = pop_evo_electS(), aes(x = t, y = Proportion, color = Type))
  })
  
  pop_evo = reactive({
    evo(input$ratio, input$ratio_h, input$ratio_l, input$vHH, input$vHL, input$vLH, input$vLL, input$K, input$time, input$pop_grow)
  })

  output$PA_P = renderPlot({
    ggplot(data = pop_evo()[[1]], aes(x = t, y = Population, color = Type)) +
      geom_line()
    })
  
  output$PA_R = renderPlot({
    ggplot(data = pop_evo()[[2]], aes(x = t, y = Growth_Rate, color = Type)) +
      geom_line()
  })
  
  output$PA_G = renderPlot({
    ggplot(data = pop_evo()[[3]], aes(x = t, y = Growth, color = Type)) +
      geom_line()
  })
  
  pop_evo_A = reactive({
    evo_apart(input$ratio, input$vHH, input$vHL, input$vLH, input$vLL, input$K, input$time, input$pop_grow)
  })
  
  output$Sep_P = renderPlot({
    ggplot(data = pop_evo_A()[[1]], aes(x = t, y = Population, color = Type)) +
      geom_line()
  })
  
  output$Sep_R = renderPlot({
    ggplot(data = pop_evo_A()[[2]], aes(x = t, y = Growth_Rate, color = Type))+
      geom_line()
  })
  
  output$Sep_G = renderPlot({
    ggplot(data = pop_evo_A()[[3]], aes(x = t, y = Growth, color = Type))+
      geom_line()
  })
  
  pop_evo_join = reactive({
    evo_join(input$ratio, input$vHH, input$vHL, input$vLH, input$vLL, input$K, input$time, input$pop_grow, input$join_scenario)
  })
  
  output$Join_P = renderPlot({
    ggplot(data = pop_evo_join()[[1]], aes(x = t, y = Population, color = Type)) +
      geom_line()+
      geom_vline(xintercept = ceiling(input$time/2))+
      geom_vline(xintercept = ceiling(input$time/2)+1)
  })
  
  output$Join_R = renderPlot({
    ggplot(data = pop_evo_join()[[2]], aes(x = t, y = Growth_Rate, color = Type))+
      geom_line()+
      geom_vline(xintercept = ceiling(input$time/2))+
      geom_vline(xintercept = ceiling(input$time/2)+2)
  })
  
  output$Join_G = renderPlot({
    ggplot(data = pop_evo_join()[[3]], aes(x = t, y = Growth, color = Type))+
      geom_line()+
      geom_vline(xintercept = ceiling(input$time/2))+
      geom_vline(xintercept = ceiling(input$time/2)+2)
  })
  
  pop_evo_A_H = reactive({
    evo_apart_high(input$ratio, input$vHH, input$vHL, input$vLH, input$vLL, input$K, input$time, input$pop_grow)
  })
  
  output$SepH_P = renderPlot({
    ggplot(data = pop_evo_A_H()[[1]], aes(x = t, y = Population, color = Type)) +
      geom_line()
  })
  
  output$SepH_R = renderPlot({
    ggplot(data = pop_evo_A_H()[[2]], aes(x = t, y = Growth_Rate, color = Type))+
      geom_line()
  })
  
  output$SepH_G = renderPlot({
    ggplot(data = pop_evo_A_H()[[3]], aes(x = t, y = Growth, color = Type))+
      geom_line()
  })
  
  pop_evo_join_high = reactive({
    evo_join_high(input$ratio, input$vHH, input$vHL, input$vLH, input$vLL, input$K, input$time, input$pop_grow, input$join_scenario, input$beta, input$start)
  })
  
  output$JoinH_P = renderPlot({
    ggplot(data = pop_evo_join_high()[[1]], aes(x = t, y = Population, color = Type)) +
      geom_line()+
      geom_vline(xintercept = input$start-1)
  })
  
  output$JoinH_R = renderPlot({
    ggplot(data = pop_evo_join_high()[[2]], aes(x = t, y = Growth_Rate, color = Type))+
      geom_line()+
      geom_vline(xintercept = input$start-1)
  })
  
  output$JoinH_G = renderPlot({
    ggplot(data = pop_evo_join_high()[[3]], aes(x = t, y = Growth, color = Type))+
      geom_line()+
      geom_vline(xintercept = input$start-1)
  })
  
  regions_out = reactive({
    regions(input$ratio, input$vHH, input$vHL, input$vLH, input$vLL, input$K, input$time, input$pop_grow, input$beta, input$start)
  })
  
  output$SKregion = renderPlot({
    ggplot(data = regions_out()[[1]], aes(x = start, y = K, fill = result)) +
      geom_tile(color = "black")+
      scale_fill_manual(values = c("N"=4, "S"=6)) +
      geom_vline(xintercept = input$start, color = "white") +
      geom_hline(yintercept = input$K, color = "white")
  })
  
  output$BKregion = renderPlot({
    ggplot(data = regions_out()[[2]], aes(x = beta, y = K, fill = result)) +
      geom_tile(color = "black")+
      scale_fill_manual(values = c("N"=4, "S"=6)) +
      geom_vline(xintercept = input$beta, color = "white") +
      geom_hline(yintercept = input$K, color = "white")
  })
  
  output$SBregion = renderPlot({
    ggplot(data = regions_out()[[3]], aes(x = start, y = beta, fill = result)) +
      geom_tile(color = "black")+
      scale_fill_manual(values = c("N"=4, "S"=6)) +
      geom_vline(xintercept = input$start, color = "white") +
      geom_hline(yintercept = input$beta, color = "white")
  })
}
