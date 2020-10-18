library("tidyverse", "shiny", "stringr")

time = 30
vHH =1.15
vHL = .9
vLH = .85
vLL = .8
K = .1
ratio = .2
ratio_l = 0
ratio_h = 1
pop_grow = "Unbounded exponential growth"
#"Fixed population", "Unbounded exponential growth", "Logistic growth to capacity"
join_scenario = "Fight"
#"Fight", "Merge"
start = 20
beta = .2


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

#make graphs for population evolution with no signaling

pop_evo = evo(ratio, 0, 0, vHH, vHL, vLH, vLL, K, time, pop_grow)[[1]]
rate_evo = evo(ratio, 0, 0, vHH, vHL, vLH, vLL, K, time, pop_grow)[[2]]
grow_evo = evo(ratio, 0, 0, vHH, vHL, vLH, vLL, K, time, pop_grow)[[3]]

pop_evo_NS = rbind(pop_evo[pop_evo$Type=="High_No_Signal",], pop_evo[pop_evo$Type=="Low_No_Signal",])
rate_evo_NS = rbind(rate_evo[rate_evo$Type=="High_No_Signal",], rate_evo[rate_evo$Type=="Low_No_Signal",])
grow_evo_NS = rbind(grow_evo[grow_evo$Type=="No_Signal",], grow_evo[grow_evo$Type=="High_No_Signal",], grow_evo[grow_evo$Type=="Low_No_Signal",])

pop_evo_NS$Type[pop_evo_NS$Type=="High_No_Signal"] = "High Type"
pop_evo_NS$Type[pop_evo_NS$Type=="Low_No_Signal"] = "Low Type"
rate_evo_NS$Type[rate_evo_NS$Type=="High_No_Signal"] = "High Type"
rate_evo_NS$Type[rate_evo_NS$Type=="Low_No_Signal"] = "Low Type"
grow_evo_NS$Type[grow_evo_NS$Type=="High_No_Signal"] = "High Type"
grow_evo_NS$Type[grow_evo_NS$Type=="Low_No_Signal"] = "Low Type"
grow_evo_NS$Type[grow_evo_NS$Type=="No_Signal"] = "Population"

ggplot(data = pop_evo_NS, aes(x = t, y = Population, color = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  ggtitle("Number of High Types and Low Types")

ggplot(data = rate_evo_NS, aes(x = t, y = Growth_Rate, color = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  labs(y = "Growth Rate") +
  ggtitle("Growth Rate of High Types and Low Types")

ggplot(data = grow_evo_NS, aes(x = t, y = Growth, color = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  ggtitle("Growth of High Types and Low Types")

#make graphs for population where only high type signals.

pop_evo = evo(ratio, 1, 0, vHH, vHL, vLH, vLL, K, time, pop_grow)[[1]]
rate_evo = evo(ratio, 1, 0, vHH, vHL, vLH, vLL, K, time, pop_grow)[[2]]
grow_evo = evo(ratio, 1, 0, vHH, vHL, vLH, vLL, K, time, pop_grow)[[3]]

grow_evo = grow_evo %>%
  pivot_wider(names_from = Type, values_from = Growth) %>%
  mutate(Signal = High_Signal+Low_No_Signal) %>%
  pivot_longer(!t, names_to = "Type", values_to = "Growth") %>%
  arrange(Type)

pop_evo_S = rbind(pop_evo[pop_evo$Type=="High_Signal",], pop_evo[pop_evo$Type=="Low_No_Signal",])
rate_evo_S = rbind(rate_evo[rate_evo$Type=="High_Signal",], rate_evo[rate_evo$Type=="Low_No_Signal",])
grow_evo_S = rbind(grow_evo[grow_evo$Type=="Signal",], grow_evo[grow_evo$Type=="High_Signal",], grow_evo[grow_evo$Type=="Low_No_Signal",])

pop_evo_S$Type[pop_evo_S$Type=="High_Signal"] = "High Type"
pop_evo_S$Type[pop_evo_S$Type=="Low_No_Signal"] = "Low Type"
rate_evo_S$Type[rate_evo_S$Type=="High_Signal"] = "High Type"
rate_evo_S$Type[rate_evo_S$Type=="Low_No_Signal"] = "Low Type"
grow_evo_S$Type[grow_evo_S$Type=="High_Signal"] = "High Type"
grow_evo_S$Type[grow_evo_S$Type=="Low_No_Signal"] = "Low Type"
grow_evo_S$Type[grow_evo_S$Type=="Signal"] = "Population"

ggplot(data = pop_evo_S, aes(x = t, y = Population, color = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  ggtitle("Number of High Types and Low Types")

ggplot(data = rate_evo_S, aes(x = t, y = Growth_Rate, color = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  labs(y = "Growth Rate") +
  ggtitle("Growth Rate of High Types and Low Types")

ggplot(data = grow_evo_S, aes(x = t, y = Growth, color = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  ggtitle("Growth of High Types and Low Types")

#Make graphs for competition between populations

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

