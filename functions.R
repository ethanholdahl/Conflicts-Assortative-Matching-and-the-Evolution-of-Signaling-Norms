library("tidyverse", "shiny", "stringr")

time = 30
vHH =1.2
vHL = 1.02
vLH = .95
vLL = .9
K = .05
ratio = .2
ratio_l = 0
ratio_h = .5
pop_grow = "Unbounded exponential growth"

#"Fixed population", "Unbounded exponential growth", "Logistic growth to capacity"

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
  
  growth = (expected_payoffs-1)*pop[2:(time+1),] 
  growth = growth %>%
    mutate(t = 1:time) %>%
    gather("High_No_Signal", "Low_No_Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth")
  
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

pop_evo = evo(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, K, time, pop_grow)[[1]]
rate_evo = evo(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, K, time, pop_grow)[[2]]
grow_evo = evo(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, K, time, pop_grow)[[3]]



ggplot(data = pop_evo, aes(x = t, y = Population, color = Type)) +
  geom_line()

ggplot(data = rate_evo, aes(x = t, y = Growth_Rate, color = Type)) +
  geom_line()

ggplot(data = grow_evo, aes(x = t, y = Growth, color = Type)) +
  geom_line()

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

pop_evo_elect = evo_elect(ratio, vHH, vHL, vLH, vLL, K, time)

pop_evo_electS = pop_evo_elect %>%
  filter(Signal == 1)
pop_evo_electS$Type = "High_Type_Signaling"

ggplot(data = pop_evo_elect, aes(x = t, y = Proportion, color = Type)) +
  geom_line()+
  geom_line(data = pop_evo_electS, aes(x = t, y = Proportion, color = Type))

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
  growth = (expected_payoffs-1)*pop[2:(time+1),] 
  growth = growth %>%
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

pop_evo_A = evo_apart(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[1]]
rate_evo_A = evo_apart(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[2]]
grow_evo_A = evo_apart(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[3]]

ggplot(data = pop_evo_A, aes(x = t, y = Population, color = Type)) +
  geom_line()

ggplot(data = rate_evo_A, aes(x = t, y = Growth_Rate, color = Type))+
  geom_line()

ggplot(data = grow_evo_A, aes(x = t, y = Growth, color = Type))+
  geom_line()

evo_join = function(ratio, vHH, vHL, vLH, vLL, K, time){
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
  fitness = data.frame()
  for(i in 1:ceiling(time/2)){
    H_N = as.numeric(tail(pop,1)[1])
    H_S = as.numeric(tail(pop,1)[2])
    L_N = as.numeric(tail(pop,1)[3])
    L_S = as.numeric(tail(pop,1)[4])
    H_N_P = H_N*(H_N*as.numeric(payoffs[1])+L_N*as.numeric(payoffs[2]))
    H_S_P = H_S*(H_S*as.numeric(payoffs[5])+L_S*as.numeric(payoffs[6]))
    L_N_P = L_N*(H_N*as.numeric(payoffs[3])+L_N*as.numeric(payoffs[4]))
    L_S_P = L_S*(H_S*as.numeric(payoffs[7])+L_S*as.numeric(payoffs[8]))
    Total_N_P = H_N_P+L_N_P
    Total_S_P = H_S_P+L_S_P
    H_N = H_N_P/Total_N_P
    H_S = H_S_P/Total_S_P
    L_N = L_N_P/Total_N_P
    L_S = L_S_P/Total_S_P
    fitness1 = data.frame(H_N_P,H_S_P,L_N_P,L_S_P)
    fitness = rbind(fitness, fitness1)
    pop1 = data.frame(H_N,H_S,L_N,L_S)
    pop = rbind(pop, pop1)
  }
  pop1 = pop1/2
  pop = rbind(pop,pop1)
  fitness1 = fitness1/2
  fitness = rbind(fitness, fitness1)
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
    Total_P = H_N_P+H_S_P+L_N_P+L_S_P
    H_N = H_N_P/Total_P
    H_S = H_S_P/Total_P
    L_N = L_N_P/Total_P
    L_S = L_S_P/Total_P
    fitness1 = data.frame(H_N_P,H_S_P,L_N_P,L_S_P)
    fitness = rbind(fitness, fitness1)
    pop1 = data.frame(H_N,H_S,L_N,L_S)
    pop = rbind(pop, pop1)
  }
  group_pay = pop[2:(time+2),]*fitness
  group_pay = group_pay %>%
    mutate(No_Signal = H_N+L_N,
           Signal = H_S+L_S,
           t = 1:(time+1)) %>%
    gather("Signal", "No_Signal", key = Population, value = Payoff)
  pop = pop %>%
    rename(High_No_Signal = H_N,
           High_Signal = H_S,
           Low_No_Signal = L_N,
           Low_Signal = L_S) %>%
    mutate(t = 0:(time+1)) %>%
    gather("High_No_Signal", "High_Signal", "Low_No_Signal", "Low_Signal", key = Type, value = Proportion)
  fitness = fitness %>%
    rename(High_No_Signal = H_N_P,
           High_Signal = H_S_P,
           Low_No_Signal = L_N_P,
           Low_Signal = L_S_P) %>%
    mutate(t = 1:(time+1)) %>%
    gather("High_No_Signal", "High_Signal", "Low_No_Signal", "Low_Signal", key = Type, value = Payoff)
  list(pop, fitness, group_pay)
}

test=evo_join(ratio, vHH, vHL, vLH, vLL, K, time)

ggplot(data = test[[3]], aes(x = t, y = Payoff, color = Population)) +
  geom_line()+
  geom_vline(xintercept = ceiling(time/2))+
  geom_vline(xintercept = ceiling(time/2)+1)


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
  growth = (expected_payoffs-1)*pop[2:(time+1),] 
  growth = growth %>%
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

pop_evo_A_H = evo_apart_high(ratio, vHH, vHL, vLH, vLL, K, time,pop_grow)[[1]]
rate_evo_A_H = evo_apart_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[2]]
grow_evo_A_H = evo_apart_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[3]]

ggplot(data = pop_evo_A_H, aes(x = t, y = Population, color = Type)) +
  geom_line()

ggplot(data = rate_evo_A_H, aes(x = t, y = Growth_Rate, color = Type))+
  geom_line()

ggplot(data = grow_evo_A_H, aes(x = t, y = Growth, color = Type))+
  geom_line()

evo_join_high = function(ratio, vHH, vHL, vLH, vLL, K, time){
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
  fitness = data.frame()
  for(i in 1:ceiling(time/2)){
    H_N = as.numeric(tail(pop,1)[1])
    H_S = as.numeric(tail(pop,1)[2])
    L_N = as.numeric(tail(pop,1)[3])
    L_S = as.numeric(tail(pop,1)[4])
    H_N_P = H_N*(H_N*as.numeric(payoffs[1])+L_N*as.numeric(payoffs[2]))
    H_S_P = H_S*(H_S*as.numeric(payoffs[5])+L_S*as.numeric(payoffs[6]))
    L_N_P = L_N*(H_N*as.numeric(payoffs[3])+L_N*as.numeric(payoffs[4]))
    L_S_P = L_S*(H_S*as.numeric(payoffs[7])+L_S*as.numeric(payoffs[8]))
    Total_N_P = H_N_P+L_N_P
    Total_S_P = H_S_P+L_S_P
    H_N = H_N_P/Total_N_P
    H_S = H_S_P/Total_S_P
    L_N = L_N_P/Total_N_P
    L_S = L_S_P/Total_S_P
    fitness1 = data.frame(H_N_P,H_S_P,L_N_P,L_S_P)
    fitness = rbind(fitness, fitness1)
    pop1 = data.frame(H_N,H_S,L_N,L_S)
    pop = rbind(pop, pop1)
  }
  pop1 = pop1/2
  pop = rbind(pop,pop1)
  fitness1 = fitness1/2
  fitness = rbind(fitness, fitness1)
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
    Total_P = H_N_P+H_S_P+L_N_P+L_S_P
    H_N = H_N_P/Total_P
    H_S = H_S_P/Total_P
    L_N = L_N_P/Total_P
    L_S = L_S_P/Total_P
    fitness1 = data.frame(H_N_P,H_S_P,L_N_P,L_S_P)
    fitness = rbind(fitness, fitness1)
    pop1 = data.frame(H_N,H_S,L_N,L_S)
    pop = rbind(pop, pop1)
  }
  group_pay = pop[2:(time+2),]*fitness
  group_pay = group_pay %>%
    mutate(No_Signal = H_N+L_N,
           Signal = H_S+L_S,
           t = 1:(time+1)) %>%
    gather("Signal", "No_Signal", key = Population, value = Payoff)
  pop = pop %>%
    rename(High_No_Signal = H_N,
           High_Signal = H_S,
           Low_No_Signal = L_N,
           Low_Signal = L_S) %>%
    mutate(t = 0:(time+1)) %>%
    gather("High_No_Signal", "High_Signal", "Low_No_Signal", "Low_Signal", key = Type, value = Proportion)
  fitness = fitness %>%
    rename(High_No_Signal = H_N_P,
           High_Signal = H_S_P,
           Low_No_Signal = L_N_P,
           Low_Signal = L_S_P) %>%
    mutate(t = 1:(time+1)) %>%
    gather("High_No_Signal", "High_Signal", "Low_No_Signal", "Low_Signal", key = Type, value = Payoff)
  list(pop, fitness, group_pay)
}
