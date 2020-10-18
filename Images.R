library("tidyverse", "shiny", "stringr")

time = 35
vHH =1.1
vHL = .9
vLH = .85
vLL = .8
K = .05
ratio = .2
ratio_l = 0
ratio_h = 1
pop_grow = "Unbounded exponential growth"
#"Fixed population", "Unbounded exponential growth", "Logistic growth to capacity"
join_scenario = "Fight"
#"Fight", "Merge"
start = 5
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

pop_evo = evo(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, K, time, pop_grow)[[1]]
rate_evo = evo(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, K, time, pop_grow)[[2]]
grow_evo = evo(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, K, time, pop_grow)[[3]]

ggplot(data = pop_evo, aes(x = t, y = Population, color = Type)) +
  geom_line(size = 1.5)

ggplot(data = rate_evo, aes(x = t, y = Growth_Rate, color = Type)) +
  geom_line(size = 1.5)

ggplot(data = grow_evo, aes(x = t, y = Growth, color = Type)) +
  geom_line(size = 1.5)


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

