###### Set Up #######



library("tidyverse", "shiny", "stringr", "plotly")
##1100*500 image size

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


###### Evolution Apart ######



#Function for evolution of populations in isolation (only high types signal in signaling population)
evo_apart_high = function(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow){
  #define initial population makeup 
  H_N = ratio
  H_S = ratio
  L_N = 1-ratio
  L_S = 1-ratio
  pop = data.frame(H_N,H_S,L_N,L_S)
  #define game table payoffs 
  SHH = max(vHH - K, 0)
  SHL = max(vHL - K, 0)
  SLH = max(vLH - K, 0)
  SLL = max(vLL - K, 0)
  payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
  expected_payoffs = data.frame()
  group_payoffs = data.frame()
  for(i in 1:time){
    #extract current population values
    H_N = as.numeric(tail(pop,1)[1])
    H_S = as.numeric(tail(pop,1)[2])
    L_N = as.numeric(tail(pop,1)[3])
    L_S = as.numeric(tail(pop,1)[4])
    #find proportion of type in each matching pool (signalers with signalers, non-signalers with non-signalers)
    S_H = if(H_S+L_S>0){
      H_S/(H_S+L_S)} else {0}
    S_L = 1-S_H
    N_H = if(H_N+L_N>0){
      H_N/(H_N+L_N)} else {0}
    N_L = 1-N_H
    #calculate next generation pop levels (current pop * expected payoff for each group)
    H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
    H_S_P = max(H_S*as.numeric(payoffs[5]),0)
    L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
    L_S_P = max(L_S*as.numeric(payoffs[4]),0)
    
    #identify growth scenario
    if (pop_grow == "Fixed population"){
      #rebalance to keep total pop steady
      Total_N_P = H_N_P+L_N_P
      Total_S_P = H_S_P+L_S_P
      expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_N_P), High_Signal = H_S_P/(H_S*Total_S_P), Low_No_Signal = L_N_P/(L_N*Total_N_P), Low_Signal = L_S_P/(L_S*Total_S_P))
      H_N = H_N_P/Total_N_P
      H_S = H_S_P/Total_S_P
      L_N = L_N_P/Total_N_P
      L_S = L_S_P/Total_S_P
    }
    if (pop_grow == "Unbounded exponential growth") {
      #set next gen pop levels to current leves and record expected payoffs
      expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
      H_N = H_N_P
      H_S = H_S_P
      L_N = L_N_P
      L_S = L_S_P
    }
    
    #record end of generation information
    pop1 = data.frame(H_N,H_S,L_N,L_S)
    pop = rbind(pop, pop1)
    group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
    group_payoffs = rbind(group_payoffs, group_payoffs1)
    expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
  }
  #clean up the data
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
  growth$Type = factor(growth$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
  
  prop = pop %>%
    rename(High_No_Signal = H_N,
           High_Signal = H_S,
           Low_No_Signal = L_N,
           Low_Signal = L_S) %>%
    mutate(t = 0:time,
           Signal = High_Signal + Low_Signal,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    mutate(High_No_Signal = High_No_Signal/No_Signal,
           High_Signal = High_Signal/Signal,
           Low_No_Signal = Low_No_Signal/No_Signal,
           Low_Signal = Low_Signal/Signal) %>%
    mutate(t = 0:time,
           Signal = High_Signal + Low_Signal,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Proportion_of_Types")
  prop$Type = factor(prop$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
    
  pop = pop %>%
    rename(High_No_Signal = H_N,
           High_Signal = H_S,
           Low_No_Signal = L_N,
           Low_Signal = L_S) %>%
    mutate(t = 0:time,
           Signal = High_Signal + Low_Signal,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Population")
  pop$Type = factor(pop$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
  
  expected_payoffs = cbind(expected_payoffs,group_payoffs)
  expected_payoffs = expected_payoffs %>%
    mutate(t = 1:time) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth_Rate")
  expected_payoffs$Type = factor(expected_payoffs$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
  list(pop, expected_payoffs, growth, prop)
}

#make graphs for population evolution separately

pop_evo = evo_apart_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[1]]
rate_evo = evo_apart_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[2]]
grow_evo = evo_apart_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[3]]
prop_evo = evo_apart_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[4]]

#No Signal population

###No_Signal
pop_evo_NS = pop_evo
pop_evo_NS$Type = factor(pop_evo_NS$Type, levels = c("No_Signal", "High_No_Signal", "Low_No_Signal"))
pop_evo_NS = pop_evo_NS[1:93,]

rate_evo_NS = rate_evo
rate_evo_NS$Type = factor(rate_evo_NS$Type, levels = c("No_Signal", "High_No_Signal", "Low_No_Signal"))
rate_evo_NS = rate_evo_NS[1:90,]

grow_evo_NS = grow_evo
grow_evo_NS$Type = factor(grow_evo_NS$Type, levels = c("No_Signal", "High_No_Signal", "Low_No_Signal"))
grow_evo_NS = grow_evo_NS[1:90,]

prop_evo_NS = prop_evo
prop_evo_NS$Type = factor(prop_evo_NS$Type, levels = c("No_Signal", "High_No_Signal", "Low_No_Signal"))
prop_evo_NS = prop_evo_NS[1:93,]

###Pop_No_Signal
ggplot(data = pop_evo_NS, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  coord_cartesian(xlim =c(0, time))

###Rate_No_Signal
ggplot(data = rate_evo_NS, aes(x = t, y = Growth_Rate, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  labs(y = "Reproductive Rate")+
  coord_cartesian(xlim =c(0, time))

###Growth_No_Signal
ggplot(data = grow_evo_NS, aes(x = t, y = Growth, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Growth")+
  coord_cartesian(xlim =c(0, time))

###Prop_No_Signal (with pop line at 1)
ggplot(data = prop_evo_NS, aes(x = t, y = Proportion_of_Types, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Proportion of Types")+
  coord_cartesian(xlim =c(0, time))

###Prop_No_Signal (without pop line at 1)
ggplot(data = prop_evo_NS, aes(x = t, y = Proportion_of_Types, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Proportion of Types")+
  coord_cartesian(xlim =c(0, time))

#Signal Population

###Signal
pop_evo_S = pop_evo
pop_evo_S$Type = factor(pop_evo_S$Type, levels = c("Signal", "High_Signal", "Low_Signal"))
pop_evo_S = pop_evo_S[94:186,]


rate_evo_S = rate_evo
rate_evo_S$Type = factor(rate_evo_S$Type, levels = c("Signal", "High_Signal", "Low_Signal"))
rate_evo_S = rate_evo_S[91:180,]

grow_evo_S = grow_evo
grow_evo_S$Type = factor(grow_evo_S$Type, levels = c("Signal", "High_Signal", "Low_Signal"))
grow_evo_S = grow_evo_S[91:180,]

prop_evo_S = prop_evo
prop_evo_S$Type = factor(prop_evo_S$Type, levels = c("Signal", "High_Signal", "Low_Signal"))
prop_evo_S = prop_evo_S[94:186,]

###Pop_Signal
ggplot(data = pop_evo_S, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1)))+
  coord_cartesian(xlim =c(0, time))

###Rate_Signal
ggplot(data = rate_evo_S, aes(x = t, y = Growth_Rate, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1)))+
  labs(y = "Reproductive Rate")+
  coord_cartesian(xlim =c(0, time))

###Growth_Signal
ggplot(data = grow_evo_S, aes(x = t, y = Growth, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1)))+
  ylab("Growth")+
  coord_cartesian(xlim =c(0, time))

###Prop_Signal (with pop line at 1)
ggplot(data = prop_evo_S, aes(x = t, y = Proportion_of_Types, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1)))+
  ylab("Proportion of Types")+
  coord_cartesian(xlim =c(0, time))

###Prop_Signal (without pop line at 1)
ggplot(data = prop_evo_S, aes(x = t, y = Proportion_of_Types, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                        values = c("High_Signal" = "dashed", "Low_Signal" = "dotted"))+
  scale_color_manual(labels = c("High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                     values = c("High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1)))+
  ylab("Proportion of Types")+
  coord_cartesian(xlim =c(0, time))

#Compare Signal vs No Signal

###Pop_Compare
ggplot(data = pop_evo, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  coord_cartesian(xlim =c(0, time))

###Rate_Compare
ggplot(data = rate_evo, aes(x = t, y = Growth_Rate, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Reproductive Rate") +
  coord_cartesian(xlim =c(0, time))

###Growth_Compare
ggplot(data = grow_evo, aes(x = t, y = Growth, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Growth") +
  coord_cartesian(xlim =c(0, time))

###Prop_Compare (with pop line at 1)
ggplot(data = prop_evo, aes(x = t, y = Proportion_of_Types, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Proportion of Types") +
  coord_cartesian(xlim =c(0, time))

###Prop_Compare (without pop line at 1)
ggplot(data = prop_evo, aes(x = t, y = Proportion_of_Types, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("High_Signal" = "dashed", "Low_Signal" = "dotted", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Proportion of Types") +
  coord_cartesian(xlim =c(0, time))

#Function for evolution of populations in isolation (both types signal in signaling population)
evo_apart_genetic = function(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow){
  #define initial population makeup 
  H_N = ratio
  H_S = ratio
  L_N = 1-ratio
  L_S = 1-ratio
  pop = data.frame(H_N,H_S,L_N,L_S)
  #define game table payoffs 
  SHH = max(vHH - K, 0)
  SHL = max(vHL - K, 0)
  SLH = max(vLH - K, 0)
  SLL = max(vLL - K, 0)
  payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
  expected_payoffs = data.frame()
  group_payoffs = data.frame()
  for(i in 1:time){
    #extract current population values
    H_N = as.numeric(tail(pop,1)[1])
    H_S = as.numeric(tail(pop,1)[2])
    L_N = as.numeric(tail(pop,1)[3])
    L_S = as.numeric(tail(pop,1)[4])
    #find proportion of type in each matching pool (signalers with signalers, non-signalers with non-signalers)
    S_H = if(H_S+L_S>0){
      H_S/(H_S+L_S)} else {0}
    S_L = 1-S_H
    N_H = if(H_N+L_N>0){
      H_N/(H_N+L_N)} else {0}
    N_L = 1-N_H
    #calculate next generation pop levels (current pop * expected payoff for each group)
    H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
    H_S_P = max(H_S*(S_H*as.numeric(payoffs[5])+S_L*as.numeric(payoffs[6])),0)
    L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
    L_S_P = max(L_S*(S_H*as.numeric(payoffs[7])+S_L*as.numeric(payoffs[8])),0)
    
    #identify growth scenario
    if (pop_grow == "Fixed population"){
      #rebalance to keep total pop steady
      Total_N_P = H_N_P+L_N_P
      Total_S_P = H_S_P+L_S_P
      expected_payoffs1 = data.frame(High_No_Signal = H_N_P/(H_N*Total_N_P), High_Signal = H_S_P/(H_S*Total_S_P), Low_No_Signal = L_N_P/(L_N*Total_N_P), Low_Signal = L_S_P/(L_S*Total_S_P))
      H_N = H_N_P/Total_N_P
      H_S = H_S_P/Total_S_P
      L_N = L_N_P/Total_N_P
      L_S = L_S_P/Total_S_P
    }
    if (pop_grow == "Unbounded exponential growth") {
      #set next gen pop levels to current leves and record expected payoffs
      expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S)
      H_N = H_N_P
      H_S = H_S_P
      L_N = L_N_P
      L_S = L_S_P
    }
    
    #record end of generation information
    pop1 = data.frame(H_N,H_S,L_N,L_S)
    pop = rbind(pop, pop1)
    group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[3]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[4]]*S_L)
    group_payoffs = rbind(group_payoffs, group_payoffs1)
    expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
  }
  #clean up the data
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
  growth$Type = factor(growth$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
  
  pop = pop %>%
    rename(High_No_Signal = H_N,
           High_Signal = H_S,
           Low_No_Signal = L_N,
           Low_Signal = L_S) %>%
    mutate(t = 0:time,
           Signal = High_Signal + Low_Signal,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Population")
  pop$Type = factor(pop$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
  
  expected_payoffs = cbind(expected_payoffs,group_payoffs)
  expected_payoffs = expected_payoffs %>%
    mutate(t = 1:time) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth_Rate")
  expected_payoffs$Type = factor(expected_payoffs$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
  list(pop, expected_payoffs, growth)
}

#make graphs for population evolution separately

pop_evo_gene = evo_apart_genetic(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[1]]
rate_evo_gene = evo_apart_genetic(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[2]]
grow_evo_gene = evo_apart_genetic(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[3]]

#Genetic population

###Genetic
pop_evo_gene_S = pop_evo_gene
pop_evo_gene_S$Type = factor(pop_evo_gene_S$Type, levels = c("Signal", "High_Signal", "Low_Signal"))
pop_evo_gene_S = pop_evo_gene_S[94:186,]

rate_evo_gene_S = rate_evo_gene
rate_evo_gene_S$Type = factor(rate_evo_gene_S$Type, levels = c("Signal", "High_Signal", "Low_Signal"))
rate_evo_gene_S = rate_evo_gene_S[91:180,]

grow_evo_gene_S = grow_evo_gene
grow_evo_gene_S$Type = factor(grow_evo_gene_S$Type, levels = c("Signal", "High_Signal", "Low_Signal"))
grow_evo_gene_S = grow_evo_gene_S[91:180,]


###Pop_Signal
ggplot(data = pop_evo_gene_S, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                     values = c("Signal" = rgb(0,.75,0), "High_Signal" = rgb(0,.95,0), "Low_Signal" = rgb(0,.5,0)))+
  coord_cartesian(xlim =c(0, time))

###Rate_Signal
ggplot(data = rate_evo_gene_S, aes(x = t, y = Growth_Rate, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                     values = c("Signal" = rgb(0,.75,0), "High_Signal" = rgb(0,.95,0), "Low_Signal" = rgb(0,.5,0)))+
  labs(y = "Reproductive Rate")+
  coord_cartesian(xlim =c(0, time))

###Growth_Signal
ggplot(data = grow_evo_gene_S, aes(x = t, y = Growth, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                     values = c("Signal" = rgb(0,.75,0), "High_Signal" = rgb(0,.95,0), "Low_Signal" = rgb(0,.5,0)))+
  ylab("Growth")+
  coord_cartesian(xlim =c(0, time))


#Compare Signal(gene) vs No Signal

#Notes: The Signal(gene) population benefits from high v(H,H), v(L,L), and a Low V(H,L),and V(L,H) and most importantly, a low initial proportion of high types.  

###Pop_Compare
ggplot(data = pop_evo_gene, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.75,0), "High_Signal" = rgb(0,.95,0), "Low_Signal" = rgb(0,.5,0), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  coord_cartesian(xlim =c(0, time))

###Rate_Compare
ggplot(data = rate_evo_gene, aes(x = t, y = Growth_Rate, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.75,0), "High_Signal" = rgb(0,.95,0), "Low_Signal" = rgb(0,.5,0), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Reproductive Rate")+
  coord_cartesian(xlim =c(0, time))

###Growth_Compare
ggplot(data = grow_evo_gene, aes(x = t, y = Growth, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.75,0), "High_Signal" = rgb(0,.95,0), "Low_Signal" = rgb(0,.5,0), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Growth")+
  coord_cartesian(xlim =c(0, time))






###### Competition ######                     



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
      H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
      H_S_P = max(SHH*H_S,0)
      L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
      L_S_P = max(vLL*L_S,0)
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
        H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
        H_S_P = max(H_S*SHH,0)
        L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
        L_S_P = max(L_S*vLL,0)
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
        H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
        H_S_P = max(H_S*SHH,0)
        L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
        L_S_P = max(L_S*vLL,0)
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
        H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
        H_S_P = max(H_S*SHH,0)
        L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
        L_S_P = max(L_S*vLL,0)
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
        H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
        H_S_P = max(H_S*SHH,0)
        L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
        L_S_P = max(L_S*vLL,0)
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
  growth$Type = factor(growth$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
  
  pop = pop %>%
    rename(High_No_Signal = H_N,
           High_Signal = H_S,
           Low_No_Signal = L_N,
           Low_Signal = L_S) %>%
    mutate(t = 0:time,
           Signal = High_Signal + Low_Signal,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Population")
  pop$Type = factor(pop$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
  
  prop = pop %>%
    rename(High_No_Signal = H_N,
           High_Signal = H_S,
           Low_No_Signal = L_N,
           Low_Signal = L_S) %>%
    mutate(t = 0:time,
           Signal = High_Signal + Low_Signal,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    mutate(High_No_Signal = High_No_Signal/No_Signal,
           High_Signal = High_Signal/Signal,
           Low_No_Signal = Low_No_Signal/No_Signal,
           Low_Signal = Low_Signal/Signal) %>%
    mutate(t = 0:time,
           Signal = High_Signal + Low_Signal,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Proportion_of_Types")
  prop$Type = factor(prop$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
  
  expected_payoffs = cbind(expected_payoffs,group_payoffs)
  expected_payoffs = expected_payoffs %>%
    mutate(t = 1:time) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", key = Type, value = "Growth_Rate")
  expected_payoffs$Type = factor(expected_payoffs$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal"))
  list(pop, expected_payoffs, growth, prop)
}

# Regions Comparative Statics Function

regions = function(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow, beta, start){
  #create a few graphs showing which group will survive the competition under different parameters
  Klow = round(max(vLH-vLL,0),2)
  Khigh = round(vHH-vHL,2)
  Krange = seq(from = Klow, to = Khigh, by = .01)
  startrange = seq(from = 1, to = time, by = 1)
  betarange = seq(from = .01, to = 1, by = .01)
  SKregion = tibble(start = rep(startrange, length(Krange)), K = rep(Krange, each = length(startrange)), result = "")
  BKregion = tibble(beta = rep(betarange, length(Krange)), K = rep(Krange, each = length(betarange)), result = "")
  SBregion = tibble(start = rep(startrange, length(betarange)), beta = rep(betarange, each = length(startrange)), result = "")
  for(Kvar in Krange){
    #KTregion and BKregion first
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
        H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
        H_S_P = max(SHH*H_S,0)
        L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
        L_S_P = max(vLL*L_S,0)
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(H_S*SHH,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(L_S*vLL,0)
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(H_S*SHH,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(L_S*vLL,0)
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(H_S*SHH,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(L_S*vLL,0)
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(H_S*SHH,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(L_S*vLL,0)
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
  ##BTregion
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
      H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
      H_S_P = max(SHH*H_S,0)
      L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
      L_S_P = max(vLL*L_S,0)
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(H_S*SHH,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(L_S*vLL,0)
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(H_S*SHH,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(L_S*vLL,0)
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



# Initial Conditions Comparative Statics

initial_condition_regions = function(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow, beta, start){
  #create a few graphs showing which group will survive the competition under different paramaters focusing on initial ratio
  #Identify range of variables
  Klow = round(max(vLH-vLL,0),2)
  Khigh = round(vHH-vHL,2)
  Krange = seq(from = Klow, to = Khigh, by = .01)
  startrange = seq(from = 1, to = time, by = 1)
  betarange = seq(from = .01, to = 1, by = .01)
  ratiorange = seq(from = .01, to = 1, by = .01)
  #Create empty tibble to store the results in
  RKregion = tibble(ratio = rep(ratiorange, length(Krange)), K = rep(Krange, each = length(ratiorange)), result = "")
  RTregion = tibble(ratio = rep(ratiorange, length(startrange)), start = rep(startrange, each = length(ratiorange)), result = "")
  RBregion = tibble(ratio = rep(ratiorange, length(betarange)), beta = rep(betarange, each = length(ratiorange)), result = "")
  for(Rvar in ratiorange){
    #Set up initial populations and payoffs
    i = match(Rvar,ratiorange)
    H_N = Rvar
    H_S = Rvar
    L_N = 1-Rvar
    L_S = 1-Rvar
    pop = data.frame(H_N,H_S,L_N,L_S)
    SHH = max(vHH - K, 0)
    SHL = max(vHL - K, 0)
    SLH = max(vLH - K, 0)
    SLL = max(vLL - K, 0)
    payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
    if (time != 1){
      for(t in 1:(time-1)){
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
        H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
        H_S_P = max(SHH*H_S,0)
        L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
        L_S_P = max(vLL*L_S,0)
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
      #RTregion
      j = match(startvar,startrange)
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
              RTregion$result[(j-1)*length(ratiorange)+i] = "S"
              stop = 1
            } else {
              RTregion$result[(j-1)*length(ratiorange)+i] = "N"
              stop = 1
            }
          }
          if((H_N+L_N)<=S_D){
            RTregion$result[(j-1)*length(ratiorange)+i] = "S"
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(H_S*SHH,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(L_S*vLL,0)
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
          #Deaths for each group
          S_D = (H_S+L_S)*beta
          N_D = (H_N+L_N)*beta
          #Check if either group eliminated, if so exit the loop
          if((H_S+L_S)<=N_D){
            if((H_N+L_N)<=S_D & S_D>N_D){
              #tiebreak goes to N
              RTregion$result[(j-1)*length(ratiorange)+i] = "S"
              stop = 1
            } else {
              RTregion$result[(j-1)*length(ratiorange)+i] = "N"
              stop = 1
            }
          }
          if((H_N+L_N)<=S_D){
            RTregion$result[(j-1)*length(ratiorange)+i] = "S"
            stop = 1
          }
          
          #Neither group eliminated, continue to iterate and let populations evolve while fighting
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(H_S*SHH,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(L_S*vLL,0)
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
      #RBregion
      j = match(betavar, betarange)
      popstart = pop[start,]
      stop = 0
      while(stop == 0){
        if (pop_grow == "Fixed population"){
          H_N = as.numeric(tail(popstart,1)[1])
          H_S = as.numeric(tail(popstart,1)[2])
          L_N = as.numeric(tail(popstart,1)[3])
          L_S = as.numeric(tail(popstart,1)[4])
          #Deaths for each group
          S_D = (H_S+L_S)*betavar
          N_D = (H_N+L_N)*betavar
          #Check if either group eliminated, if so exit the loop
          if((H_S+L_S)<=N_D){
            if((H_N+L_N)<=S_D & S_D>N_D){
              #tiebreak goes to N
              RBregion$result[(j-1)*length(ratiorange)+i] = "S"
              stop = 1
            } else {
              RBregion$result[(j-1)*length(ratiorange)+i] = "N"
              stop = 1
            }
          }
          if((H_N+L_N)<=S_D){
            RBregion$result[(j-1)*length(ratiorange)+i] = "S"
            stop = 1
          }
          
          #Neither group eliminated, continue to iterate and let populations evolve while fighting
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(H_S*SHH,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(L_S*vLL,0)
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
          #Deaths for each group
          S_D = (H_S+L_S)*betavar
          N_D = (H_N+L_N)*betavar
          #Check if either group eliminated, if so exit the loop
          if((H_S+L_S)<=N_D){
            if((H_N+L_N)<=S_D & S_D>N_D){
              #tiebreak goes to N
              RBregion$result[(j-1)*length(ratiorange)+i] = "S"
              stop = 1
            } else {
              RBregion$result[(j-1)*length(ratiorange)+i] = "N"
              stop = 1
            }
          }
          if((H_N+L_N)<=S_D){
            RBregion$result[(j-1)*length(ratiorange)+i] = "S"
            stop = 1
          }
          #Neither group eliminated, continue to iterate and let populations evolve while fighting
          
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(H_S*SHH,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(L_S*vLL,0)
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
  ##RKregion
  for(Rvar in ratiorange){
    #Set up initial populations and payoffs
    i = match(Rvar,ratiorange)
    for(Kvar in Krange){
      j = match(Kvar, Krange)
      H_N = Rvar
      H_S = Rvar
      L_N = 1-Rvar
      L_S = 1-Rvar
      pop = data.frame(H_N,H_S,L_N,L_S)
      SHH = max(vHH - Kvar, 0)
      SHL = max(vHL - Kvar, 0)
      SLH = max(vLH - Kvar, 0)
      SLL = max(vLL - Kvar, 0)
      payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
      #isolated evolution
      if (start != 1){
        for(t in 1:(start-1)){
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
          H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
          H_S_P = max(SHH*H_S,0)
          L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
          L_S_P = max(vLL*L_S,0)
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
      #Competition
      popstart = pop
      stop = 0
      while(stop == 0){
        if (pop_grow == "Fixed population"){
          H_N = as.numeric(tail(popstart,1)[1])
          H_S = as.numeric(tail(popstart,1)[2])
          L_N = as.numeric(tail(popstart,1)[3])
          L_S = as.numeric(tail(popstart,1)[4])
          #Deaths for each group
          S_D = (H_S+L_S)*beta
          N_D = (H_N+L_N)*beta
          #Check if either group eliminated, if so exit the loop
          if((H_S+L_S)<=N_D){
            if((H_N+L_N)<=S_D & S_D>N_D){
              #tiebreak goes to N
              RKregion$result[(j-1)*length(ratiorange)+i] = "S"
              stop = 1
            } else {
              RKregion$result[(j-1)*length(ratiorange)+i] = "N"
              stop = 1
            }
          }
          if((H_N+L_N)<=S_D){
            RKregion$result[(j-1)*length(ratiorange)+i] = "S"
            stop = 1
          }
          #Neither group eliminated, continue to iterate and let populations evolve while fighting
          
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
          #Deaths for each group
          S_D = (H_S+L_S)*beta
          N_D = (H_N+L_N)*beta
          #Check if either group eliminated, if so exit the loop
          if((H_S+L_S)<=N_D){
            if((H_N+L_N)<=S_D & S_D>N_D){
              #tiebreak goes to N
              RKregion$result[(j-1)*length(ratiorange)+i] = "S"
              stop = 1
            } else {
              RKregion$result[(j-1)*length(ratiorange)+i] = "N"
              stop = 1
            }
          }
          if((H_N+L_N)<=S_D){
            RKregion$result[(j-1)*length(ratiorange)+i] = "S"
            stop = 1
          }
          #Neither group eliminated, continue to iterate and let populations evolve while fighting
          
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
  list(RTregion, RKregion, RBregion)
}


# What Pop wins

#Function applied in regions_fighting
fight = function(pop, beta, vHH, vHL, vLH, vLL){
  H_N = pop[1]
  H_S = pop[2]
  L_N = pop[3]
  L_S = pop[4]
  stop = 0
  while(stop == 0) {
    S = H_S + L_S
    N = H_N + L_N
    #Fight where each kills beta of other group
    S_D = S*beta
    N_D = N*beta
    if(S<=N_D){
      if(N<=S_D & S_D>N_D){
        #tiebreak goes to N
        result = "S"
        stop = 1
      } else {
        result = "N"
        stop = 1
      }
    }
    if(N<=S_D){
      result = "S"
      stop = 1
    }
    
    #Proportion of types in each population
    S_H = if(S>0){
      H_S/S} else {0}
    S_L = 1-S_H
    N_H = if(N>0){
      H_N/N} else {0}
    N_L = 1-N_H
    
    #Populations after fight
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
    H_N_P = H_N*(N_H*vHH+N_L*vHL)
    H_S_P = H_S*(vHH - K)
    L_N_P = L_N*(N_H*vLH+N_L*vLL)
    L_S_P = L_S*vLL
    H_N = H_N_P
    H_S = H_S_P
    L_N = L_N_P
    L_S = L_S_P
  }
  return(result)
}

regions_fighting = function(vHH, vHL, vLH, vLL, K, pop_grow, beta, propSteps, SNSteps){
  
  #Create a sequence that will turn into ratios
  SNseq = seq(from = 1, to = beta*2.5, length.out = SNSteps+1)
  SNrange = c(rev(SNseq),1/SNseq)[-(SNSteps+1)]
  
  #create a few graphs showing which group will survive the competition under different parameters
  SHrange = seq(from = .01, to = 1, length.out = propSteps)
  NHrange = seq(from = .01, to = 1, length.out = propSteps)
  
  #Payoffs
  SHH = max(vHH - K, 0)
  SHL = max(vHL - K, 0)
  SLH = max(vLH - K, 0)
  SLL = max(vLL - K, 0)
  payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
  
  #Define initial population levels
  popInitial = data.frame()
  for(SN in SNrange){
    #Cross Sections of the SH&NH plane for each SN
    for(SH in SHrange){
      for(NH in NHrange){
        H_N = NH
        H_S = SH*SN
        L_N = 1-NH
        L_S = (1-SH)*SN
        pop = data.frame(H_N, H_S, L_N, L_S, SH, NH, SN)
        popInitial = bind_rows(popInitial,pop)
      }
    }
  }
  
  #apply fight function to each row of data frame
  result = apply(popInitial, 1, fight, beta = beta, vHH = vHH, vHL = vHL, vLH = vLH, vLL = vLL)
  results = cbind(popInitial, result)
  return(results)
}


data1 = regions_fighting(vHH, vHL, vLH, vLL, K, pop_grow, beta, 40, 100)
data1 = data1%>%
  group_by(SH, NH) %>%
  arrange(.by_group = TRUE) %>%
  mutate(switch = result != lag(result, default = "N"))
boundary = data1$SN[data1$switch==TRUE]
FightData = summarise(data1)
FightData = FightData %>%
  ungroup() %>%
  mutate(SNboundary = boundary)

ggplot(FightData) +
  geom_tile(aes(x = SH, y = NH, fill = SNboundary)) +
  scale_fill_viridis_c(option = "inferno")


#NEXT: Find flipping point for each SH and NH -> make surface plot, multiple surfaces for changing vars


#Fight

###Population Graphs

###Fight_NS_Win
start = 25
fight=evo_join_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow, join_scenario, beta, start)[[1]]
ggplot(data = fight, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  geom_vline(xintercept = ceiling(start-1))+
  ylab("Population")+
  coord_cartesian(xlim =c(0, time))+
  annotate("text", x = time-3, y = 1.2, size = 8, label = paste("T=",start))

###Fight_S_Win
start = 15
fight=evo_join_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow, join_scenario, beta, start)[[1]]
ggplot(data = fight, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  geom_vline(xintercept = ceiling(start-1))+
  ylab("Population")+
  coord_cartesian(xlim =c(0, time))+
  annotate("text", x = time-3, y = .95, size = 8, label = paste("T=",start))

###Special_Fight
start = 20
fight=evo_join_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow, join_scenario, beta, start)[[1]]
ggplot(data = fight, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  geom_vline(xintercept = ceiling(start-1))+
  ylab("Population")+
  coord_cartesian(xlim =c(0, time))+
  annotate("text", x = time-3, y = .95, size = 8, label = paste("T=",start))

###Regions

region = regions(ratio,vHH,vHL,vLH,vLL,K,time,pop_grow,beta,start)

ratio_regions = initial_condition_regions(ratio,vHH,vHL,vLH,vLL,K,time,pop_grow,beta,start) 

###Region_KT
ggplot(data = region[[1]], aes(x = start, y = K, fill = result)) +
  geom_tile() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(labels = c("N" = "No Signal", "S" = "Signal"),
                    values = c("N" = rgb(1,.5,0), "S" = rgb(0,.5,1))) +
  geom_vline(xintercept = start, color = "white", size = 1) +
  geom_hline(yintercept = K, color = "white", size = 1) +
  xlab("T") +
  labs(fill = "Result")

###Region_BT
ggplot(data = region[[3]], aes(x = start, y = beta, fill = result)) +
  geom_tile() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(labels = c("N" = "No Signal", "S" = "Signal"),
                    values = c("N" = rgb(1,.5,0), "S" = rgb(0,.5,1))) +
  geom_vline(xintercept = start, color = "white", size = 1) +
  geom_hline(yintercept = beta, color = "white", size=1) +
  ylab(expression(beta)) +
  xlab("T") +
  labs(fill = "Result")

###Region_KB
ggplot(data = region[[2]], aes(x = beta, y = K, fill = result)) +
  geom_tile() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(labels = c("N" = "No Signal", "S" = "Signal"),
                    values = c("N" = rgb(1,.5,0), "S" = rgb(0,.5,1))) +
  geom_vline(xintercept = beta, color = "white", size = 1) +
  geom_hline(yintercept = K, color = "white", size = 1) +
  xlab(expression(beta)) +
  labs(fill = "Result")

###Region_RT
ggplot(data = ratio_regions[[1]], aes(x = ratio, y = start, fill = result)) +
  geom_tile() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(labels = c("N" = "No Signal", "S" = "Signal"),
                    values = c("N" = rgb(1,.5,0), "S" = rgb(0,.5,1))) +
  geom_vline(xintercept = ratio, color = "white", size = 1) +
  geom_hline(yintercept = start, color = "white", size = 1) +
  xlab("Initial % High Type") +
  labs(fill = "Result") +
  ylab("T")

###Region_RK
ggplot(data = ratio_regions[[2]], aes(x = ratio, y = K, fill = result)) +
  geom_tile() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(labels = c("N" = "No Signal", "S" = "Signal"),
                    values = c("N" = rgb(1,.5,0), "S" = rgb(0,.5,1))) +
  geom_vline(xintercept = ratio, color = "white", size = 1) +
  geom_hline(yintercept = K, color = "white", size = 1) +
  xlab("Initial % High Type") +
  labs(fill = "Result")


###Region_RB
ggplot(data = ratio_regions[[3]], aes(x = ratio, y = beta, fill = result)) +
  geom_tile() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(labels = c("N" = "No Signal", "S" = "Signal"),
                    values = c("N" = rgb(1,.5,0), "S" = rgb(0,.5,1))) +
  geom_vline(xintercept = ratio, color = "white", size = 1) +
  geom_hline(yintercept = beta, color = "white", size = 1) +
  xlab("Initial % High Type") +
  labs(fill = "Result") +
  ylab(expression(beta))


##### One Population "What if" #####


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
    H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
    H_S_P = max(H_S*(S_H*as.numeric(payoffs[5])+S_L*as.numeric(payoffs[6])),0)
    L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
    L_S_P = max(L_S*(S_H*as.numeric(payoffs[7])+S_L*as.numeric(payoffs[8])),0)
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

ratio_h = .5
time = 40
one_pop = evo(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, K, time, pop_grow)
ggplot(data = one_pop[[1]], aes(x = t, y = Population, color = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  coord_cartesian(xlim =c(0, time))

## Imperfect signaling 
# p vs K
p = rep(seq(from = .0000001, to = .4999999, length.out = 100), times = 4)
SH = rep(c(.05,.1,.25,.5), each = 100)
SL = 1-SH

Krange = tibble(p) %>%
  mutate(
    SH = SH,
    upper = (vHH - vHL)*(1-(SL*p)/(SH*(1-p)+SL*p)-(SH*p)/(SH*p+SL*(1-p))),
    lower = (vLH - vLL)*(1-(SL*p)/(SH*(1-p)+SL*p)-(SH*p)/(SH*p+SL*(1-p)))
  )

Krange$SH = as.factor(Krange$SH)
levels(Krange$SH) = c("0.05 & 0.95", "0.1 & 0.9", "0.25 & 0.75", "0.5")

ggplot(data = Krange, group = SH) +
  geom_path(aes(x = upper, y = p, color = SH), size = 1) +
  geom_path(aes(x = lower, y = p, color = SH), size = 1) +
  geom_ribbon(aes(xmin = lower, xmax = upper, y = p, fill = SH), alpha = .1, show.legend = FALSE) +
  scale_fill_viridis_d(option = "C", end = .9)+
  scale_color_viridis_d(option = "C", end = .9)+
  labs(x = "K", color = "Proportion of High Types") +
  scale_x_continuous(breaks = c(.05,.25), labels = c("V(L,H) - V(L,L)", "V(H,H) - V(H,L)")) +
  theme(text = element_text(size = 20)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

#p fixed, SH vs K

p = .05
SH = rep(seq(from = 0, to = 1, length.out = 100), times = 100)
SL = 1-SH
K = rep(seq(from = 0, to = vHH-vHL, length.out = 100), each = 100)
Equilibrium = tibble(SH) %>%
  mutate(
    K = K,
    upper = (vHH - vHL)*(1-(SL*p)/(SH*(1-p)+SL*p)-(SH*p)/(SH*p+SL*(1-p))),
    lower = (vLH - vLL)*(1-(SL*p)/(SH*(1-p)+SL*p)-(SH*p)/(SH*p+SL*(1-p)))
  )

ggplot(data = Equilibrium) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = SH), fill = 'blue', alpha = .8) +
  geom_ribbon(aes(ymin = 0, ymax = lower, x = SH),alpha = .6) +
  geom_ribbon(aes(ymin = upper, ymax = .5, x = SH), alpha = .6) +
  geom_path(aes(x = SH, y = upper), color = "darkblue", size = .5) +
  geom_path(aes(x = SH, y = lower), color = "darkblue", size = .5) +
  scale_color_viridis_d(option = "C", end = .9)+
  labs(y = "K", color = "Seperating Equilibrium", x = "Proportion of High Types") +
  scale_y_continuous(breaks = c(.05,.25), labels = c("V(L,H) - V(L,L)", "V(H,H) - V(H,L)")) +
  annotate("text", x = .5, y = .12, label = "Seperating Equilibrium", color = "white", size = 8) +
  annotate("text", x = .5, y = .02, label = "Pooling Equilibrium", color = "white", size = 8) +
  annotate("text", x = .5, y = .25, label = "Pooling Equilibrium", color = "white", size = 8) +
  theme(text = element_text(size = 20)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(0,.28))

ggplot(data = Equilibrium) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = SH), fill = 'blue', alpha = .8) +
  geom_path(aes(x = SH, y = upper), color = "darkblue", size = .5) +
  geom_path(aes(x = SH, y = lower), color = "darkblue", size = .5) +
  scale_color_viridis_d(option = "C", end = .9)+
  labs(y = "K", color = "Seperating Equilibrium", x = "Proportion of High Types") +
  scale_y_continuous(breaks = c(.05*(1-2*p),.25*(1-2*p)), labels = c("(V(L,H) - V(L,L))*(1-2p)", "(V(H,H) - V(H,L))*(1-2p)")) +
  annotate("text", x = .5, y = .12, label = "Seperating Equilibrium", color = "white", size = 8) +
  annotate("text", x = .5, y = .02, label = "Seperating Equilibrium Fails", color = "black", size = 8) +
  annotate("text", x = .5, y = .25, label = "Seperating Equilibrium Fails", color = "black", size = 8) +
  theme(text = element_text(size = 20)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(0,.28))

evo_imperfect = function(ratio, vHH, vHL, vLH, vLL, K, time, p){
  #define initial population makeup 
  H_N = ratio
  H_S = ratio
  H_I = ratio
  L_N = 1-ratio
  L_S = 1-ratio
  L_I = 1-ratio
  pop = data.frame(H_N,H_S,H_I,L_N,L_S,L_I)
  #define game table payoffs 
  SHH = max(vHH - K, 0)
  SHL = max(vHL - K, 0)
  SLH = max(vLH - K, 0)
  SLL = max(vLL - K, 0)
  payoffs = data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
  expected_payoffs = data.frame()
  group_payoffs = data.frame()
  for(i in 1:time){
    #extract current population values
    H_N = as.numeric(tail(pop,1)[1])
    H_S = as.numeric(tail(pop,1)[2])
    H_I = as.numeric(tail(pop,1)[3])
    L_N = as.numeric(tail(pop,1)[4])
    L_S = as.numeric(tail(pop,1)[5])
    L_I = as.numeric(tail(pop,1)[6])
    #find proportion of type in each matching pool (signalers with signalers, non-signalers with non-signalers)
    S_H = if(H_S+L_S>0){
      H_S/(H_S+L_S)} else {0}
    S_L = 1-S_H
    I_H = if(H_I+L_I>0){
      H_I/(H_I+L_I)} else {0}
    I_L = 1-I_H
    N_H = if(H_N+L_N>0){
      H_N/(H_N+L_N)} else {0}
    N_L = 1-N_H
    
    #determine if separating equilibrium
    HighIC = (vHH-vHL)*(1-(I_L*p)/(I_H * (1 - p) + I_L * p) - (I_H * p)/(I_H * p + I_L * (1 - p))) > K
    LowIC = (vLH-vLL)*(1-(I_L*p)/(I_H * (1 - p) + I_L * p) - (I_H * p)/(I_H * p + I_L * (1 - p))) < K
    
    #Calculate next period population levels
    if(HighIC & LowIC){
      #Separating Equilibrium
      #Imperfect Signaling
      Imperfect_H_S = I_H*(1-p)
      Imperfect_H_N = I_H*(p)
      Imperfect_L_S = I_L*(p)
      Imperfect_L_N = I_L*(1-p)
      Imperfect_S_H = Imperfect_H_S/(Imperfect_H_S + Imperfect_L_S)
      Imperfect_S_L = 1-Imperfect_S_H
      Imperfect_N_H = Imperfect_H_N/(Imperfect_H_N + Imperfect_L_N)
      Imperfect_N_L = 1-Imperfect_N_H
      
      #Payoffs
      H_S_P = max(H_S*as.numeric(payoffs[5]),0)
      L_S_P = max(L_S*as.numeric(payoffs[4]),0)
      H_I_P = H_I * ((1-p) * (vHH * Imperfect_S_H + vHL * Imperfect_S_L - K) + (p) * (vHH * Imperfect_N_H + vHL * Imperfect_N_L))
      L_I_P = L_I * ((p) * (vLH * Imperfect_S_H + vLL * Imperfect_S_L - K) + (1-p) * (vLH * Imperfect_N_H + vLL * Imperfect_N_L))
      H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
      L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
    } else {
      #Pooling Equilibrium (no one chooses signal. Signaling population incurs cost p*K)
      H_S_P = max(H_S*as.numeric(payoffs[5]),0)
      L_S_P = max(L_S*as.numeric(payoffs[4]),0)
      H_I_P = max(H_I*(S_H*as.numeric(payoffs[1])+S_L*as.numeric(payoffs[2])-p*K),0)
      L_I_P = max(L_I*(S_H*as.numeric(payoffs[3])+S_L*as.numeric(payoffs[4])-p*K),0)
      H_N_P = max(H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2])),0)
      L_N_P = max(L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4])),0)
    }

    expected_payoffs1 = data.frame(High_No_Signal = H_N_P/H_N, High_Signal = H_S_P/H_S, High_Imperfect = H_I_P/H_I, Low_No_Signal = L_N_P/L_N, Low_Signal = L_S_P/L_S, Low_Imperfect = L_I_P/L_I)
    H_N = H_N_P
    H_S = H_S_P
    H_I = H_I_P
    L_I = L_I_P
    L_N = L_N_P
    L_S = L_S_P
    
    #record end of generation information
    pop1 = data.frame(H_N,H_S,H_I,L_N,L_S,L_I)
    pop = rbind(pop, pop1)
    group_payoffs1 = data.frame(No_Signal = expected_payoffs1[[1]]*N_H + expected_payoffs1[[4]]*N_L, Signal = expected_payoffs1[[2]]*S_H + expected_payoffs1[[5]]*S_L, Imperfect = expected_payoffs1[[3]]*I_H + expected_payoffs1[[6]]*I_L)
    group_payoffs = rbind(group_payoffs, group_payoffs1)
    expected_payoffs = rbind(expected_payoffs, expected_payoffs1)
  }
  #clean up the data
  growth = pop[2:(time+1),]-pop[1:time,]
  growth = growth %>%
    rename(High_No_Signal = H_N,
           High_Signal = H_S,
           High_Imperfect = H_I,
           Low_No_Signal = L_N,
           Low_Signal = L_S,
           Low_Imperfect = L_I) %>%
    mutate(t = 1:time,
           Signal = High_Signal + Low_Signal,
           Imperfect = High_Imperfect + Low_Imperfect,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", "Imperfect", "High_Imperfect", "Low_Imperfect", key = Type, value = "Growth")
  growth$Type = factor(growth$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal", "Imperfect", "High_Imperfect", "Low_Imperfect"))
  
  prop = pop %>%
    rename(High_No_Signal = H_N,
           High_Signal = H_S,
           High_Imperfect = H_I,
           Low_No_Signal = L_N,
           Low_Signal = L_S,
           Low_Imperfect = L_I) %>%
    mutate(t = 0:time,
           Signal = High_Signal + Low_Signal,
           Imperfect = High_Imperfect + Low_Imperfect,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    mutate(High_No_Signal = High_No_Signal/No_Signal,
           High_Signal = High_Signal/Signal,
           High_Imperfect = High_Imperfect/Imperfect,
           Low_No_Signal = Low_No_Signal/No_Signal,
           Low_Signal = Low_Signal/Signal,
           Low_Imperfect = Low_Imperfect/Imperfect) %>%
    mutate(t = 0:time,
           Signal = High_Signal + Low_Signal,
           Imperfect = High_Imperfect + Low_Imperfect,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", "Imperfect", "High_Imperfect", "Low_Imperfect", key = Type, value = "Proportion_of_Types")
  prop$Type = factor(prop$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal", "Imperfect", "High_Imperfect", "Low_Imperfect"))
  
  pop = pop %>%
    rename(High_No_Signal = H_N,
           High_Signal = H_S,
           High_Imperfect = H_I,
           Low_No_Signal = L_N,
           Low_Signal = L_S,
           Low_Imperfect = L_I) %>%
    mutate(t = 0:time,
           Signal = High_Signal + Low_Signal,
           Imperfect = High_Imperfect + Low_Imperfect,
           No_Signal = High_No_Signal + Low_No_Signal) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", "Imperfect", "High_Imperfect", "Low_Imperfect", key = Type, value = "Population")
  pop$Type = factor(pop$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal", "Imperfect", "High_Imperfect", "Low_Imperfect"))
  
  expected_payoffs = cbind(expected_payoffs,group_payoffs)
  expected_payoffs = expected_payoffs %>%
    mutate(t = 1:time) %>%
    gather("No_Signal", "High_No_Signal", "Low_No_Signal", "Signal", "High_Signal", "Low_Signal", "Imperfect", "High_Imperfect", "Low_Imperfect", key = Type, value = "Growth_Rate")
  expected_payoffs$Type = factor(expected_payoffs$Type, levels = c("Signal", "High_Signal", "Low_Signal", "No_Signal", "High_No_Signal", "Low_No_Signal", "Imperfect", "High_Imperfect", "Low_Imperfect"))
  list(pop, expected_payoffs, growth, prop)
}
test = evo_imperfect(.2, vHH, vHL, vLH, vLL, K, 30, .05)


#make graphs for population evolution separately

pop_evo = evo_imperfect(ratio, vHH, vHL, vLH, vLL, K, time, p)[[1]]
rate_evo = evo_imperfect(ratio, vHH, vHL, vLH, vLL, K, time, p)[[2]]
grow_evo = evo_imperfect(ratio, vHH, vHL, vLH, vLL, K, time, p)[[3]]
prop_evo = evo_imperfect(ratio, vHH, vHL, vLH, vLL, K, time, p)[[4]]

#No Signal population

###Imperfect
pop_evo_I = pop_evo
pop_evo_I$Type = factor(pop_evo_I$Type, levels = c("Imperfect", "High_Imperfect", "Low_Imperfect"))
pop_evo_I = pop_evo_I[187:279,]

rate_evo_I = rate_evo
rate_evo_I$Type = factor(rate_evo_I$Type, levels = c("Imperfect", "High_Imperfect", "Low_Imperfect"))
rate_evo_I = rate_evo_I[181:270,]

grow_evo_I = grow_evo
grow_evo_I$Type = factor(grow_evo_I$Type, levels = c("Imperfect", "High_Imperfect", "Low_Imperfect"))
grow_evo_I = grow_evo_I[181:270,]

prop_evo_I = prop_evo
prop_evo_I$Type = factor(prop_evo_I$Type, levels = c("Imperfect", "High_Imperfect", "Low_Imperfect"))
prop_evo_I = prop_evo_I[187:279,]

###Pop_Imperfect
ggplot(data = pop_evo_I, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                        values = c("Imperfect" = "solid", "High_Imperfect" = "dashed", "Low_Imperfect" = "dotted"))+
  scale_color_manual(labels = c("Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                     values = c("Imperfect" = rgb(0,.6,0), "High_Imperfect" = rgb(0,.9,0), "Low_Imperfect" = rgb(0,.3,0)))+
  coord_cartesian(xlim =c(0, time))

###Rate_Imperfect
ggplot(data = rate_evo_I, aes(x = t, y = Growth_Rate, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                        values = c("Imperfect" = "solid", "High_Imperfect" = "dashed", "Low_Imperfect" = "dotted"))+
  scale_color_manual(labels = c("Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                     values = c("Imperfect" = rgb(0,.6,0), "High_Imperfect" = rgb(0,.9,0), "Low_Imperfect" = rgb(0,.3,0)))+
  labs(y = "Reproductive Rate")+
  coord_cartesian(xlim =c(0, time))

###Growth_Imperfect
ggplot(data = grow_evo_I, aes(x = t, y = Growth, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                        values = c("Imperfect" = "solid", "High_Imperfect" = "dashed", "Low_Imperfect" = "dotted"))+
  scale_color_manual(labels = c("Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                     values = c("Imperfect" = rgb(0,.6,0), "High_Imperfect" = rgb(0,.9,0), "Low_Imperfect" = rgb(0,.3,0)))+
  ylab("Growth")+
  coord_cartesian(xlim =c(0, time))

###Prop_Imperfect (with pop line at 1)
ggplot(data = prop_evo_I, aes(x = t, y = Proportion_of_Types, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                        values = c("Imperfect" = "solid", "High_Imperfect" = "dashed", "Low_Imperfect" = "dotted"))+
  scale_color_manual(labels = c("Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                     values = c("Imperfect" = rgb(0,.6,0), "High_Imperfect" = rgb(0,.9,0), "Low_Imperfect" = rgb(0,.3,0)))+
  ylab("Proportion of Types")+
  coord_cartesian(xlim =c(0, time))

###Prop_Imperfect (without pop line at 1)
ggplot(data = prop_evo_I, aes(x = t, y = Proportion_of_Types, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                        values = c("High_Imperfect" = "dashed", "Low_Imperfect" = "dotted"))+
  scale_color_manual(labels = c("High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                     values = c("High_Imperfect" = rgb(0,.9,0), "Low_Imperfect" = rgb(0,.3,0)))+
  ylab("Proportion of Types")+
  coord_cartesian(xlim =c(0, time))

#Compare Signal vs No Signal vs Imperfect

###Pop_Compare
ggplot(data = pop_evo, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low", "Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted", "Imperfect" = "solid", "High_Imperfect" = "dashed", "Low_Imperfect" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low", "Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0), "Imperfect" = rgb(0,.6,0), "High_Imperfect" = rgb(0,.9,0), "Low_Imperfect" = rgb(0,.3,0)))+
  coord_cartesian(xlim =c(0, time))

###Rate_Compare
ggplot(data = rate_evo, aes(x = t, y = Growth_Rate, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low", "Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted", "Imperfect" = "solid", "High_Imperfect" = "dashed", "Low_Imperfect" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low", "Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0), "Imperfect" = rgb(0,.6,0), "High_Imperfect" = rgb(0,.9,0), "Low_Imperfect" = rgb(0,.3,0)))+
  ylab("Reproductive Rate") +
  coord_cartesian(xlim =c(0, time))

###Growth_Compare
ggplot(data = grow_evo, aes(x = t, y = Growth, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low", "Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted", "Imperfect" = "solid", "High_Imperfect" = "dashed", "Low_Imperfect" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low", "Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0), "Imperfect" = rgb(0,.6,0), "High_Imperfect" = rgb(0,.9,0), "Low_Imperfect" = rgb(0,.3,0)))+
  ylab("Growth") +
  coord_cartesian(xlim =c(0, time))

###Prop_Compare (with pop line at 1)
ggplot(data = prop_evo, aes(x = t, y = Proportion_of_Types, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low", "Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted", "Imperfect" = "solid", "High_Imperfect" = "dashed", "Low_Imperfect" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low", "Imperfect" = "Imperfect Signal", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0), "Imperfect" = rgb(0,.6,0), "High_Imperfect" = rgb(0,.9,0), "Low_Imperfect" = rgb(0,.3,0)))+
  ylab("Proportion of Types") +
  coord_cartesian(xlim =c(0, time))

###Prop_Compare (without pop line at 1)
ggplot(data = prop_evo, aes(x = t, y = Proportion_of_Types, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                        values = c("High_Signal" = "dashed", "Low_Signal" = "dotted", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted","High_Imperfect" = "dashed", "Low_Imperfect" = "dotted"))+
  scale_color_manual(labels = c("High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low", "High_Imperfect" = "Imperfect Signal:High", "Low_Imperfect" = "Imperfect Signal:Low"),
                     values = c("High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0), "High_Imperfect" = rgb(0,.9,0), "Low_Imperfect" = rgb(0,.3,0)))+
  ylab("Proportion of Types") +
  coord_cartesian(xlim =c(0, time))


