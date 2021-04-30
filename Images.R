###### Set Up #######



library("tidyverse", "shiny", "stringr")
##1100*500 image size

time = 30
vHH =1.15
vHL = .9
vLH = .85
vLL = .8
K = .1
ratio = .05
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
    H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
    H_S_P = H_S*as.numeric(payoffs[5])
    L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
    L_S_P = L_S*as.numeric(payoffs[4])
    
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

#make graphs for population evolution seperately

pop_evo = evo_apart_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[1]]
rate_evo = evo_apart_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[2]]
grow_evo = evo_apart_high(ratio, vHH, vHL, vLH, vLL, K, time, pop_grow)[[3]]

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
    H_N_P = H_N*(N_H*as.numeric(payoffs[1])+N_L*as.numeric(payoffs[2]))
    H_S_P = H_S*(S_H*as.numeric(payoffs[5])+S_L*as.numeric(payoffs[6]))
    L_N_P = L_N*(N_H*as.numeric(payoffs[3])+N_L*as.numeric(payoffs[4]))
    L_S_P = L_S*(S_H*as.numeric(payoffs[7])+S_L*as.numeric(payoffs[8]))
    
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

#make graphs for population evolution seperately

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
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1)))+
  coord_cartesian(xlim =c(0, time))

###Rate_Signal
ggplot(data = rate_evo_gene_S, aes(x = t, y = Growth_Rate, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1)))+
  labs(y = "Reproductive Rate")+
  coord_cartesian(xlim =c(0, time))

###Growth_Signal
ggplot(data = grow_evo_gene_S, aes(x = t, y = Growth, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1)))+
  ylab("Growth")+
  coord_cartesian(xlim =c(0, time))

#Compare Signal (choice) vs No Signal

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
  ylab("Reproductive Rate")+
  coord_cartesian(xlim =c(0, time))

###Growth_Compare
ggplot(data = grow_evo, aes(x = t, y = Growth, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Growth")+
  coord_cartesian(xlim =c(0, time))


#Compare Signal(gene) vs No Signal

###Pop_Compare
ggplot(data = pop_evo_gene, aes(x = t, y = Population, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  coord_cartesian(xlim =c(0, time))

###Rate_Compare
ggplot(data = rate_evo_gene, aes(x = t, y = Growth_Rate, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
  ylab("Reproductive Rate")+
  coord_cartesian(xlim =c(0, time))

###Growth_Compare
ggplot(data = grow_evo_gene, aes(x = t, y = Growth, color = Type, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_linetype_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                        values = c("Signal" = "solid", "High_Signal" = "dashed", "Low_Signal" = "dotted", "No_Signal" = "solid", "High_No_Signal" = "dashed", "Low_No_Signal" = "dotted"))+
  scale_color_manual(labels = c("Signal" = "Signal", "High_Signal" = "Signal:High", "Low_Signal" = "Signal:Low", "No_Signal" = "No Signal", "High_No_Signal" = "No Signal:High", "Low_No_Signal" = "No Signal:Low"),
                     values = c("Signal" = rgb(0,.5,1), "High_Signal" = rgb(0,.75,1), "Low_Signal" = rgb(0,0,1), "No_Signal" = rgb(1,.5,0), "High_No_Signal" = rgb(1,.8,0), "Low_No_Signal" = rgb(1,0,0)))+
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


  

