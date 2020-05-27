library("tidyverse", "shiny", "stringr")

time = 100
vHH = 100
vHL = 80
vLH = 79
vLL = 79
K = 5
ratio = .2
ratio_l = 0
ratio_h = .2

evo = function(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, K, time){
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

pop_evo = evo(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, K, time)

ggplot(data = pop_evo, aes(x = t, y = Proportion, color = Type)) +
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

