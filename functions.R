time = 20
vHH = 20
vHL = 15
vLH = 12
vLL = 10
A = 0
ratio = .5
ratio_l = 0
ratio_h = .5

evo = function(pop, payoffs, time){
  for(i in 1:time){
    H_N = as.numeric(tail(pop,1)[1])
    H_S = as.numeric(tail(pop,1)[2])
    L_N = as.numeric(tail(pop,1)[3])
    L_S = as.numeric(tail(pop,1)[4])
    S_H = H_S/(H_S+L_S)
    S_L = 1-S_H
    N_H = H_N/(H_N+L_N)
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
  as_tibble(pop)
}

payoffs = function(vHH, vHL, vLH, vLL, A){
  K = vLH-vLL+A*(vHH+vLL-vLH-vHL)
  SHH = vHH - K
  SHL = vHL - K
  SLH = vLH - K
  SLL = vLL - K
  data.frame(vHH,vHL,vLH,vLL,SHH,SHL,SLH,SLL)
}

pop = function(ratio, ratio_h, ratio_l){
  H_N = (1-ratio_h)*ratio
  H_S = ratio_h*ratio
  L_N = (1-ratio_l)*(1-ratio)
  L_S = ratio_l*(1-ratio)
  data.frame(H_N,H_S,L_N,L_S)
}

pop_evo = function(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, A, time){
  evo(pop(ratio, ratio_h, ratio_l), payoffs(vHH, vHL, vLH, vLL, A), time)
}

ggplot(data = pop_evo(ratio, ratio_h, ratio_l, vHH, vHL, vLH, vLL, A, time), aes(x = t, y = Proportion, color = Type)) +
  geom_line()

