#### Packages ####
library(ggplot2)

#### 1. Global input parameters ####
lambda_c = 90/2
lambda_b = 150/2
R = 15
v_w = 5
v_b = 20
v_m = 40
t_s = 60/3600
s = 0.5
k_c = 45
q_c = 500
beta = 1
delta = 2.71
d = 2/delta


#### 2.  Functions ####
#Flow across network at distance r from center
fun.qr_a <- function(r) ((14*R*lambda_b) / (15*delta)) + (lambda_c/(4*r*delta))*(R^2 - r^2)

#Flow around perimeter road
fun.qg_p <- function(g) (2*lambda_b*g*(6*R*(1 + sqrt(2)) - 11*g)*(R^2 - g^2)) / ((R-2)*delta*R^2)

#Traffic flow, parabolic
fun.qk <- function(k) (q_c*(k*(2*k_c - k)))/(k_c^2)

#Traffic density, parabolic
fun.kq <- function(q) k_c*(1-sqrt(1-(q/q_c)))

#Average flow experienced
fun.qbar <- function(r) ((14*R*lambda_b)/(15*delta)) + (lambda_c/(8*delta*(R-r))) * (2*(R^2)*log(R/r) - R^2 + r^2)

#Optimal ped zone size
fun.gamma_int <- function(g) {
  A = 11/R^2
  B = -6*(1+sqrt(2))/R
  C = -11
  D = (48*(1+sqrt(2))*R*lambda_b + lambda_c*(R-2))/(8*lambda_b)
  E = -14*R*(R-2)/30
  H = (-lambda_c*(R-2)*R^2) / (8*lambda_b)
  
  A*g^5 + B*g^4 + C*g^3 + D*g^2 + E*g + H
}

#Optimal transit zone size as function of mode choice P_D
fun.tau <- function(P) {
  if(P > 0)
    (56*P*lambda_b - 60*delta*q_T + sqrt( (56*P*lambda_b - 60*delta*q_T)^2 + 900*(R^2)*(P*lambda_c)^2))/(30*P*lambda_c)
  else
    0
}

#### DISTANCES 
#Average ped walking distance
fun.L_P <- function(g) (8*lambda_b*g^5 + 20*lambda_b*g^3(R^2 - R^2)) / 3*R^2 + 2*lambda*g^3 + g*(R^2 - g^2)

#Average driving distance
fun.L_D <- function(g) (R - g)*(14*lamba_b + 10*lambda_c) / 15*(lambda_b + lambda_c)

#Average transit distance in mixed traffic
fun.L_TM <- function(tau) (R - tau)*(14*lamba_b + 10*lambda_c) / 15*(lambda_b + lambda_c)

#Average transit priority distance
fun.L_TP <- function(tau) tau*(14*lambda_b + 10*lambda_c) / 15*(lambda_b + lambda_c)

#Average walk distance
fun.L_W <- function(g) 
  ((R^2)*(g^3)*(10*lambda_b + 3*lambda_c - 3) - 6*lambda_b*g^5 + 3*g*R^4) / 
  (3*(R^2)*(g^2)*(2*lambda_b + lambda_c - 1) - 3*lambda_b*g^4 + 3*R^4)


#### TRAVEL TIMES
#Average driving travel time
fun.tt_Dbar <- function(g) {
  lbar <- (R-g)*(14*lambda_b+10*lambda_c)/(15*(lambda_b + lambda_c))
  
  if(fun.qbar(g) < q_c)
    lbar*fun.kq(fun.qbar(g))/fun.qbar(g)
  else
    lbar*(k_c/q_c)*(fun.qbar(g)/q_c)^20
}

#Average mixed-traffic transit travel time
fun.tt_TMbar<- function(tau) {
  lbar <- (R-tau)*(14*lambda_b+10*lambda_c)/(15*(lambda_b + lambda_c))
  
  if(fun.qbar(tau) < q_c)
    lbar*fun.kq(fun.qbar(tau))/fun.qbar(tau)
  else
    lbar*(k_c/q_c)*(fun.qbar(tau)/q_c)^20
  
}

#Average walking travel time
fun.tt_Wbar <- function(g) {
  lbar <-   ((R^2)*(g^3)*(10*lambda_b + 3*lambda_c - 3) - 6*lambda_b*g^5 + 3*g*R^4) / 
    (3*(R^2)*(g^2)*(2*lambda_b + lambda_c - 1) - 3*lambda_b*g^4 + 3*R^4)
  lbar/v_w
}

#Average biking travel time
fun.tt_Bbar <- function(g) {
  lbar <- ((8*lambda_b*g^5 + 20*lambda_b*(g^3)*(R^2 - g^2))/(3*R^2)) + 2*lambda_c*g^3 + g*(R^2 - g^2)
  lbar/v_b
}

#Transit priority travel time
fun.tt_TPbar <- function(tau) {
  lbar <- ((tau*(14*lambda_b + 10*lambda_c))/(15*(lambda_b + lambda_c)))
  (lbar/v_m) + (lbar/s)*t_s
}

#Average total drive time plus walking
fun.tt_drive <- function(g) fun.tt_Dbar(g) + fun.tt_Wbar(g)

#Average total transit time with priority
fun.tt_transit <- function(tau) fun.tt_TPbar(tau) + fun.tt_TMbar(tau)


#Total average travel time 
fun.tt_total <- function(g,tau) fun.tt_Dbar(g) + fun.tt_Wbar(g) + fun.tt_TPbar(tau) + fun.tt_TMbar(tau)


#Travel time difference [ transit - driving]
fun.ttdiff <- function(g,tau) (fun.tt_Dbar(g) + fun.tt_Wbar(g)) - (fun.tt_TPbar(tau) + fun.tt_TMbar(tau))

#Logit for driving
fun.PD <- function(tdiff) 1/(1 + exp(beta*tdiff))


#### 3. Simulation ####

#Find the optimal intersection point, remains constant
g.opt <- min(uniroot(fun.gamma_int, c(0,R/2))$root,
                 uniroot(fun.gamma_int, c(R/2,R))$root)

#Find critical transit priority flow, remains constant
q_T <- {
  TT <- (1/v_m)+(t_s/s)
  tc_ratio <- (q_c/k_c)*((1/v_m) + (t_s/s))
  if(tc_ratio < 1) 
    (2*k_c/TT) - ((k_c^2)/(q_c*TT^2))
  else
    q_c*(TT*q_c/k_c)^(1/20)
}

####
# Initial probability --> Find optimal tau --> find new travel times --> update probability --> repeat
####

# Initial probability
P <- 1

# Find tau
tau.opt <- fun.tau(P)

#Find difference in travel times [ driving - transit ]
TT.diff <- fun.ttdiff(g.opt, tau.opt)
fun.tt_drive(g.opt)
fun.tt_transit(tau.opt)


#Get new probability 
P_D <- fun.PD(TT.diff)




fun.tt_drive(g.opt)


Rseq <- seq(1,14,length.out = 100)


rbind(data.frame(r = Rseq, mode='Transit', TT = sapply(Rseq, function(x) fun.tt_transit(x))),
      data.frame(r = Rseq, mode='Drive', TT = sapply(dat.ttvsprob$P, function(x) fun.tt_drive(x))))


data.frame(r = Rseq, TT = sapply(Rseq, function(x) fun.tt_total(g.opt,x))) 

ggplot(data.frame(r = Rseq, TT = sapply(Rseq, function(x) fun.tt_total(g.opt,x))),
       aes(x=r, y=TT)) +
  geom_point() +
  #scale_x_continuous(limits = c(0,1)) +
  #scale_x_continuous(limits = c(-15,15)) +
  theme_bw()

# 
# ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
#   stat_function(fun = function(x) fun.gamma_int(g=x)) +
#   stat_function(fun = function(x) fun.q_a(r=x), linetype='dashed') +
#   stat_function(fun = function(x) fun.q_p(g=x), linetype='dashed') +
#   geom_vline(xintercept = opt.gamma) +
#   geom_hline(yintercept = 0) +
#   scale_y_continuous(labels = scales::comma, limits = c(-100,7000)) +
#   scale_x_continuous(limits = c(0,15)) +
#   theme_bw()

