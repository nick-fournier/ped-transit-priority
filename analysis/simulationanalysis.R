


#### 1. Global input parameters ####
lambda_c = 90
lambda_b = 150
R = 15
alpha = 1
v_max = 40
t_s = 10
s = 2
k_c = 60
q_c = 500
delta = 2.71
d = 2/delta


#### 2.  Functions ####
#Flow across network at distance r from center
func.q_a <- function(r) {
  ((14*R*lambda_b) / (15*delta)) + (lambda_c*(R^2 - r^2) / (4*r*delta))
}

#Flow around perimeter road
func.q_p <- function(g) {
  (2*lambda_b*g*(6*R*(1 + sqrt(2)) - 11*g)*(R^2 - g^2)) / ((R-2)*delta*R^2)
}

#Traffic Density
func.kq <- function(q) {
    (k_c - sqrt(q_c - q))/alpha
}

#Optimal ped zone size
func.gamma_int <- function(g) {
  A = 11/R^2
  B = -6*(1+sqrt(2))/R
  C = -11
  D = (48*(1+sqrt(2))*R*lambda_b + lambda_c*(R-2))/(8*lambda_b)
  E = -14*R*(R-2)/30
  H = (-lambda_c*(R-2)*R^2) / (8*lambda_b)
  
  A*g^5 + B*g^4 + C*g^3 + D*g^2 + E*g + H
}

#Optimal transit zone size as function of mode choice P_D
func.tau <- function(P) {
  (2*(q_T*delta - (P*14*lambda_b*R/15)) - sqrt(4*(q_T*delta - (P*14*lambda_b*R/15))^2 + (P*lambda_c*R)^2))/(P*lambda_c)
}

#Average ped walking distance
func.L_P <- function(g) {
  (8*lambda_b*g^5 + 20*lambda_b*g^3(R^2 - R^2)) / 3*R^2 + 2*lambda*g^3 + g*(R^2 - g^2)
}

#Average driving distance
func.L_D <- function(g) {
  (R - g)*(14*lamba_b + 10*lambda_c) / 15*(lambda_b + lambda_c)
}

#Average transit distance in mixed traffic
func.L_TM <- function(tau) {
  (R - tau)*(14*lamba_b + 10*lambda_c) / 15*(lambda_b + lambda_c)
}

#Average transit priority distance
func.L_TP <- function(tau) {
  tau*(14*lambda_b + 10*lambda_c) / 15*(lambda_b + lambda_c)
}

#Travel time difference
func.ttdiff <- function(q) {
  (t_D + t_W) - (t_TP + t_TM)
}

#Driving travel time
func.tt_D <- function(q) {
  ifelse(q < q_c,
         L_D*kq(q)/(q_c - q),
         L_D*kq(q_c*0.95)/(0.05*q_c) * (q/q_c)^{20}
  )
  return(t)
}

#Transit priority travel time
func.tt_TP <- (L_TP/v_max) + (L_TP/s)*t_s


#Transit in mixed traffic travel time
func.tt_TM <- function(q) {
  t_D + (L_TP/s)*t_s
}

#### 3. Simulation ####

#Find the optimal intersection point, remains constant
opt.gamma <- min(uniroot(func.gamma_int, c(0,R/2))$root,
                 uniroot(func.gamma_int, c(R/2,R))$root)

#Find critical transit priority flow, remains constant
q_T <- alpha^2 * ((1/v_max) + (t_s/s))^2 - k_c^2 + q_c



#Initial probability
P <- 0






# 
# qplot(data = data.frame(x = 0), mapping = aes(x = x)) +
#   stat_function(fun = function(x) func.gamma_int(g=x)) +
#   stat_function(fun = function(x) func.q_a(r=x), linetype='dashed') +
#   stat_function(fun = function(x) func.q_p(g=x), linetype='dashed') +
#   geom_vline(xintercept = opt.gamma) +
#   geom_hline(yintercept = 0) +
#   scale_y_continuous(labels = scales::comma, limits = c(-100,7000)) +
#   scale_x_continuous(limits = c(0,15)) +
#   theme_bw()

