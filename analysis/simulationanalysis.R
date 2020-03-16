#### Packages ####
library(ggplot2)
library(data.table)

#### 1. Global input parameters ####
lambda_c = 90/2
lambda_b = 150/2
R = 15
v_w = 5
v_b = 20
v_m = 40
t_s = 60/3600
h = 15/60
s = 0.5
k_c = 45
q_c = 500
beta = 1
delta = 2.71
d = 2/delta


#### 2.  Functions ####
source("analysis/simfunctions.R")


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



##### Some Plots ####

#AVerage travel time, varying one at a time.
#varying only tau (optimal gamma) and
#varying only gamma (tau = gamma in this case cuz theres no traffic in ped zone)

ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
  stat_function(fun = function(x) fun.tt_total(g.opt,x), aes(color="tau", linetype="tau")) +
  stat_function(fun = function(x) fun.tt_total(x,x), aes(color="gamma", linetype="gamma")) +
  scale_x_continuous(expression("Distance from center"), limits = c(0,R)) +
  scale_y_continuous("Average travel time (hours)", limits = c(0,30)) +
  scale_color_brewer(name = NULL, breaks = c("tau", "gamma"), 
                      labels = expression("Varying"~tau~"while"~gamma==gamma*'*',
                                          "Varying"~gamma~"while"~tau==gamma),
                     guide = guide_legend(label.hjust=1), palette = "Set1") +
  scale_linetype_discrete(name = NULL, breaks = c("tau", "gamma"),
                     labels = expression("Varying"~tau~"while"~gamma==gamma*'*',
                                         "Varying"~gamma~"while"~tau==gamma)) +
  theme_bw() + theme(legend.position = "bottom", legend.spacing.x = unit(0.5, 'cm'))


#Average transit and driving travel time functions
ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
  stat_function(fun = function(x) fun.tt_transit(x), aes(color="tau", linetype="tau")) +
  stat_function(fun = function(x) fun.tt_drive(x), aes(color="gamma", linetype="gamma")) +
  scale_x_continuous(expression("Distance from center"), limits = c(0,R)) +
  scale_y_continuous("Average travel time (hours)", limits = c(0,10)) +
  scale_color_brewer(name = NULL, breaks = c("tau", "gamma"), 
                     labels = expression("Transit travel time varying"~tau~","~t[T](tau),
                                         "Driving travel time varying"~gamma~","~t[D](gamma)),
                     guide = guide_legend(label.hjust=1), palette = "Set1") +
  scale_linetype_discrete(name = NULL, breaks = c("tau", "gamma"),
                          labels = expression("Transit travel time varying"~tau~","~t[T](tau),
                                              "Driving travel time varying"~gamma~","~t[D](gamma))) +
  theme_bw() + theme(legend.position = "bottom", legend.spacing.x = unit(0.5, 'cm'))



#Average travel time, varying both simultaneously.
Rseq <- seq(0,R-(R/100),length.out = 200)
plotmat <- data.table(expand.grid(gamma=Rseq,tau=Rseq))

plotmat$tt_total <- mapply(fun.tt_total, plotmat$gamma, plotmat$tau)

ggplot() +
  geom_contour_filled(data = plotmat, aes(x = gamma, y = tau, z=log(tt_total)), breaks = 0:11) +
  geom_area(aes(x = c(0,R), y = c(0,R)), fill='white', alpha = 0.6
            ) +
  geom_point(data=plotmat[which.min(tt_total), ], aes(x = gamma, y = tau)) +
  annotate("text", x = 3*R/4, y = 1*R/4,
           label = expression("Area under diagonal\ndenotes unrealistic\nregion where:"*tau < gamma), hjust=0.5) +
  scale_x_continuous(expression(gamma), limits = c(0,R), expand = c(0,0)) +
  scale_y_continuous(expression(tau), limits = c(0,R), expand = c(0,0)) +
  scale_fill_brewer("Average travel time\n(mins, log scale)", palette = "RdYlBu", direction = -1,
                    labels = paste0(round(exp(c(-Inf,0:10)),2),"-",round(exp(0:11),2))) +
  theme_bw() + theme(legend.position = "right", legend.spacing.x = unit(0.5, 'cm'))

  
#Drive travel time vs transit travel time

  

  
    
#
  


#Optimal ped size 
# ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
#   stat_function(fun = function(x) fun.gamma_int(g=x)) +
#   stat_function(fun = function(x) fun.q_a(r=x), linetype='dashed') +
#   stat_function(fun = function(x) fun.q_p(g=x), linetype='dashed') +
#   geom_vline(xintercept = opt.gamma) +
#   geom_hline(yintercept = 0) +
#   scale_y_continuous(labels = scales::comma, limits = c(-100,7000)) +
#   scale_x_continuous(limits = c(0,15)) +
#   theme_bw()

