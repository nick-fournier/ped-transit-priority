#### Packages ####
library(ggplot2)
library(data.table)
# library(metR)
# library(ggpubr)

#### 1. Global input parameters ####
lambda_c = 0.9*90/2
lambda_b = 0.9*150/2
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
#source("analysis/simfunctions.R")


#### 3. Simulation ####

#Find the optimal intersection point, remains constant
g.opt <- min(uniroot(fun.gamma_int, c(0,R/2))[['root']],
             uniroot(fun.gamma_int, c(R/2,R))[['root']])

#Find critical transit priority flow, remains constant
q_T <- {
  TT <- (1/v_m)+(t_s/s)
  tc_ratio <- (q_c/k_c)*((1/v_m) + (t_s/s))
  if(tc_ratio < 1) 
    (2*k_c/TT) - ((k_c^2)/(q_c*TT^2))
  else
    q_c*(TT*q_c/k_c)^(1/20)
}

###



##### Some Plots ####
plots <- list()

#### Average combined travel time ####
#varying only tau (optimal gamma) and
#varying only gamma (tau = gamma in this case cuz theres no traffic in ped zone)
plots[['combottopt']] <- ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
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


#### Average combined travel time, when zones are the same size
plots[['combott']] <- ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
  stat_function(fun = function(x) fun.tt_total(x,x)) +
  scale_x_continuous(expression("Zone size,"~gamma == tau~"(miles)"), limits = c(0,R)) +
  scale_y_continuous("Average travel time (hours)", limits = c(0,30)) +
  theme_bw() + theme(legend.position = "bottom", legend.spacing.x = unit(0.5, 'cm'))



#### Average travel time for driving and transit separately ####
#Travel time varying ped zone or transit zone.
plots[['modett']] <- ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
  stat_function(fun = function(x) fun.tt_transit(x), aes(color="tau", linetype="tau")) +
  stat_function(fun = function(x) fun.tt_drive(x), aes(color="gamma", linetype="gamma")) +
  scale_x_continuous("Distance from center", limits = c(0,R)) +
  scale_y_continuous("Average travel time (hours)", limits = c(0,8)) +
  scale_color_brewer(name = NULL, breaks = c("tau", "gamma"), 
                     labels = expression("Transit travel time varying"~tau~","~t[T](tau),
                                         "Driving travel time varying"~gamma~","~t[D](gamma)),
                     guide = guide_legend(label.hjust=1), palette = "Set1") +
  scale_linetype_discrete(name = NULL, breaks = c("tau", "gamma"),
                          labels = expression("Transit travel time varying"~tau~","~t[T](tau),
                                              "Driving travel time varying"~gamma~","~t[D](gamma))) +
  theme_classic() + theme(legend.position = "bottom", legend.spacing.x = unit(0.5, 'cm'))


#Average driving travel time varying pedestrian zone size gamma
plots[['drivett']] <- ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
  stat_function(fun = function(x) fun.tt_drive(x)) +
  scale_x_continuous(expression("Pedestrian zone size,"~gamma~"(miles)"), limits = c(0,R)) +
  scale_y_continuous("Average travel time (hours)", limits = c(0,10)) +
  theme_classic() + theme(legend.position = "bottom", legend.spacing.x = unit(0.5, 'cm'))

#Average transit travel time varying transit zone size tau
plots[['transittt']] <- ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
  stat_function(fun = function(x) fun.tt_transit(x), linetype = "dashed") +
  scale_x_continuous(expression("Transit zone size"~tau~"(miles)"), limits = c(0,R)) +
  scale_y_continuous("Average travel time (hours)", limits = c(0,10)) +
  theme_classic() + theme(legend.position = "bottom", legend.spacing.x = unit(0.5, 'cm'))



#### Optimal Ped/Transit zone size ####
#Average travel time, varying both simultaneously.
Rseq <- seq(1/1000,R-(R/1000),length.out = 300)
plotmat <- data.table(expand.grid(gamma=Rseq,tau=Rseq))
#Calculate the average total travel time
plotmat[ , tt_total := mapply(fun.tt_total, plotmat[['gamma']], plotmat[['tau']])]
#Bins
wdth = 0.25
brks = exp(seq(0,ceiling(log(max(plotmat[['tt_total']]))), wdth))
#brks = exp(0:ceiling(log(max(plotmat[['tt_total']]))))
#brks = 10^(0:log10(max(plotmat[['tt_total']])))
plotmat[ , bin := .bincode(tt_total, breaks = brks)]
#sort(unique(plotmat$bin))

lower <- min(plotmat[['bin']])
upper <- lower + 8
plotmat[ bin > upper, bin := upper]

#plotting
plots[['optimal']] <- ggplot(data = plotmat, aes(x = gamma, y = tau, z = tt_total)) +
  geom_tile(aes(fill = as.factor(bin))) +
  geom_contour(breaks = brks[1:10], color = 'black', size = 0.1, alpha = 0.5) +
  geom_contour(breaks = brks[11:length(brks)], color = 'black', size = 0.01, alpha = 0.2) +
  geom_area(data = data.frame(x = c(0,R), y=  c(0,R)), aes(x = x, y = y, z=0), fill='gray90', alpha = 0.6) +
  geom_abline(slope = 1, linetype = "dashed") +
  geom_point(data=plotmat[which.min(tt_total), ]) +
  geom_text(data=plotmat[which.min(tt_total), ], color = 'white', 
            aes(x=gamma*1.75, y=tau*1.25, label = paste("Minimum =",round(tt_total,2),"hours"))) +
  annotate("text", x = 3*R/4, y = R/7, label = "Area under diagonal\ndenotes unrealistic\nregion where:", hjust=0.5, vjust=-0.25) +
  annotate("text", x = 3*R/4, y = R/7, label = "tau < gamma", hjust=0.5, parse = T, vjust=0.25) +
  scale_x_continuous(expression("Pedestrian zone size"~gamma), limits = c(0,R), expand = c(0,0), breaks = seq(0,R,2)) +
  scale_y_continuous(expression("Transit priority zone size"~tau), limits = c(0,R), expand = c(0,0), breaks = seq(0,R,2)) +
  scale_fill_brewer("Average travel time (hours)", palette = 'Blues', direction = -1,
                    label = c(paste0("<",scales::comma(brks)[lower]),
                              paste0(scales::comma(brks)[lower:(upper-2)], " - ",
                                     scales::comma(brks)[(lower+1):(upper-1)]),
                              paste0(">",scales::comma(brks)[(upper-1)]))) +
  theme_light() + coord_fixed() +
  theme(legend.position = "right", legend.spacing.x = unit(0.5, 'cm'))


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

