#### Packages ####
library(ggplot2)
library(data.table)
# library(metR)
# library(ggpubr)

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


#### Average travel time varying demand ####
#Melbourne
#Baseline car trip proportion = 67%
#Monocentric car trip proportion = 32%

#Vary demand
demdat <- rbindlist(lapply(seq(0.1, 1, by = 0.01), function(x) {
  lambda_c <<- x*90/2
  lambda_b <<- x*150/2
  data.table(prop = x, lambda_b, lambda_c,
             #combo = fun.tt_total(0.1,0.1), 
             Drive = fun.tt_drive(0.1),
             Transit = fun.tt_transit(0.1))
}))
demdat <- melt(demdat, id.vars = c("prop", "lambda_b", "lambda_c"))

plots[['demandtt']] <- ggplot(data = demdat) +
  geom_line(aes(x = prop, y = value, color = variable, linetype = variable)) +
  scale_x_continuous("Demand proportion that drives", labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous("Average travel time (hours)", limits = c(0, 10)) +
  scale_linetype(NULL) +
  scale_color_brewer(NULL, palette = "Set1") +
  theme_classic() +
  theme(legend.position = "bottom")


#Ensure it is set back to original
lambda_c = 90/2
lambda_b = 150/2

#### Average combined travel time ####
#varying only tau (optimal gamma) and
#varying only gamma (tau = gamma in this case cuz theres no traffic in ped zone)
plots[['combott2x']] <- ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
  stat_function(fun = function(x) fun.tt_total(0.5*x,x), aes(color="t=2g", linetype="t=2g")) +
  stat_function(fun = function(x) fun.tt_total(x,x), aes(color="t=g", linetype="t=g")) +
  scale_x_continuous("Zone radius (miles)", limits = c(0,R), breaks = seq(0,R,by=2)) +
  scale_y_continuous("Average travel time (hours)", limits = c(0,15)) +
  scale_color_brewer(name = NULL, limits = c("t=2g", "t=g"),
                      labels = expression("Travel time when"~tau==2*gamma,
                                          "Travel time when"~tau==gamma),
                     guide = guide_legend(label.hjust=1), palette = "Set1") +
  scale_linetype_discrete(name = NULL, limits = c("t=2g", "t=g"),
                          labels = expression("Travel time when"~tau==2*gamma,
                                              "Travel time when"~tau==gamma)) +
  theme_classic() + theme(legend.position = "bottom", legend.spacing.x = unit(0.5, 'cm'))


#### Average combined travel time, when zones are the same size
plots[['combott']] <- ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
  stat_function(fun = function(x) fun.tt_total(x,x)) +
  scale_x_continuous(expression("Zone radius,"~gamma == tau~"(miles)"), limits = c(0,R), breaks = seq(0,R,by=2)) +
  scale_y_continuous("Average travel time (hours)", limits = c(0,30)) +
  theme_classic() + theme(legend.position = "bottom", legend.spacing.x = unit(0.5, 'cm'))



#### Average travel time for driving and transit separately ####
#Travel time varying ped zone or transit zone.
plots[['modett']] <- ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
  stat_function(fun = function(x) fun.tt_transit(x), aes(color="tau", linetype="tau")) +
  stat_function(fun = function(x) fun.tt_drive(x), aes(color="gamma", linetype="gamma")) +
  scale_x_continuous("Zone radius", limits = c(0,R), breaks = seq(0,R,by=2)) +
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
  scale_x_continuous(expression("Pedestrian zone radius,"~gamma~"(miles)"), limits = c(0,R), breaks = seq(0,R,by=2)) +
  scale_y_continuous("Average travel time (hours)", limits = c(0,10)) +
  theme_classic() + theme(legend.position = "bottom", legend.spacing.x = unit(0.5, 'cm'))

#Average transit travel time varying transit zone size tau
plots[['transittt']] <- ggplot(data = data.frame(x = c(1,R)), mapping = aes(x = x)) +
  stat_function(fun = function(x) fun.tt_transit(x), linetype = "dashed") +
  scale_x_continuous(expression("Transit zone radius"~tau~"(miles)"), limits = c(0,R), breaks = seq(0,R,by=2)) +
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
brks = c(0,exp(seq(0,ceiling(log(max(plotmat[['tt_total']]))), wdth)))
#brks = exp(0:ceiling(log(max(plotmat[['tt_total']]))))
#brks = 10^(0:log10(max(plotmat[['tt_total']])))
plotmat[ , bin := .bincode(tt_total, breaks = brks)]
#sort(unique(plotmat$bin))

lower <- min(plotmat[['bin']])
upper <- lower + 8 #Number of bins
plotmat[ bin > upper, bin := upper]


#plotting
suppressWarnings(
plots[['optimal']] <- ggplot(data = plotmat, aes(x = gamma, y = tau)) +
  #geom_contour_filled(aes(z = bin)) +
  geom_tile(aes(fill = as.factor(bin))) +
  geom_contour(aes(z = tt_total), breaks = brks[1:10], color = 'black', size = 0.1, alpha = 0.5) +
  geom_contour(aes(z = tt_total), breaks = brks[11:length(brks)], color = 'black', size = 0.01, alpha = 0.2) +
  geom_area(data = data.frame(x = c(0,R), y=  c(0,R)), aes(x = x, y = y, z=0), fill='gray90', alpha = 0.6) +
  geom_abline(slope = 1, linetype = "dashed") +
  geom_point(data=plotmat[which.min(tt_total), ]) +
  # geom_text(data=plotmat[which.min(tt_total), ], color = 'white', 
  #           aes(x=gamma*1.75, y=tau*1.25, label = paste("Minimum =",round(tt_total,2),"hours"))) +
  annotate("text", x = 3*R/4, y = R/7, label = "Area under diagonal\ndenotes unrealistic\nregion where:", hjust=0.5, vjust=-0.25) +
  annotate("text", x = 3*R/4, y = R/7, label = "tau < gamma", hjust=0.5, parse = T, vjust=0.25) +
  scale_x_continuous(expression("Pedestrian zone radius"~gamma), limits = c(0,R), expand = c(0,0), breaks = seq(0,R,2)) +
  scale_y_continuous(expression("Transit priority zone radius"~tau), limits = c(0,R), expand = c(0,0), breaks = seq(0,R,2)) +
  scale_fill_brewer("Average travel time (hours)", palette = 'Blues', direction = -1,
                    label = c(paste0("<",scales::comma(brks)[lower]),
                              paste0(scales::comma(brks)[lower:(upper-2)], " - ",
                                     scales::comma(brks)[(lower+1):(upper-1)]),
                              paste0(">",scales::comma(brks)[(upper-1)]))) +
  theme_light() + coord_fixed() +
  theme( legend.position = "right", legend.spacing.x = unit(0.5, 'cm'),
         panel.grid = element_blank())
)



#Find the optimal zone sizes
opt <- optim(par = c(2,2), fn = function(x) {
  GAMMA = x[1]
  TAU = x[2]
  fun.tt_total(GAMMA,TAU)
})


opt <- setNames(c(opt[['par']], opt[['value']]),c("ped","transit","tt"))








