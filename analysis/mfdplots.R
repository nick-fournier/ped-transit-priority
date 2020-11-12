# # # # # # # # # #
#### MFD Plots ####

v_f = 22
k_j = 125
k_c = 45

#### Flow density funcs ####
#Flow-density function, linear-parabolic
fun.flowdensity_linpara <- function(k) {
  k0 = -(k_j^2 - 2*k_j*k_c - sqrt(k_j*(-2*k_c^3 + 5*k_j*k_c^2 - 4*k_c*k_j^2 + k_j^3))) / k_c
  
  sapply(k, function(k)
    if(k <= k0) 
      v_f*k
    else
      (k_c*v_f/2)*(1 - ((k - k_c)^2)/((k_j - k_c)^2))
  )
}

#Flow-density function, double parabolic
fun.flowdensity_bipara <- function(k) {
  sapply(k, function(k)
    if(k <= k_c) 
      k*v_f*(1 - (k/(2*k_c)))
    else
      (k_c*v_f/2)*(1 - ((k - k_c)^2)/((k_j - k_c)^2))
  )
}

#Flow-density function, linear by daganzo
fun.flowdensity_dag <- function(k) {
  sapply(k, function(k)
    if(k <= k_c/2)
      v_f*k
    else
      (v_f*k_c/2)*(1 + ( (k_c - 2*k)/(2*k_j - k_c) ) )
  )
}

#Flow-density function, linear by greenshields
fun.flowdensity_green <- function(k) {
  sapply(k, function(k)
    k*v_f*(1 - (k/(2*k_c))) 
  )
}


#### Flow density piece-wise ####
#Flow-density function, double parabolic
fun.flowdensity_bipara1 <- function(k) {
  sapply(k, function(k)
    if(k <= k_c) 
      k*v_f*(1 - (k/(2*k_c)))
    else
      NA
  )
}

fun.flowdensity_bipara2 <- function(k) {
  sapply(k, function(k)
    if(k <= k_c) 
      NA
    else
      (k_c*v_f/2)*(1 - ((k - k_c)^2)/((k_j - k_c)^2))
  )
}

#Flow-density function, linear by daganzo
fun.flowdensity_dag1 <- function(k) {
  sapply(k, function(k)
    if(k <= k_c/2)
      v_f*k
    else
      NA
  )
}

fun.flowdensity_dag2 <- function(k) {
  sapply(k, function(k)
    if(k <= k_c/2)
      NA
    else
      (v_f*k_c/2)*(1 + ( (k_c - 2*k)/(2*k_j - k_c) ) )
  )
}

#Flow-density function, linear by greenshields
fun.flowdensity_green1 <- function(k) {
  sapply(k, function(k)
    if(k <= k_c)
        k*v_f*(1 - (k/(2*k_c)))
    else
      NA
  )
}

fun.flowdensity_green2 <- function(k) {
  sapply(k, function(k)
    if(k <= k_c)
      NA
    else
      k*v_f*(1 - (k/(2*k_c)))
  )
}


#### Backbending travel time functions ####
#Travel time from flow by biparabolic function
fun.timeflow_bipara1 <- function(q) {
  sapply(q, function(q)
    if(q <= 2*q_c/2)
      k_c*(1 - sqrt(1 - (q/q_c))) / q
    else
      NA
  )
}

fun.timeflow_bipara2 <- function(q) {
  sapply(q, function(q)
    if(q <= 2*q_c/2)
      (-(k_c/v_f)*(-v_f - sqrt(((v_f^2)*k_c - 2*v_f*q) / k_c)) ) / q
    #( k_c + sqrt( (-2*(q - 2*q_c)*(k_c - k_j) * (k_j - k_c) + 2*q*(k_c - k_j)^2 ) / (2*q_c - q)) ) / q
    else
      NA
  )
}

#Travel time from flow by daganzo
fun.timeflow_dag1 <- function(q) {
  sapply(q, function(q)
    if(q <= 2*q_c/2)
      ( q/v_f ) / q
    else
      NA
  )
}

fun.timeflow_dag2 <- function(q) {
  sapply(q, function(q)
    if(q <= 2*q_c/2)
      (k_j - ((2*k_j - k_c) / (v_f*k_c))*q ) / q
    else
      NA
  )
  
}

#Travel time from flow by parabolic-exponential
fun.timeflow_paraexp1 <- function(q) {
  sapply(q, function(q)
    if(q < q_c)
      k_c*(1 - sqrt(1 - (q/q_c))) / q
    else
      NA
  )
}

fun.timeflow_paraexp2 <- function(q) {
  sapply(q, function(q)
    if(q < q_c)
      NA
    else
      (k_c/q_c)*(q/q_c)^20
  )
}


#### Continuous travel time functions ####
fun.timeflow_bipara <- function(q) {
  sapply(q, function(q)
    if(q < q_c)
      k_c*(1 - sqrt(1 - (q/q_c))) / q
    else
      ( k_c + sqrt( (-2*(q - 2*q_c)*(k_c - k_j) * (k_j - k_c) + 2*q*(k_c - k_j)^2 ) / (2*q_c - q)) ) / q
  )
}

fun.timeflow_paraexp <- function(q) {
  sapply(q, function(q)
    if(q < q_c)
      k_c*(1 - sqrt(1 - (q/q_c))) / q
    else
      (k_c/q_c)*(q/q_c)^20
  )
}

fun.timeflow_dag <- function(q) {
  sapply(q, function(q)
    if(q < q_c)
      1/v_f
    else
      - (k_j - ((2*k_j - k_c) / (2*q_c))*(2*q_c - q)) / (q - 2*q_c) 
  )
}



#### Flow-density function
flabs = c("Parabolic","Bi-parabolic","Bi-linear","Parabolic-Exponential")

plots[['flowdensity']] <- ggplot(data.frame(k = c(0, 40)), aes(k)) + 
  stat_function(fun = fun.flowdensity_bipara1, linetype = 1, aes(color = "bipara")) +
  stat_function(fun = fun.flowdensity_bipara2, linetype = 2, aes(color = "bipara")) +
  stat_function(fun = fun.flowdensity_dag1, linetype = 1, aes(color = "dag")) +
  stat_function(fun = fun.flowdensity_dag2, linetype = 2, aes(color = "dag")) +
  stat_function(fun = fun.flowdensity_green1, linetype = 1, aes(color = "green")) +
  stat_function(fun = fun.flowdensity_green2, linetype = 2, aes(color = "green")) +
  scale_y_continuous("Traffic flow (veh/hr/lane)", labels = scales::comma, breaks = seq(0, 600, by = 100), expand = c(0,0)) +
  scale_x_continuous("Traffic density (veh/km/lane)", limits = c(0, 150), breaks = seq(0, 150, by = 25), expand = c(0,0)) +
  scale_color_brewer("Traffic Flow Model", palette = "Set1", limits = c("green","dag","bipara","paraexp"), labels = flabs) +
  #scale_linetype_manual("Traffic Flow Model", values = c(1,2,5), limits = c("para","dag","green"), labels = flabs) +
  coord_cartesian(xlim = c(0,155), ylim = c(0,600)) +
  theme_classic() +
  theme(legend.position = "none", 
        legend.background = element_blank())
# plots[['flowdensity']]


#### Speed-density function
plots[['speeddensity']] <- ggplot(data.frame(k = c(0, 40)), aes(k)) + 
  stat_function(fun = function(k) fun.flowdensity_bipara(k)/k, aes(linetype = "bipara", color = "bipara")) +
  stat_function(fun = function(k) fun.flowdensity_dag(k)/k, aes(linetype = "dag", color = "dag"), alpha = 0.5) +
  stat_function(fun = function(k) fun.flowdensity_green(k)/k, aes(linetype = "green", color = "green"), alpha = 0.5) +
  scale_y_continuous("Traffic speed (km/hr)", labels = scales::comma, breaks = seq(0, 100, by = 25), expand = c(0,0)) +
  scale_x_continuous("Traffic density (veh/km/lane)", limits = c(0, 150), breaks = seq(0, 150, by = 25), expand = c(0,0)) +
  scale_color_brewer("Traffic Flow Model", palette = "Set1", limits = c("green","dag","bipara"), labels = flabs) +
  #scale_linetype_manual("Traffic Flow Model", values = c(1,2,5), limits = c("green","dag","bipara"), labels = flabs) +
  coord_cartesian(xlim = c(0,155), ylim = c(0,120)) +
  theme_classic() +
  theme(legend.position = c(0.75,0.5), 
        legend.background = element_blank())
# plots[['speeddensity']]

#### travel time-flow function
plots[['timeflow']] <- ggplot(data.frame(q = c(0, 2*q_c)), aes(q)) + 
  stat_function(fun = function(q) -1, linetype = 1, aes(color = "green")) +
  stat_function(fun = function(q) 600*fun.timeflow_dag1(q), linetype = 1, aes(color = "dag")) +
  stat_function(fun = function(q) 600*fun.timeflow_dag2(q), linetype = 2, aes(color = "dag")) +
  stat_function(fun = function(q) 600*fun.timeflow_bipara1(q), linetype = 1, aes(color = "bipara")) +
  stat_function(fun = function(q) 600*fun.timeflow_bipara2(q), linetype = 2, aes(color = "bipara")) +
  stat_function(fun = function(q) 600*fun.timeflow_paraexp1(q), linetype = 1, aes(color = "paraexp")) +
  stat_function(fun = function(q) 600*fun.timeflow_paraexp2(q), linetype = 2, aes(color = "paraexp")) +
  # stat_function(fun = function(q) 600*fun.timeflow_dag(q), aes(linetype = "dag", color = "dag")) +
  # stat_function(fun = function(q) 600*fun.timeflow_bipara(q), aes(linetype = "bipara", color = "bipara")) +
  scale_y_continuous("Travel time over 10km (mins)", labels = scales::comma, breaks = seq(0, 200, by = 25), expand = c(0,0)) +
  scale_x_continuous("Traffic flow (veh/hr)", limits = c(0, 1.5*q_c), breaks = seq(0, 2*q_c, by = 100), expand = c(0,0)) +
  scale_color_brewer("Traffic Flow Model", palette = "Set1", limits = c("green","dag","bipara","paraexp"), labels = flabs) +
  #scale_linetype_manual("Traffic Flow Model", values = c(2,5,1), limits = c("bipara","dag","paraexp"), labels = flabs) +
  coord_cartesian(xlim = c(0,750), ylim = c(0,150)) +
  theme_classic() +
  theme(legend.position = c(0.25,0.5), 
        legend.background = element_blank())
# plots[['timeflow']]


#text=element_text(family="Times New Roman"))
# plot[['flowdensity']]