#### CITY COMPARISON ####

# get list of files
flist <- list.files('../../analysis/city_data', full.names = T)
if(length(flist)==0) {
  flist <- list.files('./analysis/city_data', full.names = T)
}

# Function to calc distance from center
dfun <- function(X, Y, X0, Y0) { sqrt((X - X0)^2 + (Y - Y0)^2) }


# Max radius chosen
Rkey = c("Melbourne" = 15, 
         "Tucson" = 20,
         "Las Vegas" = 20, 
         "Denver"= 20,
         "Fresno" = 20, 
         "Chicago" = 30,
         "Sacramento" = 30)

deltakey = c("Melbourne" = 2.8,
             "Tucson" = 1.8,
             "Las Vegas" = 2.6,
             "Denver"= 2.7,
             "Fresno" = 2,
             "Chicago" = 2.4,
             "Sacramento" = 2.8)

# c(
#   "Melbourne" = 2.71,
#   "Tucson" = 1.72,
#   "Las Vegas" = 2.14,
#   "Denver"= 1.62,
#   "Fresno" = 2.45,
#   "Chicago" = 1.7,
#   "Sacramento" = 2.02)



# Read and calc distance
get_dist <- function(fname) {
  # read files
  dat = read.csv(fname)
  # Remove NA and get center point
  dat = dat[!is.na(dat$X),]
  X0 = dat[is.na(dat$ID), "X"]
  Y0 = dat[is.na(dat$ID), "Y"]
  # Calc distance to center
  dat$r <- mapply(dfun, dat$X, dat$Y, X0, Y0)
  # Add city name
  dat$CITY = gsub("../../analysis/city_data/|./analysis/city_data/|_data.csv", "", fname)
  # dat$CITY = gsub("./analysis/city_data|_data.csv", "", fname)
  # Remove center point
  dat = dat[!is.na(dat$ID),]
  # Assign max radius from key
  dat$R = Rkey[dat$CITY[1]]
  
  return(dat[ , c("CITY","AREA_KM","EMP","EMP_DENS","X","Y","r","R")])
}

empdata_list <- lapply(flist, get_dist)
city_data = do.call("rbind", empdata_list)
city_data$r <- city_data$r/1000

# Binned city data
binsize = 5
city_data$bin <- .bincode(city_data$r, breaks = seq(0, max(city_data$r), binsize))
city_data_agg <- aggregate(EMP_DENS~bin+CITY, city_data, function(x) c(mn = mean(x), sd = sd(x)))
city_data_agg <- data.frame(CITY=city_data_agg$CITY, r=city_data_agg$bin, city_data_agg$EMP_DENS)
city_data_agg$binsize = binsize


#### CALCULATE SUMMARY DATA ####
city_params <- aggregate(EMP_DENS ~ CITY,
                         city_data[city_data$r <= city_data$R, ],
                         function(x) c(mn = mean(x), md = median(x), sd = sd(x), n = length(x)))

city_params <- data.frame(CITY=city_params$CITY, city_params$EMP_DENS)
city_params <- merge(city_params, data.frame('CITY'=names(Rkey),"R"=Rkey, "delta"=deltakey), by = 'CITY')

# #Calculate demand
# Typical peak hour day, 5am to 8pm (20:00 - 05:00)
city_params$d <- 2/city_params$delta
# city_params$lambda_c <- with(city_params, (sd-mn)/sqrt(n)/14)
city_params$lambda_c <- with(city_params, mn*(mn/sd)/15)
city_params$lambda_b <- with(city_params, md/15)


#Chicago doesn't really work because it's cut in half by the lake, so the travel times get blown out.
chi_baseline = city_params[city_params$CITY=='Chicago', 'lambda_b']
city_params[city_params$CITY=='Chicago', 'lambda_b'] <- 0.15*city_params[city_params$CITY=='Chicago', 'lambda_b']



#### GET OPTIMAL ZONE SIZES ####

#### Global input parameters 
v_w = 5
v_m = 50
t_s = 60/3600
s = 0.5
k_c = 45
k_j = 125
q_c = 500

#Find critical transit priority flow, remains constant
q_T <- {
  TT <- (1/v_m)+(t_s/s)
  tc_ratio <- (q_c/k_c)*((1/v_m) + (t_s/s))
  if(tc_ratio < 1)
    (2*k_c/TT) - ((k_c^2)/(q_c*TT^2))
  else
    q_c*(TT*q_c/k_c)^(1/20)
}

#### Functions
# if(!exists("fun.tt_total")) source("analysis/simfunctions.R")


#### OPTIMiZATION
cities = city_params$CITY
# cities = c("Chicago","Denver","Fresno", "Melbourne", "Sacramento","Tucson")
# cities = 'Chicago'

city_params <- lapply(cities, function(city){
  # City-specific parameters
  lambda_c  <<- city_params[city_params$CITY == city, 'lambda_c']
  lambda_b  <<- city_params[city_params$CITY == city, 'lambda_b']
  R         <<- city_params[city_params$CITY == city, 'R']
  delta     <<- city_params[city_params$CITY == city, 'delta']
  d         <<- city_params[city_params$CITY == city, 'd']
  
  #Find the optimal zone sizes
  opt <- optim(par = c(1,1), 
               # lower = 0, upper = 0.99999*R,
               method = 'L-BFGS-B',
               fn = function(x) {
                 GAMMA = x[1]
                 TAU = x[2]
                 fun.tt_total(GAMMA, TAU, bounded = T)
        })
  
  
  opt <- setNames(c(opt[['par']], opt[['value']]),c("ped","transit","tt"))
  return(data.frame(city_params[city_params$CITY == city, ], t(opt)))
})

city_params <- do.call('rbind', city_params)
city_params[city_params$CITY=='Chicago', 'lambda_b'] <- chi_baseline


#### PARAMETER KEYS FOR LATEX ####
city_pars   = city_params$CITY

lc_pars     = format(city_params$lambda_c, big.mark = ",", digits=0, trim=T)
lb_pars     = format(city_params$lambda_b, big.mark = ",", digits=0, trim=T)
R_pars      = format(city_params$R, big.mark = ",", digits=0, trim=T)
delta_pars  = format(city_params$delta, big.mark = ",", digits=2, trim=T)
d_pars      = format(city_params$d, big.mark = ",", digits=0, trim=T)
n_pars      = format(city_params$n, big.mark = ",", digits=0, trim=T)

mean_pars   = format(city_params$mn, big.mark = ",", digits=0, trim=T)
median_pars = format(city_params$md, big.mark = ",", digits=0, trim=T)
sd_pars     = format(city_params$sd, big.mark = ",", digits=0, trim=T, scientific = F)
gamma_pars  = format(city_params$ped, big.mark = ",", digits=2, trim=T)
tau_pars    = format(city_params$transit, big.mark = ",", digits=2, trim=T)
tt_pars     = format(60*city_params$tt, big.mark = ",", digits=0, trim=T)



