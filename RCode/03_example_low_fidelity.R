### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date: 2024-07-31
# About: This file simulates us some low fidelity data and demonstrates a 
# deterministic linkage

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Add Basic Basic Summary Stats For Low-Fidelity Simulation and Linkage ###

# define baselines information from paper 
n_females      <- 3837
n_males        <- 4754
n_all          <- n_females + n_males
n_age_median   <- 59.6
n_age_iqr      <- c(54.3, 66.9)
n_age_sd       <- (n_age_iqr[2] - n_age_iqr[1]) / 1.35   # assuming its ~ N
p_cvd          <- c(((n_all-3363)/n_all), 3363/n_all)
p_simd         <- c(0.179,
                    0.214,
                    0.215,
                    0.189,
                    0.204)
n_simd         <- c(0.179*n_all,
                    0.214*n_all,
                    0.215*n_all,
                    0.189*n_all,
                    0.204*n_all)

# -------------- # 

### Create Some Low-Fidelity Data and Demonstrate Deterministic Linkage ###

# simulation function: create baseline cohort with medical information
.SimLowF <- function(n_all, n_females, n_males, 
                   n_age_median, n_age_sd,
                   p_cvd,
                   p_simd) {

  # create base 
  dt_base <- data.table::data.table(
  id      = as.integer(seq(from = 1, to = n_all, by = 1)),  # "id": a running id
  males   = as.integer(c(rep(x = 0, times = n_females),     # "males": 0 - female,
                         rep(x = 1, times = n_males))),     #          1 - male
  age     = as.integer(round(rnorm(n = n_all,                          # age
                        mean = n_age_median,
                        sd = n_age_sd),0)),    
  cvd     = as.integer(sample(x = seq(0,1,1),               # some have cvd
                              size = n_all,
                              prob = p_cvd,
                              replace = TRUE)))  

  # create SIMD observation table   
  dt_simd <-   data.table::data.table(
    id      = as.integer(seq(from = 1, to = n_all, by = 1)),
    simd    = as.integer(sample(x = seq(1:5),
                                size = n_all,
                                prob = p_simd,
                                replace = TRUE)))
  # link the data   
  dt_linked <- dt_base[dt_simd, on = "id"]
  
  output <- dt_linked[, .(n_pop   = .N,
                                       mean_age = mean(age),
                                       n_cvd    = sum(cvd)),
                                   by = c("simd")]
  setkey(output, simd)
  
  # explicit return of linked result
  return(output)
}

# initialise function for simulation  #
list_low_f <- vector(mode = "list", length = definitions$n_sim_runs)

# run function repeatedly
for (i in 1:length(list_low_f)) {
  list_low_f[[i]] <- .SimLowF(n_all = n_all,
                         n_females = n_females,
                         n_males = n_males, 
                         n_age_median = n_age_median,
                         n_age_sd = n_age_sd,
                         p_cvd = p_cvd, 
                         p_simd = p_simd)
}

# make a list out of it
dt_low_f <- data.table::rbindlist(list_low_f, idcol = TRUE)
dt_low_f[, p_cvd := (n_cvd / n_pop) * 100]

# make a summary table 
dt_low_f_summary <- dt_low_f[, 
                      .(n_pop    = round(mean(n_pop), 0),
                        mean_age = round(mean(mean_age), 0),
                        n_cvd    = round(mean(n_cvd), 0),
                        p_cvd    = round(mean(p_cvd), 1)),   
               by = c("simd")]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Compare Against Observed Summary Stats ###

# make simd a factor
dt_low_f[, simd := as.factor(simd)]
dt_low_f_summary[, simd := as.factor(simd)]

# plot 
plot_low_f_comparison <- 
  ggplot2::ggplot(dt_low_f, aes(x= p_cvd, colour = simd, fill = simd)) + 
  geom_histogram(bins = 1000) +
  xlim(30, 60) +
  ylim(0, 275) +
  xlab("number of individuals with CVD per simulation") + 
  ylab("absolute frequency") + 
  annotate(geom = "text", color="black", x = 30, y = 270,
           hjust = "left", size = 3, 
           label = c("OBS VS. SIM PREVALENCE BY SIMD (in %)")) +
  annotate(geom = "text", color="black", x = 30, y = 260,
           hjust = "left", size = 3, 
           label = paste("- SIMD1:", dt_observed$p_cvd[1], "vs.",
                         dt_low_f_summary$p_cvd[1])) +
  annotate(geom = "text", color="black", x = 30, y = 250,
           hjust = "left", size = 3, 
           label = paste("- SIMD2:", dt_observed$p_cvd[2], "vs.",
                         dt_low_f_summary$p_cvd[2])) +
  annotate(geom = "text", color="black", x = 30, y = 240,
           hjust = "left", size = 3, 
           label = paste("- SIMD3:", dt_observed$p_cvd[3], "vs.",
                         dt_low_f_summary$p_cvd[3])) +
  annotate(geom = "text", color="black", x = 30, y = 230,
           hjust = "left", size = 3, 
           label = paste("- SIMD4:", dt_observed$p_cvd[4], "vs.",
                         dt_low_f_summary$p_cvd[4])) +
  annotate(geom = "text", color="black", x = 30, y = 220,
           hjust = "left", size = 3, 
           label = paste("- SIMD5:", dt_observed$p_cvd[5], "vs.",
                         dt_low_f_summary$p_cvd[5])) +
  annotate(geom = "text",color="black", x = 30, y = 210,
           hjust = "left", size = 3, 
           label = paste("NUM CASES CVD:", sum(dt_observed$n_cvd), "vs.",
                         sum(dt_low_f_summary$n_cvd))) +
  annotate(geom = "text", color="black", x = 30, y = 200,
           hjust = "left", size = 3, 
           label = paste("SIMULATION RUNS:", definitions$n_sim_runs)) +
  geom_segment(inherit.aes = FALSE, data = dt_observed, 
               aes(x = p_cvd, xend = p_cvd,
                   y = 0,     yend = 100,
                   colour = simd), size = 1.0, linetype = "solid") +
  geom_segment(inherit.aes = FALSE, data = dt_low_f_summary, 
               aes(x = p_cvd, xend = p_cvd,
                   y = 0,     yend = 100,
                   colour = simd), size = 1.2, linetype = "dashed") +
  theme_bw()
ggplot2::ggsave(plot = plot_low_f_comparison,
                filename = "ROutput/plot_low_f_comparison.jpeg",
                units = "cm", height = 15, width = 20)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
