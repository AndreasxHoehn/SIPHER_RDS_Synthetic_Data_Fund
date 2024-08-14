### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date: 2024-07-31
# About: This file simulates us some lowest fidelity level data based on metadata 
# only, demonstrates a simple column bind linkage and shows how far off this is 
# from real summary stats. 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Define The Observed Dataset ###
# These are the summary stats of a dataset which we would like to replicate
# Source: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0271110#sec028

# create an output summary table based on actually observed data 
dt_observed <- data.table::data.table(
  simd     = seq(1,5,1),
  n_pop    = c(1537, 1838, 1849, 1619, 1748),
  mean_age = c(58.9, 59.7, 59.6, 59.7, 60.4),
  n_cvd    = c(699, 808, 723, 581, 552)
)
dt_observed[, p_cvd := round((n_cvd / n_pop) * 100, 1) ]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Create Lowest Level Fidelity Data and Demonstrate Cbind-Linkage ###

.SimLowestF <- function(size_input) {
  
  # create a baseline table 
  dt_base <- data.table::data.table(
  males   = as.integer(sample(x = seq(0,1,1),               
                              size = size_input,
                              prob = c(0.5,0.5),
                              replace = TRUE)),   #         
  age     = as.integer(round(runif(n = size_input)*100)),    
  cvd     = as.integer(sample(x = seq(0,1,1),               
                               size = size_input,
                              prob = c(0.5,0.5),
                              replace = TRUE)))
  
  # create ab simd observation table 
  dt_simd <-   data.table::data.table(
    simd    = as.integer(sample(x = seq(1:5),
                                size = size_input,
                                prob = rep(x = 0.2, times = 5),
                                replace = TRUE)))
  
  # simple linkage via column bind
  dt_linked <- cbind(dt_base, dt_simd)
  
  output <- dt_linked[, .(n_pop   = .N,
                          mean_age = mean(age),
                          n_cvd    = sum(cvd)),
                      by = c("simd")]
  
  setkey(output, simd)
  
  # explicit return of linked result
  return(output)
}

# initialise function for simulation  #
list_lowest_f <- vector(mode = "list", length = definitions$n_sim_runs)

# run function repeatedly
for (i in 1:length(list_lowest_f)) {
  list_lowest_f[[i]] <- .SimLowestF(size_input = definitions$n_lowest_f)
}

# make a list out of it
dt_lowest_f <- data.table::rbindlist(list_lowest_f, idcol = TRUE)
dt_lowest_f[, p_cvd := (n_cvd / n_pop) * 100]

# make a summary table 
dt_lowest_f_summary <- dt_lowest_f[,
                        .(n_pop    = round(mean(n_pop), 0),
                          mean_age = round(mean(mean_age), 0),
                          n_cvd    = round(sum(n_cvd)/definitions$n_lowest_f, 0),
                          p_cvd    = round(mean(p_cvd), 1))   ,   
               by = c("simd")]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Compare Against Observed Summary Stats ###

# make simd a factor
dt_observed[, simd := as.factor(simd)]
dt_lowest_f[, simd := as.factor(simd)]
dt_lowest_f_summary[, simd := as.factor(simd)]

# plot 
plot_lowest_f_comparison <- 
  ggplot2::ggplot(dt_lowest_f,
                  aes(x= p_cvd, colour = simd, fill = simd)) + 
  geom_histogram(bins = 1000) +
  xlim(30, 60) +
  ylim(0, 275) +
  xlab("prevalence of individuals with CVD in sumulations (in %)") + 
  ylab("absolute frequency") + 
  annotate(geom = "text", color="black", x = 30, y = 270,
           hjust = "left", size = 3, 
           label = c("OBS VS. SIM PREVALENCE BY SIMD (in %)")) +
  annotate(geom = "text", color="black", x = 30, y = 260,
           hjust = "left", size = 3, 
           label = paste("- SIMD1:", dt_observed$p_cvd[1], "vs.",
                         dt_lowest_f_summary$p_cvd[1])) +
  annotate(geom = "text", color="black", x = 30, y = 250,
           hjust = "left", size = 3, 
           label = paste("- SIMD2:", dt_observed$p_cvd[2], "vs.",
                         dt_lowest_f_summary$p_cvd[2])) +
  annotate(geom = "text", color="black", x = 30, y = 240,
           hjust = "left", size = 3, 
           label = paste("- SIMD3:", dt_observed$p_cvd[3], "vs.",
                         dt_lowest_f_summary$p_cvd[3])) +
  annotate(geom = "text", color="black", x = 30, y = 230,
           hjust = "left", size = 3, 
           label = paste("- SIMD4:", dt_observed$p_cvd[4], "vs.",
                         dt_lowest_f_summary$p_cvd[4])) +
  annotate(geom = "text", color="black", x = 30, y = 220,
           hjust = "left", size = 3, 
           label = paste("- SIMD5:", dt_observed$p_cvd[5], "vs.",
                         dt_lowest_f_summary$p_cvd[5])) +
  annotate(geom = "text", color="black", x = 30, y = 210,
           hjust = "left", size = 3, 
           label = paste("NUM CASES CVD: irrelevant")) +
  annotate(geom = "text", color="black", x = 30, y = 200,
           hjust = "left", size = 3, 
           label = paste("SIMULATION RUNS:", definitions$n_sim_runs)) +
  geom_segment(inherit.aes = FALSE, data = dt_observed, 
               aes(x = p_cvd, xend = p_cvd,
                   y = 0,     yend = 100,
                   colour = simd), size = 1.0, linetype = "solid") +
  geom_segment(inherit.aes = FALSE, data = dt_lowest_f_summary, 
               aes(x = p_cvd, xend = p_cvd,
                   y = 0,     yend = 100,
                   colour = simd), size = 1.2, linetype = "dashed") +
  theme_bw()
ggplot2::ggsave(plot = plot_lowest_f_comparison,
                filename = "ROutput/plot_lowest_f_comparison.jpeg",
                units = "cm", height = 15, width = 20)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
