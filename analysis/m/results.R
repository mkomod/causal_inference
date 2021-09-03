#
# Processing of simulation results
#
library(ggplot2)

load("results.RData")

# ----------------------------------------
# Description of settings
# ----------------------------------------
# Setting   Trt Model  %Trt   Overlap   Response Model   Trt/Rsp Alignment Heterogeneity
#    1       linear    low    penalize      linear             high           high 
#    5       linear    low    penalize    exponential          high           high
#    6     polynomial  low    penalize      linear             high           high
#    7     polynomial  low    penalize    exponential          high           high
#    9        step     low    penalize      step               high           high
#   24     polynomial  low    penalize      step               high           high
#   54        step     low    penalize    exponential          high           high


# ----------------------------------------
# Err functions
# ----------------------------------------
rmse <- function(x, y) sqrt(mean((x - y)^2, na.rm=TRUE))
mabe <- function(x, y) mean(abs(x - y), na.rm=T)


# ----------------------------------------
# RMSE and MABE
# ----------------------------------------
rmse(results$att, results$linreg.att)
rmse(results$att, results$lasso.att)
mabe(results$att, results$lasso.att)
mabe(results$att, results$linreg.att)

results$lasso.err <- abs(results$att - results$lasso.att)
results$linreg.err <- abs(results$att - results$linreg.att)

# removing the NAs doesnt really change the RMSE
results.noNa <- na.omit(results)
rmse(results.noNa$att, results.noNa$lasso.att)
rmse(results.noNa$att, results.noNa$linreg.att)

results.noNa$lasso.err <- abs(results.noNa$att - results.noNa$lasso.att)
results.noNa$linreg.err <- abs(results.noNa$att - results.noNa$linreg.att)


# ----------------------------------------
# Distribution of Abs Err
# ----------------------------------------
ggplot(data=results, aes(x=linreg.err, fill=as.factor(p))) +
    geom_histogram() +
    labs(x = "Absolute error", fill="Parameter")

ggplot(data=results, aes(x=lasso.err, fill=as.factor(p))) +
    geom_histogram() +
    labs(x = "Absolute error", fill="Parameter")

# table of the means
for (p in unique(results$p)) {
    cat(sprintf("%s\t\t%.3f\t\t%.3f\n", p, 
    	mean(results$linreg.err[results$p == p], na.rm=T),
	mean(results$lasso.err[results$p == p])))
}

# reject null - i.e. linear reg better
t.test(results.noNa$linreg.err[results.noNa$p == 1],
       results.noNa$lasso.err[ results.noNa$p == 1], paired=T)


# RMSE
for (i in unique(results$p)) {
    with(results,
    cat(sprintf("%s\t\t%.3f\t(%.3f)\t\t%.3f\t(%.3f)\n", i, 
    	rmse(att[p == i], linreg.att[p == i]),
    	sd(linreg.err[p == i], na.rm=T),
    	rmse(att[p == i], lasso.att[p == i]),
    	sd(lasso.err[p == i], na.rm=T)))
    )
}

round(rmse(results$att, results$linreg.att), 3)
sd(results$linreg.err, na.rm=T)
round(rmse(results$att, results$lasso.att), 3)
sd(results$lasso.err, na.rm=T)

# ----------------------------------------
# Coverage
# ----------------------------------------
for (p in unique(results$p)) {
    cat(sprintf("%s\t\t%.3f\t(%.3f)\n", p, 
	mean(results$linreg.cov[results$p==p], na.rm=T),
	sd(results$linreg.cov[results$p==p], na.rm=T))
    )
}


mean(results$linreg.cov, na.rm=T)
sd(results$linreg.cov, na.rm=T)
