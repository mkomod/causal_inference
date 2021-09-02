#
# Simulation study exploring the effect of different settings on
# method performance
#
library(aciccomp2016)                  # devtools::install_github("vdorie/aciccomp/2016")
library(glmnet)


# ---------------------------------------
# Data
# ---------------------------------------
ps <- parameters_2016                  # describes the different parameter settings

# filter the settings we are exploring
params <- with(ps, which(root.trt==0.35 & overlap.trt=="one-term" &
			 alignment==0.75 & te.hetero=="high"))
settings <- expand.grid(p=params, s=1:100)


# ----------------------------------------
# Experiments
# ----------------------------------------
results <- matrix(nrow=0, ncol=5)
colnames(results) <- c("att", "atc", "linreg.att", "linreg.cov", "lasso.att")

for (i in 1:nrow(settings)) {
    p <- settings[i, 1]
    s <- settings[i, 2]

    d <- as.data.frame(dgp_2016(input_2016, parameters=p, random.seed=s))
    x <- dgp_2016(input_2016, parameters=p, random.seed=s, extraInfo=TRUE)$x
    
    # create datasets for control (0) and treated (1)
    d.0 <- d[!d$z, ]
    d.1 <- d[!!d$z, ]
    df.0 <- cbind(y=d$y[!d$z  ], x[!d$z,  ])
    df.1 <- cbind(y=d$y[!!d$z ], x[!!d$z, ])

    # Model matrices
    mm <- model.matrix(y ~ ., data=df.0)
    mm.1 <- model.matrix(y ~ ., data=df.1)
    
    # --------------------
    # Regression
    # --------------------
    linreg <- lm(y ~ .  , data=df.0)
    linreg.att <- tryCatch({
	mean(d.1$y - predict(linreg, newdata=df.1))
    }, error = function(e) NA)
    linreg.cov <- tryCatch({
	pred <- predict(linreg, newdata=df.1, interval="prediction")
	mean(pred[2] <= d.1$y.0 | d.1$y.0 <= pred[3])
    }, error = function(e) NA)


    # --------------------
    # LASSO
    # --------------------
    # optimise lambda
    lasso.cv <- cv.glmnet(x=mm, y=df.0[ , 1], alpha=0)
    lasso.lmin <- lasso.cv$lambda.min

    lasso <- glmnet(x=mm, y=df.0[ , 1], alpha=0, lambda=lasso.lmin)
    lasso.att <- tryCatch({
	mean(d.1$y - predict(lasso, s=lasso.lmin, newx=mm.1))
    }, error = function(e) NA)

    
    # --------------------
    # Pkg results
    # --------------------
    results <- rbind(results, 
	c(att=mean(d.1$y.1 - d.1$y.0), atc=mean(d.0$y.1 - d.0$y.0),
	  linreg.att=linreg.att, linreg.cov=linreg.cov, lasso.att=lasso.att))
}


# ----------------------------------------
# Pkg results
# ----------------------------------------
results <- cbind(settings, results)

save(results, file="./results.RData")

