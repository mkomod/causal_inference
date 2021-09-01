# Testing causal inference methods on the ACICCOMP2016 dataset

if (require(aciccomp2016, quietly=TRUE) == FALSE) {
    devtools::install_github("https://github.com/vdorie/aciccomp/tree/master/2016")
}
library(aciccomp2016)
library(randomForest)
library(glmnet)
library(BART)


# ----------------------------------------
# Set-up
# ----------------------------------------
PARAM <- 5         # 1:77 
SEED  <- 24	   # 1:100
RES <- c()         # Store results


# ----------------------------------------
# Generating data
# ----------------------------------------
d <- dgp_2016(input_2016, PARAM, SEED) |> as.data.frame()
x <- dgp_2016(input_2016, PARAM, SEED, extraInfo=TRUE)$x

head(d); colnames(d); dim(d)
head(x); colnames(x); dim(x)

d.0 <- d[!d$z, ]
d.1 <- d[!!d$z, ]

# create data frames for regression
df.0 <- cbind(y=d$y[!d$z  ], x[!d$z,  ]) 	# control
df.1 <- cbind(y=d$y[!!d$z ], x[!!d$z, ])	# treatment


# ----------------------------------------
# Exploratory analysis
# ----------------------------------------
table(d$z)   # 0: control, 1: treated

# % treated
sum(d$z == 1) / length(d$z) |> round(digits=-2) * 100

# Sample average treatment effect
satt.0 <- mean(d.0$y.1 - d.0$y.0)	
satt.1 <- mean(d.1$y.1 - d.1$y.0)

satt.0
satt.1
# SATT bigger for control rather than treated


# ----------------------------------------
# Baseline
# ----------------------------------------
f0 <- lm(y ~ .  , data=df.0)

f0.att <- mean(d.1$y - predict(f0, newdata=df.1))
f0.att - satt.1
RES <- c(RES, f0.att)


# ----------------------------------------
# Random Forest
# ----------------------------------------
f1 <- randomForest::randomForest(y ~ ., data= df.0, ntree=100)

f1.att <- mean(d.1$y - predict(f1, newdata=df.1))
f1.att - satt.1
RES <- c(RES, f1.att)


# ----------------------------------------
# Lasso / Ridge / Elastic-Net
# ----------------------------------------
mm <- model.matrix(y ~ ., data=df.0)   # use model matrix to turn df to matrix
mm.1 <- model.matrix(y ~ ., data=df.1)

method <- c("Lasso", "Ridge", "E-Net")
alpha <- c(1, 0, 0.5)

for (i in seq_along(alpha)) {
    f2 <- glmnet(x=mm, y=df.0[ , 1], alpha=alpha[i])
    f2.cv <- cv.glmnet(x=mm, y=df.0[ , 1], alpha=alpha[i])
    f2.lmin <- f2.cv$lambda.min

    f2.att <- mean(d.1$y - predict(f2, s=f2.lmin, newx=mm.1))
    cat(sprintf("%s:\t %.3f", method[i], f2.att - satt.1), "\n")
    RES <- c(RES, f2.att)
}


# ----------------------------------------
# BART
# ----------------------------------------
f3 <- gbart(df.0[ , -1], df.0[ , 1], x.test=df.1[ , -1])

f3.att <- mean(d.1$y - apply(f3$yhat.test, 2, mean)) 
f3.att - satt.1
RES <- c(RES, f3.att)


# ----------------------------------------
# Results
# ----------------------------------------
models <- c("Linear", "Rand F", method, "BART")
cat(rep("-", 40), "\n" , rep(" ", 15), "Est. ATT\n", rep("-", 40),
    sprintf("\n%s:\t %.3f", models, RES), "\n",
    rep("-", 40), "\n" , rep(" ", 12), "Est. ATT - ATT\n", rep("-", 40),
    sprintf("\n%s:\t %.3f", models , RES - satt.1), "\n",
    sep="")


# TODO: Coverage
