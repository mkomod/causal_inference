# Testing causal inference methods on the ACICCOMP2016 dataset

if (require(aciccomp2016, quietly=TRUE) == FALSE) {
    devtools::install_github("https://github.com/vdorie/aciccomp/tree/master/2016")
}
library(aciccomp2016)


# ----------------------------------------
# Set-up
# ----------------------------------------
PARAMETERS <- 5    # 1:77 
SEED <- 24	   # 1:100


# ----------------------------------------
# Generating data
# ----------------------------------------
d <- dgp_2016(input_2016, 5, 24) |> as.data.frame()
x <- dgp_2016(input_2016, 5, 24, extraInfo=TRUE)$x

head(d)
colnames(d)
dim(d)


# ----------------------------------------
# Exploratory analysis
# ----------------------------------------
table(d$z)   # 0: control, 1: treated

# % treated
sum(d$z == 1) / length(d$z) |> round(digits=-2) * 100

# Exploring some of the confounders
plot(x$x_1, col=d$z + 1)


# ----------------------------------------
# Baseline
# ----------------------------------------
d.f0 <- data.frame(y = d$y[!d$z], x[!d$z, ])
f0 <- lm(y ~ ., data=d.f0)

d.f1 <- data.frame(y = d$y[!!d$z], x[!!d$z, ])
f1 <- lm(y ~ ., data=d.f1)

mean(d$y[!!d$z] - predict(f0, newdata=x[!!d$z, ]))
mean(d$y.1 - d$y.0)

