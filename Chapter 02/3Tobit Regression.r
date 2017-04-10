require(ggplot2)
require(GGally)
require(VGAM)

dat <- read.table("d:/tobit.csv", header=TRUE, sep=",", row.names="id")

summary(dat)

f <- function(x, var, bw = 15) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

p <- ggplot(dat, aes(x = apt, fill=prog))

p + stat_bin(binwidth=15) +
  stat_function(fun = f, size = 1,
    args = list(var = dat$apt))

p + stat_bin(binwidth = 1) + stat_function(fun = f, size = 1, args = list(var = dat$apt, 
    bw = 1))

cor(dat[, c("read", "math", "apt")])

ggpairs(dat[, c("read", "math", "apt")])

------------------------------------------------------------------------------------------

#Below we run the tobit model, using the vglm function of the VGAM package.

summary(m <- vglm(apt ~ read + math + prog, tobit(Upper = 800), data = dat))

ctable <- coef(summary(m))
pvals <- 2 * pt(abs(ctable[, "z value"]), df.residual(m), lower.tail = FALSE) 
cbind(ctable, pvals)

m2 <- vglm(apt ~ read + math, tobit(Upper = 800), data = dat)
(p <- pchisq(2 * (logLik(m) - logLik(m2)), df = 2, lower.tail = FALSE))

b <- coef(m)
se <- sqrt(diag(vcov(m)))


cbind(LL = b - qnorm(0.975) * se, UL = b + qnorm(0.975) * se)


dat$yhat <- fitted(m)[,1]
dat$rr <- resid(m, type = "response")
dat$rp <- resid(m, type = "pearson")[,1]
par(mfcol = c(2, 3))

with(dat, {
  plot(yhat, rr, main = "Fitted vs Residuals")
  qqnorm(rr)
  plot(yhat, rp, main = "Fitted vs Pearson Residuals")
  qqnorm(rp)
  plot(apt, rp, main = "Actual vs Pearson Residuals")
  plot(apt, yhat, main = "Actual vs Fitted")
})

(r <- with(dat, cor(yhat, apt)))
r^2









