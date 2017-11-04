x <- c(-0.5, 0, 1, 1, 1.5)
y <- c(0, 0, 2, 0, 0)
plot(x, y, lwd = 3, frame = FALSE, type = "l")
pbeta(0.75, 2, 1)
pbeta(c(0.4, 0.5, 0.6), 2, 1)
sqrt(0.5)
qbeta(0.5, 2, 1)

library(UsingR); data(galton); library(ggplot2)
library(reshape2)
longGalton <- melt(galton, measure.vars = c("child", "parent"))
g <- ggplot(longGalton, aes(x = value)) + geom_histogram(aes(y = ..density..,  fill = variable), binwidth=1, colour = "black") + geom_density(size = 2)
g <- g + facet_grid(. ~ variable)
g

library(manipulate)
myHist <- function(mu){
        g <- ggplot(galton, aes(x = child))
        g <- g + geom_histogram(fill = "salmon", 
                                binwidth=1, aes(y = ..density..), colour = "black")
        g <- g + geom_density(size = 2)
        g <- g + geom_vline(xintercept = mu, size = 2)
        mse <- round(mean((galton$child - mu)^2), 3)  
        g <- g + labs(title = paste('mu = ', mu, ' MSE = ', mse))
        g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

g <- ggplot(galton, aes(x = child))
g <- g + geom_histogram(fill = "salmon", 
                        binwidth=1, aes(y = ..density..), colour = "black")
g <- g + geom_density(size = 2)
g <- g + geom_vline(xintercept = mean(galton$child), size = 2)
g

ggplot(data.frame(x = factor(1 : 6), y = rep(1/6, 6)), aes(x = x, y = y)) + geom_bar(stat = "identity", colour = 'black', fill = "lightblue")

library(ggplot2)
nosim <- 10000; n <- 10
dat <- data.frame(
        x = c(rnorm(nosim), apply(matrix(rnorm(nosim * n), nosim), 1, mean)),
        what = factor(rep(c("Obs", "Mean"), c(nosim, nosim))) 
)
ggplot(dat, aes(x = x, fill = what)) + geom_density(size = 2, alpha = .2); 

dat <- data.frame(
        x = c(sample(1 : 6, nosim, replace = TRUE),
              apply(matrix(sample(1 : 6, nosim * 2, replace = TRUE), 
                           nosim), 1, mean),
              apply(matrix(sample(1 : 6, nosim * 3, replace = TRUE), 
                           nosim), 1, mean),
              apply(matrix(sample(1 : 6, nosim * 4, replace = TRUE), 
                           nosim), 1, mean)
        ),
        size = factor(rep(1 : 4, rep(nosim, 4))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.25, colour = "black") 
g + facet_grid(. ~ size)

dat <- data.frame(
        x = c(sample(0 : 1, nosim, replace = TRUE),
              apply(matrix(sample(0 : 1, nosim * 10, replace = TRUE), 
                           nosim), 1, mean),
              apply(matrix(sample(0 : 1, nosim * 20, replace = TRUE), 
                           nosim), 1, mean),
              apply(matrix(sample(0 : 1, nosim * 30, replace = TRUE), 
                           nosim), 1, mean)
        ),
        size = factor(rep(c(1, 10, 20, 30), rep(nosim, 4))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth = 1 / 12, colour = "black"); 
g + facet_grid(. ~ size)

nosim <- 1000
n <- 10
sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))
1/ sqrt(n)

nosim <- 1000
n <- 10
sd(apply(matrix(runif(nosim * n), nosim), 1, mean))
1/ sqrt(12 * n)

nosim <- 1000
n <- 10
sd(apply(matrix(rpois(nosim * n, 4), nosim), 1, mean))
2/ sqrt(n)

nosim <- 1000
n <- 10
sd(apply(matrix(sample(0:1, nosim * n, replace = TRUE), nosim), 1, mean))
1/ (2* sqrt(n))

# Binomial Distribution
## What's the chance of getting 7 girls out of 8 births
choose(8, 7) * 0.5^8 + choose(8, 8) * 0.5^8
pbinom(6, size = 8, prob = 0.5, lower.tail = FALSE)
pbin

pnorm(1160, mean = 1020, sd = 50, lower.tail = FALSE)
pnorm(2.8, lower.tail = FALSE)
qnorm(0.75, mean = 1020, sd = 50)

ppois(3, lambda = 2.5 * 4)

pbinom(2, size = 500, prob = 0.01)
ppois(2, lambda = 500*0.01)

n <- 1000
means <- cumsum(rnorm(n))/(1:n)
means

means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)
means

library(UsingR)
data(father.son)
x <- father.son$sheight
(mean(x) + c(-1, 1) * qnorm(0.975) * sd(x)/sqrt(length(x)))/12

round(1/sqrt(10^(1:6)), 3)

0.56 + c(-1, 1) * qnorm(0.975) * sqrt(0.56 * 0.44/100)
binom.test(56, 100)$conf.int

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
        phats <- rbinom(nosim, prob = p, size = n)/n
        ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        mean(ll < p & ul > p)
})

n <- 100
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
confidence  <- sapply(pvals, function(p) { #coverage is confidence level
        phats <- rbinom(nosim, prob = p, size = n)/n
        ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        mean(ll < p & ul > p)
})

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
        phats <- (rbinom(nosim, prob = p, size = n) + 2)/(n + 4)
        ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        mean(ll < p & ul > p)
})

x <- 5
t <- 94.32
lambda <- x/t
round(lambda + c(-1, 1) * qnorm(0.975) * sqrt(lambda/t), 3)

poisson.test(x, T = 94.32)$conf

lambdavals <- seq(0.005, 0.1, by = 0.01)
nosim <- 1000
t <- 100
coverage <- sapply(lambdavals, function(lambda) {
        lhats <- rpois(nosim, lambda = lambda * t)/t
        ll <- lhats - qnorm(0.975) * sqrt(lhats/t)
        ul <- lhats + qnorm(0.975) * sqrt(lhats/t)
        mean(ll < lambda & ul > lambda)
})

lambdavals <- seq(0.005, 0.1, by = 0.01)
nosim <- 1000
t <- 1000
coverage <- sapply(lambdavals, function(lambda) {
        lhats <- rpois(nosim, lambda = lambda * t)/t
        ll <- lhats - qnorm(0.975) * sqrt(lhats/t)
        ul <- lhats + qnorm(0.975) * sqrt(lhats/t)
        mean(ll < lambda & ul > lambda)
})

data(sleep)
head(sleep)
library(ggplot2)
g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g

g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
mn <- mean(difference); s <- sd(difference); n <- 10

mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)
t.test(g2, g1, paired = TRUE)
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)

n1 <- length(g1); n2 <- length(g2)
sp <- sqrt( ((n1 - 1) * sd(x1)^2 + (n2-1) * sd(x2)^2) / (n1 + n2-2))
md <- mean(g2) - mean(g1)
semd <- sp * sqrt(1 / n1 + 1/n2)
rbind(
        md + c(-1, 1) * qt(.975, n1 + n2 - 2) * semd,  
        t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf,
        t.test(g2, g1, paired = TRUE)$conf
)

library(datasets); data(ChickWeight); library(reshape2)
##define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW,
                 gain = time21 - time0
)

g <- ggplot(ChickWeight, aes(x = Time, y = weight, 
                             colour = Diet, group = Chick))
g <- g + geom_line()
g <- g + stat_summary(aes(group = 1), geom = "line", fun.y = mean, size = 1, col = "black")
g <- g + facet_grid(. ~ Diet)
g

g <- ggplot(wideCW, aes(x = factor(Diet), y = gain, fill = factor(Diet)))
g <- g + geom_violin(col = "black", size = 2)
g

wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
rbind(
        t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
        t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)

library(UsingR); data(father.son)
t.test(father.son$sheight - father.son$fheight)

wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
t.test(gain ~ Diet, paired = FALSE, 
       var.equal = TRUE, data = wideCW14)

ppois(9, 5, lower.tail = FALSE)

library(manipulate)
mu0 = 30
myplot <- function(sigma, mua, n, alpha){
        g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
        g = g + stat_function(fun=dnorm, geom = "line", 
                              args = list(mean = mu0, sd = sigma / sqrt(n)), 
                              size = 2, col = "red")
        g = g + stat_function(fun=dnorm, geom = "line", 
                              args = list(mean = mua, sd = sigma / sqrt(n)), 
                              size = 2, col = "blue")
        xitc = mu0 + qnorm(1 - alpha) * sigma / sqrt(n)
        g = g + geom_vline(xintercept=xitc, size = 3)
        g
}
manipulate(
        myplot(sigma, mua, n, alpha),
        sigma = slider(1, 10, step = 1, initial = 4),
        mua = slider(30, 35, step = 1, initial = 32),
        n = slider(1, 50, step = 1, initial = 16),
        alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
)

power.t.test(n = 16, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$power
power.t.test(n = 16, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$power
power.t.test(n = 16, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$power

power.t.test(power = .8, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$n
power.t.test(power = .8, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$n
power.t.test(power = .8, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$n

set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
        y <- rnorm(20)
        x <- rnorm(20)
        pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}

# Controls false positive rate
sum(pValues < 0.05)

# Controls FWER 
sum(p.adjust(pValues,method="bonferroni") < 0.05)
# Controls FDR 
sum(p.adjust(pValues,method="BH") < 0.05)

set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
        x <- rnorm(20)
        # First 500 beta=0, last 500 beta=2
        if(i <= 500){y <- rnorm(20)}else{ y <- rnorm(20,mean=2*x)}
        pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
trueStatus <- rep(c("zero","not zero"),each=500)
table(pValues < 0.05, trueStatus)

# Controls FWER 
table(p.adjust(pValues,method="bonferroni") < 0.05,trueStatus)
# Controls FDR 
table(p.adjust(pValues,method="BH") < 0.05,trueStatus)

library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x, n * B, replace = TRUE), B, n)
resampledMedians <- apply(resamples, 1, median)
resampledMedians
sd(resampledMedians)
quantile(resampledMedians, c(0.025, 0.975))
g = ggplot(data.frame(medians = resampledMedians), aes(x = medians))
g = g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g 

g = ggplot(data.frame(x = resampledMedians), aes(x = x)) 
g = g + geom_density(size = 2, fill = "red")
#g = g + geom_histogram(alpha = .20, binwidth=.3, colour = "black", fill = "blue", aes(y = ..density..)) 
g = g + geom_vline(xintercept = median(x), size = 2)
g

data(InsectSprays)
g = ggplot(InsectSprays, aes(spray, count, fill = spray))
g = g + geom_boxplot()
g

subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group)))
observedStat
mean(permutations > observedStat)

head(InsectSprays)

g = ggplot(data.frame(permutations = permutations),
           aes(permutations))
g = g + geom_histogram(fill = "lightblue", color = "black", binwidth = 1)
g = g + geom_vline(xintercept = observedStat, size = 2)
g

# Quiz 4
##1
x <- as.data.frame(cbind(baseline = c(140, 138, 150, 148, 135), week2 = c(132, 135, 151, 146, 130)))
t.test(x$baseline, x$week2, paired = TRUE)

##2
n <- 9
mean <- 1100
s <- 30
mean + c(-1,1)*qt(0.975, n-1)*30/sqrt(n)

##3
pbinom(2, 4, prob = 0.5, lower.tail = FALSE)
choose(4, 3)*0.5^4 + choose(4,4)*0.5^4
ppois(10, 1787 * 0.01)

##4
ppois(10, 1787*0.01)

##5
mean1 <- -3
sd1 <- 1.5
n1 <- 9
mean2 <- 1
sd2 <- 1.8
n2 <- 9
sp <- sqrt(((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n1 - 2))
mean1 - mean2 + c(-1,1) * qt(.975,27) * sp * (1 / n1 + 1 / n2) ^.5
mean1 - mean2 + c(-1,1) * qt(.995,27) * sp * (1 / n1 + 1 / n2) ^.5
ts <- (mean1 - mean2) / (sp * sqrt(1 / n1 + 1 / n2))
2 * pt(ts, n1 + n2 -1)

power.t.test(n = 100, delta = 0.01, sd = 0.04, type = "one.sample", alt = "one.sided")$power

power.t.test(power = 0.9, delta = 0.01, sd = 0.04, type = "one.sample", alt = "one.sided")$n