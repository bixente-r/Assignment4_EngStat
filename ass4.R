### 2

n <- 4700
k <- 718
prop.test(k,n, conf.level = 0.99)


### 3

c3 <- c(418,415,423,422,425,427,431,434,437,439,446,447,448,453,454,463,465)
shapiro.test(c3)
qqnorm(c3, main='Normal')
qqline(c3)

alpha <- 0.05
n <- length(c3)
sd <- sd(c3)
mean <- mean(c3)
u <- qt(1-0.05/2,n-1);u

CI_left <- mean - u * sd /sqrt(n);CI_left
CI_right <- mean + u * sd /sqrt(n);CI_right
t.test(c3)

### 4
#Q1
c4 <- c(67.5,70.9,71.1,73.1,73.3,73.5,75.5,75.7,75.8,76.1,76.2,76.2,77.0,77.9,78.1,79.6,79.7,79.9,80.1,82.2,83.7,85.7)
shapiro.test(c4)
qqnorm(c4, main='Normal')
qqline(c4)

#Q2
alpha <- 0.01
n <- length(c4)
var <- var(c4);var
ul <- qchisq(1-alpha/2,n-1);ul
ur <- qchisq(alpha/2,n-1);ur
CI_left <- (n-1) * var / ul;CI_left
CI_right <- (n-1) * var / ur;CI_right

#Q3
alpha <- 0.05
n <- length(c4)
sd <- sd(c4)
mean <- mean(c4);mean
u <- qt(1-0.05/2,n-1);u
CI_left <- mean - u * sd /sqrt(n);CI_left
CI_right <- mean + u * sd /sqrt(n);CI_right


#############

alpha <- 0.05
n <- length(c4)
sd <- sd(c4);sd
mean <- mean(c4)

u <- qt(1-alpha/2,n-1)

CI_left <- mean - u * sd /sqrt(n)
CI_right <- mean + u * sd /sqrt(n)
CI_left
CI_right





### 5

c5 <- c(99.6,92.5,91.2,96.9,96.5,91.3,98.1,102.0,99.6,101.7,103.3,92.4)

#Q1
alpha <- 0.05
n <- length(c5)
sd <- sd(c5);sd
mean <- mean(c5);mean
t <- (mean - 100)/(sd/sqrt(n));t
qt(c(0.975,0.025),n-1)
pvalue <- pt(t,11) + 1 - pt(-t,11);pvalue

t.test(c5,mu=100)

#Q2

mu0 = 96
mu = 95
t <- qt(1-0.025,11);t
beta <- pt(t + (96-95)/(sd/sqrt(n)),11) - pt(-t + (96-95)/(sd/sqrt(n)),11);beta


n = 12                # sample size 
s = sd(c5)               # sample standard deviation 
SE = s/sqrt(n); SE
alpha = .05           # significance level 
mu0 = 96               # hypothetical upper bound 
q = mu0 + qt(alpha, df=n-1, lower.tail=FALSE) * SE; q 
pt((q - mu)/SE, df=n-1) 

#Q3
sigma = 5.5
z_alpha = -1.645
z_beta = -1.28

n <- ((sigma*(z_alpha + z_beta))/(mu0 - mu))^2;n


### 6 
#Q1
prop.test(75,145,p=0.4,conf.level=0.99, correct=FALSE)

z <- ((75/145) - 0.4)/sqrt((0.4*0.6)/145);z
pvalue <- pnorm(-z) + 1 - pnorm(z);pvalue

#Q2




### 8

c1 <- c(1.1,  1.2,  1.4,  1.3,  1.7,  1.8,  1.4,  1.3)
c2 <- c(1.4,  1.7,  1.5,  1.3,  2.0,  2.1,  1.7,  1.6)

var(c1)
var(c2)
length(c2)

mean <- mean(c1) - mean(c2);mean
SE <- sqrt(var(c1)/8 + var(c2)/8)
n <- 13.7
n <- round(n)
t <- 2.997
# mean/SE
CI_right <- mean + t * SE;CI_right
CI_left <- mean - t * SE;CI_left


t.test(c1,c2,conf.level = 0.99)


### 9 

prop.test(x = c(102,118), n = c(200,210), alternative = 'less', conf.level = 0.99)

