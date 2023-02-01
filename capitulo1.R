#"1.1"

x<-c(1,2,3)
y<-c(6,5,4)

ej1a <- x * 2
ej1b <- x * y
ej1c <- x[1] * y[2]

#"1.2"

ej2a <- x + x
ej2b <- x + x
ej2c <- x + x
ej2d <- x + 1

#"1.3"

ej3a <- 1:10
ej3b <- 1:10 * 2
ej3c <- 1:10^2
ej3d <- 1:10 + 1
ej3e <- 1:(10 * 2)
ej3f <- rep(c(1,1,2), times = 2)
ej3g <- seq(from = 0, to = 10, length.out = 5)

#"1.4"

f <- function(p){
  return(p*(1-p))
}

p1 <- seq(0 ,1 ,0.2)
p2 <- seq(0 ,1 ,0.01)
q1 = f(p1)
q2 = f(p2)

plot(p1,q1,col="blue",type="l")
points(p2,q2,col="red")

#"1.5"

x <- 1:100
y <- x**2
z <- sum(y)

#"1.6"

x <- seq(from = 10, to = 30, by = 2)
ej6a <- length(x)
ej6b <- x[2] 
ej6c <- x[1:5]
ej6d <- x[1:3*2]
ej6e <- x[1:(3*2)]
ej6f <- x>25
ej6g <- x[x>25]
ej6h <- x[-1]
ej6i <- x[-1:-3]

#"1.7"

ej7_rivers <- rivers
rivers_mean <- mean(ej7_rivers)
rivers_sd <- sd(ej7_rivers)
hist(ej7_rivers)
summaty_rivers <- summary(ej7_rivers)
max_river <- max(ej7_rivers)
min_river <-min(ej7_rivers)
rivers_longer <- ej7_rivers[ej7_rivers>1000]

#"1.8"

data = airquality #153 obs 6 var
str(data)

#"1.9"

data_state <- data.frame(
    state_name = state.name,
    state_region = state.region,
    state_area = state.area,
    state.center
)

stats_state_region <- table(state.region)
state_area_less <- data_state[data_state$state_area < 10000,]
south_state <- data_state[data_state$state_region == "South",]
center_south <- south_state[which.min(south_state$y),]
