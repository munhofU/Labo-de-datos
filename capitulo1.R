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

#1.10

data_mt_cars <- mtcars

mt_cars_4_gears <- data_mt_cars[data_mt_cars$gear==4,] #a
mt_cars_4_gears_name <- rownames(mt_cars_4_gears) #a

mt_cars_sub <- mtcars[mtcars$disp > 150 & mtcars$mpg > 20,] #b

mt_cars_4_gears_y_manual <- data_mt_cars[data_mt_cars$gear==4 & data_mt_cars$am==1,]#c
mt_cars_4_gears_y_manual_len <- length(rownames(mt_cars_4_gears_y_manual)) #c

mt_cars_4_gears_o_manual <- data_mt_cars[data_mt_cars$gear==4 | data_mt_cars$am==1,]#d
mt_cars_4_gears_o_manual_len <- length(rownames(mt_cars_4_gears_o_manual))#d

mt_cars_2_carb <- data_mt_cars[data_mt_cars$carb == 2,] #e
mpg_mean_mt_cars_2_carb <- mean(mt_cars_2_carb$mpg) #e

#1.11

data_mt_cars$am <-factor(mtcars$am, levels = c(0, 1), labels = c("auto", "manual")) #a

cant_data_mt_cars_manual <- table(data_mt_cars$am)[1] #b
cant_data_mt_cars_auto <- table(data_mt_cars$am)[2] #b

mt_cars_greater_25_mpg <- data_mt_cars[data_mt_cars$mpg > 25,] #c
cant_mt_cars_greater_25_mpg_manual <- table(mt_cars_greater_25_mpg$am)[1] #c
cant_mt_cars_greater_25_mpg_auto <- table(mt_cars_greater_25_mpg$am)[2] #c

#1.12

data_hot_dogs <- fosdata::hot_dogs
var_hot_dogs <- str(data_hot_dogs) #a 3 var, type: factor,int,int
kind_hot_dogs <- data_hot_dogs$type[2] #b are: beef, meat,Poultry
beef_hot_dogs <- data_hot_dogs[data_hot_dogs$type == "Beef",] #c
beef_hot_dogs_calories_mean <- mean(beef_hot_dogs$calories) #c

#1.13