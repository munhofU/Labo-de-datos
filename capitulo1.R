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

drinks_wages <- HistData::DrinksWages

#a 70 obs, 6 variables, Factor, Factor , int int num int
str(drinks_wages)

#b factory worker
wage_profesion <- drinks_wages[which.min(drinks_wages$wage),]

#c
workers_surveyed <- sum(drinks_wages$n)

#d
mean_wage <- drinks_wages$wage * drinks_wages$n / workers_surveyed
drinks_wages <- cbind(drinks_wages , mean_wage)

#1.14

batting <- Lahman::Batting
str(batting) #a 110495 obs. of  22 variables
head(batting)
year <- names(table(batting$yearID))
player <- names(table(batting$playerID))

X3B_total_by_year <- c()
for (i in year) {
    x3b_by_year <- batting[batting$yearID == i,]$X3B
    X3B_total_by_year<- c(X3B_total_by_year, sum(x3b_by_year))
}
X3B_total_by_year_list <- data.frame(year, X3B_total_by_year)
max_year_X3B <- year[which.max(X3B_total_by_year_list$X3B_total_by_year)]

X3B_by_year_max <- c()
player_whit_max_X3B_by_year <- c()

for (i in year) 
{
    batting_in_year <- batting[batting$yearID == i,]
    player_in_year <-  names(table(batting_in_year$playerID))
    player_X3B_in_year <- c()
    X3B_in_year <- c()
    for (p in player_in_year)
    {
        batting_in_year_by_player <- batting_in_year[batting_in_year$playerID == p,]
        X3B_in_year_by_player <- sum(batting_in_year_by_player$X3B)
        player_X3B_in_year <- c(player_X3B_in_year,X3B_in_year_by_player)
        X3B_in_year <- c(X3B_in_year,X3B_in_year_by_player)
    }
    max_X3B_by_player_in_year <- which.max(player_X3B_in_year)
    player_max_X3B_in_year <- player_in_year[max_X3B_by_player_in_year]
    max_X3B_in_year <- X3B_in_year[max_X3B_by_player_in_year]
    X3B_by_year_max <- c(X3B_by_year_max, max_X3B_in_year)
    player_whit_max_X3B_by_year <- c(player_whit_max_X3B_by_year, player_max_X3B_in_year)
}

ix_max_X3B <- which.max(X3B_by_year_max)
max_X3B <- X3B_by_year_max[ix_max_X3B]
player_whit_max_X3B <- player_whit_max_X3B_by_year[ix_max_X3B]
year_whit_player_whit_max_X3B <- year[ix_max_X3B]

print(c("year:", year_whit_player_whit_max_X3B,
        ", playerId:", player_whit_max_X3B,
        ", X3B:",max_X3B)
      )

X3B_by_year_max <- c()
player_whit_max_X3B_by_year <- c()
year_since_1960 <- year[year>=1960]

for (i in year_since_1960) 
{
    batting_in_year <- batting[batting$yearID == i,]
    player_in_year <-  names(table(batting_in_year$playerID))
    player_X3B_in_year <- c()
    X3B_in_year <- c()
    for (p in player_in_year)
    {
        batting_in_year_by_player <- batting_in_year[batting_in_year$playerID == p,]
        X3B_in_year_by_player <- sum(batting_in_year_by_player$X3B)
        player_X3B_in_year <- c(player_X3B_in_year,X3B_in_year_by_player)
        X3B_in_year <- c(X3B_in_year,X3B_in_year_by_player)
    }
    max_X3B_by_player_in_year <- which.max(player_X3B_in_year)
    player_max_X3B_in_year <- player_in_year[max_X3B_by_player_in_year]
    max_X3B_in_year <- X3B_in_year[max_X3B_by_player_in_year]
    X3B_by_year_max <- c(X3B_by_year_max, max_X3B_in_year)
    player_whit_max_X3B_by_year <- c(player_whit_max_X3B_by_year, player_max_X3B_in_year)
}

ix_max_X3B <- which.max(X3B_by_year_max)
max_X3B <- X3B_by_year_max[ix_max_X3B]
player_whit_max_X3B <- player_whit_max_X3B_by_year[ix_max_X3B]
year_whit_player_whit_max_X3B <- year_since_1960[ix_max_X3B]

print(c("year:", year_whit_player_whit_max_X3B,
        ", playerId:", player_whit_max_X3B,
        ", X3B:",max_X3B)
)






