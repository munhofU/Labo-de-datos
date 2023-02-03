
#1 p(x) = ax^2 + bx + c

a <- 1
b <- 2
c <- 0

d <- b ** 2 - 4 * a *c
    
if (d >= 0){
    raices <- c((- b + (d) ** (0.5)) / (2 * a),(- b - (d) ** (0.5)) / (2 * a))
} else {
        raices <- "son imaginarias"
    }
    
#2

RaicesPolinomios <- function(a, b, c){
    d <- b ** 2 - 4 * a *c
    if (d >= 0){
        x1 <- (- b + (d) ** (0.5)) / (2 * a)
        x2 <- (- b - (d) ** (0.5)) / (2 * a)
        raices <- c(x1, x2)
    } else {
        z1 = complex(real = - b, imaginary =   sqrt(abs(d))) / (2 * a)
        z2 = complex(real = - b, imaginary = - sqrt(abs(d)))/ (2 * a)
        raices <- c(z1, z2)
    }
    return(raices)
}

RaicesPolinomios(a,b,c)

#3
#a
LetraAzar <- function(){
    return(sample(letters)[1])
}

LetraAzar()

#b
PalabraSinA <- function(){
    letra <- LetraAzar()
    palabra <- ""
    while ( letra != "a" ) {
        letra <- LetraAzar()
        palabra <- paste(palabra,
                         letra,
                         sep = "")
    }
    return(palabra)
}

#c
PromLong <- function(n){
    long <- 0
    for (i in 1:n) {
        long <- long + nchar(PalabraSinA())
    }
    return(long/n)
}

#4

acumulado <- 0
i <- 0
while (acumulado < 500) {
    acumulado <- acumulado + runif(1)
    i <- i + 1
}

print(c(acumulado, as.integer(i)))

#5

PositivosCuadrados <- function(array){
    return(array[array >= 0] ** 2)
}

PositivosCuadrados(c(-1,0,2))

#6

A <- matrix(c(1,2,3,8), nrow = 2, ncol = 2)

A[2, ] <- (A[2, ] - A[1, ] * 2) / 2
A[1, ] <- A[1, ] - A[2, ] * 3

kerA <- list(A[, 1],A[, 2])


#7

DiagonalA <- function(a,n){
    return(diag(n) * a)
}

DiagonalA(2, 10)

#8

armarDiagonal <- function(array,n){
    return(diag(n) * array)
}

armarDiagonal(c(1,2,3,4,5,6,7,8,9,0), 10)

#9

data_astronautas <- read.csv("astronauts.csv")
str(data_astronautas)
data_astronautas$Birth.Date <- as.Date.character(data_astronautas$Birth.Date, tryFormats = c("%m/%d/%Y"))

data_astronautas$Gender <- factor(data_astronautas$Gender, names(table(data_astronautas$Gender)))
data_astronautas$Status <- factor(data_astronautas$Status, names(table(data_astronautas$Status)))
data_astronautas$Year <- factor(data_astronautas$Year, names(table(data_astronautas$Year)))
data_astronautas$Group <- factor(data_astronautas$Group, names(table(data_astronautas$Group)))



Space_Flight = data_astronautas$Space.Flights
table(Space_Flight)

hist(Space_Flight, breaks = "Sturges",
     freq = NULL,
     include.lowest = TRUE, right = TRUE, fuzz = 1e-7,
     density = NULL, col = "lightgray", border = NULL,
     main = "Space Flight",
     xlab = "# Space Flight",
     axes = TRUE, plot = TRUE, labels = FALSE)

cant_caminatas <- (data_astronautas$Space.Walks)
cant_vuelos <- (data_astronautas$Space.Flights)

data_astronautas$Name[which.max(cant_vuelos)]
data_astronautas$Name[which.min(cant_vuelos)]

plot(cant_caminatas,cant_vuelos, 
     main = "Cantidad de caminatas vs vuelos",
     xlab = "Cantidad de Caminatas",
     ylab = "Cantidad de Vuelos",
     type = "p")

cant_hs_caminatas <- (data_astronautas$Space.Walks..hr)
cant_hs_vuelos <- (data_astronautas$Space.Flights..hr)

plot(cant_hs_caminatas,cant_hs_vuelos, 
     main = "Cantidad de horas de caminatas vs vuelos",
     xlab = "Cantidad de hs en Caminatas",
     ylab = "Cantidad de hs en Vuelos",
     type = "S")

nombres <- names(data_astronautas)
static_dates <- data.frame(matrix(nrow = 4,ncol = 0), 
                           row.names = c("Maximo", "Minimo", "Suma", "Promedio"))
data_astronautas_numerico <- data.frame(matrix(nrow = 357,ncol = 0))
nombre_col <- c()
for (i in 1:length(nombres))
{
    
    if (class(data_astronautas[, i]) == "integer" | class(data_astronautas[, i]) == "numeric")
    {
        col_numerica <- data_astronautas[, i]
        maximo   <- max(col_numerica)
        minimo   <- min(col_numerica)
        suma     <- sum(col_numerica)
        promedio <- mean(col_numerica)
        data_astronautas_numerico <- cbind(data_astronautas_numerico, col_numerica)
        static_dates <- cbind(static_dates, c(maximo, minimo, suma, promedio))
        nombre_col <- c(nombre_col, nombres[i])
    }
    colnames(static_dates) <- nombre_col
}

static_dates <- data.frame(t(static_dates))
