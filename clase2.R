#Ejercicio 1 a

suma <- 0
for (i in 1:10000) {
    suma = suma + i
}
suma


#Ejercicio 1 b
n<-1
suma2<-0
while (suma2<10001) {
    suma2<-1
    for (i in 1:n) {
        suma2 = suma2 + i
    }
    n<-n+1
    suma2
}
n<-n-1

#Ejercicio 1 c



sumar_positivos<- function(array)
{
    result<-0
    for (i in array) {
        if(i>=0){
            result<- result + i
        }
    }
    return(result)
}

a<-sumar_positivos(c(0,0,0))
b<-sumar_positivos(c(0,0,1))
d<-sumar_positivos(c(0,-1,1))
e<-sumar_positivos(c(0,0,-1))
f<-sumar_positivos(c(5,-8,1))

#Ejercio 2

x_array <- seq(0,5,5/(100-1))
x <- seq(0,5,0.2)
y <- cos(x)
plot(x,y,xlab = "x",col ="white", ylab = "y", title("Trigonometricas"))
lines(x_array,cos(x_array),col="red")
lines(x_array,sin(x_array),col="blue")
lines(x_array,cos(x_array**2),col="green")


#Ejercio 3

data_auto <- read.table("autos.txt",header = TRUE)

tercera_fila <- data_auto[3,]
segunda_columna <- data_auto[,2]
mas_barato <- data_auto[which.min(data_auto$precio),]
suma_4_filas <- sum(data_auto[1:4,1])
suma_col<- apply(data_auto, 2 ,sum)
suma_row<- apply(data_auto, 1 ,sum)
data_auto_ord <- data_auto[order(data_auto$precio),]
test <- sort(data_auto$precio,index.return=TRUE)
test_auto <- data_auto[test$ix,]

plot(data_auto$precio,data_auto$calidad,col="blue", title("precio vs calidad"))


#Ejercio 4

data_mt_cars <- mtcars
mt_cars_4_gears <- data_mt_cars[data_mt_cars$gear==4,]
mt_cars_4_gears_name <- rownames(mt_cars_4_gears)
mt_cars_4_gears_y_manual <- data_mt_cars[data_mt_cars$gear==4 & data_mt_cars$am==1,]
mt_cars_4_gears_y_manual_len <- length(rownames(mt_cars_4_gears_y_manual)) 
mt_cars_4_gears_o_manual <- data_mt_cars[data_mt_cars$gear==4 | data_mt_cars$am==1,]
mt_cars_4_gears_o_manual_len <- length(rownames(mt_cars_4_gears_o_manual))
data_mt_cars$am <- factor(data_mt_cars$am)
