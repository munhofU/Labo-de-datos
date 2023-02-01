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

load("/home/clinux01/Escritorio")
