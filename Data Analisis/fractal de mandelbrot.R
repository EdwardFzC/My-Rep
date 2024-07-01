library(tidyverse)

# Modulo de un numero complejo
modulo_complex <- function(a,b) {
  return(sqrt(a**2+b**2))
}

#  funcion que construye de forma recursiva el punto zn+1
forma_recursiva <- function(x,y,max_i=100) {
  a<-0
  b<-0
  z<-0
  i<-0
  while (i<max_i && z<=4) {
    a_next<-a**2-b**2+x
    b_next=2*a*b+y
    a<-a_next
    b<-b_next
    z=modulo_complex(a_next,b_next)
    i=1+i
  }
return(i)
}



# Hacer que funcione con vectores
forma_recursiva_vec <- function(v1,v2,i_max=100) {
  return(mapply(forma_recursiva,v1,v2,i_max))
}

forma_recursiva_vec(2:6,3:7)


dominio2 <- function(xi,yi,xf,yf,step) {
  x<-seq(xi,xf,by=step)
  y<-seq(yi,yf,by=step)
  df <- data.frame()
  for (i in 1:length(x)) {
    df2<-data.frame(x=rep(x[i],length(y)),
                    y=y)
    df<- rbind(df,df2)
  }
  return(df)
}

dominio(2,2,4,5,1)
dominio2(2,2,4,5,1)
# Genera todos los puntos x,y. tambien los pasos para que cumpla con las condiciones i<100 & z<=4
mandelbrot_conjunto <- function(xi,yi,xf,yf,step,i_max=100) {
  df<-dominio2(xi,yi,xf,yf,step)
  df$iteraciones <- forma_recursiva_vec(df$x, df$y, i_max)
  return(df)
}
ej4<-mandelbrot_conjunto(1,2,60,80,5,50)


resultado <- mandelbrot_conjunto(-2, -2, 2, 2, 0.02,200)


graficar <- function(data) {
  graf<-ggplot(NULL, aes(x=data[[1]],y=data[[2]], color=data[[3]]))+ 
    geom_point()+
    labs(x='x',y='y',color='iteraciones')
  return(graf)
}

graficar(resultado)

mandelbrot <- function(xi,yi,xf,yf,steps, i_max=100) {
  resultado<-mandelbrot_conjunto(xi,yi,xf,yf,steps,i_max)
  return(graficar(resultado))
}
mandelbrot(-2,-1,1,1,0.01,100)




#library(plotly)


#p <- ggplot(resultado, aes(x=x, 
 #                    y=y, 
  #                   color=iteraciones)) +
  #geom_point(size=3) +
  #labs(x = "x",
   #    y = "y",
    #   color = "i")

#ggplotly(p)

