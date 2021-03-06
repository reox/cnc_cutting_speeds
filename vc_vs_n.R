library(ggplot2)

drehzahl <- function(v_c, d){
    n_c <- (v_c * 1000) / (pi * d)
    return(n_c)
}

vorschub <- function(n, f_z, z){
    v_f <- n * f_z * z
    return(v_f)
}


# Plot v_c vs n_c
p <- ggplot(data.frame(x = c(10, 700)), aes(x)) + 
    scale_y_continuous(breaks=c(6000,12000,18000,24000)) +
    scale_x_continuous(breaks=c(10,50,100,200,300,365,450,500,550,600)) + 
    xlab(expression(v[c]~"[m/min]")) + ylab(expression(n[c]~"[1/min]")) + 
    coord_cartesian(ylim = c(6000, 24000)) +
    labs(title=bquote("cutting speed vs rotation speed for different cutter diameters"))


jpeg('drehzahl.jpg', height = 500, width = 1000, units = 'px')
print(p + 
      stat_function(fun=drehzahl, args=list(d=1)) + 
      stat_function(fun=drehzahl, args=list(d=2)) + 
      stat_function(fun=drehzahl, args=list(d=3)) + 
      stat_function(fun=drehzahl, args=list(d=4)) + 
      stat_function(fun=drehzahl, args=list(d=5)) + 
      stat_function(fun=drehzahl, args=list(d=6)) + 
      stat_function(fun=drehzahl, args=list(d=7)) + 
      stat_function(fun=drehzahl, args=list(d=8)) +
      geom_text(x=50, y=23800, label='d=1') +
      geom_text(x=125, y=23800, label='d=2') +
      geom_text(x=200, y=23800, label='d=3') +
      geom_text(x=275, y=23800, label='d=4') +
      geom_text(x=350, y=23800, label='d=5') +
      geom_text(x=425, y=23800, label='d=6') +
      geom_text(x=500, y=23800, label='d=7') +
      geom_text(x=575, y=23800, label='d=8')
  )
dev.off()


# Plot f_z vs v_f for z = 1
p <- ggplot(data.frame(x = c(0.01, 0.4)), aes(x)) + 
    xlab(expression(f[z]~"[mm/min]")) + ylab(expression(v[f]~"[mm/min]")) + 
    coord_cartesian(ylim = c(0, 2000), xlim=c(0.01, 0.4)) +
    labs(title=bquote("feed rate per tooth vs feed rate for different rotation speeds for z = 1"))


jpeg('vorschub.jpg', height = 500, width = 1000, units = 'px')
print(p + 
      stat_function(fun=vorschub, args=list(z=1,n=6000)) + 
      stat_function(fun=vorschub, args=list(z=1,n=12000)) + 
      stat_function(fun=vorschub, args=list(z=1,n=18000)) + 
      stat_function(fun=vorschub, args=list(z=1,n=24000)) +
      geom_text(x=0.07, y=2000, label='n=6000') +
      geom_text(x=0.0975, y=2000, label='n=12000') +
      geom_text(x=0.15, y=2000, label='n=18000') +
      geom_text(x=0.31, y=2000, label='n=24000')
  )
dev.off()
