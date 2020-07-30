# Replication file for: "Using R to Find Strange Attractors"
# RPubs-link: https://rpubs.com/mstefan-rpubs/chaos
# (c) Martin Stefan, July 2020

# 2D equation function
eqSys2d <- function(a=rep(0,12), n=1e5, p0=rep(1/1e6,2)){
  p <- matrix(NA,ncol=2,nrow=n)
  p[1,] <- p0
  for(i in 2:n){
    x = p[i-1,1]
    y = p[i-1,2]
    p[i,1] <- a[1] + a[2]*x + a[3]*x^2 + a[4]*x*y + a[5]*y + a[6]*y^2
    p[i,2] <- a[7] + a[8]*x + a[9]*x^2 + a[10]*x*y + a[11]*y + a[12]*y^2
  }
  return(list("parameters"=a,"points"=p,"start"=p0))
}

# 3D equation function
eqSys3d <- function(a=rep(0,30), n=1e5, p0=rep(1e-5,3), dt=1e-3){
  p <- matrix(NA,ncol=3,nrow=n)
  p[1,] <- p0
  for(i in 2:n){
    x = p[i-1,1]
    y = p[i-1,2]
    z = p[i-1,3]
    dx <-  a[1] +a[2]*x +a[3]*y +a[4]*z +a[5]*x^2 +a[6]*y^2 +a[7]*z^2 +a[8]*x*y +a[9]*x*z+a[10]*y*z
    dy <- a[11]+a[12]*x+a[13]*y+a[14]*z+a[15]*x^2+a[16]*y^2+a[17]*z^2+a[18]*x*y+a[19]*x*z+a[20]*y*z
    dz <- a[21]+a[22]*x+a[23]*y+a[24]*z+a[25]*x^2+a[26]*y^2+a[27]*z^2+a[28]*x*y+a[29]*x*z+a[30]*y*z
    p[i,1] = p[i-1,1] + dt*dx
    p[i,2] = p[i-1,2] + dt*dy
    p[i,3] = p[i-1,3] + dt*dz
  }
  return(list("parameters"=a,"points"=p,start="p0"))
}


# 2D plot function
plotSys2d <- function(p, dropfirst=T, 
                      pch=".", cex=1, 
                      color="goldenrod2", 
                      bg="black"){
  
  if(dropfirst) p <- p[((nrow(p) %/% 9) : nrow(p)), ]
  
  par(mar=rep(1,4), bg=bg, bty="n")
  plot(p, cex=cex,
       pch=pch, col=color,
       xlab="", ylab="", 
       xaxt="n", yaxt="n")    
  
}


# plot function
plotSys3d <- function(p, dropfirst=T, 
                    pch=".", 
                    color="white",
                    bg="black",
                    theta=20, phi=10){
  
  # drop initial observations
  if(dropfirst) p <- p[((nrow(p) %/% 9) : nrow(p)), ]
  
  # plot
  library(plot3D)
  par(bg = bg, bty="n")
  scatter3D(p[,1],p[,2],p[,3], 
            pch=pch, bty="n",
            colkey=F, axes=F,
            theta=theta, phi=phi,
            col=color
  )
  
  
}

# animation function
animateSys3d <- function(p, dropfirst=T, 
                       size=.2, 
                       color="white",
                       bg="black",
                       window=c(50, 50, 800, 800),
                       rotate=c(1,1,1), speed=2){
  
  # drop initial observations
  if(dropfirst) p <- p[((nrow(p) %/% 9) : nrow(p)), ]
  
  # plot
  library(rgl)
  rgl.bg(color=bg)
  points3d(x=p[,1], y=p[,2], z=p[,3],
           axes=F, xlab="", ylab="", zlab="",
           size=size, col=color)
  
  # rotate
  if(!rgl.useNULL())
    play3d(spin3d(axis=rotate, rpm=speed))
  
}
