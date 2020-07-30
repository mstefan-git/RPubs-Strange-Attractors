# Replication file for: "Using R to Find Strange Attractors"
# RPubs-link: https://rpubs.com/mstefan-rpubs/chaos
# (c) Martin Stefan, July 2020

rm(list = ls())

# source functions
source("02_functions.R")

# henon map 
henon <- eqSys2d(a=c(1,0,-1.4,0,1,0,0,.3,0,0,0,0),n=1e4)
plotSys2d(henon$points)

# henon map (with pch=x)
henon <- eqSys2d(a=c(1,0,-1.4,0,1,0,0,.3,0,0,0,0),n=1e3)
plotSys2d(henon$points, pch="x", color="firebrick")

# tinkerbell map
tinkerbell <- eqSys2d(a=c(0,.9,1,0,-.6013,-1,0,2,0,2,.5,0),n=1e5)
plotSys2d(tinkerbell$points, color="limegreen")

# lorenz attractor
lorenz <- eqSys3d(a=c(0, -10, 10, 0, 0, 0, 0, 0, 0, 0,
                      0, 28, -1, 0, 0, 0, 0, 0, -1, 0, 
                      0, 0, 0, -8/3, 0, 0, 0, 1, 0, 0))
plotSys3d(lorenz$points, color="white")
