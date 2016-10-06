# Aanwezige data verwijderen
rm(list=ls(all=TRUE))

# Data De Vylder inlezen

rm(list=ls(all=TRUE)) ## Discard old garbage
Xij <- scan(n=60)
     0      0      0     0      0  4627
     0      0      0     0  15140 13343
     0      0      0 43465  19018 12476
     0      0 116531 42390  23505 14371
     0 346807 118035 43784  12750 12284
308580 407117 132247 37086  27744     0
358211 426329 157415 68219      0     0
327996 436744 147154     0      0     0
377369 561699      0     0      0     0
333827      0      0     0      0     0

# Q1 Filling the dots

i <- rep(1:10, each=6) ## the row nrs are (1,1,1,1,1,1,2,2,2,2,2,2,...)
j <- rep(1:6,10)       ## the col nrs are (1,2,3,4,5,6,1,2,3,4,5,6,...)
k <- i+j-1             ## the calendar year of the payments
future <- which(k>10)  ## TRUE for obs with calendar year after now
valid <- which(Xij!=0) ## 1 for the non-zero obs, 0 for zero obs
