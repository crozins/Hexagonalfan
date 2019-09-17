##################################################################################  
# EXAMPLE 1 for the example in the supplimentary material (Fig. S5) 
# balanced.design.hex(0.25,1.2,7,1)  

#S<-33
#N<-7
#x<-.25
#delta=1.2
#key=1

 
##################################################################################  

balanced.design.hex <- function(x,delta,N,key,SpokePlot=F,Cartesian=F,GridLines=T){
  
  #set working directory 
  #setwd()
  
  #the minimum balanced design must have 33 spokes. 
  
  S=33
  #key=1;
  ###################################################
  ######        6 Balanced Designs    ###############
  ###################################################
  
  
  # key 1
  if(key==1){
    type2<-c(0,0,0,0,1,0,0,0,1,1,1,1,0,1,1,1) # has filler plant (augmented), top g type, spoke 2
    type1<-c(0,1,0,0,0,0,1,1,1,0,1,1,1,1,0,0,0) # middle , o type, spoke 1
  }
  # key 2
  if(key==2){
    type2<-c(0,0,0,1,1,1,1,0,1,1,1,0,0,0,0,1)
    type1<-c(0,1,1,1,0,1,1,1,1,0,0,0,1,0,0,0,0)
  }
  
  # key 5
  if(key==3){
    type2<-c(0,0,0,0,1,1,1,0,1,1,1,1,0,0,0,1)
    type1<-c(0,1,0,0,0,0,0,1,1,0,1,1,1,1,1,0,0)
  }
  # key 6
  if(key==4){
    type2<-c(0,0,1,1,1,1,1,0,1,1,0,0,0,0,0,1)
    type1<-c(0,1,0,0,0,1,1,1,1,0,1,1,1,0,0,0,0)
  }

  if(key==5){
    type2<-c(0,0,1,0,0,0,0,0,1,1,0,1,1,1,1,1)
    type1<-c(0,1,0,0,0,0,1,1,1,0,1,1,1,1,0,0,0)
  }
  # key 10
  if(key==6){
    type2<-c(0,0,0,1,1,1,1,0,1,1,1,0,0,0,0,1)
    type1<-c(0,1,1,1,1,1,0,1,1,0,0,0,0,0,1,0,0)
  }
  ##################################################################################  
  # Some math that needs to be calculated, see Supplimentary Material for details ##
  ##################################################################################
 
  
  # make sure S is at least 3
  if (S<3){
    S<-3
  }
  # determine the number of type 1 (t2) and type 1 (t1) spokes. Spokes are added right then left of the y-axis
  if (S%%2==0) {
    t2<-S/2
    t1<-S/2
  } else if ((S-1)%%4==0) {
    t2<-(S-1)/2
    t1<-(S-1)/2+1
  } else {
    t2<-(S-1)/2+1
    t1<-(S-1)/2  
  }
  
  
  r1<-x*(sqrt((1/delta^2)-(1/4)))+x*((sqrt(3)/2)+sqrt((1/delta^2)-(1/4)))*((1/delta^2)/(1-(1/delta^2))) #r1 is the distance from the origin, up the y-axis, to the first whorle (perpendicular, directly between two plants)
  theta<-atan(x/(2*r1)); # theta is the angle (at the origin) betweet two neighbouring spokes (type1 and type2)
  r2<-x/(2*sin(theta)); # r2 is the distance from the origin to the first whorl (inner circle of plants, bit farther than r1)
  
  # determine dots per spoke type, based on whorls N  
  # first make sure N is at least 2
  if (N<2){
    N<-2
  }
  # t2w=green whorls, t1w= orange whorls
  if (N%%2==0){
    t2w<-N/2-1 # -1 in all cases b/c the first 2 whorles are already set 
    t1w<-N/2-1
  } else {
    t2w<-(N-1)/2+1-1
    t1w<-(N-1)/2  -1
  }
  
  
  ############     WHORL 1  ; SPOKE TYPE 2 MATH         ############
  
  #remember, mathematically degree 0 is on the x-axis, we shift to the y-axis, hence pi/2-theta
  
  # two sets of angles, those right of the y-axis and those left of it. 
  Th1<-pi/2-seq(from = theta, to = (2*(ceiling(t2/2))-1)*theta, by = 2*theta) # if there are an odd numner of t2 spokes, put more to the right
  Th2<-pi/2+seq(from = theta, to=(2*(floor(t2/2))-1)*theta, by = 2*theta)
  #combines all the angles for the green spokes 
  th1=c(rev(Th1), Th2) # angles for all green spokes
  
  y1<-x*(sqrt(1-(delta^2)/4)+(delta*sqrt(3))/(2)); #distance b/w first two plants (green spoke)
  if (t2w>0){
    v<-rep(delta,t2w) #multipliers for number of dots along the green spokes
    ex<-seq(from = 0, to=2*t2w-1, by=2) #exponents
    m<-y1*(v^ex) # vector of y's: y1, y2, y3... 
    M<-cumsum(m) # add up distances to get total distance from origin
    r_t2<-c(r2, r2+M) #position of the dot along spoke type 2
  } else {
    r_t2<-r2
  }
  
  
  
  ############      WHORL 2 ; SPOKE TYPE 1 MATH         ############
  
  
  if (S>4){
    Th2.1<-pi/2-seq(from = 2*theta, to = 2*(ceiling((t1-1)/2))*theta, by = 2*theta) # right of y-axis, sub 1 for the one of the y-axis
    Th2.2<-pi/2+seq(from = 2*theta, to=2*(floor((t1-1)/2))*theta, by = 2*theta) # left of y-axis
    th2=c(rev(Th2.1),pi/2, Th2.2) 
  } else if (S==4){
    Th2.1<-pi/2-seq(from = 2*theta, to = 2*(ceiling((t1-1)/2))*theta, by = 2*theta) # right of y-axis, sub 1 for the one of the y-axis
    th2=c(rev(Th2.1),pi/2) # recall that a spoke 1 is on the y-axis.   
  } else {
    th2<-pi/2
  }
  
  r3<-r1+(x*sqrt(3))/2 #radius of the second row. r1 (dist to fist triangle)+height of triangle
  
  y2<-delta*y1 #distance between first two plants (delta more than y1, go figure)
  
  if (t1w>0){
    exo<-seq(from = 0, to=2*t1w-1, by=2) #exponents
    vo<-rep(delta,t1w) #multipliers for number of dots along the green spokes
    m2<-y2*(vo^exo) # vector of y's y1, y2, y3... for orange spoke (dist bw points)
    M2<-cumsum(m2)
    r_t1<-c(r3, r3+M2) #vector of all distance bw points on the spoke type 1
  } else {
    r_t1<-r3
  }
  
  
  ##################################################
  ############# PLOTTING SECTION  ##################
  ##################################################
  
  #preps a window 
  # quartz() # makes figure windown pop up.
  
  GetPlotSize=cartesian.plant(x,delta,r_t2,r_t1,t1,t2,th2,th1)
  
  X.max=max(GetPlotSize$Xcor)
  X.min=min(GetPlotSize$Xcor)
  Y.max=max(GetPlotSize$Ycor)
  Y.min=min(c(0,min(GetPlotSize$Ycor)))
  
  switch(Sys.info()[['sysname']],
         Windows= {windows()},
         Linux  = {x11()},
         Darwin = {quartz()})
  
  #pdf("Fig.pdf")
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  #plot(NA, xlab='', ylab='',axes = FALSE, xlim=c(-max(tail(r_t1,1),tail(r_t2)),max(tail(r_t1,1),tail(r_t2))), ylim=c(min(r_t1[1],r_t2[1]),max(tail(r_t1,1),tail(r_t2))), asp = 1)
  plot(NA, xlab='', ylab='',axes = FALSE, xlim = c(X.min, X.max), ylim = c(Y.min, Y.max), asp = 1)
  ##################################
  ##    PLOT GRID LINES         ####
  ##################################
  
  # The fanplot function plots the hexigons. 
  if (GridLines == T){
    fanplot(S,t2,t1,th1,th2,r_t1,r_t2)
  }

  
  ########   PLOTTING THE DOTS ######
  
  ## PLOT THE SPOKE 2 DOTS (first whorl type) ##
  
  # loop over all the spokes (th1) and the dots along the spokes (r_t2)
  for(i in 1:length(th1)){
    for(j in 1:length(r_t2)){
      X <- r_t2[j] * cos(th1[i]) #polar to cartesian x
      Y <-r_t2[j] * sin(th1[i]) #polar to cartesian y
      if (type2[i]==0){
        points(X, Y, type = 'b', pch = 20, col = "black")
      } else 
        points(X, Y, type = 'b', pch = 21, bg = "white", col = "black", 
               lwd = 0.9, cex = 1)
    }
  }
  
  ## PLOT THE SPOKE 2 DOTS (second whorl type) ##
  
  # loop over all the spokes (th2) and the dots along the spokes (r_t1)
  for(i in 1:length(th2)){
    for(j in 1:length(r_t1)){
      X <- r_t1[j] * cos(th2[i]); #polar to cartesian x
      Y <-r_t1[j] * sin(th2[i]); #polar to cartesian y
      if (type1[i]==0){
        points(X, Y, type = 'b', pch = 20, col = "black")
      } else 
        points(X, Y, type = 'b', pch = 21, bg = "white", col = "black", 
               lwd = 0.9, cex = 1)
    }
  }
  
  

  
  
  
  ###################################
  ######## PLOTTING THE SPOKES ######
  ###################################
  # this gets x and y corr of the top of each spoke 2 type
  xcor<-tail(r_t2,1)*cos(th1) 
  ycor<-tail(r_t2,1)*sin(th1)
  
  # this gets x and y corr of the to top of each spoke 1 type
  xcor.orange<-tail(r_t1,1)*cos(th2) 
  ycor.orange<-tail(r_t1,1)*sin(th2)
  
  
  ## 
  if (SpokePlot == T){
    segments(0, 0, xcor, ycor, col="black",lwd=1,lty=2) # spoke type 1
    segments(0, 0, xcor.orange, ycor.orange, col="black",lwd=1) # spoke type 2
  }
  
  
  
  if (Cartesian == T){
    # Uncomment to save cartesian (x,y) coordinates of all plants, spoke by spoke from left to right
    XYPlantPositionTable<-cartesian.plant(x,delta,r_t2,r_t1,t1,t2,th2,th1)
    print(XYPlantPositionTable)
    xside <- max(XYPlantPositionTable$Xcor) + abs(min(XYPlantPositionTable$Xcor))
    yside <- max(XYPlantPositionTable$Ycor)
    
    dimensions <- c(x=xside,y=yside)
    print("Fan box dimensions are: ")
    print(dimensions)
    write.csv(XYPlantPositionTable, file="XYPlantPositionTable.csv",row.names=FALSE) # not working
  }
  
  
  
  ######################################################
  # EXPORT PLANT SPACING ON SPOKES AND MAX/MIN SPACING #
  ######################################################
  
  if (length(r_t1)==length(r_t2)){
    spokes=cbind(r_t1,r_t2)
  } else {
    r_new=c(r_t1,NA)
    spokes=cbind(r_new,r_t2) # if there are fewer plants on one spoke type, add NA
  }
  colnames(spokes) <- c("Spoke1","Spoke2")
  
  
  # also return the distance between first two and last two plants on spokes of the same colour. recall x is the distance bw the first green spoke plants. 
  Type2Dist<-c(x,x*delta^(2*(length(r_t2)-1)))
  Type1Dist<-c(delta*x,(delta*x)*delta^(2*(length(r_t1)-1)))
  
  Min.Max.Dist=cbind(Type1Dist,Type2Dist)
  colnames(Min.Max.Dist) <- c("Spoke1.Min.Max","Spoke2.Min.Max")
  
  
  newList <- list("SPOKES" = spokes, "MAX.MIN.DIST" = Min.Max.Dist)
  return(newList)
}

##########################################################
# this outputs the cartesian coordinates of all plants. ##
#   and the rectange dimensions that contain the fan    ##
##########################################################

cartesian.plant <- function(x,delta,r_t2,r_t1,t1,t2,th2,th1){
  
  
  
  
  
  if (max(th2)>max(th1)){
    pointsTableType1 <- (matrix(ncol=3,nrow=t2*length(r_t2)))
    for(i in 1:length(th1)){
      for(j in 1:length(r_t2)){
        X <- rev(r_t2)[j] * cos(rev(th1)[i]) #polar to cartesian x
        Y <- rev(r_t2)[j] * sin(rev(th1)[i]) #polar to cartesian y
        pointsTableType1[length(r_t2)*(i-1)+j,2:3] = c(X,Y)
      }
    }
    pointsTableType1 <- data.frame(pointsTableType1)
    names(pointsTableType1) <- c("spoke","Xcor","Ycor")
    pointsTableType1$spoke = paste0(rep(seq(from=2, to=(t1+t2), by=2) ,each=length(r_t2)))
    #print(pointsTableType1)
    
    pointsTableType2 <- (matrix(ncol=3,nrow=t1*length(r_t1)))
    for(i in 1:length(th2)){
      for(j in 1:length(r_t1)){
        X <- rev(r_t1)[j] * cos(rev(th2)[i]) #polar to cartesian x
        Y <- rev(r_t1)[j] * sin(rev(th2)[i]) #polar to cartesian y
        pointsTableType2[length(r_t1)*(i-1)+j,2:3] = c(X,Y)
      }
    }
    pointsTableType2 <- data.frame(pointsTableType2)
    names(pointsTableType2) <- c("spoke","Xcor","Ycor")
    pointsTableType2$spoke = paste0(rep(seq(from=1, to=(t1+t2), by=2) ,each=length(r_t1)))
    #print(pointsTableType2)
    total <- rbind.data.frame(pointsTableType1,pointsTableType2)
    #print(total)
    total2<-total[order(total$spoke),] # reordr by spoke number
    return(total2)
  } else {
    pointsTableType1 <- (matrix(ncol=3,nrow=t2*length(r_t2)))
    for(i in 1:length(th1)){
      for(j in 1:length(r_t2)){
        X <- rev(r_t2)[j] * cos(rev(th1)[i]) #polar to cartesian x
        Y <- rev(r_t2)[j] * sin(rev(th1)[i]) #polar to cartesian y
        pointsTableType1[length(r_t2)*(i-1)+j,2:3] = c(X,Y)
      }
    }
    pointsTableType1 <- data.frame(pointsTableType1)
    names(pointsTableType1) <- c("spoke","Xcor","Ycor")
    pointsTableType1$spoke = paste0(rep(seq(from=1, to=(t1+t2), by=2) ,each=length(r_t2)))
    print(pointsTableType1)
    
    print(t1*length(r_t1))
    pointsTableType2 <- (matrix(ncol=3,nrow=t1*length(r_t1)))
    for(i in 1:length(th2)){
      for(j in 1:length(r_t1)){
        X <- rev(r_t1)[j] * cos(rev(th2)[i]) #polar to cartesian x
        Y <- rev(r_t1)[j] * sin(rev(th2)[i]) #polar to cartesian y
        pointsTableType2[length(r_t1)*(i-1)+j,2:3] = c(X,Y)
      }
    }
    pointsTableType2 <- data.frame(pointsTableType2)
    names(pointsTableType2) <- c("spoke","Xcor","Ycor")
    pointsTableType2$spoke = paste0(rep(seq(from=2, to=(t1+t2), by=2) ,each=length(r_t1)))
    print(pointsTableType2)
    total <- rbind.data.frame(pointsTableType1,pointsTableType2)
    #print(total)
    total2<-total[order(total$spoke),]
    xside <- max(pointsTableType2$Xcor) + abs(min(pointsTableType2$Xcor))
    yside <- max(pointsTableType2$Ycor)
    
    dimensions <- c(x=xside,y=yside)
    print("Fan box dimensions are: ")
    print(dimensions)
    
    return(total2)
  }
  
  
}


# this plots the hexigons

# x = min plant distance
# delta = multimplier
# S = spokes
# N = whorls
fanplot<-function(S,t2,t1,th1,th2,r_r,r_t2){
  
  # Determine all the connections for the hexigons
  if (S==3){
    kg<-c(1,2)
    ko<-c(1,1)
  } else if (S==4){
    kg<-c(1,1,2)
    ko<-c(1,2,2)
  } else if(((S+1)%%2) == 0){
    if (t2<t1){
      kg<-rep(1:t2, each=2)
      ko<-c(1, rep(2:(t1-1), each=2),t1)
    } else {
      kg<-c(1, rep(2:(t2-1), each=2),t2)
      ko<-rep(1:t1, each=2)
    }
  } else if (S%%2==0){ 
    if (S%%4 == 0){
      kg<-c(rep(1:(t2-1),each=2),t2)
      ko<-c(1,rep(2:t1, each=2))
    } else {
      kg<-c(1,rep(2:t2, each=2))
      ko<-c(rep(1:(t1-1),each=2),t1)
    }
  } else { 
    if (((S-1)%%4) ==0){  
      kg<-rep(1:t2, each=2)
      ko<-c(1,rep(2:(t1-1), each=2),t1)  
    } else {
      kg<-c(1,rep(2:(t2-1), each=2),t2)
      ko<-rep(1:t1, each=2)  
    }
    
  }
  
  
  n.th1 <- seq(length(th1)-1) 
  n.th2 <- seq(length(th2)-1) 
  
  
  
  # connect type 1 to type 2 dots
  for(l in 1:length(r_r)){
    segments(r_r[l] * cos(th2[ko]), r_r[l] * sin(th2[ko]), r_t2[l] * cos(th1[kg]), r_t2[l] * sin(th1[kg]), col="black",lwd=1)
  }
  
  # diagonal lines from type 2 down to type 1 dots
  for(f in 2:length(r_t2)){
    segments(r_t2[f] * cos(th1[kg]), r_t2[f] * sin(th1[kg]), r_r[f-1] * cos(th2[ko]),r_r[f-1] * sin(th2[ko]), col="black",lwd=1)
  }
  
  #connect the spoke type 2 dots of same distance from origin
  for(k in 1:length(r_t2)){    
    segments(r_t2[k] * cos(th1[n.th1]), r_t2[k] * sin(th1[n.th1]), r_t2[k] * cos(th1[n.th1+1]),r_t2[k] * sin(th1[n.th1+1]), col="black",lwd=1)
  }
  
  #connect the spoke 1 type dots of same distance from origin
  for(l in 1:length(r_r)){    
    segments(r_r[l] * cos(th2[n.th2]), r_r[l] * sin(th2[n.th2]), r_r[l] * cos(th2[n.th2+1]),r_r[l] * sin(th2[n.th2+1]), col="black",lwd=1)
  }
  
  # ################################
  
}
