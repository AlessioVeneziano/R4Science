##################################################################
##################################################### DIY AMMONITE

spiral<-function(n=100,turns=10,a=1,b=0.1,center=c(0,0,0)){
  th<-seq(0,turns*pi,length=n)
  r<-a*exp(b*th)
  
  x<-center[1]+r*cos(th)
  y<-center[2]+r*sin(th)
  z<-center[3]
  
  return(list(xyz=cbind(x,y,z),radii=r))
}
circle<-function(n=30,r=1,center=c(0,0,0)){
  th<-seq(0,2*pi,length=n+1)
  th<-th[-length(th)]
  
  x<-center[1]+r*cos(th)
  y<-center[2]+r*sin(th)
  z<-center[3]
  
  return(cbind(x,y,z))
}
normalVector<-function(v1,v2){
  m<-outer(v1,v2,"*")
  nor<-c(m[2,3]-m[3,2],
         m[3,1]-m[1,3],
         m[1,2]-m[2,1])
  
  return(nor)
}
angleVector<-function(v1,v2){
  a<-acos(v1 %*% v2/(sqrt(v1 %*% v1) * sqrt(v2 %*% v2)))
  
  return(c(a))
}
triAmmonite<-function(pts,nsection){
  pos<-1:nrow(pts)
  
  tr0<-c(1,nsection,nsection+1)
  tr1<-cbind(pos,nsection+pos,nsection+pos+1)
  tr1<-ifelse(tr1>max(pos),NA,tr1)
  tr1<-na.omit(tr1)
  tr2<-cbind(pos,pos+1,nsection+pos+1)
  tr2<-ifelse(tr2>max(pos),NA,tr2)
  tr2<-na.omit(tr2)
  tr<-rbind(tr0,tr1,tr2)
  
  return(tr)
}

diyAmmonite<-function(nspiral=200,nsection=30,turns=10,a=1.0,
                      b=0.13,thick=2,center=c(0,0,0)){
  require(rgl)
  
  spi<-spiral(n=nspiral,turns=turns,a=a,b=b)
  xyz<-spi$xyz
  r<-spi$radii
  
  rsection<-r/thick
  
  pts<-NULL
  for(i in length(rsection):2){
    sect<-circle(n=nsection,r=rsection[i],center=xyz[i,c(1,3,2)])
    sect<-sect[,c(1,3,2)]
    
    v1<-xyz[i,]-sect[1,]
    v2<-xyz[i,]-sect[round(nsection/4),]
    nor<-normalVector(v1,v2)
    dif<-xyz[i,]-xyz[i-1,]
    
    ang<-angleVector(nor,dif)
    if(dif[1]<0){ang<- -ang}
    
    rot<-rotate3d(sect,-ang,0,0,1)
    tr<-xyz[i,]-apply(rot,2,mean)
    rot<-translate3d(rot,tr[1],tr[2],tr[3])
    
    pts<-rbind(pts,rot)
  }
  
  tri<-triAmmonite(pts,nsection)
  mesh<-mesh3d(vertices=t(cbind(pts,1)),triangles=t(tri))
  
  return(list(pts=pts,mesh=mesh))
}


#################################################### END OF SCRIPT
##################################################################

