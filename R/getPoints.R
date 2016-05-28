source('./poissonDisc.R')
GetPointsUniformDistribution = function(dimension,N,a,b)
{
  points=list();
  for(i in c(1:N))
  {
    points[[i]]=runif(dimension, a, b);
  }
  return (points);
}

GetPointsHyperMesh = function(dimension,N,a,b)
{
  points=list();
  scale=ceiling(N^(1/dimension)); #scale of mesh (because scale^dimension=N)
  if(scale<2)
    scale=2; #we need at least 2 points on scale
  h=(b-a)/(scale-1);
  values=array(dim=scale);
  for(i in c(1:scale))
  {
    values[i]=a+(i-1)*h;
  }
  for(i in c(1:N)) 
  { 
    whichValues=round(runif(dimension, 1, scale),digits=0); #we randomly choose points from mesh 
    points[[i]]=values[whichValues];
  }
  return (points);
}

GetPointsPoissonDisc = function(dimension,N,a,b)
{
  if((dimension!=2) & (dimension!=5)) 
    return ();
  points=list();
  if(dimension==2)
    accuracy=50;
  if(dimension==5)
    accuracy=125;
  repeat 
  {
    if(dimension==2)
      newPoints=GetPointsPoissonDisc2D(accuracy);
    if(dimension==5)
      newPoints=GetPointsPoissonDisc5D(accuracy);
    if (length(newPoints)>=N)
    {
      break
    }
    accuracy=accuracy-3;
  }
  
  for(i in c(1:N)) #we need only N points
  { 
    points[[i]]=newPoints[[i]];
  }
  return (points);
}