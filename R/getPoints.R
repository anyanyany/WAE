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
  scale=ceiling(N^(1/dimension)); #podzialka siatki - sufit z pierwiastka z N stopnia dimension
  if(scale<2)
    scale=2;
  h=(b-a)/(scale-1);
  values=array(dim=scale);
  for(i in c(1:scale))
  {
    values[i]=a+(i-1)*h;
  }
  for(i in c(1:N)) 
  {
    whichValues=round(runif(dimension, 1, scale),digits=0);
    points[[i]]=values[whichValues];
  }
  return (points);
}

GetPointsPoissonDisc = function(dimension,N,a,b) #TODO
{
  points=list();
  for(i in c(1:N))
  {
    points[[i]]=runif(dimension, a, b);
  }
  return (points);
}