#install.packages("gtools");
#library("gtools");

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
  scale=ceiling(N^(1/dimension)); #podzia³ka siatki - sufit z pierwiastka z N stopnia dimension
  h=(b-a)/(scale-1);
  values=array(dim=scale);
  for(i in c(1:scale))
  {
    values[i]=a+(i-1)*h;
  }
  whichPoints=round(runif(dimension, 1, scale),digits=0)
  print(whichPoints);
  allPoints=permutations(scale, dimension, v=values, set=TRUE, repeats.allowed=TRUE)
  for(i in c(1:N)) #obcina sie nadwyzka punktow
  {
    points[[i]]=allPoints[i,];
  }
  return (points);
}

GetPointsPoissonDisc = function(dimension,N,a,b)
{
  points=list();
  for(i in c(1:N))
  {
    points[[i]]=runif(dimension, a, b);
  }
  return (points);
}