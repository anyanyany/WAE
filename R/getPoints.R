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
  for(i in c(1:N))
  {
    points[[i]]=runif(dimension, a, b);
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