main=function() 
{
  dimensions=c(2,5,10,20,30,40,50,60,70,80,90,100);
  functions=c(1:28);
  N=10; #N-ile punktow
  delta=0.6;
  a=-100;
  b=100;
  results=array(dim = c(length(dimensions),3,length(functions),N));
  
  for(d in dimensions)
  {
    indx=match(d,dimensions);
    startPointsUniformDistribution=GetPointsUniformDistribution(d,N,a,b);
    startPointsHyperMesh=GetPointsHyperMesh(d,N,a,b);
    startPointsPoissonDisc=GetPointsPoissonDisc(d,N,a,b);
    for(f in functions)
    {
      for(p in c(1:N))
      {
        point=startPointsUniformDistribution[[p]];
        results[indx,1,f,p]=hillClimbing(point,f,delta)[1000];
      }
      for(p in c(1:N))
      {
        point=startPointsHyperMesh[[p]];
        results[indx,2,f,p]=hillClimbing(point,f,delta)[1000];
      }
      for(p in c(1:N))
      {
        point=startPointsPoissonDisc[[p]];
        results[indx,3,f,p]=hillClimbing(point,f,delta)[1000];
      }
    }
  }
  return (results);
  
}