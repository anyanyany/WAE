main=function() 
{
  n=10; #n-ile punktow, dim-ile wymiarow
  dim=5;
  delta=0.6;
  func=1;
  
  startPoints=list();
  values=list();
  for(i in c(1:n))
    {
      startPoints[[i]]=runif(dim, -100, 100); #rozklad rownomierny
      values[[i]]=hillClimbing(startPoints[[i]],func,delta)[1000];
    }
  return (values);
}