#classical hill climbing algorithm
hillClimbing = function(x0,func,delta,A,B) {
  counter=1;
  iteration=0;
  numberOfSavedPoints=1000;
  H=array(dim=numberOfSavedPoints) #list of cec2013 function values for the best individual in population (we save 1000 values)
  x=x0;
  H[counter]=cec2013(func,x0);
  while(counter<numberOfSavedPoints) #we need to collect 1000 observations
  {
    y=randomNeighbor(x, delta,A,B);
    if(cec2013(func,y)>cec2013(func,x)) 
      x=y; 

    iteration=iteration+1;
    if((iteration%%1000)==0) #we save observation every 1000 iterations
    {
      counter=counter+1;
      H[counter]=cec2013(func,x);
    }
  }
  return (H);
}
