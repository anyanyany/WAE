#classical hill climbing algorithm
maxIterations = 100

hillClimbing = function(x0,func,delta,A,B) {
  counter=1;
  iteration=0;
  numberOfSavedPoints=50;
  H=array(dim=numberOfSavedPoints)
  #list of cec2013 function values for the best individual in population (we save 1000 values)
  x=x0;
  H[counter]=cec2013(func,x0);
  #cat("Saved point: ", counter, "\n")
  while(counter<numberOfSavedPoints) #we need to collect 1000 observations
  {
    y=randomNeighbor(x, delta,A,B);
    if(cec2013(func,y)>cec2013(func,x))
      x=y;

    iteration=iteration+1;
    if((iteration%%maxIterations)==0) #we save observation every 1000 iterations
    {
      counter=counter+1;
      H[counter]=cec2013(func,x);
      #cat("Saved point: ", counter, "\n")
    }
  }
  return (H);
}
