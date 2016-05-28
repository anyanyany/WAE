#classical hill climbing algorithm
hillClimbing = function(x0,func,delta,A,B,extreme) {
  counter=1;
  H=list();
  #list of cec2013 function values for the best individual in population 
  x0 = as.vector(x0)
  x=x0;
  H[[counter]]=cec2013(func,x0);
  repeats=0;
  while(TRUE) 
  {
    if(abs(H[[counter]] - extreme) < 0.5) #if it's almost extreme - we can stop
      break;
    y=randomNeighbor(x, delta,A,B);
    if(cec2013(func,y)<cec2013(func,x))
      x=y;

    counter=counter+1;
    H[[counter]]=cec2013(func,x);
    
    if(H[[counter]]==H[[counter-1]])
    {
      repeats=repeats+1;
    }
    else
    {
      repeats=0;
    }
    
    if(repeats==100) #if value doesn't change long time - break
      break;
  }
  return (H);
}
