hillClimbing = function(x0,func,delta) {
  counter=1;
  iteration=0;
  numberOfSavedPoints=1000;
  H=array(dim=numberOfSavedPoints) #lista zawiera wartosci funkcji celu dla najlepszego osobnika z populacji (zapisujemy 1000 takich obserwacji)
  x=x0;
  H[counter]=cec2013(func,x0);
  while(counter<numberOfSavedPoints)
  {
    y=randomNeighbor(x, delta);
    if(cec2013(func,y)>cec2013(func,x)) 
      x=y; 

    iteration=iteration+1;
    if((iteration%%1000)==0)
    {
      counter=counter+1;
      H[counter]=cec2013(func,x);
    }
  }
  return (H);
}