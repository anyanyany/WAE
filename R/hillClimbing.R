hillClimbing = function(x0,func,delta) {
  counter=1;
  H=list(); #lista zawiera wartosc fukcji celu dla kazdego "lepszego" punktu
  x=x0;
  H[[counter]]=cec2013(func,x0);
  while(counter<1000)
  {
    y=randomNeighbor(x, delta);
    if(cec2013(func,y)>cec2013(func,x)) 
    {
      x=y; 
      counter=counter+1;
      H[[counter]]=cec2013(func,x);
    }	
  }
  return (H);
}