randomNeighbor=function(point, delta)
{
  newPoint=point+runif(length(point), -delta, delta);
  return (newPoint);
}

#Funkcja randomNeighbor zwraca losowy punkt w sasiedztwie x-a, znajdujacy
#sie w odleg�osci nie wiekszej niz delta od x-a. Do obliczania odleg�osci stosowana
#bedzie metryka euklidesowa. --> nie do konca sie zgadza 
#narazie jest odleglosc delta ze wzgledu na kazda wspolrzedna