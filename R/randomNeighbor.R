attr(randomNeighbor, "comment") <- "Returns new point lying not further than delta from point"
randomNeighbor = function(point, delta,A,B)
{
  # For algorithm explanation look here:
  # https://en.wikipedia.org/wiki/N-sphere#Spherical_coordinates
  dim = length(point)                 # getting dimension number
  newPoint = c(1:dim)                 # prepare new point
  rand = runif(dim, 0, 1)             # get randoms for calculations
  radius = delta * rand[1]            # get radius within n-sphere
  angles = c(1:(dim-1))               # vector of angles
  sinValue = 1                        # cumulative value of sin() multiplication

  # Getting all newPoint coordinates
  for(i in c(1:(dim-1)))
  {
    angles[i] = 2 * pi * rand[i+1]
    newPoint[i] = point[i] + radius * cos(angles[i]) * sinValue
    sinValue = sinValue * sin(angles[i])
  }
  newPoint[dim] = point[dim] + radius * sinValue

  # Fixing coordinates
  for(i in c(1:dim))
    newPoint[i] = min(B, max(newPoint[i], A))

  return (newPoint)
}
