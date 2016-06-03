A = -100
B = 100
x = list()
y = list()

plotPoints = function(accuracy)
{
    plot(A:B, A:B, type="n", ylim=c(A,B))
    grid = generatePoints(accuracy, 2)
    x = list()
    y = list()
    for(i in c(1:length(grid[1,,1])))
    {
        for(j in c(1:length(grid[1,,1])))
        {
            if(all(grid[i, j,] == c(-1, -1)) == FALSE)
            {
                point = grid[i, j, ]
                x[[length(x) + 1]] <- point[1]
                y[[length(y) + 1]] <- point[2]
            }
        }
    }

    plot(x, y, type="p")
}

GetPointsPoissonDisc2D = function(accuracy)
{
  grid = generatePoints(accuracy, 2)
  x = list()
  y = list()
  for(i in c(1:length(grid[1,,1])))
  {
    for(j in c(1:length(grid[1,,1])))
    {
      if(all(grid[i, j,] == c(-1, -1)) == FALSE)
      {
        point = grid[i, j, ]
        x[[length(x) + 1]] <- point[1]
        y[[length(y) + 1]] <- point[2]
      }
    }
  }
  points=list();
  for(i in c(1:length(x)))
  {
    points[[i]]=c(x[[i]],y[[i]]);
  }
  return (points);
}

GetPointsPoissonDisc5D = function(accuracy)
{
  grid = generatePoints(accuracy, 5)
  x = list()
  y = list()
  z = list()
  u = list()
  v = list()
  for(i in c(1:length(grid[1,,1,1,1,1])))
  {
    for(j in c(1:length(grid[1,,1,1,1,1])))
    {
      for(k in c(1:length(grid[1,,1,1,1,1])))
      {
        for(l in c(1:length(grid[1,,1,1,1,1])))
        {
          for(m in c(1:length(grid[1,,1,1,1,1])))
          {
            if(all(grid[i, j, k, l, m, ] == c(-1, -1, -1, -1, -1)) == FALSE)
            {
              point = grid[i, j, k, l, m, ]
              x[[length(x) + 1]] <- point[1]
              y[[length(y) + 1]] <- point[2]
              z[[length(z) + 1]] <- point[3]
              u[[length(u) + 1]] <- point[4]
              v[[length(v) + 1]] <- point[5]
            }
          }
        }
      }
    }
  }
  points=list();
  for(i in c(1:length(x)))
  {
    points[[i]]=c(x[[i]],y[[i]],z[[i]],u[[i]],v[[i]]);
  }
  return (points);
}

pointToGridCell = function(point, cellSize)
{
    gridCell = c(1:length(point))
    for(i in c(1:length(point)))
        gridCell[i] = floor((point[i] - A) / cellSize) + 1

    return(gridCell)
}

getGridVector = function(grid, position, dim)
{
    result = c(1:dim)
    len = length(position) + 1
    for(i in c(1:dim))
    {
        position[len] = i
        result[i] = grid[t(position)]
    }
    length(position) <- (len - 1)

    return (result)
}

setGridVector = function(grid, position, dim, input)
{
    len = length(position) + 1
    for(i in c(1:dim))
    {
        position[len] = i
        grid[t(position)] = input[i]
    }
    length(position) <- (len - 1)

    return (grid)
}

generatePoints = function(minDist, dim)
{
    cellSize = minDist / sqrt(dim)
    cellNum = round((B - A) / cellSize)
    grid = array(rep(-1, dim), dim = c(rep(cellNum + 1, dim), dim))

    activeList = list()
    initialPoint = runif(dim, A, B)
    position = pointToGridCell(initialPoint, cellSize)
    setGridVector(grid, position, dim, position)
    activeList[[length(activeList) + 1]] <- initialPoint
    grid = poissonDisc(minDist, cellSize, cellNum+1, grid, activeList, dim)

    return (grid)
}

poissonDisc = function(minDist, cellSize, cellNum, grid, activeList, dim)
{
    # Until active list is not empty
    while(length(activeList) > 0 )
    {
        # Randomize current active point
        listLength = length(activeList)
        index = round(runif(1, 1, listLength))
        activePoint = activeList[[index]]
        activeList[[index]] = NULL
        isStillActive = FALSE

        # Select k neighbours for point
        for(i in c(1:30))
        {
            # Get new coordinates
            nPoint = nearPoint(activePoint, minDist)
            nPointCell = pointToGridCell(nPoint, cellSize)
            delta = ceiling(minDist / cellSize) # number of cells to look up
            position = c(1:dim)
            isOk = recursiveInsert(grid, cellNum, minDist, dim, nPointCell, nPoint, delta, position, 1)

            if(isOk)
            {
                isStillActive = TRUE
                grid = setGridVector(grid, nPointCell, dim, nPoint)
                activeList[[length(activeList) + 1]] <- nPoint
            }
        }
        if(isStillActive)
            activeList[[length(activeList) + 1]] <- activePoint
    }

    return (grid)
}

recursiveInsert = function(grid, cellNum, minDist, dim, nPointCell, nPoint, delta, position, recursion)
{
    if(recursion > dim)
    {
        # We can try to insert value
        if(all(getGridVector(grid, position, dim) == rep(-1, dim)) == TRUE)
           return (TRUE)
        if(getDistance(nPoint, getGridVector(grid, position, dim)) < minDist)
            return (FALSE)
        return (TRUE)
    }
    isGood = FALSE
    for(i in c((nPointCell[recursion] - delta) : (nPointCell[recursion] + delta)))
    {
        if(i < 1 || i > cellNum) next()
        isGood = TRUE
        position[recursion] = i
        result = recursiveInsert(grid, cellNum, minDist, dim, nPointCell, nPoint, delta, position, recursion+1)
        if(!result) return (FALSE)
    }
    return (isGood)
}

nearPoint = function(point, minDist)
{
    # For algorithm explanation look here:
    # https://en.wikipedia.org/wiki/N-sphere#Spherical_coordinates
    dim = length(point)                 # getting dimension number
    newPoint = c(1:dim)                 # prepare new point
    rand = runif(dim, 0, 1)             # get randoms for calculations
    radius = minDist * (rand[1] + 1)    # get radius for n-sphere
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
attr(nearPoint, "comment") <- "Returns new point lying not further than minDist from point"

getDistance = function(point1, point2)
{
    return (dist(rbind(point1, point2))[1])
}
