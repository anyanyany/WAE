A = -1
B = 1
dim = 2
x = list()
y = list()

plotPoints = function(accuracy)
{
    plot(A:B, A:B, type="n", ylim=c(-1.1,1))
    grid = generatePoints(accuracy)
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

pointToGridCell = function(point, cellSize)
{
    x = point[1] - A
    y = point[2] - A
    x = floor(x / cellSize) + 1
    y = floor(y / cellSize) + 1

    return(c(x, y))
}

generatePoints = function(minDist)
{
    cellSize = minDist / sqrt(dim)
    cellNum = (B - A) / cellSize
    grid = array(c(-1, -1), dim = c(cellNum + 1, cellNum + 1, dim))

    activeList = list()
    initialPoint = runif(dim, A, B)
    position = pointToGridCell(initialPoint, cellSize)
    grid[position[1], position[2], ] = initialPoint
    activeList[[length(activeList) + 1]] <- initialPoint
    grid = poissonDisc(minDist, cellSize, grid, activeList)

    return (grid)
}

poissonDisc = function(minDist, cellSize, grid, activeList)
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

        # print(it)
        # print(listLength)

        # Select k neighbours for point
        for(i in c(1:30))
        {
            # Get new coordinates
            nPoint = nearPoint(activePoint, minDist)
            nPointCell = pointToGridCell(nPoint, cellSize)
            delta = ceiling(minDist / cellSize) # number of cells to look up
            isOk = TRUE
            for(j in c((nPointCell[1] - delta) : (nPointCell[1] + delta)))
            {
                for(k in c((nPointCell[2] - delta) : (nPointCell[2] + delta)))
                {
                    if(j <= 0 || j > length(grid[1,,1]) || k <= 0 || k > length(grid[1,,1]) || all(grid[j, k, ] == c(-1, -1)) == TRUE)
                        next()
                    if(getDistance(nPoint, grid[j, k, ]) < minDist)
                    {
                        isOk = FALSE
                        break()
                    }
                }
                if(!isOk)
                    break()
            }

            if(isOk)
            {
                x[[length(x) + 1]] <- nPoint[1]
                y[[length(y) + 1]] <- nPoint[2]
                plot(x, y, type="p", ylim=c(-1,1), xlim=c(-1, 1))
                isStillActive = TRUE
                grid[nPointCell[1], nPointCell[2], ] = nPoint
                activeList[[length(activeList) + 1]] <- nPoint
            }
        }
        if(isStillActive)
            activeList[[length(activeList) + 1]] <- activePoint
    }

    return (grid)
}

nearPoint = function(point, minDist)
{
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
attr(newPoint, "comment") <- "Returns new point lying not further than minDist from point"

getDistance = function(point1, point2)
{
    return (dist(rbind(point1, point2))[1])
}
