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
            nPoint = newPoint(activePoint, minDist)
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

newPoint = function(point, minDist)
{
    rand = runif(2, 0, 1)
    radius = minDist * (rand[1] + 1)
    angle = 2 * pi * rand[2]
    newX = point[1] + radius * cos(angle)
    newY = point[2] + radius * sin(angle)
    newX = min(B, max(newX, A))
    newY = min(B, max(newY, A))

    return (c(newX, newY))
}

getDistance = function(point1, point2)
{
    return (sqrt((point1[1] - point2[1])^2 + (point1[2] - point2[2])^2))
}
