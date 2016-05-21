library("cec2013");
this.dir=dirname(parent.frame(2)$ofile)
setwd(this.dir)
source('./hillClimbing.R')
source('./getPoints.R')
source('./randomNeighbor.R')

main=function()
{
  dimensions=c(2,5,10,20,30,40,50,60,70,80,90,100);
  functions=c(1:28);
  N=10; #N-ile punktow
  delta=1e-2;
  a=-1;
  b=1;
  maxIt=10;
  results=array(dim = c(length(dimensions),3,length(functions),50)); #najepsza wartosc funkcji celu dla kazdego wymiaru, kazdego sposobu wyboru pkt startowych, kazdej funkcji i kazdego punktu startowego

  for(d in dimensions)
  {
    dim=match(d,dimensions);
    startPointsUniformDistribution=GetPointsUniformDistribution(d,N,a,b);
    startPointsHyperMesh=GetPointsHyperMesh(d,N,a,b);
    startPointsPoissonDisc=GetPointsPoissonDisc(d,N,a,b);
    for(func in functions)
    {
        result=runTest(dim, func, startPointsUniformDistribution, delta, maxIt, a, b)
        results[dim,1,func,]=result;

        result=runTest(dim, func, startPointsHyperMesh, delta, maxIt, a, b)
        results[dim,2,func,]=result;

        result=runTest(dim, func, startPointsPoissonDisc, delta, maxIt, a, b)
        results[dim,3,func,]=result;
        cat("Finished function ", func, '\n')
    }
    cat("Finished dimension ", d, '\n')
  }
  saveRDS(results, "C:/Users/Anna/Desktop/a.rds")
  return (results);
}

runTest = function(dim, func, points, minDist, maxIt, A, B)
{
    library(parallel)
    coresCount = detectCores()
    cluster = makeCluster(coresCount, outfile="")

    # points = GetPointsUniformDistribution(dim, pointCount, A, B)
    pointCount = length(points)
    results = rep(0, 50)

    for(i in c(1:maxIt))
    {
        pResults = parLapply(cluster, points, function(point) {
            library("cec2013");
            source('./hillClimbing.R')
            source('./getPoints.R')
            source('./randomNeighbor.R')
            return (hillClimbing(point, func, minDist, A, B))
        })
        for(j in c(1:pointCount))
            results = results + pResults[[j]]
        print(paste0("Finished iteration: ", i))
#        for(j in c(1:pointCount))
#            results = results + hillClimbing(points[[j]], func, minDist, A, B)
    }
    stopCluster(cluster)

    results = results / (maxIt * pointCount)

    return (results)
}

showSolution=function(values)
{
  x1=values[1,1,1,]
  x2=values[1,2,1,]
  x3=values[1,3,1,]
  plot(1:length(x1), x1, type="l",col="red");
  lines(1:length(x2), x2, col="blue")
  lines(1:length(x3), x3, col="green")
}
