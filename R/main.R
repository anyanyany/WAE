library("cec2013");
this.dir=dirname(parent.frame(2)$ofile)
setwd(this.dir)
source('./hillClimbing.R')
source('./getPoints.R')
source('./randomNeighbor.R')

main=function()
{
  dimensions=c(2,5,10);
  functions=c(1,8,12,3);
  delta=1;
  a=-100;
  b=100;
  maxIt=20;
  numberOfSavedPoints=100;
  results=array(0,dim = c(length(dimensions),3,length(functions),numberOfSavedPoints)); #najepsza wartosc funkcji celu dla kazdego wymiaru, kazdego sposobu wyboru pkt startowych, kazdej funkcji i kazdego punktu startowego
  dim_index = 1

  for(d in dimensions)
  {
    N=10*d;
    dim=match(d,dimensions);
    startPointsUniformDistribution=GetPointsUniformDistribution(d,N,a,b);
    startPointsHyperMesh=GetPointsHyperMesh(d,N,a,b);
    #startPointsPoissonDisc=GetPointsPoissonDisc(d,N,a,b);
    func_index = 1
    for(func in functions)
    {
        result=runTest(dim, func, startPointsUniformDistribution, delta, maxIt, a, b,numberOfSavedPoints)
        results[dim_index,1,func_index,]=result;

        result=runTest(dim, func, startPointsHyperMesh, delta, maxIt, a, b,numberOfSavedPoints)
        results[dim_index,2,func_index,]=result;

        #result=runTest(dim, func, startPointsPoissonDisc, delta, maxIt, a, b,numberOfSavedPoints)
        #results[dim_index,3,func_index,]=result;
        
        cat("Finished function ", func, '\n')
        func_index = func_index + 1
    }
    cat("Finished dimension ", d, '\n')
    dim_index = dim_index + 1
  }
  saveRDS(results, "D:/results.rds")
  return (results);
}

runTest = function(dim, func, points, minDist, maxIt, A, B,numberOfSavedPoints)
{
    library(parallel)
    coresCount = detectCores()
    cluster = makeCluster(coresCount, outfile="")

    pointCount = length(points)
    results = rep(0, numberOfSavedPoints)

    for(i in c(1:maxIt))
    {
        pResults = parLapply(cluster, points, function(point) {
            library("cec2013");
            source('./hillClimbing.R')
            source('./getPoints.R')
            source('./randomNeighbor.R')
            return (hillClimbing(point, func, minDist, A, B,numberOfSavedPoints))
        })
        for(j in c(1:pointCount))
            results = results + pResults[[j]]
        print(paste0("Finished iteration: ", i))
    }
    stopCluster(cluster)

    results = results / (maxIt * pointCount)

    return (results)
}

