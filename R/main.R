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
  globalExtremes=c(-1400,-700,-300,-1200);
  delta=1;
  a=-100;
  b=100;
  maxIt=20;
  dim_index = 1

  for(d in dimensions)
  {
    N=10*d;
    dim=match(d,dimensions);
    startPointsUniformDistribution=GetPointsUniformDistribution(d,N,a,b);
    startPointsHyperMesh=GetPointsHyperMesh(d,N,a,b);
    if((d==2) | (d==5)) 
      startPointsPoissonDisc=GetPointsPoissonDisc(d,N,a,b);
    func_index = 1
    for(func in functions)
    {
        extreme=globalExtremes[func_index];
        
        results=list();
        results[[1]]=runTest(dim, func, startPointsUniformDistribution, delta, maxIt, a, b,extreme)
        results[[2]]=runTest(dim, func, startPointsHyperMesh, delta, maxIt, a, b,extreme)
        if((d==2) | (d==5)) 
        {
          results[[3]]=runTest(dim, func, startPointsPoissonDisc, delta, maxIt, a, b,extreme)
        }
        path=paste0('D:/result__dim_',d,'__func_',func,'.rds',collapse = NULL);
        saveRDS(results, path)
        
        cat("Finished function ", func, '\n')
        func_index = func_index + 1
    }
    cat("Finished dimension ", d, '\n')
    dim_index = dim_index + 1
  }
}

runTest = function(dim, func, points, minDist, maxIt, A, B,extreme)
{
    library(parallel)
    coresCount = detectCores()
    cluster = makeCluster(coresCount, outfile="")

    pointCount = length(points)
    results = list();

    for(i in c(1:maxIt))
    {
        pResults = parLapply(cluster, points, function(point) {
            library("cec2013");
            source('./hillClimbing.R')
            source('./getPoints.R')
            source('./randomNeighbor.R')
            return (hillClimbing(point, func, minDist, A, B,extreme))
        })
        
        #every execution of hillclimbing can return different dimension of result, we need to merge this results
        #fistly, need to find max lenght of results lists
        maxLen=length(results);
        for(j in c(1:pointCount))
        {
          if(length(pResults[[j]])>maxLen)
            maxLen=length(pResults[[j]]);
        }
        
        #save old results
        tmp=results;
        
        #create new results
        results=list();
        #we need to fill all of fields (maxLen fields) by some value,
        #if partial result is shorter than maxLen, we repeat the last value to fill all the fields in result list
        for(l in c(1:maxLen))
        {
          results[[l]]=0;
          if(length(tmp)>0)
          {
            if(l<=length(tmp))
            {
              results[[l]]=results[[l]]+tmp[[l]];
            }
            else
            {
              results[[l]]=results[[l]]+tmp[[length(tmp)]];
            }
          }
        }
        
        #and the same thing for the rest of results
        for(j in c(1:pointCount))
        {
          for(l in c(1:maxLen))
          {
            tmp=pResults[[j]];
            if(l<=length(tmp))
            {
              results[[l]]=results[[l]]+tmp[[l]];
            }
            else
            {
              results[[l]]=results[[l]]+tmp[[length(tmp)]];
            }
          }
        }
        print(paste0("Finished iteration: ", i))
    }
    stopCluster(cluster)
    for(l in c(1:length(results)))
      results[[l]] = results[[l]] / (maxIt * pointCount)

    return (results)
}

