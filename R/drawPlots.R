drawPlots=function(values)
{
  dimensions=c(2,5,10,20);
  functions=c(7,9,11,15,20);
  
  dim_index = 1;
  for(d in dimensions)
  {
    func_index = 1
    for(func in functions)
    {
      UniformDistribution=values[dim_index,1,func_index,];
      HyperMesh=values[dim_index,2,func_index,];
      #PoissonDisc=values[dim_index,3,func_index,];
      
      path=paste0('D:/PROJECTS/WAE/R/Plots/dim_',d,'__func_',func,'.jpg',collapse = NULL)
      jpeg(path)
      plot(1:length(UniformDistribution), UniformDistribution, type="l",col="red", xlab="point", ylab="function value",main=paste0('dimension=',d,' function=',func,collapse = NULL));
      lines(1:length(HyperMesh), HyperMesh, col="blue")
      #lines(1:length(PoissonDisc), PoissonDisc, col="green")
      dev.off()
      
      func_index = func_index + 1
    }
    dim_index = dim_index + 1
  }
}


