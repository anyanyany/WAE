drawPlots=function()
{
  dimensions=c(2,5,10);
  functions=c(1,8,12,6);
  
  dim_index = 1;
  for(d in dimensions)
  {
    func_index = 1
    for(func in functions)
    {
      path=paste0('D:/result__dim_',d,'__func_',func,'.rds',collapse = NULL);
      results=readRDS(path);
      
      maxLen=0;
      for(i in c(1:length(results)))
      {
        if(length(results[[i]])>maxLen)
          maxLen=length(results[[i]]);
      }

      minValue=Inf;
      maxValue=-Inf;
      
      for(i in c(1:length(results)))
      {
        for(j in c(1:length(results[[i]])))
        {
            if(results[[i]][[j]]<minValue)
              minValue=results[[i]][[j]];
            if(results[[i]][[j]]>maxValue)
              maxValue=results[[i]][[j]];
        }
      }
      
      path=paste0('D:/Plots/dim_',d,'__func_',func,'.jpg',collapse = NULL)
      jpeg(path)
      plot(x=NULL, y=NULL,xlab="succeeding points", ylab="value of cec2013",xlim=c(1, maxLen), ylim=c(minValue, maxValue),main=paste0('dimension=',d,' function=',func,collapse = NULL));
      
      lines(1:length(results[[1]]), results[[1]], col="red")
      lines(1:length(results[[2]]), results[[2]], col="green")
      if(length(results)>2)
        lines(1:length(results[[3]]), results[[3]], col="blue")
      
      dev.off()
      
      func_index = func_index + 1
    }
    dim_index = dim_index + 1
  }
}


