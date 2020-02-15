#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lattice)
library(gridExtra)
library(grid)
library(latex2exp)
library(tikzDevice)
library(stringr)

fancy_scientific <- function(l) {
                                        # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
                                        # quote the part before the exponent to keep all the digits
  ## l <- gsub("0e\\+00","0",l)
  ## l <- gsub("^(.*)e", "'\\1'e", l)
  ##                                       # turn the 'e+' into plotmath format
  ## l <- gsub("e", "%*%10^", l)
                                        # return this as an expression
  parse(text=l)
}
compvar=args[1]


pdf("./plots/bandwidth_onesided_gpu_summit.pdf", width=7, height=5)
## plot_var=sprintf("./tex/ %s_summit.tex",compvar)
## tikz(file = sprintf("%s",plot_var), width = 2.5, height = 2.35)

## file_var=sprintf("./one-sided/1nodes/d2d/allocate/flush/get_bw.csv")
## fvnodes=args[1]
## fvbench=args[2]
## fvdev=args[3]
## fvwindow=args[4]
## fvsync=args[5]
## frun=args[6]

load_from_file <- function(file_var){
  ldf <- as.data.frame(read.table(sprintf("%s",file_var)))
  return(ldf)
}

## bench=c("one-sided", "pt2pt", "collective", "startup")
dev=c("d2h", "h2d", "h2h", "d2d")
bench=c("one-sided", "pt2pt")
window=c("create", "allocate", "dynamic")
sync=c("lock_all", "lock", "flush", "flush_local", "pscw", "fence")
## sync=c("lock_all", "lock", "flush", "pscw", "fence")
run=c("put_bw", "get_bw", "put_latency", "get_latency")
run=c("put_bw", "get_bw", "put_latency", "get_latency")
run_pt2pt=c("bw", "latency")
nodes=c("1", "2")

print(compvar)
if(compvar == "bench"){
  param=bench
} else if(compvar == "dev"){
  param=dev
} else if(compvar == "window"){
  param=window
} else if(compvar == "sync"){
  param=sync
} else if(compvar == "run"){
  param=run
} else if(compvar == "nodes"){
  param=nodes
} else{
  param=""
  print("ERROR")
}
for (k in 1:(length(param)))
{
  if(compvar == "bench"){
    vbench=bench[k]
    vnodes=nodes[1]
    vwindow=window[1]
    vsync=sync[4]
    vrun=run[4]
    vrun_pt2pt=run_pt2pt[2]
    vdev=dev[4]
  } else if(compvar == "dev"){
    vbench=bench[1]
    vnodes=nodes[1]
    vwindow=window[1]
    vsync=sync[3]
    vrun=run[4]
    vdev=dev[k]
  } else if(compvar == "nodes"){
    vbench=bench[1]
    vnodes=nodes[k]
    vwindow=window[1]
    vsync=sync[3]
    vrun=run[1]
    vdev=dev[3]
  } else if(compvar == "window"){
    vbench=bench[1]
    vnodes=nodes[2]
    vwindow=window[k]
    vsync=sync[4]
    vrun=run[4]
    vdev=dev[3]
  } else if(compvar == "sync"){
    vbench=bench[1]
    vnodes=nodes[2]
    vwindow=window[1]
    vsync=sync[k]
    vrun=run[4]
    vdev=dev[3]
  } else if(compvar == "run"){
    vbench=bench[1]
    vnodes=nodes[1]
    vwindow=window[1]
    vsync=sync[3]
    vrun=run[k]
    vdev=dev[3]
  } else {
    param="error"
  }
  if(str_detect(vrun, "[bw]"))
  {
    run_var="bw"
    ylabel="Bandwidth(MB/s)"
  } else {
    run_var="lat"
    ylabel="Latency(us)"
  }
  print(vbench)
  if(vbench == "one-sided"){
    file_var=sprintf("./data/%s/%snodes/%s/%s/%s/%s.csv",vbench,vnodes,vdev,vwindow,vsync,vrun)
  } else if(vbench == "pt2pt"){
    file_var=sprintf("./data/%s/%snodes/%s/%s.csv",vbench,vnodes,vdev,vrun_pt2pt)
  }
  ldf <- vector(mode="list", length=length(param))
  ldf[[k]] <- load_from_file(file_var)
  colnames(ldf[[k]])<-c("size", sprintf("%s",run_var ))

  if(k==1){
    full <- ldf[[k]]
  } else{
    full <- rbind(full,ldf[[k]])
  }
}
  ## full <- ldf[[1]]
  ## full <- rbind(full,ldf[[2]])

print(full)

pl1 <- ggplot()+
  theme_linedraw()
if(compvar=="dev" && run_var=="bw"){
  pl2<-pl1 + geom_line(aes(x =full$size[1:23],y = full$bw[1:23], colour = "d2h"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[24:46], colour = "h2d"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[47:69], colour = "h2h"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[70:92], colour = "d2d"))
} else if(compvar=="dev" && run_var=="lat"){
  pl2<-pl1 + geom_line(aes(x =full$size[1:23],y = full$lat[1:23], colour = "d2h"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[24:46], colour = "h2d"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[47:69], colour = "h2h"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[70:92], colour = "d2d"))+
    scale_y_log10(labels=fancy_scientific,breaks = scales::pretty_breaks(n = 10))
}else if(compvar=="window" && run_var=="bw"){
  pl2<-pl1 + geom_line(aes(x =full$size[1:23],y = full$bw[1:23], colour = "create"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[24:46], colour = "allocate"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[47:69], colour = "dynamic"))
} else if(compvar=="window" && run_var=="lat"){
  pl2<-pl1 + geom_line(aes(x =full$size[1:23],y = full$lat[1:23], colour = "create"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[24:46], colour = "allocate"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[47:69], colour = "dynamic"))+
    scale_y_log10(labels=fancy_scientific,breaks = scales::pretty_breaks(n = 10))
}else if(compvar=="bench" && run_var=="bw"){
  pl2<-pl1 + geom_line(aes(x =full$size[1:23],y = full$bw[1:23], colour = "onesided"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[24:46], colour = "twosided"))
} else if(compvar=="bench" && run_var=="lat"){
  pl2<-pl1 + geom_line(aes(x =full$size[1:23],y = full$lat[1:23], colour = "one-sided"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[24:46], colour = "two-sided"))+
    ## scale_y_log10(labels=fancy_scientific,breaks = scales::pretty_breaks(n = 50))
    scale_y_log10(labels=fancy_scientific,breaks = round(seq(min(full$lat), max(full$lat), by = 20),1))
} else if(compvar=="sync" && run_var=="bw"){
  pl2<-pl1 + geom_line(aes(x =full$size[1:23],y = full$bw[1:23], colour = "lock_all"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[24:46], colour = "lock"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[47:69], colour = "flush"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[70:92], colour = "flush_local"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[93:115], colour = "pscw"))+
    geom_line(aes(x =full$size[1:23],y = full$bw[116:138], colour = "fence"))
} else if(compvar=="sync" && run_var=="lat"){
  pl2<-pl1 + geom_line(aes(x =full$size[1:23],y = full$lat[1:23], colour = "lock_all"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[24:46], colour = "lock"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[47:69], colour = "flush"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[70:92], colour = "flush_local"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[93:115], colour = "pscw"))+
    geom_line(aes(x =full$size[1:23],y = full$lat[116:138], colour = "fence"))+
    ## scale_y_log10(labels=fancy_scientific,breaks = scales::pretty_breaks(n = 15))
    scale_y_log10(labels=fancy_scientific,breaks = round(seq(min(full$lat), max(full$lat), by = 20),1))
}
pl3<-pl2 + scale_x_log10()+
  ## scale_y_log10(labels=fancy_scientific)+
  xlab("Message sizes (B)")+
  ylab(sprintf("%s",ylabel))
print(pl3)

dev.off()
## line_plot1 <- ggplot(full,
##                      aes(x = Size))+
##     theme_linedraw() +
##     ## facet_wrap(full ~ .,nrow=2,ncol=1,scales='fixed')
##     ## geom_line()+
##     ## geom_point()+
##     ## geom_line(aes(y = non_merge, colour = "non_merge"))+
##     ## geom_line(aes(y = ldf$put_bw, colour = "2 nodes"))+
##     geom_line(aes(y = ldf3$Bandwidth(MB/s), colour = "Intra-node"))+
##     geom_line(aes(y = ldf4$Bandwidth(MB/s), colour = "Inter-node"))+
##     ## geom_line(aes(y = merge, colour = "merge"))+
##     scale_x_log10()+
##     ## stat_smooth()+
##     ## theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=0))+
##     scale_y_continuous(labels=fancy_scientific)+
##     ## scale_y_continuous(labels=fancy_scientific,limits=(c(0,50)))+
##     ## scale_y_continuous(labels=fancy_scientific,limits=(c(0,4e3)))+
##     ## scale_y_continuous(labels=fancy_scientific,limits=(c(0,3e)))+
##     xlab("Message sizes (B)")+
##     ylab("Bandwidth(MB/s)")
## print(line_plot1)

## dev.off()
