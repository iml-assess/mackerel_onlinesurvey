##' save pngs in one line
##' @param plot plot
##' @param wd working directory in which to plase image
##' @param name file name
##' @param dim dimensions of png in cm
##' @param ... arguments passed to png
##' @importFrom gridExtra grid.arrange
##' @details Saves plot as png in one line
##' @rdname savepng
##' @export
savepng <- function(plot,wd, name,dim,res=300,...){
    png(file=paste0(wd,name,".png"),units="cm",width=dim[1],height=dim[2],res=res,...)
    if("ggplot" %in% class(plot)) grid.arrange(plot) else plot
    dev.off()
}

##' get.legend
##' @param p plot of type ggplot
##' @details Extracts legend from plot
##' @export
get.legend<-function(p){
    tmp <- ggplot_gtable(ggplot_build(p))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
} 

##' Pie plot by province
##' @param x dataframe
##' @param id factor with pie segments
##' @param value column name with values (to sum). If NULL the length is taken.
##' @param facet column name with factor levels
##' @param total logical. if facetted, add one with the total?
##' @param col vector of colors
##' @param ncol number of facet columns (excluding the legend)
##' @param width vector of length 2 with the widths of the plot vs legend part
##' @param N vector with numbers to use in subs
##' @param reperl logical. scatter percentage labels?
##' @importFrom gridExtra grid.arrange
##' @details Makes faceted pie plots
##' @rdname pieplot
##' @export
pieplot = function(x,id,value=NULL,facet=NULL,total=TRUE,col=c("yellowgreen","violetred","orange",'grey',"slateblue"),
                   ncol=1,width=c(0.8,0.2),N=NULL,repel=FALSE){
    if(!is.null(facet)) x[,facet] = as.character(x[,facet])
    x[,id] = as.character(x[,id])

    if(is.null(value)){
        fct = function(y){c(length(y[,1]),NA)}
    }else{
        fct = function(y){c(sum(y[,value]),nrow(y))}
    }
    x = ddply(x,c(id,facet),fct)
    names(x)[1] = 'id'
    
    if(total & !is.null(facet)){
        names(x)[2] = 'facet'
        tot = ddply(x,c('id'),summarise,V1=sum(V1),V2=sum(V2))
        tot$facet = "TOT"
        x = rbind(x,tot) 
    }else{
        x$facet = ""
    }

    plots = list()
    colid = unique(as.character(x$id)) 
    col = col[1:length(colid)]
    names(col)= colid
    lev = unique(x[,'facet'])
    lev = lev[!lev=='NA']
    legplot = ifelse(length(which(lev=='TOTAL'))==0,1,which(lev=='TOTAL'))
    
    for(i in lev){
        j=which(lev==i)
        d=transform(transform(x[x$facet==i,], value=V1/sum(V1)), labPos=cumsum(V1)-V1/2)
        
        if(is.null(value)){
            sub=paste0('N=',sum(d$V1))
        }else{
            if(is.null(N)){Nfac=sum(d$V2)}else{Nfac=N[j]}
            sub=paste0(round(sum(d$V1)),'t (N=',Nfac,')')
        }

        plots[[j]]=ggplot(d,aes(x="", y = V1, fill = forcats::fct_rev(id))) +
            geom_bar(width = 1, stat = "identity") +
            scale_fill_manual(values = col[as.character(d$id)]) +
            coord_polar(theta = "y") +
            ggtitle(bquote(atop(.(i), atop(italic(.(sub)), "")))) +
            theme_minimal()+
            theme(plot.margin = unit(c(0,0,0,0), "cm"),
                  plot.title = element_text(hjust = 0.5))+
            labs(fill='',x='',y='')
        if(isTRUE(repel)){ 
                require(ggrepel)
                plots[[j]] =  plots[[j]]+geom_label_repel(aes(label = scales::percent(value), y = labPos),show.legend = FALSE)

            }else{ 
                plots[[j]]=  plots[[j]]+geom_text(aes(y=labPos, label=scales::percent(value)),fontface='bold')
            }
        if(j!=legplot){plots[[j]]=plots[[j]]+theme(legend.position="none")}
    }
   
    mylegend = get.legend(plots[[legplot]])
    plots[[legplot]] = plots[[legplot]]+theme(legend.position="none")
    grid.arrange(arrangeGrob(grobs=plots,ncol=ncol),mylegend,ncol=2,widths=width)
}

