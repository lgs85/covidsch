#' Create partial function to sample from gamma distributions
#' @author Joel Hellewell
#' @param dist_shape numeric shape parameter of Weibull distribution
#' @param dist_scale numeric scale parameter of Weibull distribution
#'
#' @return partial function that takes a numeric argument for number of samples
#' @export
#' @importFrom purrr partial
#' @examples
#'
dist_setup <- function(dist_shape = NULL, dist_scale = NULL) {
  out <- purrr::partial(rweibull,
                 shape = dist_shape,
                 scale = dist_scale)
  return(out)
}




#' Matrix to pairwise list without diagonal
#' @author lewis Spurgin
#' @param m matrix of contacts
#' @return
#' @export
#' @importFrom tibble as_tibble
#' @importFrom reshape2 melt
#' @importFrom dplyr filter
#' @examples
#'

format_network <- function(m, idvec = NULL) {
  diag(m) <- NA

  if(!is.null(idvec)){
    rownames(m) <- idvec
    colnames(m) <- idvec
  }else {
    rownames(m) <- c(1:nrow(m))
    colnames(m) <- c(1:nrow(m))
  }

  out <- as_tibble(melt(m)) %>%
    filter(!is.na(value))
  return(out)
}




#' Samples the serial interval for given incubation period samples
#'
#' @param inc_samp vector of samples from the incubation period distribution
#' @param theta rate of presymtomatic transmission
#' @author Lewis Spurgin
#'
#' @return
#' @export
#' @importFrom sn rsn
#' @examples
#'

inf_fn <- function(inc_samp = NULL, theta = NULL) {

  presym <- rbernoulli(length(inc_samp),p = theta)

  postsym_inds <- rsn(n = sum(!presym),
                          xi = inc_samp[!presym],
                          omega = 2,
                          alpha = Inf)
  presym_inds <- rsn(n = sum(presym),
                         xi = inc_samp[presym],
                         omega = (inc_samp[presym]/max(inc_samp[presym]))*3,
                         alpha = -Inf)


  out <- rep(NA,length(inc_samp))
  out[presym] <- presym_inds
  out[!presym] <- postsym_inds

  return(out)
}



#' Samples the serial interval for given incubation period samples - gives an infection prob for a given day
#'
#' @param day day of simulation
#' @param inc_samp vector of samples from the incubation period distribution
#' @param k numeric skew parameter for sampling the serial interval from the incubation period
#' @param R scaling factor
#' @param infasym vector of weights based on whether inds are asymptomatic
#'
#' @return
#' @export
#' @importFrom sn dsn
#' @importFrom purrr rbernoulli
#' @examples
#'
#'
#'

inf_prob <- function(day = NULL, inc_samp = NULL, theta = NULL, R = NULL, contactrate = NULL, infasym = NULL) {

  presym <- rbernoulli(length(inc_samp),p = theta)

  presym_inds <- NA
  postsym_inds <- NA

  if(sum(presym) > 0)
  {
    presym_inds <- dsn(x = day[presym],
                           xi = inc_samp[presym],
                           omega = (inc_samp[presym]/max(inc_samp[presym]))*3,
                           alpha = -Inf)
  }
  if(sum(!presym) > 0)
  {
    postsym_inds <- dsn(x = day[!presym],
                            xi = inc_samp[!presym],
                            omega = 2,
                            alpha = Inf)
  }




  out <- rep(NA,length(inc_samp))
  out[presym] <- presym_inds
  out[!presym] <- postsym_inds

  out2 <- 1 - exp(1)^(-(out*R*contactrate*infasym))

  return(out2)
}





#' Create null networks
#'
#' @author Josh Firth
#' @param am association matrix network
#' @param returns format for output - either "graph", "matrix" or "edgelist"
#' @param null type of simulation to perform "edge" = randomised edges, "deg" = degrees retained, "latt" = lattice, "clust" = clustered
#' @return
#' @export
#' @import igraph
#'
#' @examples
#'

network_null<-function(am,returns=c("graph","matrix","edgelist"),null=c("edge","deg","latt","clust")){ #takes an association matrix network, and returns a network in specified format ("graph","matrix","edgelist") under the specified null ("edge","deg","latt","clust").
  am.i<-graph_from_adjacency_matrix(am,"undirected",weighted=T) #convert to igraph object
  n.edges<-length(E(am.i))
  n.nodes<-length(V(am.i))

  if(null=="edge"){
    #1 - Edge null - using same nodes and edges (and weights),  and re-shuffling the edges around them:
    #Maintains: General network structure (n nodes, n links),  daily consistency. Randomises/losses: Individual differences in social contact propensity, Clustering, hidden structure
    am.r.i<-rewire(am.i,each_edge(1))
  }
  if(null=="deg"){
    #2 - Degree null - using the same nodes and edges (and weights) and degree distribution (number of unique partners) and re-shuffling the edges around them:
    #Maintains: General network structure (n nodes, n links), daily consistency, Individual differences in social contact propensity, daily consistency. Randomises/losses: Clustering, hidden structure
    am.r.i<-rewire(am.i,keeping_degseq(niter=n.edges))
  }
  if(null=="latt"){
    #3 - Lattice null - using same nodes and edges (and weights), but reassigning edges into lattice structure (a ring w/ edges to 2nd&3rd neighbours minus additional edges)
    #Maintains: General network structure (n nodes, n links), daily consistency, lattices, Randomises/losses: Individual differences in social contact propensity, non-clustering, hidden structure
    am.latt.i<-make_lattice(n.nodes,nei=3,circular=T)
    n.latt.edges<-length(E(am.latt.i))
    to.del<-n.latt.edges-n.edges
    if(to.del<1){print("NO EXTRA EDGES")} else{remove.edges<-sort(sample(1:n.latt.edges,to.del))
    am.r.i<-delete.edges(am.latt.i,remove.edges)}
  }

  if(null=="clust"){
    #4 - Cluster null - using same nodes and edges (and weights), but generating a matching clustered structure (using lattice but keeping connected/unconnected nodes and unlinking triads)
    #Maintains: General network structure (n nodes, n links), daily consistency, clustering, Randomises/losses: Individual differences in social contact propensity, hidden structure
    unconnected.nodes<-sum(components(am.i)$csize==1)
    connected.nodes<-n.nodes-unconnected.nodes
    am.clust.i<-make_lattice(connected.nodes,nei=3,circular=T)
    n.clust.edges<-length(E(am.clust.i))
    to.add<-n.edges-n.clust.edges
    if(to.add<1){print("NO DEFICIT OF EDGES")} else{
      am.clust.i.4<-make_lattice(connected.nodes,nei=4,circular=T)
      el1<-apply(as_edgelist(am.clust.i),1,paste,collapse=" ")
      el2<-apply(as_edgelist(am.clust.i.4),1,paste,collapse=" ")
      new.ones.use<-sort(sample(which(!el2 %in% el1),to.add))
      delete.ones<-1:length(E(am.clust.i.4))
      delete.ones<-delete.ones[!as.character(delete.ones) %in% as.character(new.ones.use)]
      am.req.t<-am.clust.i %u% delete.edges(am.clust.i.4,delete.ones)
      am.req.t<-rewire(am.req.t,keeping_degseq(niter=sample(c(255,256),1)))
      am.r.i<-add_vertices(am.req.t,unconnected.nodes)
    }
  }

  #object return type:
  E(am.r.i)$weight<-eweights<-sample(E(am.i)$weight,length(E(am.i)$weight))
  if(returns=="graph"){return(am.r.i)}
  if(returns=="matrix"){return(as.matrix(as_adj(am.r.i,type="both",attr="weight")))}
  if(returns=="edgelist"){return(cbind(as_edgelist(am.r.i),eweights))}
}




#' Simulate social distancing using data across days
#'
#' @author Josh Firth
#' @param am association matrix network
#' @param returns format for output - either "graph", "matrix" or "edgelist"
#' @param dist.prop the proportion of 'rare' ties to reassign. Between zero and one.
#' @return
#' @export
#' @import igraph
#'
#' @examples
#'


dist1_func<-function(am,returns,dist.prop){
  am.i<-graph_from_adjacency_matrix(am,"undirected",T)
  ews<-E(am.i)$weight
  d1<-which(ews==1)
  nd1<-length(d1)
  d2<-which(ews==2)
  nd2<-length(d2)
  max.prop<-(nd2/nd1)+(2/3)
  if(dist.prop>=max.prop){print("Err: High Dis Prop")}
  dist.n<-round(dist.prop*nd1)
  d1.cut<-sample(d1,dist.n) #these are the ones to go
  d1.remain<-d1[!d1 %in% d1.cut]
  dist.reassign<-c(rep(d1.remain,2),d2) #these ones can be reassigned to
  dist.assigned<-sample(dist.reassign,dist.n,prob=c(rep(c(1,2),c(length(rep(d1.remain,2)),length(d2))))) #these are assigned another day
  dist.counts<-table(dist.assigned)
  news<-ews
  news[as.numeric(names(dist.counts))]<-news[as.numeric(names(dist.counts))]+dist.counts
  E(am.i)$weight<-eweights<-news
  am.r.i<-delete.edges(am.i,d1.cut)
  if(returns=="graph"){return(am.r.i)}
  if(returns=="matrix"){return(as.matrix(as_adj(am.r.i,type="both",attr="weight")))}
  if(returns=="edgelist"){return(cbind(as_edgelist(am.r.i),eweights))}
}








#' Simulate social distancing using data within days
#'
#' @author Josh Firth
#' @param am association matrix network
#' @param returns format for output - either "graph", "matrix" or "edgelist"
#' @param dist.prop the proportion of 'rare' ties to reassign. Between zero and one.
#' @return
#' @export
#' @import igraph
#'
#' @examples
#'

#Distancing 2 - taking uncommon encounters within days and reassigning them to other contacts

dist2_func<-function(am,am.contacts,returns,dist.prop){
  am.i<-graph_from_adjacency_matrix(am,"undirected",T)
  ews<-E(am.i)$weight
  d1<-which(ews==1)
  nd1<-length(d1)
  d2<-which(ews==2)
  nd2<-length(d2)
  max.prop<-(nd2/nd1)+(2/3)
  if(dist.prop>=max.prop){print("Err: High Dis Prop")}
  dist.n<-round(dist.prop*nd1)
  am.i.contacts<-graph_from_adjacency_matrix(am.contacts,"undirected",T)
  ews.contacts<-E(am.i.contacts)$weight
  d1.probs<-(max(ews.contacts[d1])-ews.contacts[d1])+1

  d1.probs<-ews.contacts[d1]/sum(ews.contacts[d1])

  d1.remain<-sample(d1,nd1-dist.n,prob=d1.probs) #these are the ones not to go
  d1.cut<-d1[!d1 %in% d1.remain]

  dist.reassign<-c(rep(d1.remain,2),d2) #these ones can be reassigned to links
  dist.reassign.probs<-c(rep(ews.contacts[d1.remain],2),ews.contacts[d2]*2) #this is the chance they are reassigned, with the d2 doubled because there are 2sets of the d1
  dist.assigned<-sample(dist.reassign,dist.n,prob=dist.reassign.probs) #these are assigned another day
  dist.counts<-table(dist.assigned)
  news<-ews
  news[as.numeric(names(dist.counts))]<-news[as.numeric(names(dist.counts))]+dist.counts
  E(am.i)$weight<-eweights<-news
  am.r.i<-delete.edges(am.i,d1.cut)
  if(returns=="graph"){return(am.r.i)}
  if(returns=="matrix"){return(as.matrix(as_adj(am.r.i,type="both",attr="weight")))}
  if(returns=="edgelist"){return(cbind(as_edgelist(am.r.i),eweights))}
}




#' GGplot theme for figures
#' @author Lewis Spurgin
#' @return
#' @export
#' @importFrom ggplot2 theme theme_bw
#' @examples
#'

theme_ls <- function() {
  theme_bw() +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          strip.text = element_text(size = 20),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          plot.title = element_text(size = 22),
          legend.key.size = unit(2.5,"lines"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}



#' Case plot for figures
#' @author Lewis Spurgin
#' @param df data frame of cases. Can be generated by `outbreak_model` (with `weekly = FALSE`)
#' @param testing logical - do you want to plot testing rates?
#' @param facet character - 'wrap' if you want to plot one variable, 'grid' if you want to plot two
#' @param nrow integer - number of rows (only use if facet == 'wrap')
#' @param gridvar name of second variable if plotting two (first is always 'intervention')
#' @return
#' @export
#' @rawNamespace import(dplyr, except = c(union,as_data_frame,groups))
#' @import ggplot2
#' @examples
#'

case_plot <- function(out)
{

  for(i in c(1:3))
  {
    assign(paste0("c",i,"x"),
           out %>%
             group_by(sim) %>%
             filter(cumcases >= i) %>%
             summarise(c1 = min(day)) %>%
             pull(c1) %>%
             median() %>%
             round())
    assign(paste0("c",i,"y"),out %>%
             filter(day == get(paste0("c",i,"x"))) %>%
             pull(cuminf) %>%
             median())

  }

  dl <- data.frame(xmin = rep(-Inf,3),xmax = c(c1x,c2x,c3x),
                   ymin = rep(-Inf,3),ymax = c(c1y,c2y,c3y))



  out %>%
    gather(key = type, value = n_cases,cuminf:cumcases) %>%
    group_by(day,type,intervention) %>%
    summarise(med_cases = median(n_cases),
              Q75 = quantile(n_cases,0.75),
              Q25 = quantile(n_cases,0.25)) %>%
    ungroup() %>%
    mutate(type = recode(type,cumcases = "Symptomatic cases",cuminf = "Infections")) %>%
    ggplot(aes(x = day,y = med_cases))+
    geom_line(aes(group = type, col = type))+
    geom_ribbon(aes(ymax = Q75,ymin = Q25, col = type, fill= type),col = NA,alpha = 0.2)+
    theme_ls()+
    scale_colour_manual(values = c("indianred1",
                                   "darkslategray",
                                   "darkorange",
                                   "steelblue"
    ))+
    scale_fill_manual(values = c("indianred1",
                                 "darkslategray",
                                 "darkorange",
                                 "steelblue"
    ))+
    ylab("Cumulative cases")+
    xlab("Day")+
    geom_segment(data = dl,aes(x = xmin,xend = xmax, y = ymax, yend = ymax),lty = 2)+
    geom_segment(data = dl,aes(x = xmax,xend = xmax, y = ymin, yend = ymax),lty = 2)

}





#' Generate network plot with contagion
#'
#' @author Josh Firth
#' @param am association matrix
#' @param use.df the dataframe information of the contagion
#' @param day the day to do the contagion for
#' @param am.layout (if wanting to specify a layout e.g. to keep the layout the same across comparisons)
#'
#' @return
#' @export
#' @import igraph
#' @import clue
#' @import diagram
#' @import fields
#'
#' @examples
#'
#' \dontrun{
#'
#' #example of using function
#' a<-load(file="data-raw/am_list.RData") #need this for a network
#' b<-load(file="/network_examples.RData") #need this for contagion data
#'
#'
#' #here, we're going to draw 2 networks, both of the same day (time period of the contagion) and both in the same layout (but  assuming you don't have a pre-specified layout) then just like this works:
#' dev.new(height=6,width=12);par(mar=c(0,0,0,0));par(mai=c(0,0,0,0));par(mfrow=c(1,2))
#' am<-am_list[[1]]
#' #draw your first network and save the layout
#' lay<-draw.contagion(am,use.df=primary_tracing,day=20)

#' #now, if you want to use that same layout again for a different network you can just:
#'draw.contagion(am,am.layout=lay,use.df=secondary_tracing,day=20)
#'}
#'



draw.contagion<-function(am,am.layout=NULL,use.df,day){#takes the association matrix, the dataframe information of the contagion, the day to do the contagion for, and the layout (if wanting to specify a layout e.g. to keep the layout the same across comparisons)

  range.use<-function(x,min.use,max.use){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (max.use - min.use) + min.use } #define for later

  #arrange data:
  use.df<-use.df[order(use.df$caseid),]
  use.df$v<-use.df$caseid
  use.df$degree<-colSums(am>0)
  use.df$isolated_time[is.infinite(use.df$isolated_time)]<-day+100 #to avoid infinite values
  use.df$release_time[is.infinite(use.df$release_time)]<-day+100 #to avoid infinite values

  #make the needed igraph object from the association matrix
  am.i<-graph_from_adjacency_matrix(am,"undirected",weighted=T,diag=F)

  #now, if a layout isn't given in the function, generate the circular ones wanted here:
  if(is.null(am.layout)){
    am.lay<-layout_nicely(am.i)
    circlay<-T
    if(circlay==T){
      #making circular
      ps<-nrow(am.lay)
      dim.tl<-ceiling(sqrt(ps))*2 #needs *2 just to make sure we have enough points
      #grid.lay<-expand.grid(floor(-dim.tl/2):ceiling(dim.tl/2),((floor(-dim.tl/2))-0.5):(ceiling((dim.tl/2))+0.5))
      xpoints1<-floor(-dim.tl/2):ceiling(dim.tl/2)
      xpoints2<-xpoints1+0.5
      ypoints1<-xpoints1
      ypoints2<-xpoints2
      grid.lay1<-expand.grid(xpoints2,ypoints1)
      grid.lay2<-expand.grid(xpoints1,ypoints2)
      grid.lay3<-expand.grid(xpoints1,ypoints1)
      grid.lay4<-expand.grid(xpoints2,ypoints2)
      grid.lay<-rbind(grid.lay1,grid.lay2,grid.lay3,grid.lay4)
      grid.lay[,1]<-jitter(grid.lay[,1],1)
      grid.lay[,2]<-jitter(grid.lay[,2],1)
      euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
      grid.lay[,3]<-apply(grid.lay,1,function(a)euc.dist(matrix(c(mean(grid.lay[,1]),mean(grid.lay[,2])),1,2),a))
      max.dist<-grid.lay[,3][((1:length(unique(grid.lay[,3])))[match(grid.lay[,3],sort(unique(grid.lay[,3])))])==ps]
      g.lay.c<-grid.lay[grid.lay[,3]<=max.dist,]
      g.lay.c[,1]<-scale(g.lay.c[,1])[,1]
      g.lay.c[,2]<-scale(g.lay.c[,2])[,1]
      am.lay[,1]<-scale(am.lay[,1])[,1]
      am.lay[,2]<-scale(am.lay[,2])[,1]
      distances <- rdist(am.lay[,1:2],g.lay.c[,1:2])
      sol <- solve_LSAP(t(distances))
      am.lay[as.numeric(sol),1:2]<-as.matrix(g.lay.c[,1:2])
    }####finished making circular
  } #finished making am.lay if layout null
  if(!is.null(am.layout)){am.lay<-am.layout}

  #standardise am.lay into desired range
  range.scale<-c(0,2)
  am.lay<-apply(am.lay,2,function(a)range.use(a,min(range.scale),max(range.scale)))
  #finished providing layout


  #set time information from the given day periods:
  max.period<-0:day


  #start getting plot info
  #These could be integrated within the loop but kept seperate for now to allow comparisons across time periods in needed
  #infected = node colour (grey=not yet vs red=yes)
  use.df$infected.t1<-use.df$exposure%in%max.period

  #Recovered = node shading (red=no vs pinky red=yes)
  use.df$recovered.t1<-round(use.df$recovery_time)%in%max.period


  #isolated = node square and outlined - need to make this list like or something
  use.df$isolated.t1<-(round(use.df$isolated_time)<=day) & (round(use.df$release_time)>day) |
    (round(use.df$quarantine_time)<=day) & (round(use.df$release_time)>day)

  #released = node crectangle without outline
  use.df$released.t1<-round(use.df$release_time)<=day


  #make infection lists
  use.inf.el<-use.df[!is.na(use.df$infector),c("infector","v","infected.t1")]

  use.inf.el$x0<-am.lay[use.inf.el$infector,1]
  use.inf.el$y0<-am.lay[use.inf.el$infector,2]
  use.inf.el$x1<-am.lay[use.inf.el$v,1]
  use.inf.el$y1<-am.lay[use.inf.el$v,2]
  #arrows of infection:
  use.inf.el.use<-use.inf.el[use.inf.el$infected.t1,]

  #Plotting information from here
  makeTrans<-function(..., alpha=0.5) {
    if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
    alpha = floor(255*alpha)
    newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
    .makeTrans = function(col, alpha) {
      rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)}
    newColor = apply(newColor, 2, .makeTrans, alpha=alpha)
    return(newColor)}

  #edge info
  edgew<-E(am.i)$weight
  edgew<-range.use(edgew,0.5,1.5)
  edge.cols<-makeTrans("deepskyblue1",alpha=0.7)

  #make size of nodes
  ndeg<-colSums(am>0)
  vert.sizes<-range.use(rank(ndeg),2,6)
  vert.sizes<-vert.sizes+0

  #make vert colours
  vert.bg<-rep('darkgrey',ncol(am))
  vert.bg[use.df$infected.t1]<-"indianred1"
  vert.bg[use.df$recovered.t1]<-"pink"
  vert.bg<-makeTrans(vert.bg,alpha=0.7)

  vert.outline<-makeTrans(rep('darkgrey',ncol(am)),alpha=0.7)
  vert.outline[use.df$infected.t1]<-"indianred1"
  vert.outline[use.df$recovered.t1]<-"pink"
  vert.outline[use.df$isolated.t1]<-"black"

  vert.shapes<-rep("circle",ncol(am))
  vert.shapes[use.df$isolated.t1]<-"square"

  vert.labels<-rep(NA,ncol(am))
  vert.labels.cols<-"deepskyblue1";vert.labels.sizes<-1.5


  #Plotting
  plot.igraph(am.i,layout=am.lay,rescale=F,edge.width=edgew,edge.curved=T,
              edge.color=edge.cols,vertex.label=vert.labels,
              vertex.label.color=vert.labels.cols,
              vertex.label.cex=vert.labels.sizes,
              vertex.size=vert.sizes,vertex.color=vert.bg,
              vertex.frame.color=vert.outline,vertex.shape=vert.shapes,
              xlim=range.scale,ylim=range.scale)

    #infection arrows:
  if(day > 1)
  {
    apply(use.inf.el.use,1,function(a){curvedarrow(a[c("x0","y0")],a[c("x1","y1")],lwd=0.8,lty=1,lcol="firebrick3",arr.col="firebrick3",arr.pos=0.94,curve=0.3,dr=0.01,endhead = T, segment = c(0, 1),arr.type="curved",arr.length=0.25)})
  }

  #if you wanted to generate a layout and then use it next time, this returns it
  if(is.null(am.layout)){return(am.lay)}
}#ends function





#' Run an outbreak model and generate network plot with contagion
#'
#' @author Josh Firth
#' @author Lewis Spurgin
#' @param am association matrix
#' @param day the day to do the contagion for
#' @param am.layout (if wanting to specify a layout e.g. to keep the layout the same across comparisons)
#' @inheritParams outbreak_model
#' @return
#' @export
#' @import igraph
#' @import clue
#' @import diagram
#' @import fields
#'
#' @examples
#'
#' \dontrun{
#'
#' load("data-raw/am_list.RData")
#' m <- am_list[[1]] #This is the association matrix for haslemere
#' plot_network(
#' am = m,
#' day = 20,
#' num.initial.cases = 1,
#' prop.asym = 0.4,
#' delay_shape =  1,
#' delay_scale = 1.4,
#' prop.ascertain = 0.8,
#' presymrate = 0.4,
#' R = 6.5,
#' outside = 0.001,
#' sensitivity = "high",
#' testing = "none",
#' isolation = FALSE,
#' secondary = FALSE,
#' tracing = FALSE,
#' quarantine = FALSE)
#'}

plot_network <- function(am,
                         am.layout = NULL,
                         day,
                          num.initial.cases = NULL,
                          prop.ascertain = NULL,
                          R = NULL, presymrate = NULL, delay_shape = NULL,
                          delay_scale = NULL, prop.asym = NULL,
                          quarantine = NULL, isolation = NULL,
                          tracing = NULL, secondary = NULL,
                          outside = NULL, sensitivity = NULL,
                          testing = NULL, cap_max_tests = NULL, s = NULL) {

  net1 <- format_network(am)
  case_data <- outbreak_model(net = net1,
                              num.initial.cases = num.initial.cases,
                              prop.ascertain = prop.ascertain,
                              cap_max_days = day,
                              R = R, presymrate = presymrate,
                              delay_shape = delay_shape,
                              delay_scale = delay_scale, prop.asym = prop.asym,
                              quarantine = quarantine, isolation = isolation,
                              tracing = tracing, secondary = secondary,
                              outside = outside, sensitivity = sensitivity,
                              testing = testing, cap_max_tests = cap_max_tests,
                              weekly = FALSE, s = s)
  set.seed(s)
  draw.contagion(am = am,
                        am.layout = am.layout,
                        use.df = case_data,
                        day = day)

}






#' Plot a newtwork (shiny app version)
#'
#' @author Josh Firth
#' @author Lewis Spurgin
#' @return
#' @export
#' @import igraph
#' @import clue
#' @import diagram
#' @import fields
#'

plot_network_app <- function(
  am,
  day,
  num.initial.cases,
  prop.asym,
  delay,
  prop.ascertain,
  presymrate,
  R,
  outside,
  sensitivity,
  testing,
  cap_max_tests = NULL,
  scenario){


  if(scenario == "Nothing") {
    isolation <- FALSE
    tracing <- FALSE
    quarantine <- FALSE
    secondary <- FALSE
  } else {
    if(scenario == "Case isolation") {
      isolation <- TRUE
      tracing <- FALSE
      quarantine <- FALSE
      secondary <- FALSE
    } else {
      if(scenario == "Primary tracing") {
        isolation <- TRUE
        tracing <- TRUE
        quarantine <- TRUE
        secondary <- FALSE
      } else {
        if(scenario == "Secondary tracing") {
          isolation <- TRUE
          tracing <- TRUE
          quarantine <- TRUE
          secondary <- TRUE
        }
      }
    }
  }

  if(delay == "Short (0-2 days)")
  {
    delay_shape = 1
    delay_scale = 1.4
  }

  if(delay == "Medium (2-5 days)")
  {
    delay_shape = 1.6
    delay_scale = 2.3
  }

  if(delay == "Long (5-10 days)")
  {
    delay_shape = 4.3
    delay_scale = 9.5
  }

  layout(matrix(1:2,2,1))
  par(mar = c(1,0,0,0))
  plot_network(
    am = am,
    day = day,
    num.initial.cases = num.initial.cases,
    prop.asym = prop.asym,
    delay_shape =  delay_shape,
    delay_scale = delay_scale,
    prop.ascertain = prop.ascertain,
    presymrate = presymrate,
    R = R,
    outside = outside,
    sensitivity = presymrate,
    testing = testing,
    isolation = isolation,
    secondary = secondary,
    tracing = tracing,
    quarantine = quarantine,
    cap_max_tests = cap_max_tests)

  plot.new()
  legend("top",
         pch = c(19,19,19,NA,NA,0),
         lty = c(NA,NA,NA,1,1,NA),
         col = c("darkgrey",
                 "indianred1",
                 "pink",
                 "deepskyblue1",
                 "indianred1",
                 "black"),
         legend = c("Not infected",
                    "Infected",
                    "Recovered",
                    "Contacts",
                    "Infections",
                    "Isolated/quarantined"),
         bty = "n",
         cex = 1.4)

}








#' Generate school network plot with contagion
#'
#' @author Josh Firth
#' @param am association matrix
#' @param use.df the dataframe information of the contagion
#' @param day the day to do the contagion for
#' @param am.layout (if wanting to specify a layout e.g. to keep the layout the same across comparisons)
#'
#' @return
#' @export
#' @import igraph
#' @import clue
#' @import diagram
#' @import fields
#'
#' @examples


draw.sch.contagion<-function(am,am.layout=NULL,layout.type=NULL,use.df,day,groups=NULL){#takes the association matrix, the dataframe information of the contagion, the day to do the contagion for, and the layout (if wanting to specify a layout e.g. to keep the layout the same across comparisons), and the layout.type, ie. forced circular layout (circ) or forced increased group layout (group), and takes the groups

  range.use<-function(x,min.use,max.use){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (max.use - min.use) + min.use } #define for later
  if(!is.null(groups)){
    groupm<-name.class$group==matrix(name.class$group,nrow(name.class),nrow(name.class),byrow=T)
    diag(groupm)<-F
  } #make a ind-by-ind group matrix

  #arrange data:
  use.df<-use.df[order(use.df$caseid),]
  use.df$v<-use.df$caseid
  use.df$degree<-colSums(am>0)
  use.df$isolated_time[is.infinite(use.df$isolated_time)]<-day+1000 #to avoid infinite values
  use.df$release_time[is.infinite(use.df$release_time)]<-day+1000 #to avoid infinite values

  #make the needed igraph object from the association matrix
  am.i<-graph_from_adjacency_matrix(am,"undirected",weighted=T,diag=F)
  am.betgroup<-am
  am.betgroup[groupm]<-0

  i<-graph_from_adjacency_matrix(am,"undirected",weighted=T,diag=F)
  #now, if a layout isn't given in the function, generate the wanted one here:
  if(is.null(am.layout)){
    am.lay<-layout_nicely(am.i)


    if(layout.type%in%c("group","groupcirc")){ #~75% of ties within groups are scored 3, only 2.6% are 0, only 3.2% are 1
      am.group.lay<-am
      am.group.lay[groupm]<-am.group.lay[groupm]*5
      am.group.lay.i<-graph_from_adjacency_matrix(am.group.lay,"undirected",weighted=T,diag=F)
      am.lay<-layout_nicely(am.group.lay.i)
    } #finshed making group based

    #plot(am.lay,col=groupcols[groups])

    if(layout.type%in%c("circ","groupcirc")){
      #making circular
      ps<-nrow(am.lay)
      dim.tl<-ceiling(sqrt(ps))*2 #needs *2 just to make sure we have enough points
      #grid.lay<-expand.grid(floor(-dim.tl/2):ceiling(dim.tl/2),((floor(-dim.tl/2))-0.5):(ceiling((dim.tl/2))+0.5))
      xpoints1<-floor(-dim.tl/2):ceiling(dim.tl/2)
      xpoints2<-xpoints1+0.5
      ypoints1<-xpoints1
      ypoints2<-xpoints2
      grid.lay1<-expand.grid(xpoints2,ypoints1)
      grid.lay2<-expand.grid(xpoints1,ypoints2)
      grid.lay3<-expand.grid(xpoints1,ypoints1)
      grid.lay4<-expand.grid(xpoints2,ypoints2)
      grid.lay<-rbind(grid.lay1,grid.lay2,grid.lay3,grid.lay4)
      grid.lay[,1]<-jitter(grid.lay[,1],1)
      grid.lay[,2]<-jitter(grid.lay[,2],1)
      euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
      grid.lay[,3]<-apply(grid.lay,1,function(a)euc.dist(matrix(c(mean(grid.lay[,1]),mean(grid.lay[,2])),1,2),a))
      max.dist<-grid.lay[,3][((1:length(unique(grid.lay[,3])))[match(grid.lay[,3],sort(unique(grid.lay[,3])))])==ps]
      g.lay.c<-grid.lay[grid.lay[,3]<=max.dist,]
      g.lay.c[,1]<-scale(g.lay.c[,1])[,1]
      g.lay.c[,2]<-scale(g.lay.c[,2])[,1]
      am.lay[,1]<-scale(am.lay[,1])[,1]
      am.lay[,2]<-scale(am.lay[,2])[,1]
      distances <- rdist(am.lay[,1:2],g.lay.c[,1:2])
      sol <- solve_LSAP(t(distances))
      am.lay[as.numeric(sol),1:2]<-as.matrix(g.lay.c[,1:2])
    }####finished making circular

    #plot(am.lay,col=groupcols[groups])


  } #finished making am.lay if layout null


  if(!is.null(am.layout)){am.lay<-am.layout} #finished is null layout

  #standardise am.lay into desired range
  range.scale<-c(0,2)
  am.lay<-apply(am.lay,2,function(a)range.use(a,min(range.scale),max(range.scale)))
  #finished providing layout


  #set time information from the given day periods:
  max.period<-0:day


  #start getting plot info
  #These could be integrated within the loop but kept seperate for now to allow comparisons across time periods in needed
  #infected = node colour (grey=not yet vs red=yes)
  use.df$infected.t1<-use.df$exposure%in%max.period

  use.df$onset.t1<-ceiling(use.df$onset)%in%max.period
  use.df$observed.t1<-ceiling(use.df$onset)%in%max.period & !use.df$asym


  #Recovered = node shading (red=no vs pinky red=yes)
  use.df$recovered.t1<-round(use.df$recovery_time)%in%max.period

  #isolated = node square and outlined - need to make this list like or something
  use.df$isolated.t1<-(round(use.df$isolated_time)<=day) & (round(use.df$release_time)>day)

  #released = node crectangle without outline
  use.df$released.t1<-round(use.df$release_time)<=day


  #make infection lists
  use.inf.el<-use.df[!is.na(use.df$infector),c("infector","v","infected.t1")]

  use.inf.el$x0<-am.lay[use.inf.el$infector,1]
  use.inf.el$y0<-am.lay[use.inf.el$infector,2]
  use.inf.el$x1<-am.lay[use.inf.el$v,1]
  use.inf.el$y1<-am.lay[use.inf.el$v,2]
  #arrows of infection:
  use.inf.el.use<-use.inf.el[use.inf.el$infected.t1,]

  #Plotting information from here
  makeTrans<-function(..., alpha=0.5) {
    if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
    alpha = floor(255*alpha)
    newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
    .makeTrans = function(col, alpha) {
      rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)}
    newColor = apply(newColor, 2, .makeTrans, alpha=alpha)
    return(newColor)}

  #edge info
  edgew<-E(am.i)$weight
  #E(am.i)$weight==am[lower.tri(am)][am[lower.tri(am)]>0]
  #knock out within-group links
  edge.from<-row(am)[lower.tri(am)][am[lower.tri(am)]>0]
  edge.to<-col(am)[lower.tri(am)][am[lower.tri(am)]>0]
  edge.in.group<-groups[edge.from]==groups[edge.to]
  edgew[edge.in.group]<- 0

  edgew<-range.use(edgew,0,0.6)
  edge.cols<-makeTrans("darkgrey",alpha=0.7)


  #make size of nodes
  ndeg<-colSums(am>0)
  nstr<-colSums(am)

  vert.sizes<-range.use(rank(ndeg),3,6)
  vert.sizes<-vert.sizes+0

  #make vert colours:
  groupcols<-unlist(strsplit("aquamarine4,chocolate3,pink4,cyan3,darkgoldenrod3,chartreuse4,darkorchid3,deeppink3,khaki4,wheat4",split=",") )
  vert.bg<-groupcols[groups]
  vert.bg<-makeTrans(vert.bg,alpha=0.6)

  vert.outline<-vert.bg
  vert.outline[use.df$infected.t1]<-"black"
  vert.outline[use.df$infected.t1 & !use.df$asym]<-"black"
  vert.outline[use.df$recovered.t1]<-"darkgrey"


  vert.shapes<-rep("circle",ncol(am))
  vert.shapes[use.df$age=="adult"]<-"square"

  vert.labels<-rep(NA,ncol(am))
  vert.labels[use.df$infected.t1]<-"I"
  vert.labels[use.df$observed.t1]<-"S"
  vert.labels.cols<-"black"
  vert.labels.sizes<-vert.sizes/3

  #Plotting
  plot.igraph(am.i,layout=am.lay,rescale=F,edge.width=edgew,edge.curved=T,edge.color=edge.cols,vertex.label=vert.labels,vertex.label.color=vert.labels.cols,vertex.label.cex=vert.labels.sizes,vertex.size=vert.sizes,vertex.color=vert.bg,vertex.frame.color=vert.outline,vertex.shape=vert.shapes,xlim=range.scale,ylim=range.scale)
  #infection arrows:
  if(sum(!is.na(use.df$infector)) > 0){
  apply(use.inf.el.use,1,function(a){curvedarrow(a[c("x0","y0")],a[c("x1","y1")],lwd=0.7,lty=1,lcol="firebrick3",arr.col="firebrick3",arr.pos=0.925,curve=0.3,dr=0.01,endhead = T, segment = c(0, 1),arr.type="curved",arr.length=0.25)})
}
  #if you wanted to generate a layout and then use it next time, this returns it
  if(is.null(am.layout)){return(am.lay)}
}#ends function





#' Run an outbreak model and generate network plot with contagion
#'
#' @author Josh Firth
#' @author Lewis Spurgin
#' @param am association matrix
#' @param day the day to do the contagion for
#' @param am.layout (if wanting to specify a layout e.g. to keep the layout the same across comparisons)
#' @inheritParams outbreak_model
#' @return
#' @export
#' @import igraph
#' @import clue
#' @import diagram
#' @import fields
#'
#' @examples
#'
#' \dontrun{
#'
#' load("data-raw/am_list.RData")
#' m <- am_list[[1]] #This is the association matrix for haslemere
#' plot_network(
#' am = m,
#' day = 20,
#' num.initial.cases = 1,
#' prop.asym = 0.4,
#' delay_shape =  1,
#' delay_scale = 1.4,
#' prop.ascertain = 0.8,
#' presymrate = 0.4,
#' R = 6.5,
#' outside = 0.001,
#' sensitivity = "high",
#' testing = "none",
#' isolation = FALSE,
#' secondary = FALSE,
#' tracing = FALSE,
#' quarantine = FALSE)
#'}

plot_sch_network <- function(am,
                             df,
                         am.layout = NULL,
                         day,
                         groups,
                         num.initial.cases = NULL,
                         prop.ascertain = NULL,
                         R = NULL, presymrate = NULL, delay_shape = NULL,
                         delay_scale = NULL,
                         asym.adult = NULL,
                         asym.child = NULL,
                         asym.adult.inf = NULL,
                         sym.child.inf = NULL,
                         asym.child.inf = NULL,
                         quarantine = NULL, isolation = NULL,
                         tracing = NULL, secondary = NULL,
                         outside = NULL, sensitivity = NULL,
                         testing = NULL, cap_max_tests = NULL, s = NULL) {

  net1 <- format_network(am)
  case_data <- outbreak_model(net = net1,
                              df = df,
                              num.initial.cases = num.initial.cases,
                              prop.ascertain = prop.ascertain,
                              cap_max_days = day,
                              R = R, presymrate = presymrate,
                              delay_shape = delay_shape,
                              delay_scale = delay_scale,
                              asym.adult = asym.adult,
                              asym.child = asym.child,
                              asym.adult.inf = asym.adult.inf,
                              sym.child.inf = sym.child.inf,
                              asym.child.inf = asym.child.inf,
                              quarantine = quarantine, isolation = isolation,
                              tracing = tracing, secondary = secondary,
                              outside = outside, sensitivity = sensitivity,
                              testing = testing, cap_max_tests = cap_max_tests,
                              output = "raw", s = s)
  set.seed(s)
  draw.sch.contagion(am = am,
                 layout.type = "group",
                 use.df = case_data,
                 day = day,
                 groups = groups)
}
