
resample_blocks_by_area = function ( x, y, nBoot, lookup_table, block_L, Grid_space, area_or_sites="area", shape=shape,lookup_tablename="lookup_table",lookuptables.folderpath,...) {
  if(missing(Grid_space)) {
    Grid_space = block_L/3
  }
  if(Grid_space > block_L) {
    Grid_space = block_L
  }
  Grid = create_Grid (x = x,y = y, Grid_space = Grid_space,shape = shape) 
  if(is.na(lookup_table)) {
    lookup_table = create_moving_block_lookup(x = x, y = y, block_L = block_L, Grid_space = Grid_space, Grid = Grid, shape=shape)
    if (is.na(lookuptables.folderpath) ==FALSE){
      lookup.coords = list(lookup.x = x,lookup.y = y)
      save(lookup_table,lookup.coords, file=paste0(lookuptables.folderpath,lookup_tablename,"_L",block_L,"_grid_space_",Grid_space,"_",shape,".RData"))
    }
  }
  nGrid=nrow(Grid)
  new.samples.index = list()

    if ( area_or_sites =="area"){
    x_range = max(x)-min(x)
    y_range = max(y)-min(y)
    reg_size = x_range*y_range
    if (shape == "disc") {  block_size = pi*block_L^2 }
    if (shape == "square") { block_size = block_L^2 }
    n_blocks = round(reg_size/block_size)
    #perform resampling to get the same area as orginial region
    for (i in 1:nBoot){
      sites = c()
      for ( j in 1:n_blocks){
        #print(j)
        random_block = sample ( 1:nGrid , 1 ,replace=TRUE)
        if (random_block <= length(lookup_table)){
          sites_in_block = unlist (lookup_table [[ random_block ]] )
          sites = c( sites, sites_in_block )
          #print(length(sites_in_block))
          #print(length(sites))
        } 
      }
      new.samples.index [[i]] = sites
    }
  }
  if ( area_or_sites =="sites"){
    #perform resampling to get the same number of sites as orginial dataset
    for (i in 1:nBoot){
      nsites=0
      sites = c()
      while (nsites < length(x)){
        random_block = sample ( 1: length(lookup_table) , 1 ,replace=TRUE)
        sites_in_block = unlist (lookup_table [[ random_block ]] )
        sites = c( sites, sites_in_block )
        nsites=length(sites)
      }
      new.samples.index [[i]] = sites[1:length(x)]
    }
  }
  
  new.samples.index
}


create_Grid = function(Grid_space, x, y, shape,...){
  #Grid_space is the spacing of the Grid points which form the corners/ centres of the sampling units (squares/ circles)
  #x , y are coordinates of the sites
  
  #order x,y by x then y
  o=order(x,partial=y)
  
  x_range = max(x)-min(x)
  y_range = max(y)-min(y)
  nx = ceiling ( x_range/Grid_space ) #number of vertical grid lines
  ny = ceiling ( y_range/Grid_space ) #number of horizontal grid lines
  trimx =  ceiling ( x_range/Grid_space ) - x_range/Grid_space 
  trimy =  ceiling ( y_range/Grid_space ) - y_range/Grid_space 
  x0 = min ( x ) - trimx/2 * Grid_space  #where the vertical grid lines start
  y0 = min ( y ) - trimy/2 * Grid_space  #where the horizontal grid lines start
  
  xM = x0 + nx*Grid_space
  yM = y0 + ny*Grid_space
  
  
  #the x and y Grid coordinates
  if (shape=="disc"){
    fineGrid_x = x0 + 1:(nx-1)  * Grid_space
    fineGrid_y = y0 + 1:(ny-1)  * Grid_space
  }
  if(shape == "square"){
    seqx = c ( 0, 1:(nx-1))
    seqy = c ( 0, 1:(ny-1))
    fineGrid_x = x0 + seqx  * Grid_space
    fineGrid_y = y0 + seqy  * Grid_space
  }
  
  
  #the x and y Grid coordinates to resample
  
  Grid = merge ( fineGrid_x, fineGrid_y, all.x = TRUE, all.y = TRUE )
  return(Grid)
}