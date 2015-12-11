
library(raster)
grd.size = c(20,20)
clusters = 10
grd <- matrix(NA, nr = grd.size[1], nc = grd.size[2])
rast <- raster(grd)
start <- sample(which(is.na(values(rast))), size = clusters)
rast[start] <- seq(clusters)

while(sum(is.na(values(rast)))>0){
  for(i in seq(clusters)){
    tryagain <- try(
      rast[adjacent(rast, cells = start[i], directions = 8, 
                    target = which(is.na(values(rast))))[,2]] <- i,
      silent = TRUE
    )
    while(inherits(tryagain, "try-error")){
      newstart <- sample(which(values(rast) == i), size = 1)
      tryagain <- try(rast[adjacent(rast, cells = newstart, directions = 8, 
                    target = which(is.na(values(rast))))[,2]] <- i,
                    silent = TRUE
      )
    }
  }
  start <- sapply(seq(clusters), function(x) sample(which(values(rast) == x), size = 1))
}

# random cluster function

grd.size = c(20,20)
random = FALSE

# set up (randomized) values
if(random){
  vals.rand <- sample(clusters, size = prod(grd.size), replace = T)
  vals <- lapply(seq(clusters), function(x) vals.rand[vals.rand == x])
}else{
  if((prod(grd.size)/10) %% 2 == 0){
    vals <- lapply(seq(clusters), function(x) rep(x, times = prod(grd.size)/clusters))
  }else{
    stop("Number of grid cells is not a multiple of the number of clusters. Set random to TRUE")  
  }
}

# set up grid
grd <- matrix(NA, nr = grd.size[1]+2, nc = grd.size[2]+2)
grd[1, ] <- grd[grd.size[1]+2, ] <- 999
grd[, 1] <- grd[, grd.size[2]+2] <- 999


for(i in seq(clusters)){
  # set start point
  start <- sample(which(is.na(grd)), size = 1)
  start <- as.numeric(arrayInd(start, dim(grd)))
  used <- length(vals[[i]])
  while(used > 0){
    grd[start[1], start[2]] <- i 
    used <- used - 1 # remove one from the list
    
    # define window
    win <- grd[(start[1]-1) : (start[1]+1), (start[2]-1) : (start[2]+1)]
    #if(sum(is.na(win)) > 0){
      # if empty cells available select from von-Neumann neighborhood
    if(sum(is.na(win[seq(2,8,2)])) > 0){
      move <- sample(which(is.na(win))[which(which(is.na(win)) %in% seq(2,8,2))] , size = 1)
#       }else{ # if no empty cells in von-Neumann neighborhood, try Moore
#         move <- sample(which(is.na(win)), size = 1)
#       }
      move <- as.numeric(arrayInd(move, dim(win))) - c(2,2)
      start <- start + move
    }else{
      newstart <- FALSE
      while(!newstart){
        same.val <- sample(which(grd == i), size = 1)
        same.val <- as.numeric(arrayInd(same.val, dim(grd)))
        win.same.val <- grd[(same.val[1]-1) : (same.val[1]+1), 
                            (same.val[2]-1) : (same.val[2]+1)]
        if(sum(is.na(win.same.val[seq(2,8,2)])) > 0){
          start <- same.val + as.numeric(arrayInd(sample(
            which(is.na(win.same.val))[which(which(is.na(win.same.val)) %in% seq(2,8,2))], 
            size = 1), dim(win.same.val))) - c(2,2)
          
          newstart <- TRUE
        }
      }
    }
  }
}
  



# gimmicks
if(start[1] %in% 2:(grd.size[1]-1) & start[2] %in% 2:(grd.size[2]-1)){
  win <- grd[(start[1]-1) : (start[1]+1), (start[2]-1) : (start[2]+1)]
}else{
  # if start somewhere at the frame but not corner
  if(start[1] == 1 & start[2] %in% 2:(grd.size[2]-1)){ # top
    win <- grd[start[1] : (start[1]+1), (start[2]-1) : (start[2]+1)]
  }else{
    if(start[1] == NROW(grd) & start[2] %in% 2:(grd.size[2]-1)){ # bottom
      win <- grd[(start[1]-1) : start[1], (start[2]-1) : (start[2]+1)]
    }else{
      if(start[1] %in% 2:(grd.size[1]-1) & start[2]  == 1){ # left
        win <- grd[(start[1]-1) : (start[1]+1), start[2] : (start[2]+1)]
      }else{
        if(start[1] %in% 2:(grd.size[1]-1) & start[2] == NCOL(grd)){ # right
          win <- grd[(start[1]-1) : (start[1]+1), (start[2]-1) : start[2]]
        }else{
          
          # if start somewhere at in the corner
          if(start[1] == 1 & start[2] == 1){ # topleft
            win <- grd[start[1] : (start[1]+1), start[2] : (start[2]+1)]
          }else{
            if(start[1] == NROW(grd) & start[2] == 1){ # bottomleft
              win <- grd[(start[1]-1) : start[1], start[2] : (start[2]+1)]
            }else{
              if(start[1] == NROW(grd) & start[2]  == NCOL(grd)){ # bottomright
                win <- grd[(start[1]-1) : start[1], (start[2]-1) : start[2]]
              }else{
                if(start[1] == 1 & start[2] == NCOL(grd)){ # topright
                  win <- grd[start[1] : (start[1]+1), (start[2]-1) : start[2]]
                }
              }
            }
          }
        }
      }
    }
  }
}




#
if(sum(is.na(c(grd[start[1], (start[2]-1)],
               grd[(start[1]-1), start[2]],
               grd[(start[1]+1), start[2]],
               grd[start[1], (start[2]+1)]))) > 0){
  win <- c(grd[start[1], (start[2]-1)],
           grd[(start[1]-1), start[2]],
           grd[(start[1]+1), start[2]],
           grd[start[1], (start[2]+1)])
  move <- sample(which(is.na(win)), size = 1)
  if(move == 1) move <- c(0, -1) else
    if(move == 2) move <- c(-1, 0) else
      if(move == 3) move <- c(1, 0) else
        if(move == 4) move <- c(0, 1)