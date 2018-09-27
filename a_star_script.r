#node(node number, cost from origin, heuristic, list of path from origin )

#List() implementation not written by myself, found online. Source: 
List <- function() {
  listValues <<- NULL
  insert <- function(value) listValues <<- c(listValues, list(value))
  exists <- function(value) isTRUE(which(listValues %in% list(value) == TRUE) > 0)
  getAllValues <- function() listValues
  list(insert = insert, exists = exists, getAllValues = getAllValues)
}


#coordinate of current node where we're at
#cx,cy are the car coords and p is packages, px, py are the package coords

get_closest_package <- function(car, packages) {
  #compare lengths
  #next_package <- c(p[1,1],p[1,2])
  #loop, get out all packages with status 0, not delivered
  package_statusees <- c(packages[,5])
  no_of_packages <- length(package_statusees)
  
  
  for( n in no_of_packages){
    #spit out the distance to packages
    distances_vector <-c(distance_manhatt(car$x,car$y,packages[n,1],packages[n,2]))
    
  }
  
  row_of_closest_package <- which.min(distances_vector)
  
  return(packages[row_of_closest_package,])
  
  ##this function can successfully returns the index or the (row) of where the closest package is.
  ##use this function to compare if the next closest package is closer than the closest delivery point. 
}


compare_package_and_dev <- function(package, dev_point){
 
}

distance_manhatt <- function (x1, x2, y1, y2) {
  manhatt_distance <- abs(x2-x1) + abs(y2-y1)
  return(manhatt_distance)
}

####EDGE COST FUNCTIONS
horizontal_edge_cost <- function (start_point, destination, roads) {
  #LEFT
  if(start_point[1] > destination[1])     {
     
    return(roads$hroads[destination[1],destination[2]])
  }
  #moving right
  else {
     
    return(roads$hroads[start_point[1], start_point[2] ] )
  }
}
vertical_edge_cost <- function (start_point, destination, roads) {
  #moving down
  if(start_point[2] > destination[2]) {
     
    return(roads$vroads[destination[1],destination[2]])
  }
  #moving up
  else {
     
    return(roads$vroads[start_point[1], start_point[2] ] )
  }
}


is_destination <- function(start_point, goal){
  if(start_point[1]==goal[1]&&start_point[2]==goal[2]){
       return(TRUE)
   }
  return(FALSE)
}



#this function will return the nodes surrounding our given point
get_adjacent_nodes <-function(start_point, roads , destination){
  x <- start_point[1]
  y <- start_point[2]
  frontier = list()
  #horizontal node costs
  if(x>1){frontier=append(frontier, list(list(node=c(x-1,y), price = horizontal_edge_cost(start_point,c(x-1,y),roads),
    heuristic = distance_manhatt(x, destination[1], y, destination[2]) ,path= list())) ) }
  #horizontal node costs
  if(x<10){frontier=append(frontier, list(list(node=c(x+1 ,y), price=horizontal_edge_cost(start_point,c(x+1,y),roads),
    heuristic = distance_manhatt(x +1, destination[1],y , destination[2]),path= list())) )}
  
  #vertical node costs
  if(y>1){frontier=append(frontier, list(list(node=c(x,y-1), price=vertical_edge_cost(start_point,c(x,y-1),roads),
    heuristic = distance_manhatt(x, destination[1],y-1, destination[2] ),path= list())) ) }
  
  #vertical node costs
  if(y<10){frontier=append(frontier, list(list(node=c(x,y+1), price=vertical_edge_cost(start_point,c(x,y+1),roads),
   heuristic = distance_manhatt(x, destination[1],y+1, destination[2] ),path= list())) ) }
  
  #the most compressed line ever. organizes the frontier, need to fix the leadup to this if this is to work
  return(frontier[order(sapply(frontier, function(x) {return(x[[2]] + x[[3]])}))]  )
  
}




##goal is either the pickup point or the delivery point
a_star_search<- function(car, roads, goal){
  current_node = c(car$x,car$y)
 
  #start point is a double vector
  visited <- List()
  frontier <- list(list(node = c(current_node[1],current_node[2]), price = 0, heuristic = distance_manhatt(current_node[1], goal[1], current_node[2], goal[2]), path= list()) ) 

  
  while(length(frontier) != 0){
  
   if(is_destination(current_node,goal)){
      return(frontier)
    }else{
      
       #Add current node to the visited list
       visited_node <- frontier[[1]]
       visited$insert(visited_node)
       
       ##Expand the node
       new_frontier <- get_adjacent_nodes(current_node,roads,goal)
       
       ##check if node already exists 
       for(i in length(new_frontier)){
         
         print(length(new_frontier))
         cat("New frontier[[i]]$node:",new_frontier[[i]]$node )
         print(typeof(new_frontier[[i]]$node ))
         cat("visited_node$node:", visited_node$node)
         print(typeof(visited_node$node))
         
         if(isTRUE(all.equal(new_frontier[[i]]$node, visited_node$node))){
           
            new_frontier[[i]]$price =  new_frontier[[i]]$price + visited_node$price
            
         }else{
           frontier=append(frontier, new_frontier[[i]])
         }
         
       }
      
       ##if something already exists, then we need to add the cost
       current_node<- frontier[[1]]$node
     
       
    }
  }
  
  ##Get the first node but don't return it yet, just expand that one and re-run this function from that node

  #organise the frontier taking into consideration the cost of getting to get there and the distance from destination
  
  
}






myFunction <- function(roads,car,packages){
    

    carcoords<- c(car$x,car$y)

    nextMove=0
    toGo=0
    offset=0
    dest_point=c(0,0)
    
    if (car$load==0) {
      ##which returns a vectur with all the rows where the fifth element is 0, the [1] at the end means that we pick out the first 
      ##element in this vector. 
      toGo=which(packages[,5]==0)[1]
      dest_point = c(packages[toGo,1],packages[toGo,2])
      print("Pickup package")
      print(dest_point)
     } else {
      toGo=car$load
      dest_point = c(packages[toGo,3], packages[toGo,4])
      print("Deliver package")
      print(dest_point)
      offset=2 # To skip the columns of the matrix where pickup points are
    }
    
   

    #frontier = a_star_search(car, roads, dest_point)
    #next_node = frontier[[1]]$node
    
    path = a_star_search(car,roads,dest_point)
    next_node = path[[1]]$node
    
    if (car$x<next_node[1]) {nextMove=6}
    else if (car$x>next_node[1]) {nextMove=4}
    else if (car$y<next_node[2]) {nextMove=8}
    else if (car$y>next_node[2]) {nextMove=2}
    else {nextMove=5}
    

    
    print("###")
    car$nextMove=nextMove
    car$mem=list()
    return (car)

  
}

