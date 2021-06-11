#######################################################################
#####       Estimating the primary diet sources in food webs      #####
#####          J. A. Smith Nov 2015 & June 2020                   #####
#######################################################################


## This is the function that is iterated at each level of the food web

diet_product <- function(Props_x, Propi, Rowi) {
  
  #find non-zero cells for next group
  valsNext <- diet_matrix_sim[diet_matrix_sim[,Rowi] > 0, Rowi] #gives non-zero values for a column
  rowsNext <- which(diet_matrix_sim[,Rowi] > 0) #gives you the row numbers
  
  Props_x <- c(Props_x, Propi)
  Proppi <- prod(Props_x, na.rm=TRUE)
  
  if (sum(rowsNext) == 0) {  #if prey is a primary source (phyto, macro, detritus)
    Propsi <- c(Propsi,Proppi)
    loop_links <- loop_links + 1
  }
  
  if (Rowi == row_zoo) {  #if prey is a source of interest (zoopl, phytopl, macro, detritus)
    Propsi_Z <- c(Propsi_Z,Proppi)  #add that diet fraction to source proportion vectors
  }  
  if (Rowi == row_phy) {
    Propsi_Ph <- c(Propsi_Ph,Proppi)
  }
  if (Rowi == row_mac) {
    Propsi_M <- c(Propsi_M,Proppi)
  }
  if (Rowi == row_det) {
    Propsi_D <- c(Propsi_D,Proppi)
  }
  
  #if (sum(rowsPi) == 0) { next }  #if prey was a primary source, don't go any 'deeper' into foodweb
  if (sum(rowsNext) == 0)  {
    skip <- "y"
  } else {
    skip <- "n"
  }  #use this to 'next' outside the function
  
  return(list(valsNext=valsNext, rowsNext=rowsNext, Props_x=Props_x, Propsi=Propsi,
              Propsi_Z=Propsi_Z, Propsi_Ph=Propsi_Ph, Propsi_M=Propsi_M, Propsi_D=Propsi_D,
              skip=skip, loop_links=loop_links))
  #valsPi and rowsPi are data used in the next loop
  
}


####  Run algorithm, using the above function  ####

for (sp in 1:length(fish_cols)) {  #for each species...
  
  print(paste0("Species = ", sp))
  coli <- fish_cols[sp]
  
  # proportion vectors to be saved for this species
  Propsi <- 0  #total primary proportions (should sum to 1; used to check algorithm works)
  Propsi_Z <- 0  #proportion of diet ending at zooplankton
  Propsi_Ph <- 0  #proportion of diet ending at phytoplankton
  Propsi_M <- 0  #proportion of diet ending at macroalgae
  Propsi_D <- 0  #proportion of diet ending at detritus
  
  valsPi <- diet_matrix_sim[diet_matrix_sim[,coli] > 0, coli] #gives non-zero values for a column
  rowsPi <- which(diet_matrix_sim[,coli] > 0) #gives you the row numbers of these values
  
  for (a in 1:length(valsPi)) {  #for each prey group...
    Props_x <- numeric()  #start this fresh with each direct prey group
    level1 <- diet_product(Props_x=Props_x, Propi=valsPi[a], Rowi=rowsPi[a])
    Propsi <- level1$Propsi; Propsi_Z <- level1$Propsi_Z; Propsi_Ph <- level1$Propsi_Ph
    Propsi_M <- level1$Propsi_M; Propsi_D <- level1$Propsi_D; loop_links <- level1$loop_links
    if (level1$skip == "y") { next }  #if prey is a primary source, don't go any 'deeper' into foodweb
    
    for (b in 1:length(level1$valsNext)) {  #for each prey-of-prey group...
      level2 <- diet_product(Props_x=level1$Props_x, Propi=level1$valsNext[b], Rowi=level1$rowsNext[b])
      Propsi <- level2$Propsi; Propsi_Z <- level2$Propsi_Z; Propsi_Ph <- level2$Propsi_Ph
      Propsi_M <- level2$Propsi_M; Propsi_D <- level2$Propsi_D; loop_links <- level2$loop_links
      if (level2$skip == "y")  { next }
      
      for (c in 1:length(level2$valsNext)) {  #for each prey-of-prey-of-prey group, etc...
        level3 <- diet_product(Props_x=level2$Props_x, Propi=level2$valsNext[c], Rowi=level2$rowsNext[c])
        Propsi <- level3$Propsi; Propsi_Z <- level3$Propsi_Z; Propsi_Ph <- level3$Propsi_Ph
        Propsi_M <- level3$Propsi_M; Propsi_D <- level3$Propsi_D; loop_links <- level3$loop_links
        if (level3$skip == "y") { next }
        
        for (d in 1:length(level3$valsNext)) {
          level4 <- diet_product(Props_x=level3$Props_x, Propi=level3$valsNext[d], Rowi=level3$rowsNext[d])
          Propsi <- level4$Propsi; Propsi_Z <- level4$Propsi_Z; Propsi_Ph <- level4$Propsi_Ph
          Propsi_M <- level4$Propsi_M; Propsi_D <- level4$Propsi_D; loop_links <- level4$loop_links
          if (level4$skip == "y") { next }
          
          for (e in 1:length(level4$valsNext)) {
            level5 <- diet_product(Props_x=level4$Props_x, Propi=level4$valsNext[e], Rowi=level4$rowsNext[e])
            Propsi <- level5$Propsi; Propsi_Z <- level5$Propsi_Z; Propsi_Ph <- level5$Propsi_Ph
            Propsi_M <- level5$Propsi_M; Propsi_D <- level5$Propsi_D; loop_links <- level5$loop_links
            if (level5$skip == "y") { next }
            
            for (f in 1:length(level5$valsNext)) {
              level6 <- diet_product(Props_x=level5$Props_x, Propi=level5$valsNext[f], Rowi=level5$rowsNext[f])
              Propsi <- level6$Propsi; Propsi_Z <- level6$Propsi_Z; Propsi_Ph <- level6$Propsi_Ph
              Propsi_M <- level6$Propsi_M; Propsi_D <- level6$Propsi_D; loop_links <- level6$loop_links
              if (level6$skip == "y") { next }
              
              for (g in 1:length(level6$valsNext)) {
                level7 <- diet_product(Props_x=level6$Props_x, Propi=level6$valsNext[g], Rowi=level6$rowsNext[g])
                Propsi <- level7$Propsi; Propsi_Z <- level7$Propsi_Z; Propsi_Ph <- level7$Propsi_Ph
                Propsi_M <- level7$Propsi_M; Propsi_D <- level7$Propsi_D; loop_links <- level7$loop_links
                if (level7$skip == "y") { next }
                
                for (h in 1:length(level7$valsNext)) {
                  level8 <- diet_product(Props_x=level7$Props_x, Propi=level7$valsNext[h], Rowi=level7$rowsNext[h])
                  Propsi <- level8$Propsi; Propsi_Z <- level8$Propsi_Z; Propsi_Ph <- level8$Propsi_Ph
                  Propsi_M <- level8$Propsi_M; Propsi_D <- level8$Propsi_D; loop_links <- level8$loop_links
                  if (level8$skip == "y") { next }
                  
                  for (j in 1:length(level8$valsNext)) {
                    level9 <- diet_product(Props_x=level8$Props_x, Propi=level8$valsNext[j], Rowi=level8$rowsNext[j])
                    Propsi <- level9$Propsi; Propsi_Z <- level9$Propsi_Z; Propsi_Ph <- level9$Propsi_Ph
                    Propsi_M <- level9$Propsi_M; Propsi_D <- level9$Propsi_D; loop_links <- level9$loop_links
                    if (level9$skip == "y") { next }
                    
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  results[1,sp] <- sum(Propsi)
  results[2,sp] <- sum(Propsi_Z)
  results[3,sp] <- sum(Propsi_Ph)
  results[4,sp] <- sum(Propsi_M)
  results[5,sp] <- sum(Propsi_D)
  
}