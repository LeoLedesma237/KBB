related_fun <- function(dataset) {
  
  # For testing
  #dataset = HOH.Potential.Matches.unnested.final.shorted
  
  # Extract the DD from the nonDD children
  DD <- dataset[dataset$KBB_DD_status == "Yes",]
  noDD <- dataset[dataset$KBB_DD_status == "No",]
  
  # Create a list that will report the relationship of nonDD children to DD children
  Relationship <- list()
  
  for(ii in 1:nrow(dataset)) {
    
    if (dataset$KBB_DD_status[ii] == "Yes") {
      
      # Extract current DD Child
      current_DD = dataset[ii,]
      
      # Extract all comparisons to current DD
      noDD_com = noDD[noDD$HOH_ID == current_DD$HOH_ID,]
      
      # Empty lists for sibling, half-sibling, or cousin
      sibling_list <- list()
      half_sibling_list <- list()
      cousin_list <- list()
      other_list <- list()
      
      for(iii in 1:nrow(noDD_com)) {
        # For each comp, identify if they are sibling, half-sibling, or cousins
        current_noDD <- noDD_com[iii,]
        
        # check if they are siblings
        if (paste0(current_DD$BM,current_DD$BF) == paste0(current_noDD$BM,current_noDD$BF)) {
          
          sibling_list[[iii]] <- current_noDD$Child_First_Name
          
        } else if(current_DD$BM == current_noDD$BM | current_DD$BF == current_noDD$BF) {
          
          half_sibling_list[[iii]] <- current_noDD$Child_First_Name
          
        } else if (current_DD$mom_sib_group == current_noDD$mom_sib_group | 
                   current_DD$mom_sib_group == current_noDD$dad_sib_group |
                   current_DD$dad_sib_group == current_noDD$mom_sib_group | # Might be redundant
                   current_DD$dad_sib_group == current_noDD$dad_sib_group ) { 
          
          cousin_list[[iii]] <- current_noDD$Child_First_Name
          
        } else {
          
          other_list[[iii]] <- current_noDD$Child_First_Name
        }
        
      }
      
      # Create a new variable for that DD child
      sibling = ""
      half_sibling = ""
      cousin = ""
      other = ""
      
      if (length(sibling_list) != 0) {
        sibling =  paste0("sibling=", paste(do.call(c, sibling_list),collapse = ", "), "; ")
      }
      if (length(half_sibling_list) != 0) {
        half_sibling = paste0("half-sibling=", paste(do.call(c, half_sibling_list),collapse = ", "), "; ")
      }
      if (length(cousin_list) != 0) {
        cousin= paste0("cousin=", paste(do.call(c, cousin_list),collapse = ", "), "; ")
      }
      if (length(other_list) != 0) {
        other = paste0("not-related=", paste(do.call(c, other_list),collapse = ", "), "; ")
      }
      
      # Save the nonDD relationships to current DD 
      Relationship[[ii]] <- paste0(sibling, half_sibling, cousin, other)
      
    } else {
      
      # Save no relationship to the nonDD child 
      Relationship[[ii]] <- "-"
      
    } 
    
  }
  # Return the relationship status as a vector
  do.call(c, Relationship)
}
