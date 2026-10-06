xml_to_df <- function(xml_data = ., target_node_name = "scenario"){
    
    #top_name <- xml2::xml_name(xml2::xml_root(xml_data))
    
    # Find all target nodes (e.g., <scenario>)
    target_nodes <- xml2::xml_find_all(xml_data, paste0(".//", target_node_name))
    
    # Get child node names from the first target node
    target_sub_node_names <- purrr::map(target_nodes, 
                                        ~ {
                                            xml2::xml_children(.x) %>% xml2::xml_name() 
                                        })[[1]]
    
    # Build a data frame: one row per node
    purrr::map_dfr(target_nodes,
                   # for each target node (e.g., scenario)
                   function(n){
                       # Loop through the names of the subnotes
                       purrr::map(target_sub_node_names,
                                  
                                  ~ tibble::tibble(!!rlang::sym(.x) := 
                                                       xml2::xml_find_first(n, paste0("./", .x)) %>% 
                                                       xml2::xml_text())) %>% 
                           # Combine the one colum tibbles into a single row
                           purrr::list_cbind()
                   })
    
}

