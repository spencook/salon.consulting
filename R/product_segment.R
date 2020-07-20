product_segment <- function(tidy_transactions){
  
  # Get a table that shows the customers favorite product
  favorite_product <- tidy_transactions %>%
    dplyr::select(customer_id, first_name, last_name, service_product_discount,
           category, code, description) %>%
    dplyr::filter(service_product_discount == "product") %>%
    dplyr::group_by(customer_id, first_name, last_name, code, description) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(customer_id, .keep_all = TRUE) %>%
    dplyr::select(customer_id, fav_product = description)
  
  # Match customer_id data type so join works later
  favorite_product$customer_id <- as.numeric(favorite_product$customer_id)
  
  return(favorite_product)
  
}