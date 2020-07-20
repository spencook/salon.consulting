service_segment <- function(tidy_transactions){
  
  # Get a table that shows the customers favorite service
  favorite_service <- tidy_transactions %>%
    dplyr::select(customer_id, first_name, last_name, service_product_discount,
           category, code, description) %>%
    dplyr::filter(service_product_discount == "service") %>%
    dplyr::group_by(customer_id, first_name, last_name, code, description) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::arrange(deplyr::desc(count)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(customer_id, .keep_all = TRUE) %>%
    dplyr::select(customer_id, fav_service = description)
  
  # Match customer_id data type so join works later
  favorite_service$customer_id <- as.numeric(favorite_service$customer_id)
  
  return(favorite_service)
}