promo_segment <- function(tidy_transactions){
  
  # Get a table that gets the customers favorite promo codes
  promo_codes <- tidy_transactions %>%
    dplyr::select(customer_id, first_name, last_name, service_product_discount, 
           category, code, description) %>%
    dplyr::filter(service_product_discount == "discount") %>%
    dplyr::group_by(customer_id, first_name, last_name, code, description) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(customer_id, .keep_all = TRUE) %>%
    dplyr::select(customer_id, fav_service = description)
  
  # Match customer_id data type so join works later
  promo_codes$customer_id <- as.numeric(promo_codes$customer_id)
  
  
  return(promo_codes)
}