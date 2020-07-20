transactions_cleaning <- function(transactions_file){
  
  # Read in data and 
  transactions <- readr::read_csv(transactions_file)
  
  
  # Clean the transactions dataset
  tidy_transactions <- transactions %>%
    dplyr::mutate(date = lubridate::mdy(Date)) %>%
    dplyr::select(store = `Store Name`, transaction_id = `Transaction ID`, customer_id = `Customer ID`,
           first_name = `First Name`, last_name = `Last Name`, date, ticket_number = `Ticket Number`,
           service_product_discount = `Service/Product/Discount`, category = Category,
           brand = Brand, code = Code, description = Description) 
  
  return(tidy_transactions)
}