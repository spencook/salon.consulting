# The Master Function of the package, uses all the functions in a workflow and prepares
# final cleaned datasets as excel files
#' @export
Salon_Wrangling <- function(){
  
  # Prompt the user for the files of the datasets
  readline(prompt = "Please choose the file for your 'customers' dataset, \n
                    Press [enter] to continue")
  customers_file <- file.choose()
  
  # Again but for the transactions
  readline(prompt = "Please choose the file for your 'transactions' dataset, \n
                    press [enter] to continue")
  transactions_file <- file.choose()
  
  # Read and clean the data
  tidy_transactions <- transactions_cleaning(transactions_file)
  tidy_customers <- customers_cleaning(customers_file)
  
  # Run the segmentation functions
  # This first one gives the customers favorite product
  favorite_products <- product_segment(tidy_transactions)
  favorite_service <- service_segment(tidy_transactions)
  promo_codes <- promo_segment(tidy_transactions)
  
  # identify locals by the first 3 digits of their phone number
  locals <- tidy_customers %>%
    dplyr::mutate(local = ifelse(stringr::str_detect(cell, "^208"), "yes", 'no'))
  
  # Relation function
  relatives <- relation_detection(tidy_transactions) %>%
    dplyr::select(customer_id, guardian = guardian2)
  
  # Create datasets that show the top products/services and discounts
  top_products <- tidy_transactions %>%
    dplyr::filter(service_product_discount == "product") %>%
    dplyr::group_by(code, description) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(count))
  
  top_services <- tidy_transactions %>%
    dplyr::filter(service_product_discount == "service") %>%
    dplyr::group_by(code, description) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    aplyr::arrange(dplyr::desc(count))
  
  top_codes <- tidy_transactions %>%
    dplyr::filter(service_product_discount == "discount") %>%
    dplyr::group_by(code, description) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(dplyr::desc(count))
    
  
  # Match the favorite products onto the customers dataset
  final_customers <- tidy_customers %>%
    dplyr::select(store, customer_number, first_name, last_name, phone, gender, email,
           age_group) %>%
    dplyr::left_join(favorite_products, by = c("customer_number" = "customer_id")) %>%
    dplyr::left_join(favorite_service, by = c("customer_number" = "customer_id")) %>%
    dplyr::left_join(promo_codes, by = c("customer_number" = "customer_id")) %>%
    dplyr::left_join(relatives, by = c("customer_number" = "customer_id"))
    
  
  
  # Write to excel
  # Create the workbook
  wb <- openxlsx::createWorkbook()
  
  # Create the 4 different sheets
  openxlsx::addWorksheet(wb, "Customers")
  openxlsx::addWorksheet(wb, "Top Products")
  openxlsx::addWorksheet(wb, "Top Services")
  openxlsx::addWorksheet(wb, "Top Promo Codes")
  
  # Write the data into the worksheets
  openxlsx::writeData(wb, sheet = "Customers", x = final_customers)
  openxlsx::writeData(wb, sheet = "Top Products", x = top_products)
  openxlsx::writeData(wb, sheet = "Top Services", x = top_services)
  openxlsx::writeData(wb, sheet = "Top Promo Codes", x = top_codes)
  
  # Write workbook to desktop
  # TODO: prompt user for where he wants it saved
  dir <- utils::choose.dir(default = "", caption = "choose a file path")
  openxlsx::saveWorkbook(wb, dir)
  
  return()
}
