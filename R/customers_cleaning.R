customers_cleaning <- function(customers_file){
  
  # Read in the data and 
  
  customers <- readr::read_csv(customers_file)
  
  # Clean the customers dataset
  # Normalize phone numbers
  # Remove unnecesary columns
  tidy_customers <- customers %>%
    dplyr::select(store = Store, customer_number = `Customer Number`, first_name = `First Name`,
           last_name = `Last Name`, cell = CellPhone, gender = Gender, email = Email,
           age_group = `Age Group`, last_visit = `Last Visit`, notes = Notes,
           active = Active) %>%
    dplyr::mutate(phone = stringr::str_remove_all(cell, '-'),
          phone_length = stringr::str_length(phone))
  
  # TODO: Clean the date format
  
  
  return(tidy_customers)
    
}