# This function gives a customers dataset that shows which customers have a guardian
# on file
# @'
relation_detection <- function(tidy_transactions){
  # First, prepare the data to have guardians - child pairs identified
  # Create columns that look at the value before and after the sorted rows
  transacs_relation_prep <- tidy_transactions %>%
    dplyr::arrange(last_name, date, ticket_number) %>%
    dplyr::select(transaction_id, customer_id, first_name, last_name, ticket_number,
           description, date) %>%
    dplyr::mutate(jr = dplyr::case_when(
      startsWith(description, "JR") ~ "yes", 
      TRUE ~ "no"),
           last_name_before = dplyr::lag(last_name, 1),
           last_name_after = dplyr::lead(last_name, 1),
           date_before = dplyr::lag(date, 1),
           date_after = dplyr::lead(date, 1),
           ticket_before = dplyr::lag(ticket_number, 1),
           ticket_after = dplyr::lead(ticket_number, 1))

  # Add a column that indicates if there is relation
  transacs_relation_prep2 <- transacs_relation_prep %>%
    dplyr::mutate(relation = ifelse(
      transacs_relation_prep$jr == "yes" 
      & transacs_relation_prep$last_name == transacs_relation_prep$last_name_before 
      & transacs_relation_prep$date == transacs_relation_prep$date_before, "before", 
      ifelse(transacs_relation_prep$jr == "yes"
          & transacs_relation_prep$last_name == transacs_relation_prep$last_name_after
          & transacs_relation_prep$date == transacs_relation_prep$date_after, "after", "no")))

  # Add another column that shows the customer_id of the guardian
  transacs_relation_prep3 <- transacs_relation_prep2 %>%
    dplyr::mutate(guardian = ifelse(
      transacs_relation_prep2$relation == "before" & lag(relation, 1) == "no", lag(customer_id, 1),
      ifelse(
        transacs_relation_prep2$relation == "before" & lag(relation, 1) == "before", lag(customer_id, 2),
      ifelse(
        transacs_relation_prep2$relation == "after" & lead(relation, 1) == "no", lead(customer_id, 1),
      ifelse(
        transacs_relation_prep2$relation == "after" & lead(relation, 1) == "after", lead(customer_id, 2),
        "none"
      )))))
      
  # Delete duplicates and finalize the dataset
  final_relation <- transacs_relation_prep3 %>%
    dplyr::mutate(guardian2 = ifelse(
      transacs_relation_prep3$customer_id == transacs_relation_prep3$guardian, "none", 
      transacs_relation_prep3$guardian))
  
  
  return(final_relation)
}



# TODO: remove guardian records of same customer ID, example: Joey Poulson