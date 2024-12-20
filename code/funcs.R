##### Question 2 Solution Function ######

get_cdc_data <- function(endpoint) {
  request(endpoint) %>% 
  req_url_query("$limit" = 10000000) %>% 
  req_perform() %>%  
  resp_body_json(simplifyVector = TRUE)
}



