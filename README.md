library(tidyr)
library(dplyr)



#function used to make column converting product codes to product categories
code_to_category <- function(x) {
  ifelse(x == "p", "Smartphone",
  ifelse(x == "v", "TV",
  ifelse(x == "x", "Laptop",
  ifelse(x == "q", "Tablet", NA))))
}

refine_original <- refine_original.csv #renameing the csv file
refine_original <- refine_original %>% 
  arrange(company)#arranging data frame by company name, this puts the rows in the right place to be changed to the proper names.
refine_original$company[1:7] <- "akzo" #changing mispellings of akzo 
refine_original$company[8:16] <- "phillips" #changing mispellings of philips
refine_original$company[17:20] <- "unilever" #changing mispellings of unilever
refine_original$company[21:25] <- "van houten" #changing mispellings of van houten

refine_original <- separate(refine_original, Product.code...number, c("product_code", "product_number"), sep = "-") %>% #generates product code and product number columns
  mutate(product_category = code_to_category(c(product_code))) %>% #uses created function at the top to generate product category column
  mutate(full_address = paste(address, city, country, sep=", ")) %>% #creates full address column from address, city and country
  mutate(company_philips = ifelse(company == "phillips", 1, 0)) %>% #Next four lines create binary columns for company names
  mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>% 
  mutate(company_van_houten = ifelse(company == "van houten",1, 0)) %>% 
  mutate(company_unilever = ifelse(company == "unilever",1 ,0)) %>% 
  mutate(product_smartphone = ifelse(product_code == "p", 1, 0)) %>% #next four lines generate binary product coulmns
  mutate(product_tv = ifelse(product_code == "v", 1, 0)) %>% 
  mutate(product_laptop = ifelse(product_code == "x", 1, 0)) %>% 
  mutate(product_tablet = ifelse(product_code == "q", 1, 0))
