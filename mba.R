library(arules)
library(arulesViz)
library(tidyverse)
library(plyr)
library(dplyr)
library(readr)

coffee_transactions <- read.csv("coffee_shop_transactions.csv", header = TRUE)
coffee_transactions <-  janitor::clean_names(coffee_transactions)






coffee_transactions <-  coffee_transactions |> mutate(latte = if_else(latte == 0, NA, "latte"),
  hazelnut_latte = if_else(hazelnut_latte == 0, NA, "hazelnut_latte"), 
  vanilla_latte = if_else(vanilla_latte == 0, NA, "vanilla_latte"), 
  creamy_aren_latte = if_else(creamy_aren_latte == 0, NA, "creamy_aren_latte"), 
  caramel_latte = if_else(caramel_latte == 0, NA, "caramel_latte"), 
  matcha_latte = if_else(matcha_latte == 0, NA, "matcha_latte"), 
  cappuccino = if_else(cappuccino == 0, NA, "cappuccino"), 
  caramel_macchiato = if_else(caramel_macchiato == 0, NA, "caramel_macchiato"), 
  americano = if_else(americano == 0, NA, "americano"), 
  matcha_bun = if_else(matcha_bun == 0, NA, "matcha_bun"), 
  cheese_toast = if_else(cheese_toast == 0, NA, "cheese_toast"), 
  chocolate_toast = if_else(chocolate_toast == 0, NA, "chocolate_toast"), 
  butter_toast = if_else(butter_toast == 0, NA, "butter_toast"), 
  cookies = if_else(cookies == 0, NA, "cookies"), 
  croissant = if_else(croissant == 0, NA, "croissant"), 
  egg_salad_toast = if_else(egg_salad_toast == 0, NA, "egg_salad_toast"),
  fries = if_else(fries == 0, NA, "fries"),
  crispy_chicken_nuggets = if_else(crispy_chicken_nuggets == 0, NA, "crispy_chicken_nuggets")
  
)

#to create a list of col names we want to pivot by instead of writing them all manually.
list_of_col_names <- c(colnames(coffee_transactions))
list_of_col_names_to_pivot <- list_of_col_names[2:19] 

coffee_transactions <- coffee_transactions |> group_by(transaction_id) |> 
                                              pivot_longer(cols = list_of_col_names_to_pivot, 
                                                           names_to = NULL, values_drop_na = TRUE
                                                           )


coffee_transactions_basket <- ddply(coffee_transactions, .variables = "transaction_id", 
                                    .fun = function(df)paste(df$value, collapse = ",")
                                    )
coffee_transactions_basket$transaction_id <- NULL
names(coffee_transactions_basket)[1] <- "items"

write.csv(coffee_transactions_basket, "coffee_shop_transactions_basket.csv", row.names = FALSE, 
                                                                             quote = FALSE
          )





coffee_transactions_obj <- read.transactions("coffee_shop_transactions_basket.csv", header = TRUE, 
                                             format = "basket", sep = ",", cols = 1)

summary(coffee_transactions_obj)



itemFrequencyPlot(coffee_transactions_obj, topN = 10, type = "absolute", 
                  main = "Absolute Item Frequency Plot", 
                  mai = c(3,2, 1,2))

inspect(coffee_transactions_obj[1:10])

coffee_association_rules <- apriori(coffee_transactions_obj, parameter = list(supp = .005, conf = .05))

summary(coffee_association_rules)





plot(coffee_association_rules, measure = c("confidence", "lift"), main = "Association Plot for Coffee Shop")



coffee_association_rules <-  coffee_association_rules[!is.redundant(coffee_association_rules, measure = "confidence",
                                               confint = TRUE, level = .50
                                               )]

plot(coffee_association_rules, measure = c("confidence", "lift"), main = "Association Plot for Coffee Shop")


coffee_association_rules <- sort(coffee_association_rules, by = "lift")

inspect(coffee_association_rules[1:14])


plot(coffee_association_rules, measure = c("support", "lift"), main = "Association Plot for Coffee Shop", engine = "html")




