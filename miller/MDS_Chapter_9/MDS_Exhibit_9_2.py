# Association Rules for Market Basket Analysis (Python)

# import package for analysis and modeling
from rpy2.robjects import r   # interface from Python to R

r('library(arules)')  # association rules
r('library(arulesViz)')  # data visualization of association rules
r('library(RColorBrewer)')  # color palettes for plots

r('data(Groceries)')  # grocery transactions object from arules package

# show the dimensions of the transactions object
r('print(dim(Groceries))')

r('print(dim(Groceries)[1])')  # 9835 market baskets for shopping trips
r('print(dim(Groceries)[2])')  # 169 initial store items  

# examine frequency for each item with support greater than 0.025
r('pdf(file="fig_market_basket_initial_item_support.pdf", \
    width = 8.5, height = 11)')
r('itemFrequencyPlot(Groceries, support = 0.025, \
    cex.names=0.8, xlim = c(0,0.3), \
    type = "relative", horiz = TRUE, col = "dark red", las = 1, \
    xlab = paste("Proportion of Market Baskets Containing Item", \
      "\n(Item Relative Frequency or Support)"))')
r('dev.off()')    

# explore possibilities for combining similar items
r('print(head(itemInfo(Groceries)))') 
r('print(levels(itemInfo(Groceries)[["level1"]]))')  # 10 levels... too few 
r('print(levels(itemInfo(Groceries)[["level2"]]))')  # 55 distinct levels

# aggregate items using the 55 level2 levels for food categories
# to create a more meaningful set of items
r('groceries <- aggregate(Groceries, itemInfo(Groceries)[["level2"]])')  

r('print(dim(groceries)[1])')  # 9835 market baskets for shopping trips
r('print(dim(groceries)[2])')  # 55 final store items (categories)  

r('pdf(file="fig_market_basket_final_item_support.pdf", \
      width = 8.5, height = 11)')
r('itemFrequencyPlot(groceries, support = 0.025, \
       cex.names=1.0, xlim = c(0,0.5),\
       type = "relative", horiz = TRUE, col = "blue", las = 1,\
       xlab = paste("Proportion of Market Baskets Containing Item",\
    "\n(Item Relative Frequency or Support)"))')
r('dev.off()')   

# obtain large set of association rules for items by category and all shoppers
# this is done by setting very low criteria for support and confidence
r('first.rules <- apriori(groceries, \
       parameter = list(support = 0.001, confidence = 0.05))')
r('print(summary(first.rules))')  # yields 69,921 rules... too many

# select association rules using thresholds for support and confidence 
r('second.rules <- apriori(groceries, \
       parameter = list(support = 0.025, confidence = 0.05))')
r('print(summary(second.rules))')  # yields 344 rules
  
# data visualization of association rules in scatter plot
r('pdf(file="fig_market_basket_rules.pdf", width = 8.5, height = 8.5)')
r('plot(second.rules, \
       control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])), \
  shading = "lift")')   
r('dev.off()')    
  
# grouped matrix of rules 
r('pdf(file="fig_market_basket_rules_matrix.pdf", \
       width = 8.5, height = 8.5)')
r('plot(second.rules, method="grouped", \
       control=list(col = rev(brewer.pal(9, "Greens")[4:9])))')
r('dev.off()')    

# select rules with vegetables in consequent (right-hand-side) item subsets
r('vegie.rules <- subset(second.rules, subset = rhs %pin% "vegetables")')
r('inspect(vegie.rules)')  # 41 rules

# sort by lift and identify the top 10 rules
r('top.vegie.rules <- head(sort(vegie.rules, \
       decreasing = TRUE, by = "lift"), 10)')
r('inspect(top.vegie.rules)') 

r('pdf(file="fig_market_basket_farmer_rules.pdf", width = 11, height = 8.5)')
r('plot(top.vegie.rules, method="graph", \
       control=list(type="items"), \
       shading = "lift")')
r('dev.off()')  

# Suggestions for the student:
# Suppose your client is someone other than the local farmer,
# a meat producer/butcher, dairy, or brewer perhaps.
# Determine association rules relevant to your client's products
# guided by the market basket model. What recommendations
# would you make about future marketplace actions?
