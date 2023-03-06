office_year <- office_supplies %>% 
  mutate(`Order Date` = format(as.Date(office_supplies$`Order Date`, format="%d/%m/%Y"),"%Y"))

office_year %>% group_by(`Order Date`, Region) %>% summarise(annual_profit = sum(Profit)) %>% 
  ggplot(aes(`Order Date`, annual_profit, color = Region, group = 1)) + 
  geom_point() + geom_line() + facet_wrap(~Region) +
  scale_color_manual(values = c("#C3D7A4", "#52854C", "#4E84C4", "#293352")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  ggtitle("Annual Profit in each Region") +
  theme(plot.title = element_text(size=14, face="bold"))

table2 <- office_year %>% group_by(`Order Date`, Region) %>% summarise(annual_profit = sum(Profit)) %>%
  ungroup() %>% select(Region, `Order Date`, annual_profit) %>% arrange(Region)

office_year %>% group_by(`Order Date`, Region) %>% summarise(total_profit = sum(Profit), n = n()) %>%
  mutate(profit_per_order = total_profit / n) %>%
  ggplot(aes(`Order Date`, profit_per_order, color = Region, group = 1)) + 
  geom_point() + geom_line() + facet_wrap(~Region) +
  scale_color_manual(values = c("#C3D7A4", "#52854C", "#4E84C4", "#293352")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  ggtitle("Annual Profit per order in each Region") +
  theme(plot.title = element_text(size=14, face="bold"))
  
table3 <- office_year %>% group_by(`Order Date`, Region) %>%
  summarise(total_profit = sum(Profit), n = n()) %>% mutate(profit_per_order = total_profit / n) %>%
  ungroup() %>% select(Region, `Order Date`, profit_per_order) %>% arrange(Region)

office_supplies %>% ggplot(aes(`Order Date`, fill = Region)) + geom_histogram(bins = 12) + 
  facet_grid(~Region) + scale_fill_manual(values = c("#C3D7A4", "#52854C", "#4E84C4", "#293352")) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  ggtitle("Orders over time in each Region") + 
  theme(plot.title = element_text(size=28, face="bold"))

office_supplies %>% ggplot(aes(`Order Date`)) + geom_histogram(bins = 16) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  ggtitle("Orders over time") + 
  theme(plot.title = element_text(size=22, face="bold"))

write_xlsx(table2, "/Users/theo/Desktop/table2.xlsx")
write_xlsx(table3, "/Users/theo/Desktop/table3.xlsx")