sub_prop %>% ggplot(aes(Region, prop, fill = Region)) 
+ geom_col(position = "dodge") 
+ facet_grid(~`Sub-Category`) 
+ geom_text(aes(label = round(prop,2)), vjust = -0.4, alpha = 0.6, size = 3) 
+ theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
+ ggtitle("Proportion of products sold in each Region per Category") 
+ theme(plot.title = element_text(size=14, face="bold")) 
+ scale_fill_manual(values = c("#C3D7A4", "#52854C", "#4E84C4", "#293352"))

sub_prop_furn <- office_furn %>% group_by(Region, `Sub-Category`) %>% 
  summarise(items = sum(Quantity)) %>% ungroup() %>% group_by(`Sub-Category`) %>% 
  mutate(total_items = sum(items), prop = items/total_items) %>% 
  ungroup() %>% select(`Sub-Category`, Region, items, total_items, prop) %>% arrange(`Sub-Category`)

sub_prop_tech <- office_tech %>% group_by(Region, `Sub-Category`) %>% 
  summarise(items = sum(Quantity)) %>% ungroup() %>% group_by(`Sub-Category`) %>% 
  mutate(total_items = sum(items), prop = items/total_items) %>% ungroup() %>% 
  select(`Sub-Category`, Region, items, total_items, prop) %>% arrange(`Sub-Category`)

sub_prop_tech <- office_off %>% group_by(Region, `Sub-Category`) %>% 
  summarise(items = sum(Quantity)) %>% ungroup() %>% group_by(`Sub-Category`) %>% 
  mutate(total_items = sum(items), prop = items/total_items) %>% ungroup() %>% 
  select(`Sub-Category`, Region, items, total_items, prop) %>% arrange(`Sub-Category`)

props_2014 <- office_supp %>% filter(format(as.Date(`Order Date`, format="%d/%m/%Y"),"%Y") == 2014) %>% 
  group_by(Region, Category) %>% summarise(items = sum(Quantity)) %>% ungroup() %>% 
  group_by(Category) %>% mutate(total_items = sum(items), prop = items/total_items) %>% ungroup()

props_2015 <- office_supp %>% filter(format(as.Date(`Order Date`, format="%d/%m/%Y"),"%Y") == 2015) %>% 
  group_by(Region, Category) %>% summarise(items = sum(Quantity)) %>% ungroup() %>% 
  group_by(Category) %>% mutate(total_items = sum(items), prop = items/total_items) %>% ungroup()

props_2016 <- office_supp %>% filter(format(as.Date(`Order Date`, format="%d/%m/%Y"),"%Y") == 2016) %>% 
  group_by(Region, Category) %>% summarise(items = sum(Quantity)) %>% ungroup() %>% 
  group_by(Category) %>% mutate(total_items = sum(items), prop = items/total_items) %>% ungroup()

props_2017 <- office_supp %>% filter(format(as.Date(`Order Date`, format="%d/%m/%Y"),"%Y") == 2017) %>% 
  group_by(Region, Category) %>% summarise(items = sum(Quantity)) %>% ungroup() %>% 
  group_by(Category) %>% mutate(total_items = sum(items), prop = items/total_items) %>% ungroup()
