library(tidyverse)
test_price = read.csv("data.csv")
head(test_price)

top_brand_all <- test_price %>% count(Brand, sort = TRUE) %>%  .[[1, 1]]
top_category_all <- test_price %>% count(Category, sort = TRUE) %>% .[[1, 1]]
top_subcategory_all <- test_price %>% count(SubCategoryNr, sort = TRUE) %>% .[[1, 1]]
tops <- tibble(Top = "Top", Brand = top_brand_all, Category = top_category_all, SubCategoryNr = top_subcategory_all) %>% column_to_rownames("Top")



#переопределим функции с заданными аргументами по умолчанию для 25 и 75 процентиля
quantile75 <- function(x) quantile(x, probs = 0.75)
quantile25 <- function(x) quantile(x, probs = 0.25)
fun_range <- function(x) range(x)[2]-range(x)[1]
descriptive_stat_on_top_subcategory_in_top_category_in_top_brand <- 
  test_price %>% 
  filter(Brand == top_brand_all & Category == top_category_all & SubCategoryNr == top_subcategory_all) %>%
  summarise_at('SellPrice', 
               list(Mean_price = mean, 
                    Median_price = median, 
                    Min_price = min, 
                    Max_price = max, 
                    Q25 = quantile25, 
                    Q75 = quantile75, 
                    Range = fun_range)) %>% 
  mutate(Top = str_c('Brand:', top_brand_all, 
                     'Category:', top_category_all, 
                     'SubCategoryNr:', top_subcategory_all, 
                     sep = ' '), 
         .before = everything())

top_categories <- c('Brand', 'Category', 'SubCategoryNr')
# создаем именованный вектор. Значения (текстовые) нужны для filter, имена нужены для summarise_at .id
# цифровые значения бренда, категории, субкатегории нужны для filter
names(top_categories) <- map2_chr(top_categories, c(top_brand_all, top_category_all, top_subcategory_all), ~str_c(.x, .y, sep = ": ")) 

descriptive_stat_on_top <- top_categories %>%  
  syms() %>% 
  map2_df(c(top_brand_all, top_category_all, top_subcategory_all), 
          #анонимная функция фильтрует датафрейм по самому частому бренду, категории или субкатегории, считает статистику для каждого значения filter. map2_df собирает все в единый датафрейм
          ~(test_price %>% 
              filter(!!.x == .y) %>% 
              summarise_at('SellPrice', 
                           list(Mean_price = mean, 
                                Median_price = median, 
                                Min_price = min, 
                                Max_price = max, 
                                Q25 = quantile25, 
                                Q75 = quantile75, 
                                Range = fun_range))),
          .id = 'Top')

#соединяем два датафрейма в один
descriptive_stat_on_top <- descriptive_stat_on_top %>% bind_rows(descriptive_stat_on_top_subcategory_in_top_category_in_top_brand)

descriptive_stat_on_top %>% select(Top, Range) %>% print()

descriptive_stat_on_top %>% select(Top, Median_price) %>% print()

top_brand_35 <-  test_price %>% count(Brand, sort = TRUE) %>% .[c(1:35), 1]
top_category_35 <- test_price %>% count(Category, sort = TRUE) %>% .[c(1:35), 1]
top_subcategory_35 <- test_price %>% count(SubCategoryNr, sort = TRUE) %>% .[c(1:35), 1]

top_brand_bar <- test_price %>% 
  filter(Brand %in% top_brand_35) %>% 
  ggplot(aes(x = as_factor(Brand), y = SellPrice))+
  stat_summary(fun = "mean", geom = "bar")+
  labs(x = 'Brand')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
top_brand_bar

top_category_bar <- test_price %>% 
  filter(Category %in% top_category_35) %>% 
  ggplot(aes(x = as_factor(Category), y = SellPrice))+
  stat_summary(fun = "mean", geom = "bar")+
  labs(x = 'Category')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
top_category_bar

top_subcategory_bar <- test_price %>% 
  filter(SubCategoryNr %in% top_subcategory_35) %>% 
  ggplot(aes(x = as_factor(SubCategoryNr), y = SellPrice))+
  stat_summary(fun = "mean", geom = "bar")+
  labs(x = 'SubCategoryNr')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
top_subcategory_bar

plots_bar <- c(list(top_brand_bar), list(top_category_bar), list(top_subcategory_bar))

#boxplot w/ outliers. Для визуализации всех значений нужно ось 'y' преобразовать в log10
top_brand_box <- test_price %>% 
  filter(Brand %in% top_brand_35) %>% 
  ggplot(aes(x = as_factor(Brand), y = SellPrice))+
  geom_boxplot()+
  scale_y_log10()+
  labs(x = 'Brand')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
top_brand_box

top_category_box <- test_price %>% 
  filter(Category %in% top_category_35) %>% 
  ggplot(aes(x = as_factor(Category), y = SellPrice))+
  geom_boxplot()+
  scale_y_log10()+
  labs(x = 'Category')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
top_category_box

top_subcategory_box <- test_price %>% 
  filter(SubCategoryNr %in% top_subcategory_35) %>% 
  ggplot(aes(x = as_factor(SubCategoryNr), y = SellPrice))+
  geom_boxplot()+
  scale_y_log10()+
  labs(x = 'SubCategoryNr')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
top_subcategory_box

plots_box <- c(list(top_brand_box), list(top_category_box), list(top_subcategory_box))

# для выбора шага интервала наиболее часто (среди прочих) используют формулу Стерджеса. Воспользуемся ею, передав описывающую ее функцию в geom_histogram(binwidth) в качестве аргумента. Также для визуализации преобразуем ось 'x' и 'y' в log10.

top_brand_hist <- test_price %>% filter(Brand %in% top_brand_all) %>%  
  ggplot(aes(SellPrice))+
  geom_histogram(binwidth = function(x) (max(x)-min(x))/nclass.Sturges(x))+
  scale_x_log10()+
  scale_y_log10()+
  labs(title =str_c('Brand: ', top_brand_all))+
  theme_minimal()
top_brand_hist
top_category_hist <- test_price %>% filter(Category %in% top_category_all) %>% 
  ggplot(aes(SellPrice))+
  geom_histogram(binwidth = function(x) (max(x)-min(x))/nclass.Sturges(x))+
  scale_x_log10()+
  scale_y_log10()+
  labs(title =str_c('Category: ', top_category_all))+
  theme_minimal()
top_category_hist
top_subcategory_hist <- test_price %>% filter(SubCategoryNr %in% top_subcategory_all) %>% 
  ggplot(aes(SellPrice))+
  geom_histogram(binwidth = function(x) ((max(x)-min(x))/nclass.Sturges(x)))+
  scale_x_log10()+
  scale_y_log10()+
  labs(title =str_c('Subcategory: ', top_subcategory_all))+
  theme_minimal()
top_subcategory_hist

test_price_clustering <- test_price
# Определим количество кластеров с помощью avarage silhouette width
library(cluster)
set.seed(735)
sil.width <- map_dbl(2:10,  
                     function(k){model <- clara(x = test_price_clustering[5], 
                                                k = k, 
                                                samples = 50, 
                                                rngR = TRUE)
                     return(model$silinfo$avg.width)
                     }
)


plot(2:10, sil.width, type = "b",
     xlab = "Number of Clusters", 
     ylab = "Sil width")

# -> Выбираем с наимбольшим "Sil width". Таким образом получаем 6 кластеров
kn = which.max(sil.width)+1 # отсчет от 2х, поэтому +1
k6 <- clara(x = test_price_clustering[5], k = kn, samples = 50, rngR = TRUE)
test_price_clustering$claster <- as.factor(k6$clustering)

test_price_clustering %>% ggplot(aes(x = SellPrice, colour = claster, fill = claster))+ 
  geom_histogram(binwidth = function(x) (max(x)-min(x))/nclass.Sturges(x))+ 
  scale_x_continuous(limits = c(0,500))+
  labs(x = "SellPrice < 500")+
  theme_minimal()
test_price_clustering %>% ggplot(aes(x = SellPrice, colour = claster, fill = claster))+
  geom_histogram(binwidth = function(x) (max(x)-min(x))/nclass.Sturges(x))+
  scale_x_log10()+
  theme_minimal()

library(cluster)
test_price_clustering_subcategory_top <- test_price_clustering %>% 
  filter(SubCategoryNr == top_subcategory_all)

set.seed(735)
sil.width <- map_dbl(2:10,  
                     function(k){model <- clara(x = test_price_clustering_subcategory_top[5], 
                                                k = k, 
                                                samples = 50, 
                                                rngR = TRUE)
                     return(model$silinfo$avg.width)
                     }
)

plot(2:10, sil.width, type = "b",
     xlab = "Number of Clusters", 
     ylab = "Sil width")

# -> Выбираем с наимбольшим "Sil width". Таким образом получаем 2 кластеров
kn = which.max(sil.width)+1 # отсчет от 2х, поэтому +1
k2 <- clara(x = test_price_clustering_subcategory_top[5], k = kn, samples = 50, rngR = TRUE)
test_price_clustering_subcategory_top$claster <- as.factor(k2$clustering)

test_price_clustering_subcategory_top %>% 
  filter(Brand %in% top_brand_35) %>% 
  ggplot(aes(x = as.factor(Brand), color = claster, fill = claster))+
  geom_bar(stat = "count")+
  labs(x = "Brand", title = str_c("Subcategory: ", top_subcategory_all))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

test_price_clustering_top <- test_price_clustering %>% 
  group_by(claster) %>% 
  count(Brand, sort = TRUE) %>% 
  slice_head(n = 20) %>% 
  left_join(test_price_clustering) 
head(test_price_clustering_top)


stat_test_price_clustering_top <- test_price_clustering_top %>% 
  group_by(Brand, claster, Category, SubCategoryNr) %>% 
  summarise(Mean = mean(SellPrice))
stat_test_price_clustering_top %>% ggplot(aes(y = Mean, x = as.factor(Brand)))+
  geom_jitter(aes(color = as.factor(claster)), alpha=0.2)+
  scale_y_log10()+
  labs(x = "Brand", color = "Claster")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

test_price_clustering_top_chr <- test_price_clustering_top %>% 
  mutate_at(c("Category", "SubCategoryNr", "Brand"), as.character) %>% 
  mutate(SellPrice = log10(SellPrice)/max(log10(SellPrice))) %>% 
  group_by(Brand) %>% 
  summarise(Mean = mean(SellPrice)) %>% 
  remove_rownames() %>% 
  column_to_rownames("Brand") %>% 
  arrange(desc(Mean))
res.diana <- diana(test_price_clustering_top_chr)
plot(as.dendrogram(res.diana))
test_price_clustering_top_chr

set.seed(934)
test_price_clustering_top %>% mutate(brand35 = case_when(Brand == 35 ~ "35",
                                                         TRUE ~ "Other")) %>%
  ungroup() %>%
  filter(Category == sample(Category, 1), claster == "5") %>%
  ggplot(aes(x = SellPrice, fill = brand35))+
  geom_density(alpha = 0.4)+
  labs(title = str_c("Category: ", test_price_clustering_top$Category[1]))+
  theme_minimal()