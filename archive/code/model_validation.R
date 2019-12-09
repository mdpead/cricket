library(tidyverse)


model_validation <- function(sample, pop, pr, pf){

sample$pop <- sample[[pop]]
sample$pr <- sample[[pr]]
sample$pf <- sample[[pf]]
  

##### ROC Curve #####

roc <- sample %>%
  filter(pop %in% c("build", "validation")) %>%
  group_by(pop) %>%
  arrange(pr) %>%
  mutate(tm_perc = cumsum(pf == 1) / sum(pf == 1), 
         fp_perc = cumsum(pf == 0) / sum(pf == 0)) %>%
  select(pop, tm_perc, fp_perc) %>%
  ungroup()

roc_plot <- ggplot(roc) +
  geom_line(aes(tm_perc, fp_perc, color = pop)) +
  geom_abline(slope = 1, intercept = 0)



##### Gini #####

gini_table <- roc %>%
  group_by(pop, fp_perc) %>%
  summarise(gap = max(tm_perc) - min(tm_perc)) %>%
  mutate(area_part = fp_perc * gap) %>%
  ungroup() %>%
  group_by(pop) %>%
  summarise(area = (2 * sum(area_part) - 1) * 100)



##### Accuracy #####

accuracy <- sample %>%
  filter(pop == "validation") %>%
  arrange(pr) %>%
  mutate(dec = ceiling(row_number() * 20 / max(row_number()))) %>%
  gather(type, vol, pr, pf) %>%
  group_by(dec, type) %>%
  summarise(vol = sum(vol) / n())

accuracy_plot <- ggplot(accuracy) +
  geom_line(aes(dec, vol, colour = type))



##### Results list #####

results <- list(roc_plot = roc_plot,
                gini_table = gini_table,
                accuracy_plot = accuracy_plot)


return(results)

}
