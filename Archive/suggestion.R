frequency <- bind_rows(mutate(tc_about, product = "MyTalkTools"),
                       mutate(qt_about, product = "QuickTalk"), 
                       mutate(mtt_about, product = "TouchChat"),
                       mutate(plq2g_about, product = "Proloquo2go")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(product, word) %>%
  group_by(product) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = product, values_from = proportion) %>%
  pivot_longer(`TouchChat`:`QuickTalk`:`MyTalkTools`:`Proloquo2go`,
               names_to = "product", values_to = "proportion")

# keep only top 20 words
top20_words <- frequency %>% 
  group_by(product) %>% 
  top_n(20, proportion) %>% 
  pull(word)

# create the plot with all words only top 20 shown as text on plot
ggplot(frequency, aes(x = proportion, y = `MyTalkTools`, 
                      color = abs(`MyTalkTools` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(data = filter(frequency, word %in% top20_words),
            aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~product, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "MyTalkTools", x = NULL)

# save
write_csv(frequency, "frequency.csv")
ggsave("plot.png", width = 10, height = 8, dpi = 300)