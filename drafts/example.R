# show 3 ways of calculating covariance for IRT
library(polycor)
apply(item_scores, 1, . %>% {!is.na(.)} %>% sum()) %>% table() %>% sum()
ttl <- apply(item_scores, 1, sum, na.rm = TRUE)
n_obs <- apply(item_scores, 1, . %>% {sum(!is.na(.))})
# correlate item against average of others, note -1 could be removed (it's a constant)


pear <- apply(item_scores, 2, . %>% cor((ttl - .) / (n_obs - 1), "pairwise.complete.obs"))

poly <- apply(item_scores, 2, . %>% {
  keep <- !is.na(.)
  ttl_others <- (ttl - .) / (n_obs - 1)
  polyserial(ttl_others[keep], as.logical(.[keep]))
  })

fit.glm <- apply(item_scores, 2, . %>% {
  keep <- !is.na(.)
  ttl_others <- (ttl - .) / (n_obs - 1)
  coef(glm(.[keep] ~ scale(ttl_others[keep]), family = binomial))
}) %>% t()

library(mirt)
itemtype <-
  data$challenges %>%
  filter(is.na(deleted_at)) %>%
  mutate(model = ifelse(type != "BlanksChallenge", '3PL', '2PL')) %>%
  arrange(match(id, colnames(item_scores))) %>%
  select(model) %>% unlist()
