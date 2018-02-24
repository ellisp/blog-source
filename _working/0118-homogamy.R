library(tidyverse)
library(twitteR)
library(GGally)
library(mgcv)

# question is "do people with more followers follow people with more followers"
# ie is there a positive correlation between the number of followers someone has,
# and the "average" number of followers the people they follow have.
# we need a good sense of "average" of couirse - mean is highly unlikely to work well


many_following_user <- getUser("toddcarey")
many_ids <- many_following_user$getFriendIDs()
length(many_ids)      # returns only 75000 ids; I think the most recent 75,000 he's followed

#=======================snowball sampling method================

follow_data <- data_frame(
  number_followers = integer(),
  number_following = integer(),
  mean_ff = numeric(),
  trmean_ff = numeric(),
  median_ff = numeric(),
  screenName = character()
)

# Terminology
# A "friend" is someone you follow
# A "follower" is someone who follows you

current_sn <- "ellis2013nz"

set.seed(124)

for(i in 1:1000){
  # save latest copy of results in case of crash
  save(follow_data, file = "follow_data.rda")
  
  # number of calls left to make under Twitter's limits
  lims <- getCurRateLimitInfo()
  x <- as.numeric(filter(lims, resource == "/followers/ids")$remaining) *
    as.numeric(filter(lims, resource == "/friends/ids")$remaining)
  while(x == 0)  {
    lims <- getCurRateLimitInfo()
    x <- as.numeric(filter(lims, resource == "/followers/ids")$remaining) *
      as.numeric(filter(lims, resource == "/friends/ids")$remaining)
    message("Waiting")
    Sys.sleep(60) # wait 15 minutes (or 1 minute, 15 times)
  }
  
  
  cat(paste(i, current_sn, " | "))
  
  current_user <- getUser(current_sn)
  follow_data[i, "screenName"] <- current_sn
  
  # who this user follows (what happens if this is eg 1million people? I think it gets truncated at 15,000,
  # which is not as bad as it sounds as we only need a sample; but the problem will be that this is their
  # 15,000 most recent people they have followed, who might be biased to be newcomers?):
  
  # certain privacy settings stop you seeing someone's friends and followers, in which case
  # we try to sample another node from the last good person
  all_my_friends <- character()
  try(all_my_friends <- current_user$getFriendIDs())          # about 1 second for c.1500 IDs, longer for more
  if(length(all_my_friends) > 0){
  
    # sample some of those friends
    n <- min(2000, length(all_my_friends))
    sample_my_friends <- sample(all_my_friends, n, replace = FALSE)
    friend_details  <- lookupUsers(sample_my_friends)            # about 15 seconds
  
    friend_details_df <- twListToDF(friend_details)
    
    follow_data[i, "number_followers"] <- current_user$followersCount
    follow_data[i, "number_following"] <- current_user$friendsCount
    
    # mean number of followers of the people this user follows
    follow_data[i, "mean_ff"] <- mean(friend_details_df$followersCount)
    follow_data[i, "trmean_ff"] <- mean(friend_details_df$followersCount, tr = 0.2)
    follow_data[i, "median_ff"] <- median(friend_details_df$followersCount)
    
  }
  
  current_sn <- "non_existent_user"
  while(current_sn == "non_existent_user"){
      
    if(sample(1:2, 1) == 1){
      # 50% chance
      # pick one of the user's friends (ie people they follow) at random as the new node to sample
      # note that as a sampling strategy this will bias us towards people who are followed.  People
      # who have few followers have little chance of being selected here.  Does this matter?
      # Probably not for our main research question, but it does stop us doing meaningful inference
      # on population totals of number of people followed and number of people following - unless
      # we could estimate *how much* more likely people are to be followed, then we could estimate
      # weights to make up for it.
      cat("\nsampling a friend\n")
      try(current_sn <- sample_n(friend_details_df, 1)$screenName)
    } else {
      # 50% chance
    # pick one of the user's followers as the new node.  This is to mitigate the problem
    # noted above.  We will still end up over-representing people with lots of followers
    # (who will be sampled by the previuos method), and people who follow lots of people
    # (more likely to be picked up in this alternative procedure in the next line), but
    # that is better than *only* over sampling people with lots of followers.  Probably.
      cat("\nsampling a follower\n")
      
      # problem here when current_user has no followers:
      # a random sample from the last 1000 people to follow this person (ie biased to more 
      # recent, for people with lots of followers, but c'est la vie - important to speed things
      # up a bit)
      try(follower_samp <- sample(current_user$getFollowerIDs(n = 1000), 1))
      try(current_sn <- lookupUsers(follower_samp)[[1]]$screenName)
    }
  }
}

follow_data %>%
  arrange(desc(number_followers)) 

#==================snapshot sampling method===========
# The first method of sampling will oversample people with lots of followers and people
# with lots of friends.  As this is potentially linked to the response variable (indirectly)
# it might be better to try an alternative sampling method, by grabbing a bunch of tweets.
# This method will oversample people who are active tweeting today, which is a different
# sort of coverage problem, so still need to be careful.
follow_data_2 <- data_frame(
  number_followers = integer(),
  number_following = integer(),
  mean_ff = numeric(),
  trmean_ff = numeric(),
  median_ff = numeric(),
  screenName = character()
)


# 1000 random tweets with the letter "e" in them:
tweets <- searchTwitter("e", n = 1000)

# the users
users <- unique(sapply(tweets, function(x){x$screenName}))

for(i in 207:length(users)){
  # save latest copy of results in case of crash
  save(follow_data_2, file = "follow_data_2.rda")
  
  # number of calls left to make under Twitter's limits
  lims <- getCurRateLimitInfo()
  x <- as.numeric(filter(lims, resource == "/followers/ids")$remaining) *
    as.numeric(filter(lims, resource == "/friends/ids")$remaining)
  while(x == 0)  {
    lims <- getCurRateLimitInfo()
    x <- as.numeric(filter(lims, resource == "/followers/ids")$remaining) *
      as.numeric(filter(lims, resource == "/friends/ids")$remaining)
    message("Waiting")
    Sys.sleep(60) # wait 15 minutes (or 1 minute, 15 times)
  }
  current_sn <- users[i]
  
  cat(paste(i, current_sn, " | "))
  
  try({
    current_user <- getUser(current_sn) # this fails surprisingly often, not sure why
    follow_data_2[i, "screenName"] <- current_sn
    
    # who this user follows (what happens if this is eg 1million people? I think it gets truncated at 15,000,
    # which is not as bad as it sounds as we only need a sample; but the problem will be that this is their
    # 15,000 most recent people they have followed, who might be biased to be newcomers?):
    
    # certain privacy settings stop you seeing someone's friends and followers, in which case
    # we try to sample another node from the last good person
    all_my_friends <- character()
    try(all_my_friends <- current_user$getFriendIDs())          # about 1 second for c.1500 IDs, longer for more
    if(length(all_my_friends) > 0){
      
      # sample some of those friends
      n <- min(2000, length(all_my_friends))
      sample_my_friends <- sample(all_my_friends, n, replace = FALSE)
      friend_details  <- lookupUsers(sample_my_friends)            # about 15 seconds
      
      friend_details_df <- twListToDF(friend_details)
      
      follow_data_2[i, "number_followers"] <- current_user$followersCount
      follow_data_2[i, "number_following"] <- current_user$friendsCount
      
      # mean number of followers of the people this user follows
      follow_data_2[i, "mean_ff"] <- mean(friend_details_df$followersCount)
      follow_data_2[i, "trmean_ff"] <- mean(friend_details_df$followersCount, tr = 0.2)
      follow_data_2[i, "median_ff"] <- median(friend_details_df$followersCount)
      
    }
  })
}





#==================analysis========================

follow_data_proc <- follow_data %>%
  mutate(sampling_method = "Snowball sampling\nalong network")

follow_data_proc_2 <- follow_data_2 %>%
  mutate(sampling_method = "Sample of today's tweeters") 

follow_data_comb <- rbind(follow_data_proc, follow_data_proc_2) %>%
  filter(!is.na(trmean_ff)) %>%
  group_by(screenName, sampling_method) %>%
  summarise(
    number_followers = mean(number_followers),
    number_following = mean(number_following),
    mean_ff = mean(mean_ff),
    trmean_ff = mean(trmean_ff),
    median_ff = mean(median_ff),
    n_obs = n()
  ) %>%
  ungroup() %>%
  mutate(many_following = cut(number_following, breaks = c(0, 1000, 5000, 15000, 75000, 10 ^ 9),
                              labels = c("0 - 1,000", "1,001 - 5,000", "5,001 - 15,000", "15,001 - 75,000", "75,001 or more")))



follow_data_trans <- follow_data_comb %>%
  mutate(number_followers_l = log10(number_followers + 1),
         number_following_l = log10(number_following + 1),
         trmean_ff_l = log10(trmean_ff + 1)) %>%
  select(number_followers_l, number_following_l, trmean_ff_l, sampling_method) 

png("../img/0118-pairs.png", 6000, 6000, res = 600)
  ggpairs(follow_data_trans, columns = 1:3, mapping = aes(colour = sampling_method))
dev.off()


svg("../img/0118-main-scatter.svg", 10, 5)
ggplot(follow_data_comb, aes(x = number_followers, y = trmean_ff)) + 
  facet_wrap(~sampling_method) +
  geom_point(aes(fill = many_following), pch = 21) +
  geom_smooth(method = "loess") +
  scale_x_log10("Number of followers, of this person", label = comma) +
  scale_y_log10("Trimmed mean number of followers,\nof the people this person follows", label = comma) +
  ggtitle("Do people with more followers follow people with more followers?",
          "Apparently 'it depends'.") +
  scale_fill_brewer("Following how\nmany people:", palette = "Spectral",
                    guide = guide_legend(title.hjust = 0.5, override.aes = list(size = 5))) +
  theme(legend.position = "right")
dev.off()  

  
  # better modeling would treat each point as correlated with its subsequent point
  
  # interesting cases like TheRoyalPosts, median number of followers of her friends is 7 million...
  
  # things to do - distribution of number of followers and number of friends(followed)
  # scatter plot of followers compared to friends
  # she (or the bot that she might be) makes a point of following people with many many followers (eg football teams)

mod_full <- gam(trmean_ff_l ~ s(number_followers_l, number_following_l), data = follow_data_trans)
mod_simp <- gam(trmean_ff_l ~ s(number_followers_l) + s(number_following_l), data = follow_data_trans)
anova(mod_full, mod_simp, test = "F")

svg("../img/0118-simple.svg", 8, 4)
par(family = "myfont", bty = "l", font.main = 1)
plot(mod_simp, pages = 1, main = "No-interaction model")  
dev.off()  


svg("../img/0118-persp.svg", 8, 6)
par(family = "myfont", bty = "l", font.main = 1)
plot(mod_full, pages = 1, scheme = 1, main = ("Impact on the trimmed mean number\nof followers of people that are followed"))
dev.off()

svg("../img/0118-heatmap.svg", 8, 7)
par(family = "myfont", bty = "l", font.main = 1)
plot(mod_full, pages = 1, scheme = 2, hcolors = topo.colors(50),
     main = ("Impact on the trimmed mean number\nof followers of people that are followed"),
     xlab = "Logarithm of number of followers",
     ylab = "Logarithm of number people following")
legend("topleft", legend = c("low", "medium", "high"), fill = topo.colors(3), bty = "n", cex = 1.2)
dev.off()

convert_pngs("0118")
  