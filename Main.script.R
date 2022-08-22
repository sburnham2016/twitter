library(rtweet)
library(data.table)


auth <- rtweet_bot(api_key = consumer.key, api_secret = consumer.secret, access_token = access.token, access_secret = access.token.secret)
#auth_setup_default()
#auth <- rtweet_app()

#token <- create_token(
# consumer_key = consumer.key,
# app = appname,
#  consumer_secret = consumer.secret,
#  access_token = access.token,
# access_secret = access.token.secret
#)

#token <- create_token(app = 'miranda.test',
#                      consumer_key = consumer.key,
#                      consumer_secret = consumer.secret,
#                      access_token = access.token,
#                      access_secret = access.token.secret)

#x <- search_tweets('#competition', n = 25, include_rts = FALSE)

#dt <- get_timeline(user = "samburnham12", n=1)

# destroy = False to favourite a tweet
#post_favorite(status_id = dt$id_str[[1]])

# retweet
#post_tweet(retweet_id = dt$id_str[[1]])

# follow
#post_follow(user = "samburnham12", destroy = FALSE)

# send reply
#post_tweet(status = "very good @samburnham12",
#           in_reply_to_status_id = dt$id_str[[1]])

# post to my timeline
#post_tweet(status = "ok then")


##########################################################################################################


MyAnswer <- function(id.str, user.selected){
  
  
  # tweets pointed at user since tweet of interest
  replies <- data.table(search_tweets(paste0("to:", user.selected),
                                      since_id = id.str,
                                      n = 10, 
                                      include_rts = FALSE))
  
  # replies related to the tweet ID of interest
  replies.to.this.tweet <- replies[in_reply_to_status_id_str == id.str, ]
  
  # summary of answers
  summary.table <- data.table(table(replies.to.this.tweet$full_text))
  
  # any used more than two 
  output <- if(nrow(summary.table[N>2,]) > 0){summary.table[N>2, V1][[1]]} else(" ")
  
  # return output
  output
  
  
}


##########################################################################################################
engine <- function(id.str, favorited, retweeted, user.selected, already.follow){
  
  answer <- MyAnswer(id.str = id.str,
                     user.selected = user.selected)
  
  # follow
  if(!already.follow) {post_follow(user = user.selected)}
  
  #favourite
  post_favorite(status_id = id.str)
  
  # retweet
  post_tweet(retweet_id = id.str)
  
  # send reply
  post_tweet(status = paste0(answer, " @", user.selected),
             in_reply_to_status_id = id.str)
  
  output <- "RUN"
  
}






##########################################################################################################


CycleComps <- function(description, dt){
  
  
  line <- dt[full_text == description, ]
  
  line[, run.engine := engine(id.str = id_str, 
                              favorited = favorited, 
                              retweeted = retweeted,
                              user.selected = line$entities[[1]]$user_mentions$screen_name, 
                              already.follow = FALSE)]
  
  
}


##########################################################################################################
#  MAIN PROCESS

dt <- data.table(search_tweets('#Rstudio', n = 1, include_rts = FALSE))

#dt <- dt[(favorited != "TRUE") & (retweeted != "TRUE"),]

cycle.list <- unique(dt$full_text)

lapply(cycle.list, CycleComps, dt = dt)


##########################################################################################################






