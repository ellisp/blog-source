# Count Bluesky followers who liked, posted, replied or reposted in the last N months.
# Needs: httr2, dplyr, purrr, tibble, readr, lubridate
# No login required; everything read here is public.

library(httr2)
library(tidyverse)
library(lubridate)

# ---- settings ---------------------------------------------------------------
actor <- "freerangestats.info" # your handle or DID
months_back <- 6
cache_file <- "bsky_follower_activity_raw.csv" # partial results; rerun resumes from here
out_file <- "bsky_follower_activity.csv"
req_per_sec <- 8 # per host; bsky.social allows roughly 10/s

cutoff <- now(tzone = "UTC") %m-% months(months_back)

collections <- c(
    like = "app.bsky.feed.like",
    post = "app.bsky.feed.post", # replies are posts with a `reply` field
    repost = "app.bsky.feed.repost"
)

# ---- 1. followers -----------------------------------------------------------
get_followers <- function(actor) {
    out <- list()
    cursor <- NULL
    repeat {
        resp <- request(
            "https://public.api.bsky.app/xrpc/app.bsky.graph.getFollowers"
        ) |>
            req_url_query(actor = actor, limit = 100, cursor = cursor) |>
            req_retry(max_tries = 4) |>
            req_perform() |>
            resp_body_json()
        out <- c(out, resp$followers)
        cursor <- resp$cursor
        message("Followers fetched: ", length(out))
        if (is.null(cursor) || length(resp$followers) == 0) break
    }
    tibble(
        did = map_chr(out, "did"),
        handle = map_chr(out, "handle")
    )
}

#-------------------followers' own followers and follows, etc--------
get_profile_counts <- function(dids) {
    batches <- split(dids, ceiling(seq_along(dids) / 25))
    map(batches, \(b) {
        resp <- request(
            "https://public.api.bsky.app/xrpc/app.bsky.actor.getProfiles"
        ) |>
            req_url_query(actors = b, .multi = "explode") |>
            req_throttle(rate = req_per_sec) |>
            req_retry(max_tries = 4) |>
            req_perform() |>
            resp_body_json()
        tibble(
            did = map_chr(resp$profiles, "did"),
            followers_count = map_int(resp$profiles, \(p) {
                p$followersCount %||% NA_integer_
            }),
            follows_count = map_int(resp$profiles, \(p) {
                p$followsCount %||% NA_integer_
            }),
            posts_count = map_int(resp$profiles, \(p) {
                p$postsCount %||% NA_integer_
            })
        )
    }) |>
        list_rbind()
}

# ---- 2. find the server (PDS) that holds each account's records ------------
resolve_pds <- function(did) {
    url <- if (startsWith(did, "did:web:")) {
        paste0("https://", sub("did:web:", "", did), "/.well-known/did.json")
    } else {
        paste0("https://plc.directory/", did)
    }
    doc <- request(url) |>
        req_throttle(rate = req_per_sec) |>
        req_retry(max_tries = 3) |>
        req_perform() |>
        resp_body_json(check_type = FALSE) # plc.directory serves application/did+ld+json
    svc <- keep(doc$service, \(s) {
        identical(s$type, "AtprotoPersonalDataServer")
    })
    if (length(svc) == 0) NA_character_ else svc[[1]]$serviceEndpoint
}

# ---- 3. newest record timestamp in one collection --------------------------
# listRecords returns newest first, so limit = 1 is enough.
latest_record <- function(pds, did, collection) {
    resp <- request(paste0(pds, "/xrpc/com.atproto.repo.listRecords")) |>
        req_url_query(repo = did, collection = collection, limit = 1) |>
        req_throttle(rate = req_per_sec) |>
        req_retry(
            max_tries = 4,
            is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504)
        ) |>
        req_error(is_error = \(r) FALSE) |>
        req_perform()
    if (resp_status(resp) != 200) {
        return(NA_character_)
    }
    recs <- resp_body_json(resp)$records
    if (length(recs) == 0) {
        return(NA_character_)
    }
    recs[[1]]$value$createdAt %||% NA_character_
}

safe_pds <- possibly(resolve_pds, otherwise = NA_character_)
safe_latest <- possibly(latest_record, otherwise = NA_character_)

check_follower <- function(did) {
    pds <- safe_pds(did)
    if (is.na(pds)) {
        return(tibble(
            did = did,
            pds = NA_character_,
            last_like = NA_character_,
            last_post = NA_character_,
            last_repost = NA_character_
        ))
    }
    ts <- map_chr(collections, \(col) safe_latest(pds, did, col))
    tibble(
        did = did,
        pds = pds,
        last_like = ts[["like"]],
        last_post = ts[["post"]],
        last_repost = ts[["repost"]]
    )
}

# ---- 4. run, in chunks, saving as it goes ----------------------------------
all_followers <- get_followers(actor)
# followers <- sample_n(all_followers, 100)
followers <- all_followers

done <- if (file.exists(cache_file)) {
    read_csv(cache_file, col_types = cols(.default = "c"))
} else {
    tibble(did = character())
}

todo <- followers |> filter(!did %in% done$did)
chunks <- split(todo$did, ceiling(seq_along(todo$did) / 100))
message(nrow(todo), " followers left to check")

for (i in seq_along(chunks)) {
    res <- map(chunks[[i]], check_follower) |> list_rbind()
    write_csv(res, cache_file, append = file.exists(cache_file))
    message("Chunk ", i, "/", length(chunks), " done")
}

# ---- 5. summarise -----------------------------------------------------------
parse_ts <- \(x) ymd_hms(as.character(x), quiet = TRUE)

counts <- get_profile_counts(followers$did)

activity <- read_csv(cache_file, col_types = cols(.default = "c")) |>
    distinct(did, .keep_all = TRUE) |>
    left_join(followers, by = "did") |>
    left_join(counts, by = "did") |> # <- new line
    mutate(across(c(last_like, last_post, last_repost), parse_ts)) |>
    mutate(
        last_any = pmax(last_like, last_post, last_repost, na.rm = TRUE),
        active = !is.na(last_any) & last_any >= cutoff
    )

summary_tbl <- tibble(
    # coverage
    followers_checked = nrow(activity),
    pds_unreachable = sum(is.na(activity$pds)),
    profile_missing = sum(is.na(activity$followers_count)),

    # activity in window
    active_any = sum(activity$active),
    pct_active = round(100 * mean(activity$active), 1),
    liked_in_window = sum(activity$last_like >= cutoff, na.rm = TRUE),
    posted_or_replied = sum(activity$last_post >= cutoff, na.rm = TRUE),
    reposted_in_window = sum(activity$last_repost >= cutoff, na.rm = TRUE),

    # who the followers are
    median_their_followers = median(activity$followers_count, na.rm = TRUE),
    median_their_follows = median(activity$follows_count, na.rm = TRUE),
    max_their_followers = max(activity$followers_count, na.rm = TRUE),
    max_their_follows = max(activity$follows_count, na.rm = TRUE),
    median_their_posts = median(activity$posts_count, na.rm = TRUE),
    pct_zero_posts = round(
        100 * mean(activity$posts_count == 0, na.rm = TRUE),
        1
    ),
    pct_under_100_followers = round(
        100 * mean(activity$followers_count < 100, na.rm = TRUE),
        1
    ),
    pct_over_1000_followers = round(
        100 * mean(activity$followers_count >= 1000, na.rm = TRUE),
        1
    ),

    # crude spam/bot heuristic: follows many, followed by few
    likely_spam_or_bot = sum(
        activity$follows_count > 1000 &
            activity$followers_count < 50,
        na.rm = TRUE
    ),

    # sum of followers-of-followers; overlaps between audiences are double counted
    gross_secondary_reach = sum(activity$followers_count, na.rm = TRUE)
)

print(t(summary_tbl))

activity |> 
  ungroup() |> 
  arrange(desc(followers_count)) |> 
  select(handle, followers_count, follows_count)

# active vs inactive

compare_tbl <- activity |>
    group_by(active) |>
    summarise(
        n = n(),
        median_followers = median(followers_count, na.rm = TRUE),
        median_follows = median(follows_count, na.rm = TRUE),
        max_followers = max(followers_count, na.rm = TRUE),
        max_follows = max(follows_count, na.rm = TRUE),
        median_posts = median(posts_count, na.rm = TRUE),
        pct_zero_posts = round(100 * mean(posts_count == 0, na.rm = TRUE), 1),
        pct_likely_spam = round(
            100 *
                mean(follows_count > 1000 & followers_count < 50, na.rm = TRUE),
            1
        ),
        .groups = "drop"
    )

print(compare_tbl)

write_csv(activity, out_file)
message("Per-follower results written to ", out_file)


#-------------trouble shooting-------------

did <- followers$did[1]
resolve_pds(did)
