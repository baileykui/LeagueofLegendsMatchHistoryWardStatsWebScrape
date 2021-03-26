library(tidyverse)
library(rvest)
library(dplyr)

# creates a session and gets links and names of each region, season, and year for league of legends pro matches
s <- html_session("https://lol.gamepedia.com/League_Championship_Series")
region_year_season_link <- s %>% html_nodes(".wikitable:nth-child(7) td > .to_hasTooltip") %>% html_attr("href")
region_year_season_text <- s %>% html_nodes(".wikitable:nth-child(7) td > .to_hasTooltip") %>% html_text()

# gets overview match history link for all LCS 2021 Spring season
match_history_link <- s %>%
  jump_to(paste0("/LCS/2021_Season/Spring_Season", "/Match_History")) %>% 
  html_nodes("td:nth-child(13) .text") %>% 
  html_attr("href") 

# changes match history overview to match history stats link
if (!all(str_detect(match_history_link, "&tab=overview"))) {
  tfvec <- str_detect(match_history_link, "&tab=overview")
  match_history_link[which(!tfvec)] <- paste0(match_history_link[which(!tfvec)], "&tab=stats")
  match_history_link <- str_replace(match_history_link, "&tab=overview", "&tab=stats")
  match_history_link <- as.data.frame(match_history_link)
}

# gets unique player names
player_names <- s %>% 
  jump_to(paste0(region_year_season_link[4], "/Match_History")) %>% 
  html_nodes(".pWAN") %>% 
  html_text()
player_mat <- matrix(0, byrow = TRUE, ncol = 10)
player_df <- as.data.frame(player_mat)
player_df[1, ] <- player_names[(1:10)]
for (i in seq_along(player_names)) {
  player_df[1+i, ] <- player_names[(i*10 + 1):(i*10 + 10)]
}
player_df <- player_df[complete.cases(player_df),]
colnames(player_df) <- c("BTop", "BJungle", "BMid", "BBot", "BSupport", "RTop", "RJungle", "RMid", "RBot", "RSupport")

# binds match history stats link with players' warding stats
player_match <- cbind(match_history_link, player_df)

# reads in raw warding statistics 
# manually copy and pasted from each match history stats link 
# into a google sheets and converted into a csv file
raw_ward <- read.csv("LCS2021SpringWardStatsRaw.csv", header = FALSE)
colnames(raw_ward) <- c("Ward", c("BTop", "BJungle", "BMid", "BBot", "BSupport", "RTop", "RJungle", "RMid", "RBot", "RSupport"))

# adds an empty column in the 1st column
player_df <- cbind("", player_df)

# assigns player names to their warding statistics
game1 <- raw_ward[1:3,]
colnames(game1) <- player_df[1,]
master_list <- list(game1)
real_rows <- nrow(raw_ward)
for (i in 1:89) {
  game <- raw_ward[((i * 3) + (1:3)),]
  colnames(game) <- player_df[1 + i,]
  master_list[[1 + i]] <- game
}

# finds total control wards purchased by side and role
control_wards_purchased <- raw_ward %>% 
  filter(Ward == "ControlWardsPurchased") %>% 
  summarise(
    BTop = sum(BTop),
    RTop = sum(RTop),
    BJungle = sum(BJungle),
    RJungle = sum(RJungle),
    BMid = sum(BMid),
    RMid = sum(RMid),
    BBot = sum(BBot),
    RBot = sum(RBot),
    BSupport = sum(BSupport),
    RSupport = sum(RSupport),
  )

# finds total wards destroyed by side and role
wards_destroyed <- raw_ward %>% 
  filter(Ward == "WardsDestroyed") %>% 
  summarise(
    BTop = sum(BTop),
    RTop = sum(RTop),
    BJungle = sum(BJungle),
    RJungle = sum(RJungle),
    BMid = sum(BMid),
    RMid = sum(RMid),
    BBot = sum(BBot),
    RBot = sum(RBot),
    BSupport = sum(BSupport),
    RSupport = sum(RSupport),
  )

# finds total wards placed by side and role
wards_placed <- raw_ward %>%
  filter(Ward == "WardsPlaced") %>% 
  summarise(
    BTop = sum(BTop),
    RTop = sum(RTop),
    BJungle = sum(BJungle),
    RJungle = sum(RJungle),
    BMid = sum(BMid),
    RMid = sum(RMid),
    BBot = sum(BBot),
    RBot = sum(RBot),
    BSupport = sum(BSupport),
    RSupport = sum(RSupport),
  )

# combines into one data frame
total_wards_stats <- rbind(control_wards_purchased,
                           wards_destroyed, wards_placed)
rownames(total_wards_stats) <- c("ControlWardsPurchased",
                                 "WardsDestroyed", "WardsPlaced")

# barplot seeing total wards place by side and role of a player
par(mar = c(5, 5, 2, 3))
barplot(as.matrix(total_wards_stats),
        col = c("red2", "grey", "lightgreen"),
        main = "Ward Stat Totals by Side and Role",
        horiz = TRUE,
        las = 1)
legend("bottomright",
       c("ControlWardsPurchased", "WardsDestroyed", "WardsPlaced"),
       fill = c("red2", "grey", "lightgreen"))

# warding stats by side
blue_raw_ward <- raw_ward[,1:6]
red_raw_ward <- cbind(raw_ward[,1], raw_ward[,7:11])
colnames(red_raw_ward) <- c("Ward", "RTop", "RJungle", "RMid", "RBot", "RSupport")

# converts master list into a data frame 
master_df <- bind_rows(master_list)
colnames(master_df)[1] <- c("Ward")

# each players warding stats for each game
# filled with NA values
cwp <- master_df[which(master_df[1] == "ControlWardsPurchased"),]
wd <- master_df[which(master_df[1] == "WardsDestroyed"),]
wp <- master_df[which(master_df[1] == "WardsPlaced"),]

# cleans NA values
total_cwp <- cwp %>% 
  group_by(Ward) %>% 
  summarise_each(
    funs(sum(., na.rm = TRUE))
  )
t_total_cwp <- as.data.frame(as.numeric(unlist(transpose(total_cwp))[-1]))
rownames(t_total_cwp) <- colnames(total_cwp)[-1]
colnames(t_total_cwp) <- total_cwp[1,1]
total_cwp <- t_total_cwp

total_wd <- wd %>% 
  group_by(Ward) %>% 
  summarise_each(
    funs(sum(., na.rm = TRUE))
  )
t_total_wd <- as.data.frame(as.numeric(unlist(transpose(total_wd))[-1]))
rownames(t_total_wd) <- colnames(total_wd)[-1]
colnames(t_total_wd) <- total_wd[1,1]
total_wd <- t_total_wd

total_wp <- wp %>% 
  group_by(Ward) %>% 
  summarise_each(
    funs(sum(., na.rm = TRUE))
  )
t_total_wp <- as.data.frame(as.numeric(unlist(transpose(total_wp))[-1]))
rownames(t_total_wp) <- colnames(total_wp)[-1]
colnames(t_total_wp) <- total_wp[1,1]
total_wp <- t_total_wp

# clean warding stat totals by player
total_wards_stats_player <- cbind(total_cwp, total_wd, total_wp)

# max control wards purchased by player
total_wards_stats_player[which.max(total_wards_stats_player[,1]),]

# max wards destroyed by player
total_wards_stats_player[which.max(total_wards_stats_player[,2]),]

# max wards placed by player
total_wards_stats_player[which.max(total_wards_stats_player[,3]),]

# organizing total ward stats by team
imt_player_ward_stats <- total_wards_stats_player[1:5,]
c9_player_ward_stats <- total_wards_stats_player[6:10,]
eg_player_ward_stats <- total_wards_stats_player[11:15,]
clg_player_ward_stats <- total_wards_stats_player[c(16:20, 53, 54),]
onet_player_ward_stats <- total_wards_stats_player[c(21:25, 52),]
tl_player_ward_stats <- total_wards_stats_player[26:30,]
gg_player_ward_stats <- total_wards_stats_player[31:35,]
tsm_player_ward_stats <- total_wards_stats_player[36:40,]
fly_player_ward_stats <- total_wards_stats_player[c(41:45, 51),]
dig_player_ward_stats <- total_wards_stats_player[46:50,]

# sums player warding stats
imt_ward_stats <- apply(imt_player_ward_stats, 2, sum)
c9_ward_stats <- apply(c9_player_ward_stats, 2, sum)
eg_ward_stats <- apply(eg_player_ward_stats, 2, sum)
clg_ward_stats <- apply(clg_player_ward_stats, 2, sum)
onet_ward_stats <- apply(onet_player_ward_stats, 2, sum)
tl_ward_stats <- apply(tl_player_ward_stats, 2, sum)
gg_ward_stats <- apply(gg_player_ward_stats, 2, sum)
tsm_ward_stats <- apply(tsm_player_ward_stats, 2, sum)
fly_ward_stats <- apply(fly_player_ward_stats, 2, sum)
dig_ward_stats <- apply(dig_player_ward_stats, 2, sum)

# data frame of all LCS teams warding stats
lcs_ward_stats <- as.data.frame(rbind(imt_ward_stats, c9_ward_stats, eg_ward_stats, clg_ward_stats, onet_ward_stats, tl_ward_stats, gg_ward_stats, tsm_ward_stats, fly_ward_stats, dig_ward_stats))

# gold spent on control wards 
# note players can undo the control ward purchase and the counter will stay the same
lcs_ward_stats_g <- lcs_ward_stats %>% 
  mutate(
    GoldSpentCW = ControlWardsPurchased * 75
  )

# max control wards purchased by team
lcs_ward_stats[which.max(lcs_ward_stats[,1]),]

# max wards destroyed by team
lcs_ward_stats[which.max(lcs_ward_stats[,2]),]

# max wards placed by team
lcs_ward_stats[which.max(lcs_ward_stats[,3]),]

write.csv(total_wards_stats_player, "LCS2021SpringPlayerWardStats.csv")
write.csv(lcs_ward_stats, "LCS2021SpringTeamWardStats.csv")