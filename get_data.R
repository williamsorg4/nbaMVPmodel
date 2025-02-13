library(rvest)
library(tidyverse)

# Get MVP Voting Results --------------------------------------------------
mvp_data <- tibble()
for (year in 1981:2024) {
  link <- paste0("https://www.basketball-reference.com/awards/awards_", year, ".html")
  page <- read_html(link)
  
  temp <- page %>% 
    html_nodes("table") %>% 
    .[1] %>% 
    html_table() %>% 
    .[[1]]
  
  colnames(temp) <- temp[1,]
  temp <- temp[-1,]
  temp <- temp %>% 
    mutate(year = year)
  
  href <- page %>% 
    html_nodes("#mvp > tbody > tr > td:nth-child(2) > a") %>% 
    html_attr("href")
  
  temp <- temp %>% 
    add_column(href = href)
  
  mvp_data <- mvp_data %>% 
    rbind(temp)
  Sys.sleep(5)
}

# Change data types
mvp_data <- mvp_data %>% 
  mutate(Rank = gsub("T", "", Rank)) %>% 
  mutate_at(c("Rank", "Age", "First", "Pts Won", "Pts Max", "G"), as.integer) %>% 
  mutate_at(c("Share", "MP", "PTS", "TRB", "AST", "STL", "BLK", "FG%", "3P%",
              "FT%", "WS", "WS/48"), as.double)

# Create winning column
mvp_data <- mvp_data %>% 
  mutate(won = Rank == 1)

# Get per game stats ------------------------------------------------------
per_game <- tibble()
for (year in 1981:2024) {
  link <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_per_game.html")
  page <- read_html(link)
  
  temp <- page %>% 
    html_nodes("table") %>% 
    .[1] %>% 
    html_table() %>% 
    .[[1]]
  
  temp <- temp[-nrow(temp), ]
  temp <- temp %>% 
    mutate(year = year)
  
  href <- page %>% 
    html_nodes("#per_game_stats > tbody > tr > td:nth-child(2) > a") %>% 
    html_attr("href")
  
  temp <- temp %>% 
    add_column(href = href)
  
  per_game <- per_game %>% 
    rbind(temp)
  
  Sys.sleep(5)
}

# Get advanced stats -------------------------------------------------------
advanced <- tibble()
for (year in 1981:2024) {
  link <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_advanced.html")
  page <- read_html(link)
  
  temp <- page %>% 
    html_nodes("table") %>% 
    .[1] %>% 
    html_table() %>% 
    .[[1]]
  
  temp <- temp[-nrow(temp), ]
  temp <- temp %>% 
    mutate(year = year)
  
  href <- page %>% 
    html_nodes("#advanced > tbody > tr > td:nth-child(2) > a") %>% 
    html_attr("href")
  
  temp <- temp %>% 
    add_column(href = href)
  
  advanced <- advanced %>% 
    rbind(temp)
  
  Sys.sleep(5)
}

# Get adjusted shooting stats ---------------------------------------------
adjusted_shooting <- tibble()
for (year in 1981:2024) {
  link <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_adj_shooting.html")
  page <- read_html(link)
  
  temp <- page %>% 
    html_nodes("table") %>% 
    .[1] %>% 
    html_table() %>% 
    .[[1]]
  
  colnames(temp) <- temp[1,]
  temp <- temp[-1,]
  temp <- temp[-nrow(temp), ]
  temp <- temp %>% 
    mutate(year = year)
  
  href <- page %>% 
    html_nodes("#adj_shooting > tbody > tr > td:nth-child(2) > a") %>% 
    html_attr("href")
  
  temp <- temp %>% 
    add_column(href = href)
  
  adjusted_shooting <- adjusted_shooting %>% 
    rbind(temp)
  
  Sys.sleep(5)
}


# Join all stats ------------------------------------------------------------
all_stats <- per_game %>% 
  select(-c(Rk)) %>% 
  full_join(advanced %>% 
              select(-c(Rk, Awards, G, GS, MP))) %>% 
  full_join(adjusted_shooting %>% 
              select(-c(Rk, Age, G, `TS%`, `3PAr`, FTr, GS, MP, `FG%`, `3P%`, `2P%`, `eFG%`,
                        `FT%`, Awards)))

all_stats <- all_stats %>% 
  mutate_at(53:62, as.double)

mvp_data <- mvp_data %>% 
  select(Rank, Player, First, `Pts Won`, `Pts Max`, Share, href, year) %>% 
  full_join(all_stats)

mvp_data <- mvp_data %>% 
  mutate(Rank = ifelse(is.na(Rank), 0, Rank),
         First = ifelse(is.na(First), 0, First),
         `Pts Won` = ifelse(is.na(`Pts Won`), 0, `Pts Won`),
         `Pts Max`= ifelse(is.na(`Pts Max`), 0, `Pts Max`), 
         Share = ifelse(is.na(Share), 0, Share),
         won = Rank == 1)

mvp_data <- mvp_data %>% 
  mutate(`G%` = G/max(G), .by = year, .before = href)


test <- mvp_data %>% 
  select(Player, year, `WS/48`)

mvp_data %>% 
  ggplot(aes(x = year, y = `G%`)) +
  geom_point()


mvp_data %>% 
  ggplot(aes(x = `G%`, y = as.integer(won))) +
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'))

mvp_data %>% 
  filter(won) %>% 
  ggplot(aes(x = year, y = PTS)) +
  geom_point()

mvp_data %>% 
  filter(Rank == 1) %>% 
  arrange(desc(PTS))


m1 <- glm(won ~ WS + `G%` + PTS, data = mvp_data, family = 'binomial')
m2 <- lm(Share ~ WS + `G%` + PTS, data = mvp_data)
summary(m2)
predict(m2, 
        tibble(WS = 15,
               `G%` = 1,
               PTS = 29.6),
        type = 'response')
