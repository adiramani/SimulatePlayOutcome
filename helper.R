airYardsModel <- xgboost::xgb.load('models/ExpectedAirYards.model')
rushYardsModel <- xgboost::xgb.load('models/ExpectedRushYards.model')
yacModel <- xgboost::xgb.load('models/ExpectedYAC.model')
cpModel <- xgboost::xgb.load('models/ExpectedCompletionPercentage.model')


pbp_data <- readRDS("./data/pbpData.rds")

getExpectedAirYards <- function(playData) {
  xAirYardsData <- playData %>% select(yardline_100, era, qtr, down, ydstogo, 
                                                                 shotgun, redzone, half_seconds_remaining, pass_side, score_differential)
  predAirYards <- xgboost::xgb.DMatrix(as.matrix(xAirYardsData))
  phat <- matrix(predict(airYardsModel, predAirYards), ncol=84, byrow = TRUE)
  yhat <- sweep(phat, MARGIN=2, seq(-5, 78, 1), `*`) %*% matrix(1, 84, 1)
  return(yhat)
}

getExpectedRushYards <- function(playData) {
  xRushYardsData <- playData %>% select(yardline_100, era, qtr, down, ydstogo, 
                                                                      shotgun, redzone, half_seconds_remaining, run_gap, run_side, score_differential)

  predRushYards <- xgboost::xgb.DMatrix(as.matrix(xRushYardsData))
  phat <- matrix(predict(rushYardsModel, predRushYards), ncol=105, byrow = TRUE)
  yhat <- sweep(phat, MARGIN=2, seq(-5, 99, 1), `*`) %*% matrix(1, 105, 1)
  return(yhat)
}

getExpectedYAC <- function(playData) {
  xYACData <- playData %>% select(air_yards, yardline_100, era, qtr, down, ydstogo, 
                                        shotgun, redzone, pass_side, roof)
  
  predYAC <- xgboost::xgb.DMatrix(as.matrix(xYACData))
  phat <- matrix(predict(yacModel, predYAC), ncol=56, byrow = TRUE)
  yhat <- sweep(phat, MARGIN=2, seq(-5, 50, 1), `*`) %*% matrix(1, 56, 1)
  return(yhat)
}

getComplectionPct <- function(playData) {
  xCPData <- playData %>% select(roof, yardline_100, era, qtr, down, ydstogo, 
                                                             shotgun, redzone, distance_to_sticks, pass_side, air_yards)
  
  predCP <- xgboost::xgb.DMatrix(as.matrix(xCPData))
  yhat <- predict(cpModel, predCP)
  return(yhat)
}

generalMutateData <- function(dataToMutate) {
  return(dataToMutate %>% mutate(
      pass_side = case_when(
        pass_location == 'left' ~ 1,
        pass_location == 'right' ~ 1,
        pass_location == 'middle' ~ 2,
        TRUE ~ 0
      ),
      redzone = case_when(
        yardline_100 <= 20 ~ 1,
        TRUE ~ 0
      ),
      run_side = case_when(
        pass_location == 'left' ~ 1,
        pass_location == 'right' ~ 1,
        pass_location == 'middle' ~ 2,
        TRUE ~ 0
      ),
      run_gap = case_when(
        pass_location == 'guard' ~ 1,
        pass_location == 'tackle' ~ 2,
        pass_location == 'end' ~ 3,
        TRUE ~ 0
      ),
      era = case_when(
        season < 2001 ~ 0,
        season < 2013 ~ 1,
        season < 2017 ~ 2,
        TRUE ~ 3
      ),
      distance_to_sticks = ydstogo - air_yards,
      distance_to_goal = yardline_100 - air_yards,
      air_is_zero = ifelse(air_yards == 0, 1, 0),
      roof = case_when(
        roof == 'outdoors' ~ 0,
        roof == 'dome' ~ 1,
        roof == 'closed' ~ 2,
        TRUE ~ 3
      ),
      half_seconds_remaining = half_seconds_remaining / 1800 * 100,
      game_seconds_remaining = game_seconds_remaining / 1800 * 100
    ))
}

isPass <- function(passPct) {
  return(runif(1) < passPct)
}

isComplete <- function(compPct) {
  return(runif(1) < compPct)
}