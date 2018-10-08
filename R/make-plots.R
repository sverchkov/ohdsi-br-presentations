library( ggplot2 )
library( dplyr )
library( BaselineRegularization )

# Tiny toy dataset:
# * 2 patients, with 8, 6 intervals respectively
# * 3 drugs, a, b, c. x marks adverse event, - marks empty interval, (#) marks length
# Patient 1: c(5) -(2) x(1) a(5) ax(10) a(10) abx(5) b(10)
# Patient 2: a(4) abx(7) b(3) bc(12) abcx(20) -(12)

brData <- list(
  X = Matrix::t( Matrix::Matrix(
    c( 0,0,1, 0,0,0, 0,0,0, 1,0,0, 1,0,0, 1,0,0, 1,1,0, 0,1,0,
       1,0,0, 1,1,1, 0,1,0, 0,1,1, 0,1,0, 0,0,0 ),
    nrow=3, ncol=14 ) ),
  interval_baseline_parameter = 1:14,
  baseline_parameter_obs_period = rep( c(1:2), c(8,6) ),
  l = c( 5, 2, 1, 5, 10, 10, 5, 10,
         4, 7, 3, 12, 20, 12 ),
  n = c( 0, 0, 1, 0, 1, 0, 1, 0,
         0, 1, 0, 0, 1, 0 ),
  patients = c( rep( 1, 8 ), rep( 2, 6 ) )
)

# Patient data as a tibble
intervals <- tibble(
  Patient = brData$patients,
  Interval = 1:14,
  Length = brData$l ) %>%
  group_by( Patient ) %>%
  mutate( Day = cumsum( Length ) - Length/2,
          `Start Day` = cumsum( Length ) - Length,
          `End Day` = cumsum( Length )-0.001 ) %>% ungroup()
drugs <- tibble(
  Interval = rep( 1:14, 3 ),
  Drug = as.factor( rep( 1:3, each = 14 ) ),
  `Taking Drug` = as.numeric( brData$X ) ) %>%
  filter( `Taking Drug` > 0 ) %>%
  select( -`Taking Drug` ) %>%
  mutate( Plot = "Drugs" )
conditions <- tibble(
  Interval = 1:14,
  Condition = brData$n ) %>%
  filter( Condition > 0 ) %>%
  mutate( Condition = as.factor( Condition ) ) %>%
  mutate( Plot = "Condition occurrence")

data <- left_join( intervals, bind_rows( drugs, conditions, intervals %>% select(Interval) %>% mutate( Plot = "Intervals" ) ) )

# Figure for patient data
ggplot( data = data, aes( x=Day ) ) +
  facet_grid( Patient+Plot~., labeller = label_both ) +
  geom_tile(
    aes( y=Drug, width = Length, fill = Drug ),
    data = data %>% filter( Plot == "Drugs" ) ) +
  geom_vline(
    aes( xintercept = `Start Day` ),
    data = data %>% filter( Plot == "Condition occurrence" ) ) +
  geom_segment(
    aes( x = `Start Day`, xend = `End Day`, y = 0, yend = 0 ),
    data = data %>% filter( Plot == "Intervals" ),
    arrow = arrow( ends = "both", angle = 90 ) )

# Fit model
fit <- fitBaselineRegularization( brData, defineBRParameters( lambda1=0.5, lambda3 = 0) )

# Make a stacked area chart
risk_data <- bind_rows(
  intervals %>% mutate( Risk = fit$t[Interval], `Risk Source` = "Baseline" ),
  intervals %>% inner_join( drugs ) %>%
    mutate( Risk = fit$beta[as.integer(Drug)], `Risk Source` = paste( "Drug", Drug ) ) %>%
    select( -Drug ) )

risk_data <- bind_rows(
  risk_data %>% rename( Day = `Start Day` ),
  risk_data %>% rename( Day = `End Day` )
)

ggplot( risk_data, aes( x = Day, y = Risk, fill = `Risk Source` ) ) +
  facet_grid( rows = vars( Patient ), labeller = label_both ) +
  geom_area()
