library( ggplot2 )
library( dplyr )
library( RColorBrewer )
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

# Fit model
fit <- fitBaselineRegularization( brData, defineBRParameters( lambda1=0.5, lambda3 = 0) )

# Patient data as a tibble

intervals <- tibble(
  Patient = paste( "Patient", brData$patients),
  Interval = 1:14,
  Length = brData$l ) %>%
  group_by( Patient ) %>%
  mutate( `Interval Mid` = cumsum( Length ) - Length/2,
          `Start Day` = cumsum( Length ) - Length,
          `End Day` = cumsum( Length )-0.001 ) %>%
  ungroup()

drugs_unfiltered <- tibble(
  Interval = rep( 1:14, 3 ),
  Drug = rep( 1:3, each = 14 ),
  `Taking Drug` = as.numeric( brData$X ) )

drugs <- drugs_unfiltered %>%
  filter( `Taking Drug` > 0 ) %>%
  left_join( intervals ) %>%
  select( Patient, Interval, `Interval Mid`, `Start Day`, `End Day`, Length, Drug ) %>%
  mutate( Plot = "LOD", Color = paste( "Drug", Drug ) )

conditions <- tibble(
  Interval = 1:14,
  Condition = brData$n ) %>%
  filter( Condition > 0 ) %>%
  left_join( intervals ) %>%
  select( Patient, Interval, `Condition Day` = `Start Day` ) %>%
  mutate( Plot = "LOD", Color = "Condition occurrence" ) #"Condition occurrence")

# Data for the stacked area chart - real version
#risk_data <- bind_rows(
#  intervals %>% mutate( Risk = fit$t[Interval], `Fill` = "Baseline" ),
#  intervals %>% inner_join( drugs ) %>%
#    mutate( Risk = fit$beta[as.integer(Drug)], `Fill` = paste( "Drug", Drug ) ) )

# Data for the stacked area chart - fake version for a pretty illustrating plot
fake_tau <- c( c(5:1,1.5,2,2.5)/5, c(1,1,1,3,5,2.5)/10 )
fake_beta <- c(1.7, 0.7, 0.3)
risk_data <- bind_rows(
  intervals %>% full_join( drugs_unfiltered ) %>%
    mutate( Risk = if_else( `Taking Drug` > 0, fake_beta[as.integer(Drug)], 0 ), `Fill` = paste( "Drug", Drug ) ),
  intervals %>% mutate( Risk = fake_tau, `Fill` = "Baseline" ) )

risk_data <- bind_rows(
  risk_data %>% rename( Day = `Start Day` ),
  risk_data %>% rename( Day = `End Day` )
) %>%
  select( Patient, Interval, Day, Fill, Risk ) %>%
  mutate( Plot = "Risk" )

data <- bind_rows( drugs, conditions, risk_data ) %>%
  mutate( Fill = factor( Fill, c("Condition occurrence", "Drug 1", "Drug 2", "Drug 3", "Baseline" ) ) )

p = ggplot( data = data ) +
  facet_grid( Plot~Patient, scale = "free" ) +
  #geom_tile( aes( x = `Interval Mid`, y = Drug, width = Length ) ) +
  geom_point( aes( color = Color, x = `Condition Day`, y = 0 ), size = 4L ) +
  geom_segment(
    aes( color = Color,
         x = `Start Day`,
         xend = `End Day`,
         y = Drug,
         yend = Drug
         ),
    size = 5
    ) +
  ylim( -0.25, 3.25 ) +
  scale_color_manual( values = brewer.pal( n=10, name="Paired")[c(10,2,4,6)], na.translate = FALSE ) +
                      #breaks = c( "Condition occurrence", "1", "Drug 1", "2", "Drug 2", "3", "Drug 3" ), palette = "Paired" ) +
  geom_area( aes( fill = Fill, x = Day, y = Risk ) ) +
  theme( axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         legend.title = element_blank(),
         legend.position = "top",
         #legend.direction = "vertical",
         legend.text = element_text( margin = margin( r=5, l=2 ) ) # For spacing between legend entries+text
         ) +
  labs( x = "Day" ) +
  scale_fill_manual( values = brewer.pal( n=10, name="Paired")[c(1,3,5,7)],
                     labels = c( expression(x[ij1]*beta[1]),
                                 expression(x[ij2]*beta[2]),
                                 expression(x[ij3]*beta[3]),
                                 expression(tau[ij]) ) )

ggsave(
  filename = "figures/patients_plot.pdf",
  plot = p,
  width = 12, height = 4, units = "in" )