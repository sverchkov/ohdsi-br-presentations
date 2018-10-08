library(BaselineRegularization)

# Connect to your database
con <- DBI::dbConnect( ... )

# Set a condition of interest, for example
response_event = 137829 # Aplastic anemia

# Prepare the data
br_data <- prepareBRData(
   con,
   response_event = response_event )

# Fit Baseline Regularization
fit <- fitBaselineRegularization( br_data )

# Get drug coefficients
getCoefficients( fit, con )
