library(mizer)
remotes::install_github("gustavdelius/mizerShelf")
library(mizerShelf)
library(therMizer)

# 1. Define species parameters for two consumer groups
species_params <- data.frame(
  species = c("pelagic_predators", "benthic_detritivores"),
  w_inf = c(1000, 500),
  w_mat = c(100, 50),
  beta = c(100, 50),
  sigma = c(1.5, 1.5),
  h = c(40, 20),
  gamma = c(5e-4, 5e-4),
  ks = c(10, 10),
  alpha = c(0.6, 0.6),
  fc = c(0.2, 0.2),
  z0 = c(0.6, 0.6),
  stringsAsFactors = FALSE
)

# 2. Create params object
params <- newMultispeciesParams(species_params)

# 3. Define region(s)
region_names <- "global"

# 4. Define resource spectra names
resource_names <- c("pelagic", "benthic_detritus", "benthic_carrion")

# 5. Initialize resource array (size x region x resource)
resource_array <- array(
  NA,
  dim = c(length(params@w_full), length(region_names), length(resource_names)),
  dimnames = list(
    size = params@w_full,
    region = region_names,
    resource = resource_names
  )
)


# 7. Set the resource array on params
params <- setResource(params, resource_array = resource_array)

# 8. Define the custom resource dynamics function (must be in global env)
my_resource_dynamics <- function(params, n, n_pp, n_other, rates, t, dt, resource_rate, resource_capacity, ...) {
  r_rates <- c(pelagic = 10, benthic_detritus = 3, benthic_carrion = 1)
  K <- c(pelagic = 1e6, benthic_detritus = 2e5, benthic_carrion = 1e5)
  
  dn <- list()
  res_names <- dimnames(params@npp_array)[[3]]
  
  for (res in res_names) {
    dn[[res]] <- r_rates[res] * n_pp[[res]] * (1 - n_pp[[res]] / K[res])
  }
  
  return(dn)
}

# 9. Assign the name of the function as a string to params slot
params@resource_dynamics <- "my_resource_dynamics"

# 10. Check your params object
print(params)
# Set the resource array in params
params <- setResource(params)
params<-steady(params)
# 11. Run a simple projection (e.g., 10 years, 1 step per year)
sim <- project(params)
# 12. Plot results
plotBiomass(sim)
