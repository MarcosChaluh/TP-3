# Convenience script to execute all analyses sequentially.
source(file.path("scripts", "1a_market_rates.R"))
source(file.path("scripts", "1b_inequality.R"))
source(file.path("scripts", "1c_poverty.R"))
source(file.path("scripts", "1d_mincer.R"))
source(file.path("scripts", "1e_structural_indicators.R"))
source(file.path("scripts", "2_remote_work.R"))
