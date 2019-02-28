
# Code from 
# https://github.com/EpiCompBio/stats_utils/blob/master/stats_utils/run_mice_impute.R
# which works in that script and may be needed here

# Further TODOs check the script itself

# Explore attributes of imputed object which contains the multiply imputed
# data set (class mids) and all information from the procedure including:
# original data, imputed values, number of missing values,
# number of iterations, etc.
# attributes(imp_merged)

# Save predictor matrix:
fwrite(as.data.frame(imp_merged$pred),
			 sprintf('predictor_matrix_%s', output_file_name),
			 sep = '\t',
			 na = 'NA',
			 col.names = TRUE,
			 row.names = TRUE,
			 quote = FALSE
)

# Save methods:
# imputation method used, "" empty string means no NAs
fwrite(as.list(imp_merged$meth),
			 sprintf('methods_%s', output_file_name),
			 sep = '\t',
			 na = 'NA',
			 col.names = TRUE,
			 row.names = FALSE
)



