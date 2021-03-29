#divide sample into groups of five years each
sample_16_20_data <-  add_column(sample_16_20_data, year_group = NA)
year_group_allocations <- rbind(c(1985:2014), rep(c(1:6),each=5))
for (i in c(1:30)) {
  sample_16_20_data$year_group[sample_16_20_data$syear==year_group_allocations[1,i]] <- year_group_allocations[2,i] 
}