
create_inputs <- function(data_dir, required_data_types = c("pop"), data_type = "parquet") {
  
  inputs <- list()
  for (proc_data_type in required_data_types) {
    proc_data_path <- paste0(
      data_dir, "/", paste0(proc_data_type, "_data.", data_type)
    )
    inputs[[proc_data_type]] <- read_parquet(proc_data_path)
  }
  
  return (inputs)
}