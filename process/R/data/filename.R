

create_hash_filename <- function(params, test_flag = FALSE, filename_suffix = NULL) {
  
  if (test_flag) {
    return ("test")
  }
  
  if (is.list(params) && !is.null(names(params))) {
    params <- params[order(names(params))]
  }
  
  # auto_unbox=TRUE ensures scalars are not wrapped in brackets (e.g., 1 vs [1])
  json_str <- toJSON(params, auto_unbox = TRUE, digits = NA)
  
  # 2. Generate MD5 hash
  signature <- digest(json_str, algo = "md5", serialize = FALSE)
  
  if (! is.null(filename_suffix)) {
    signature <- paste0(signature, "_", filename_suffix)
  }
  
  return(signature)
}