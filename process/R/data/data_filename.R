

create_hash_filename <- function(params, test_flag = TEST_RUN) {
  
  if (test_flag) {
    return ("test")
  }
  
  # 1. Consistent JSON string
  
  # To mimic Python's sort_keys=True, we order the list by name if it has names
  if (is.list(params) && !is.null(names(params))) {
    params <- params[order(names(params))]
  }
  
  # auto_unbox=TRUE ensures scalars are not wrapped in brackets (e.g., 1 vs [1])
  # digits=NA ensures maximum precision for numbers
  json_str <- toJSON(params, auto_unbox = TRUE, digits = NA)
  
  # 2. Generate MD5 hash
  # serialize=FALSE ensures we hash the string content itself, not the R object wrapper
  signature <- digest(json_str, algo = "md5", serialize = FALSE)
  
  return(signature)
}