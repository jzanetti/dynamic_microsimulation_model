

withr::with_dir("ddmR", {
    # Add license
    use_mit_license("Sijin Zhang")
    
    # Setup testing framework
    use_testthat()
    
    # Add dependencies (Must use quotes!)
    use_package("devtools", type = "Suggests") # devtools is usually for dev, not the package code itself
    use_package("data.table")
    use_package("ggplot2")
    use_package("base64enc")
    use_package("yaml")
    use_package("dplyr")
    use_package("caret")
    use_package("stringr")
    use_package("tidyr")
    use_package("arrow")
    use_package("purrr")
    use_package("stats")
    use_package("gridExtra")
    use_package("jsonlite")
    use_package("digest")
    use_package("readr")
    
    # Create documentation setup
    use_roxygen_md()
})