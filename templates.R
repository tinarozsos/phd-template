ref <- readr::read_file("references.bib") |> 
  stringr::str_remove("^[.\\n]*@") |> 
  stringr::str_split_1("@") |> 
  tibble::as_tibble() |> 
  dplyr::mutate(value = stringr::str_replace_all(value, " +", " "), 
                key = stringr::str_extract(value, "^[\\w{-]+") |> 
                  stringr::str_remove("^\\w+\\{"),
                title = stringr::str_extract(value, "(?<=title ?= ?\\{).+(?=\\})"),
                author = stringr::str_extract(value, "(?<=author ?= ?\\{).+(?=\\})"))

paper <- function(file_name = NULL) {
  if (is.null(file_name)) {
    stop("You must provide a valid file_name")
  }
  if (file_name %in% stringr::str_remove(list.files("research"), "\\.qmd")) {
    stop("File already exists")
  }
  # create new qmd report based on skeleton
  file.copy("_extensions/paper-template.qmd",
            paste0("research/", file_name, ".qmd", collapse = ""))
  # open the new file in the editor
  file.edit(paste0("research/", file_name, ".qmd", collapse = ""))
}

meeting <- function() {
  file_name <- as.character(Sys.Date())
  if (file_name %in% stringr::str_remove(list.files("meetings"), "\\.qmd")) {
    stop("File already exists")
  }
  # create new qmd report based on skeleton
  file.copy("_extensions/meeting-template.qmd",
            paste0("meetings/", file_name, ".qmd", collapse = ""))
  # open the new file in the editor
  file.edit(paste0("meetings/", file_name, ".qmd", collapse = ""))
  writeClipboard(file_name)
}

idea <- function() {
  file.edit("ideas.qmd")
'::: {.callout-note icon=false collapse="true"}

## 



:::' |> writeClipboard()
}

find_ref <- function(term, any = FALSE) {
  if (any == TRUE) {
    dplyr::filter(ref, grepl(term, value, ignore.case = TRUE)) |> View()
  } else {
    dplyr::filter(ref, grepl(term, title, ignore.case = TRUE) |
                    grepl(term, author, ignore.case = TRUE)) |> View()
  }
}

add_ref <- function(zkey, overwrite = FALSE) {
  if (!zkey %in% ref$key) {
    stop("Reference not found")
  }
  if (!paste0(zkey, ".qmd", collapse = "") %in% list.files("references") | overwrite == TRUE) {
    ref1 <- ref |> 
      dplyr::filter(key == zkey) |> 
      dplyr::mutate(year = stringr::str_extract(value, "(?<=year = \\{).+(?=\\})"),
                    pub = stringr::str_extract(value, "(?<=journal = \\{).+(?=\\})"),
                    pub = ifelse(is.na(pub), year, paste0(pub, " (", year, ")")),
                    dplyr::across(c(title, author, pub),
                                  ~ stringr::str_remove_all(., "[{}]"))) |> 
      dplyr::select(-value)
    
    paste0(
      '---
title: "', ref1$title, '"
author: "', ref1$author, '"
subtitle: "', zkey, '"
description: "', ref1$pub, '"
categories: [paper1,paper2,misc,UNREAD]
---
') |> write(paste0("references/", zkey, ".qmd", collapse = ""))
  }
  file.edit(paste0("references/", zkey, ".qmd", collapse = ""))
}
