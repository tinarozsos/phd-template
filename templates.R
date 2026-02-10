ref <- readr::read_file("references.bib") |> 
  stringr::str_remove("^[.\\n]*@") |> 
  stringr::str_split_1("@") |> 
  tibble::as_tibble() |> 
  dplyr::mutate(value = stringr::str_replace_all(value, " +", " "), 
                key = stringr::str_extract(value, "^[\\w{-]+") |> 
                  stringr::str_remove("^\\w+\\{"),
                title = stringr::str_extract(value, "(?<=title ?= ?\\{).+(?=\\})"),
                author = stringr::str_extract(value, "(?<=author ?= ?\\{).+(?=\\})"))

#' Generate template for new paper
#' 
#' This function creates a new .qmd file in the research folder based on the paper template. 
#' The file name is provided by the user and must be unique. 
#' If the file name is valid, the new .qmd file will be created and opened in the editor for immediate editing. 
#' 
#' Modify the paper template in the _extensions folder if desired.
#'
#' @param file_name Character string of the file name (without .qmd extension) for the new paper. Must be unique. 
#'
#' @returns File created: research/{file_name}.qmd
#' @export
#'
#' @examples paper("paper_title")
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

#' Generate template for new meeting notes
#' 
#' This function creates a new .qmd file in the meetings folder based on the meeting template.
#' The file name is automatically generated as the current date in the format YYYY-MM-DD.
#' If a file with the same name already exists, an error will be thrown to prevent
#' overwriting existing meeting notes. 
#' If the file name is unique, the new .qmd file will be created and opened in the editor for immediate editing.
#' 
#' Modify the meeting template in the _extensions folder if desired.
#'
#' @returns File created: meetings/{current_date}.qmd
#' @export
#'
#' @examples meeting()
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

#' Generate markdown chunk for new research idea
#'
#' This function creates a markdown chunk for a new research idea and copies it to the clipboard.
#' It also opens the ideas.qmd file in the editor for immediate editing.
#' Paste the markdown chunk into the ideas.qmd file to add a new research idea.
#' 
#' Modify the markdown chunk in template.R if desired. 
#'
#' @returns Markdown chunk copied to clipboard
#' @export
#'
#' @examples idea()
idea <- function() {
  file.edit("ideas.qmd")
'::: {.callout-note icon=false collapse="true"}

## 



:::' |> writeClipboard()
}

#' Find reference in references.bib based on search term
#' 
#' This function searches for a term in the references.bib file and displays the results in a viewer.
#' By default, the search term is matched against the title and author fields of the references
#' If `any = TRUE`, the search term is matched against the entire reference entry.
#' 
#' To be used to find the BibTeX key of a reference for use with `add_ref()`.
#'
#' @param term Character string of the search term to find in the references.bib file. Case insensitive.
#' @param any Logical value indicating whether to search for the term in any part of the reference entry (TRUE) or only in the title and author fields (FALSE, default).
#'
#' @returns A viewer displaying the search results from the references.bib file that match the search term.
#' @export
#'
#' @examples find_ref("Smith")
find_ref <- function(term, any = FALSE) {
  if (any == TRUE) {
    dplyr::filter(ref, grepl(term, value, ignore.case = TRUE)) |> View()
  } else {
    dplyr::filter(ref, grepl(term, title, ignore.case = TRUE) |
                    grepl(term, author, ignore.case = TRUE)) |> View()
  }
}

#' Generate template for new reference
#'
#' This function creates a new .qmd file in the references folder based on the reference template.
#' The file name is provided by the user and must correspond to a BibTeX key
#' in the references.bib file. If the file name is valid, the new .qmd file will be created and opened in the editor for immediate editing.
#' 
#' The function extracts the title, author, and publication information from the references.bib file based on the provided BibTeX key and populates the YAML header of the .qmd file accordingly.
#' Modify the YAML header in templates.R if desired.
#'
#' @param zkey Character string of the BibTeX key for the reference. Must correspond to a key in the references.bib file.
#' @param overwrite Logical value indicating whether to overwrite an existing .qmd file for the reference if it already exists in the references folder. Default is FALSE.
#'
#' @returns File created: references/{zkey}.qmd
#' @export
#'
#' @examples add_ref("smith2022example")
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
