
require(openxlsx)
require(dplyr)


#' Convert a flextable object to an xlsx file
#'
#' @param ft
#' @param file
#' @param sheet_name
#' @param start_row
#' @param start_col
#' @param append
#'
#' @return
#' @export
#'
#' @examples
#'
ft_to_xlsx <- function(ft, file = "./output/ft_to_xl.xlsx",
                       sheet_name ="ft1",
                       start_row = 1,
                       start_col = 1,
                       append = FALSE) {


  sheet_col_offset <- start_col - 1
  sheet_row_offset <- start_row - 1

  if(append) {
    wb <- openxlsx::loadWorkbook(file = file)
  } else {
    wb <- openxlsx::createWorkbook()
  }

  # ===========================================
  #   add sheet

  ws = wb %>% openxlsx::addWorksheet(sheetName = sheet_name)

  ncols <- ft$body$content$ncol
  nrows <- ft$body$content$nrow

  # get row offsets to body from header size

  start_header_row <-  start_row
  start_body_row <-  start_header_row + ft$header$content$nrow
  start_footer_row <-  start_body_row + nrows

  end_body_row <- start_footer_row - 1

  # add datasets

  df_header <- ft$header$dataset %>% select(all_of(ft$header$col_keys))
  df_body <- ft$body$dataset %>% select(all_of(ft$body$col_keys))

  wb %>% openxlsx::writeData(ws, df_header,
                   startCol = start_col,
                   startRow = start_header_row,
                   colNames = FALSE)

  wb %>% openxlsx::writeData(ws, df_body,
                   startCol = start_col,
                   startRow = start_body_row,
                   colNames = FALSE)

  # ===========================================
  #
  # *************** HEADER ********************

  # format the merge spans

  wb %>% xl_merge_from_spans(ws, ft$header$spans,
                             start_row = start_header_row,
                             start_col = start_col)

  wb %>% xl_styling(ws,ft ,
                    part = "header",
                    start_row = start_header_row,
                    start_col = start_col)

  # ===========================================
  #
  # *************** BODY ********************

  # format the merge spans

  # row_spans <- ft$body$spans$rows
  # col_spans <- ft$body$spans$columns

  wb %>% xl_merge_from_spans(ws, ft$body$spans,
                             start_row = start_body_row,
                             start_col = start_col)


  wb %>% xl_styling(ws, ft ,
                    part = "body",
                    start_row = start_body_row,
                    start_col = start_col)


  pts_per_inch <- 72

  wids_in <- ft$body$colwidths
  wids_pts <- pts_per_inch*wids_in
  pts_ea <- ft$body$styles$text$font.size$data[1,] %>% as.vector()

  wid <- wids_pts/pts_ea*2

  wb %>% openxlsx::setColWidths(ws, cols = 1:ncols, widths = wid) # ft$header$colwidths)

  # ===========================================
  #
  # *************** FOOTER ********************


  ftr_content <- ft$footer$content$data
  if(length(ftr_content) > 0) {
    nlines <- ftr_content %>% dim() %>% {.[1]}

    sapply(1:nlines, function(iline) {

      line_info <- ftr_content[[iline]]

      nparts <- line_info %>% nrow()

      sapply(1:nparts, function(ipart) {

        #browser()


      })

    })

  }

  # ================================================
  #
  #       save what we have

  wb %>% openxlsx::saveWorkbook(file = file, overwrite = TRUE)

}


xl_styling <- function(wb,ws, ft, part, start_row = 1, start_col = 1) {


  this_part <- ft[[part]]

  nrows <- this_part$content$nrow
  ncols <- this_part$content$ncol

  row_offset <- start_row - 1
  col_offset <- start_col - 1

  cell_styles = this_part$styles$cells
  par_styles = this_part$styles$pars
  text_styles = this_part$styles$text

  sapply(1:ncols, function(c) {


    sapply(1:nrows, function(r) {

      txt <- ft[[part]]$content$data[r,c][[1]]$txt
      #cat("txt = ", txt, "\n")
      # fmt <- class(this_part$dataset %>% select(c))
      fill_val <- cell_styles$background.color$data[r,c]
      if(fill_val == "transparent") fill_val <- NULL

      border_data <- border_data(styles = cell_styles, row = r, col = c)
      border_widths <- as.integer(border_data$width)
      border_styles <- border_data$style %>%
        gsub("solid","thin",.) %>%
        {.[border_widths == 0] <- "none";.} %>%
        {.[border_widths > 1] <- "thick";.} %>%
        {.[border_data$color == "transparent"] <- "none";.}

      border_colors <- border_data$color %>%
        gsub("transparent","white",.)

      valign <- cell_styles$vertical.align$data[r,c]
      halign <- par_styles$text.align$data[r,c]
      color <- text_styles$color$data[r,c]
      font_family <- text_styles$font.family$data[r,c] %>% unname()
      font_size <- text_styles$font.size$data[r,c] %>% unname()

      decorations <- ft_text_decoration(text_styles = text_styles, r,c)

      if(decorations$bold) bold <- "bold" else bold <- NULL
      if(decorations$italic) italic <- "italic" else italic <- NULL
      if(decorations$underline) underline <- "underline" else underline <- NULL

      text_dec <- c(bold, italic, underline)

      style <- openxlsx::createStyle(fgFill = fill_val,
                           fontName = font_family,
                           fontSize = font_size,
                           fontColour = color,
                           border = borders(),
                           borderStyle = border_styles,
                           borderColour = border_colors,
                           valign = valign, halign = halign,
                           textDecoration = text_dec
                           #bold = bold, italic = italic, underline = underline
      )

      style_col <- c + col_offset
      style_row <- r + row_offset
      wb %>% openxlsx::addStyle(ws, style = style, rows = style_row, cols = style_col,
                      stack = TRUE)

      wb %>%
        openxlsx::writeData(ws, x = txt, startCol = style_col, startRow = style_row)

      #cat("writing [", txt, "] to ", style_col, ",",  style_row, "\n")
    })
    if(part == "body" && FALSE) {

      num_fmt <- get_colformats_xl(ft,c)
      if(num_fmt != "")
        wb %>% openxlsx::addStyle(ws,
                        style = openxlsx::createStyle(numFmt = num_fmt),
                        rows = 1:nrows,
                        cols = c,
                        stack = TRUE)

    }

  })

  wb

}

ft_text_decoration <- function(text_styles, row, col) {


  bold <- text_styles$bold$data[row,col] %>% unname()
  italic <- text_styles$italic$data[row,col] %>% unname()
  underline <- text_styles$underlined$data[row,col] %>% unname()

  list(bold = bold, italic = italic, underline = underline)
}

borders <- function() {c("top", "bottom", "left", "right")}

border_element <- function(styles, type,  row, col) {

  bords <- borders()

  x <- sapply(bords, function(bord) {
    el <- paste0("border.",type,".", bord)

    styles[[el]]$data[row,col]

  })
  names(x) <- bords
  x
}

border_data <- function(styles, row, col) {


  types <- styles %>% names() %>% {.[grepl("^border[.]",.)]} %>%
    gsub("border.","",.) %>% gsub("(.*)[.].*","\\1",.) %>% unique()

  x <- lapply(types, function(type) {

    border_element(styles, type , row , col )

  })
  names(x) <- types
  x
}

padding_data <- function(styles, row, col) {

  bords <- borders()

  x <- sapply(bords, function(bord) {
    el <- paste0("padding.", bord)

    styles[[el]]$data[row,col]

  })

  names(x) <- bords
  x
}

margin_data <- function(styles, row, col) {

  bords <- borders()

  x <- sapply(bords, function(bord) {
    el <- paste0("margin.", bord)

    styles[[el]]$data[row,col]

  })

  names(x) <- bords
  x
}

ft_style_elements <- function(ft, type = "cells", part = "body") {

  ft[[part]]$styles[[type]] %>% names()

}

xl_merge_from_spans <- function(wb, ws, spans,
                                start_row = 1,
                                start_col = 1) {



  row_spans <- spans$rows
  col_spans <- spans$columns

  wb %>% xl_merge_from_rowspans(ws, row_spans,
                                start_row = start_row,
                                start_col = start_col)

  wb %>% xl_merge_from_colspans(ws, col_spans,
                                start_row = start_row,
                                start_col = start_col)
  wb
}


xl_merge_from_rowspans <- function(wb,ws,spans, start_row = 1, start_col = 1) {

  nrows <- nrow(spans)
  ncols <- ncol(spans)

  row_offset <- start_row - 1
  col_offset <- start_col - 1

  sapply(1:nrows, function(r) {
    sapply(1:ncols, function(c) {
      span_val <- spans[r,c]
      if(span_val > 1) {
        spn_cols <- (c:(c + span_val - 1)) + col_offset
        spn_row <- r + row_offset
        wb %>% openxlsx::mergeCells(ws, cols = spn_cols, rows = spn_row)
      }

    })
  })

  wb
}


xl_merge_from_colspans <- function(wb,ws,spans, start_row = 1, start_col = 1) {

  nrows <- nrow(spans)
  ncols <- ncol(spans)

  row_offset <- start_row - 1
  col_offset <- start_col - 1

  sapply(1:ncols, function(c) {
    sapply(1:nrows, function(r) {
      span_val <- spans[r,c]
      if(span_val > 1) {

        # if(row_offset > 0) browser()
        spn_col <- c + col_offset
        spn_rows <- (r:(r + span_val - 1)) + row_offset
        wb %>% openxlsx::mergeCells(ws, cols = spn_col, rows = spn_rows)
      }

    })
  })

  wb
}


colformat_xl_init <- function(ft) {

  if(is.null(ft$excel)) {
    types = sapply(ft$body$dataset, class)
    formats <- rep("",length(types))
    names(formats) <- names(types)

    ft$excel <- list(types = types, formats = formats)
  }

  ft
}

get_colformats_xl <- function(ft, j=NULL) {

  format <- "GENERAL"
  if(!is.null(ft$excel)) return(ft$excel$formats[j])

  format

}

colformat_xl <- function(ft, j=NULL,format = NULL) {

  if(is.null(j)) j <- col_keys
  ft$excel$formats[j] <- format

  ft

}



colformat_int_xl <- function(ft, i= NULL, j= NULL, ..., fmt = "") {

  ft <- ft %>%
    colformat_xl_init() %>%
    colformat_xl(j,fmt) %>%
    colformat_int(i, j, ...)

  ft
}
#

colformat_num_xl <- function(ft, i= NULL, j= NULL, ..., fmt = "") {

  ft <- ft %>%
    colformat_xl_init() %>%
    colformat_xl(j,fmt) %>%
    colformat_num(i, j, ...)

  ft
}

colformat_date_xl <- function(ft, i= NULL, j= NULL, ..., fmt = "") {

  ft <- ft %>%
    colformat_xl_init() %>%
    colformat_xl(j,fmt) %>%
    colformat_date(i, j, ...)

  ft
}

