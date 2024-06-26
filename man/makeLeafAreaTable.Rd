% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_imagej_results.R
\name{makeLeafAreaTable}
\alias{makeLeafAreaTable}
\title{Load series of ImageJ output of leaf area measurement (`.xls` files) and make them into a single data.table.}
\usage{
makeLeafAreaTable(
  xls_dir,
  name_pat = "^[0-9]*",
  area_convertion_rate = 1,
  frame_interval = 1,
  roi_info_file = NULL
)
}
\arguments{
\item{xls_dir}{Character.Path to directory that contains `.xls` files of leaf area measurement for each frame.}

\item{name_pat}{Character. Regular expression for finding files of leaf area measurement for each frame.}

\item{area_convertion_rate}{Numeric. Conversion rate for leaf area. Default = 1.}

\item{frame_interval}{Numeric. Time intervals between frames. Default = 1.}

\item{roi_info_file}{Character. Path to ROI information file(csv, "roi" column with sequential number from 1, and extra columns that characterize each ROI). Set `NULL` if ROI information is not provided at this stage.}
}
\value{
Long-format data.table of frame, time, ROI, and leaf area.
}
\description{
Load series of ImageJ output of leaf area measurement (`.xls` files) and make them into a single data.table.
}
