% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feeding_mark_visualization.R
\name{feedingMarkDiffMap}
\alias{feedingMarkDiffMap}
\alias{drawFeedingMark}
\title{Create leaf mask images with highlights of the latest and previous feeding marks.}
\usage{
drawFeedingMark(
  peaks,
  roi_number,
  image_dir,
  roi_path = NULL,
  file_prefix = "mask2_feeding_track_",
  extension = ".png",
  frame_number_digit = 3,
  out_dir = NULL,
  out_prefix = "diff_",
  out_format = "png"
)
}
\arguments{
\item{peaks}{data.table of peak detection result.}

\item{roi_number}{The number of ROIs to be plotted. Only 1 argument is accepted.}

\item{image_dir}{Path to leaf detection binary images (mask2)}

\item{roi_path}{Character. Path to ROI file for imageJ. If NULL, the image will not be trimmed.}

\item{file_prefix}{File name before frame numbers. Default "mask2_feeding_track_".}

\item{extension}{Character. File extension of input mask images. Default = `png`.}

\item{frame_number_digit}{digit of frame number. Default 3 (001~999).}

\item{out_dir}{Directory to save output image.}

\item{out_prefix}{Character. A prefix that will be added to all the output file.}

\item{out_format}{Character. File type of output images.'tif', 'png', 'jpg'.}
}
\value{
Results will be saved to `out_dir`.
}
\description{
Create leaf mask images with highlights of the latest and previous feeding marks.
}
