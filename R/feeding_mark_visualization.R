#' Create leaf mask images with highlights of the latest and previous feeding marks.
#' @name feedingMarkDiffMap
#' @param peaks data.table of peak detection result.
#' @param roi_number The number of ROIs to be plotted. Only 1 argument is accepted.
#' @param image_dir Path to leaf detection binary images (mask2)
#' @param file_prefix File name before frame numbers. Default "mask2_feeding_track_".
#' @param roi_path Character. Path to ROI file for imageJ. If NULL, the image will not be trimmed.
#' @param out_format Character. File type of output images.'tif', 'png', 'jpg'.
#' @param frame_number_digit digit of frame number. Default 3 (001~999).
#' @param extension Character. File extension of input mask images. Default = `png`.
#' @param out_prefix Character. A prefix that will be added to all the output file.
#' @param out_dir Directory to save output image.
#' @return Results will be saved to `out_dir`.
#' @importFrom  imager as.cimg imsub where R G B R<- G<- B<-
#' @importFrom  RImageJROI read.ijzip read.ijroi
#' @importFrom png readPNG
#' @importFrom dplyr summarise
#' @importFrom grDevices dev.off jpeg png tiff
#' @export
drawFeedingMark <- function(peaks,
                               roi_number,
                               image_dir,
                               roi_path = NULL,
                               file_prefix = "mask2_feeding_track_",
                               extension = ".png",
                               frame_number_digit = 3,
                               out_dir = NULL,
                               out_prefix = "diff_",
                               out_format = "png"){

  # Loar ImageJ ROI
  if(is.null(roi_path)){

  }else if(endsWith(roi_path,".zip")){
    roi_coord <- read.ijzip(roi_path)
  }else if (endsWith(roi_path,".roi")){
    roi_coord <- read.ijroi(roi_path)
  }else{
    stop("Invalid ROI extention for roi_path. It should be .zip or .roi.")
  }

  # For restoring par later.
  tmp_par <- par()

  # Extract frames of end of each feeding event (=peak end).
  frame_list_start <- peaks[roi == roi_number, start] - 1  # Start in peaks is the first point above the threshold.
  frame_list_start[frame_list_start == 0] <- 1

  frame_list_end <- peaks[roi == roi_number, end]

  if(length(frame_list_start) < 2){
    warning("Total ROI number is less than 2. No diff images were produced.\n")
    return()
  }


  # Add first frame as reference
  image_path_first <- file.path(image_dir,
                                paste0(file_prefix,formatC(1,width = frame_number_digit,flag = "0"),extension))
  image_path_start <- file.path(image_dir,
                          paste0(file_prefix,formatC(frame_list_start,width = frame_number_digit,flag = "0"),extension))
  image_path_end <- file.path(image_dir,
                                paste0(file_prefix,formatC(frame_list_end,width = frame_number_digit,flag = "0"),extension))


  # Load all images as list
  # imgs_start = lapply(image_path_start,
  #                     load.image )

  png2cimg <- function(x){
    suppressWarnings(
      as.cimg(aperm(readPNG(x),c(2,1,3)) )
    )
  }


  # Do not use load.image, because it is too slow.
  imgs_first <- png2cimg(image_path_first)
  imgs_start = lapply(image_path_start,
                      png2cimg)
  imgs_end = lapply(image_path_end,
                    png2cimg)

  # Crop ROI if ROI file is supplied.
  if(is.null(roi_path)){
    imgs_first_sub <- imgs_first
    imgs_start_sub <- imgs_start
    imgs_end_sub <- imgs_end
  }else{
    imgs_first_sub  <-
                                imsub(
                                  imgs_first,
                                  x > roi_coord[[roi_number]]$xrange[1] &
                                    x < roi_coord[[roi_number]]$xrange[2],
                                  y > roi_coord[[roi_number]]$yrange[1] &
                                    y < roi_coord[[roi_number]]$yrange[2]
                                )

    imgs_start_sub  <- lapply(imgs_start,
                            function(tmp) {
                              imsub(
                                tmp,
                                x > roi_coord[[roi_number]]$xrange[1] &
                                  x < roi_coord[[roi_number]]$xrange[2],
                                y > roi_coord[[roi_number]]$yrange[1] &
                                  y < roi_coord[[roi_number]]$yrange[2]
                              )
                            })
    imgs_end_sub  <- lapply(imgs_end,
                        function(tmp) {
                          imsub(
                            tmp,
                            x > roi_coord[[roi_number]]$xrange[1] &
                              x < roi_coord[[roi_number]]$xrange[2],
                            y > roi_coord[[roi_number]]$yrange[1] &
                              y < roi_coord[[roi_number]]$yrange[2]
                          )
                        })

  }


  mark_pos <- data.table()

  for(i.img in 1:(length(frame_list_start) ) ){

    # old feeding marks: cyan, latest feeding mark: red

    img_diff_old <- imgs_start_sub[[i.img]] -  imgs_first_sub
    img_diff <- imgs_end_sub[[i.img]] -  imgs_start_sub[[i.img]]

    img_out <- imgs_first_sub

    # Do not change the colors of pixels that were white(background) in the first frame.

    # # Red for the lated feeding marks.
    # R(img_out)[R(imgs_sub[[1]])==0] <- R(img_diff)[R(imgs_sub[[1]])==0]
    # # Blue for the old feeding marks.
    # B(img_out)[B(imgs_sub[[1]])==0] <- B(img_diff_old)[B(imgs_sub[[1]])==0]

    # Mask for not chaning colors of the background in the first image.
    first_mask <- R(imgs_first_sub) == 0

    # Red for the lated feeding marks.
    R(img_out)[first_mask] <- R(img_diff)[first_mask]
    # Green for the old feeding marks.
    G(img_out)[first_mask] <- B(img_diff_old)[first_mask]

    # Red + Blue = Magenta, Green + Blue = Cyan
    B(img_out)[first_mask] <- R(img_diff)[first_mask] | B(img_diff_old)[first_mask]

    # plot(img_out)




    out_path <- file.path(out_dir,
                          paste0(out_prefix,formatC(i.img,width = 3,flag = "0"),".",out_format)
    )

    if(out_format == "png"){
      png(out_path,
          width = dim(img_out)[1],
          height = dim(img_out)[2])
    }else if(out_format %in% c("tif","tiff")){
      tiff(out_path,
          width = dim(img_out)[1],
          height = dim(img_out)[2])
    }else if(out_format %in% c("jpg","jpeg")){
      jpeg(out_path,
           width = dim(img_out)[1],
           height = dim(img_out)[2])
    }else{
      stop("out_format must be either of 'png', 'tif' or 'jpg")
    }

      par(mar= c(0,0,0,0))
      plot( img_out,
            rescale = F,
            interpolate = F,
            axes = F)
    dev.off()




    # Calculate center of mass for the latest feeding mark
    px_diff <- img_diff == 1

    # Get center of gravity
    mark_cog <- imager::where(px_diff) %>%
      dplyr::summarise(mx=mean(x),my=mean(y))
    mark_cog$mark_no <- i.img
    mark_cog$start_time <- peaks[roi == roi_number, start][i.img]
    mark_cog$end_time <- peaks[roi == roi_number, end][i.img]
    mark_pos <- rbind(mark_pos,
                      mark_cog)
  } # i.img


  # Distance between feeding marks
  for(i.mark in 2:nrow(mark_pos)  ){
    mark_pos[mark_no == i.mark,
               dist := sqrt((mark_pos[i.mark,mx]-mark_pos[i.mark-1,mx])^2 + (mark_pos[i.mark,my]-mark_pos[i.mark-1,my])^2)
    ]
  }

  # Restore par.
  suppressWarnings(
    par(tmp_par)
  )

  fwrite(mark_pos,
         file.path(out_dir,"feeding_mark_center_pos.csv")
  )
}


