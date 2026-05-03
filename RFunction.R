library(move2)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(sf)
library(lutz)
library(rlang)
library(units)
library(lubridate)

# dt <- readRDS("~/Downloads/test__Workflow_Instance_001__Movebank_Location__2026-04-24_22-01-30.rds")
# data <- filter_track_data(dt, .track_id = c("Lina.ABA63..ebos.9679."))
# data <- dt
# names(data)
# summary(data)
# 
# bat_attr <- "eobs_battery_voltage"
# unts_fix_rate <- "min"
# use_local_time <- T
# attr_line <- "eobs_fix_battery_voltage,eobs_speed_accuracy_estimate,eobs_temperature,eobs_used_time_to_get_fix,gpsdop,gpssatellite_count"
# attr_boxplot <- ""# "eobs_fix_battery_voltage,eobs_speed_accuracy_estimate,eobstemperature,eobs_used_timeto_get_fix,gps_dop,gps_satellite_count"
# pdfMode <- "perAttrib" #"perTrack" "perAttrib"
# pdf_file <- "plots_prt.pdf"
# 
# 
# 
# dtt <- readRDS("~/Downloads/test__Workflow_Instance_002__Movebank_Location__2026-04-26_19-18-22.rds")
# # data <- filter_track_data(dtt, .track_id = c("Floreana_131_11964"))
# data <- dtt
# 
# names(data)
# summary(data)
# 
# bat_attr <- "tag_voltage"
# unts_fix_rate <- "min"
# use_local_time <- T
# attr_line <- "sigfox_rssi"
# attr_boxplot <- "sigfox_rssi"
# pdfMode <- "perAttrib" #"perTrack" "perAttrib"
# pdf_file <- "plots_per_attribute.pdf"#"plots_per_attribute.pdf" plots_per_track.pdf


rFunction <- function(data,
                      plot_nb_lcs,
                      add_vot,
                      bat_attr,
                      plot_fix_rate,
                      unts_fix_rate,
                      attr_line,
                      attr_boxplot,
                      use_local_time,
                      pdfMode = c("perTrack", "perAttrib")
                      ) {
  
  ## check bat attr name
  batvot_ok <- bat_attr[bat_attr %in% names(data)]
  ## check line attr name  
  attr_line_L <- strsplit(attr_line, ",")[[1]]
  attr_line_L <- gsub(" ", "", attr_line_L, fixed = TRUE)
  attr_line_ok <- attr_line_L[attr_line_L %in% names(data)]
  attr_line_error <- attr_line_L[!attr_line_L %in% names(data)]
  if (length(attr_line_error) > 0) {logger.info(paste0("Warning! Your defined attributes: ",paste0('"',attr_line_error,'"', collapse = ", ")," do not exist in the data set. They will not be plotted."))}
  ## check boxplot attr name  
  attr_boxplot_L <- strsplit(attr_boxplot, ",")[[1]]
  attr_boxplot_L <- gsub(" ", "", attr_boxplot_L, fixed = TRUE)
  attr_boxplot_ok <- attr_boxplot_L[attr_boxplot_L %in% names(data)]
  attr_boxplot_error <- attr_boxplot_L[!attr_boxplot_L %in% names(data)]
  if (length(attr_boxplot_error) > 0) {logger.info(paste0("Warning! Your defined attributes: ",paste0('"',attr_boxplot_error,'"', collapse = ", ")," do not exist in the data set. They will not be plotted."))}
  
  
  data_L <- split(data, mt_track_id(data))
  
  # helper to build all plots for one track, return as named list
  make_track_plots <- function(trk, id) {
    
    if (use_local_time) {
      coords <- st_coordinates(trk)
      timezns <- tz_lookup_coords(lat = coords[, "Y"], lon = coords[, "X"], method = "accurate")
      most_frequent_tz <- names(which.max(table(timezns)))
      trk$timestamp_local <- with_tz(mt_time(trk), tzone = most_frequent_tz)
      if (length(unique(timezns)) != 1) {logger.warn(paste0("There are multiple local timezones present in the track. ","The most frequent local timezone present is used: ", most_frequent_tz))}
      mt_time(trk) <- "timestamp_local"
    }
    
    trk$date <- as.Date(mt_time(trk))
    data_daily <- trk |> count(date, name = "n_fix") |> sf::st_drop_geometry()
    
    # if (nrow(data_daily) == 0) {return(NULL)}
    
    breaks_noon <- as.POSIXct(paste(seq(min(unique(data_daily$date)), max(unique(data_daily$date)), by = "day"), "12:00:00"), tz = attr(mt_time(trk), "tzone"))
    if (length(breaks_noon) >= 2) {
      breaks_noon_subset <- breaks_noon[seq(2, length(breaks_noon), 2)]
    } else {
      breaks_noon_subset <- breaks_noon
    }
    
    data_daily <- data_daily |> mutate(date_noon = as.POSIXct(paste(date, "12:00:00"), tz = attr(mt_time(trk), "tzone")))
    
    if(plot_nb_lcs){
    
    ## nb_volt
      if(add_vot){
    if (length(batvot_ok) == 1) {
      bat_units <- units(trk[[bat_attr]])
      bat_label <- paste("Battery voltage", ifelse(is.null(bat_units), "", paste0("(", bat_units, ")")))
      
      coeff <- max(data_daily$n_fix, na.rm = TRUE) /
        as.numeric(max(trk[[bat_attr]], na.rm = TRUE))
      
      nb_volt <- ggplot() +
        geom_bar(data = data_daily, aes(x = date_noon, y = n_fix), stat = "identity", fill = "grey70") +
        geom_path(data = trk, aes(x = mt_time(trk), y = as.numeric(.data[[bat_attr]]) * coeff), colour = "blue") +
        geom_point(data = trk, aes(x = mt_time(trk), y = as.numeric(.data[[bat_attr]]) * coeff), shape = 4, colour = "blue") +
        scale_x_datetime(breaks = breaks_noon_subset, name = "", date_labels = "%d %b") +
        scale_y_continuous(name = "Number GPS fixes per day", sec.axis = sec_axis(~ . / coeff, name = bat_label)) +
        labs(title = "Number of GPS fixes per day and battery voltage", subtitle = paste("Track: ", id))+
        theme_bw()
    } else {
      nb_volt <- ggplot() +
        geom_bar(data = data_daily, aes(x = date_noon, y = n_fix), stat = "identity", fill = "grey70") +
        scale_x_datetime(breaks = breaks_noon_subset, name = "", date_labels = "%d %b") +
        scale_y_continuous(name = "Number GPS fixes per day") +
        labs(title = "Number of GPS fixes", subtitle = paste("Track: ", id))+
        theme_bw()
    }
      }else{
        nb_volt <- ggplot() +
          geom_bar(data = data_daily, aes(x = date_noon, y = n_fix), stat = "identity", fill = "grey70") +
          scale_x_datetime(breaks = breaks_noon_subset, name = "", date_labels = "%d %b") +
          scale_y_continuous(name = "Number GPS fixes per day") +
          labs(title = "Number of GPS fixes", subtitle = paste("Track: ", id))+
          theme_bw() 
      }
    }
    ## fix rate
    if(plot_fix_rate){
    fixrt <- ggplot(trk) +
      geom_boxplot(aes(x = date, y = mt_time_lags(trk, unts_fix_rate), group = date), outliers = FALSE, na.rm = TRUE) +
      theme_bw() +
      labs(title = "Fix rate (approx)", subtitle = paste("Track: ", id))+
      xlab("") +
      ylab("")
    }
    
    ## other attr lines
    ggtrk_all_ls <- lapply(seq_along(attr_line_ok), function(i) {
      atr <- attr_line_ok[i]
      ggplot(trk) + 
        geom_line(aes(x = mt_time(trk), y = !!sym(atr)), show.legend = TRUE) +
        labs(title = atr, subtitle = paste("Track: ", id))+
        xlab("") +
        ylab("") +
        theme_bw()
    })
    names(ggtrk_all_ls) <- paste0(attr_line_ok, "_line")
    
    ## other attr boxplot
    ggtrk_all_bx <- lapply(seq_along(attr_boxplot_ok), function(i) {
      atr <- attr_boxplot_ok[i]
      ggplot(trk) + 
        geom_boxplot(aes(x = date, y =!!sym(atr), group = date), outliers = FALSE, na.rm = TRUE) +
        labs(title = atr, subtitle = paste("Track: ", id))+
        xlab("") +
        ylab("") +
        theme_bw()
    })
    names(ggtrk_all_bx) <- paste0(attr_boxplot_ok, "_box")
    
    # return as a named list
    c(list(nb_volt = nb_volt, fixrt   = fixrt), ggtrk_all_ls, ggtrk_all_bx)
  }
  
  # lapply over tracks
  track_plots_list <- lapply(names(data_L), function(id) {
    trk <- data_L[[id]]
    make_track_plots(trk, id)
  })
  names(track_plots_list) <- names(data_L)
  
  # remove NULL entries for tracks without plots
  track_plots_list <- Filter(Negate(is.null), track_plots_list)
  
  # --- PDF creation logic ---------------------------------------------------
  if (pdfMode == "perTrack") {
    
    # one multi-page PDF; per track, plots are consecutive, 2x3 grid
    all_pages <- lapply(names(track_plots_list), function(id) {
      # list of grobs for this track, in fixed order:
      # nb_volt, fixrt, then attributes
      grobs_id <- track_plots_list[[id]]
      # marrangeGrob will paginate if more than 6 for a track [web:18][web:19]
      marrangeGrob(grobs = grobs_id, nrow = 3, ncol = 2)
    })
    
    # all_pages is a list of "grob lists"; bind them
    # all_pages <- do.call(c, all_pages)
    
    ggsave("tag_diagnostics_plots.pdf", all_pages, width = 10, height = 10)
    
  } else if (pdfMode == "perAttrib") {
    
    # collect per-attribute (including nb_volt, fixrt) across tracks
    # 1) named vector of attribute keys per plot list
    # nb_volt and fixrt are always the first two names
    # any extra names are attributes
    # build a long list of plots with tags
    plot_long <- list()
    for (id in names(track_plots_list)) {
      pl <- track_plots_list[[id]]
      for (nm in names(pl)) {
        key <- nm              # "nb_volt", "fixrt", or attribute name
        plot_long[[length(plot_long) + 1]] <-
          list(key = key, id = id, grob = pl[[nm]])
      }
    }
    
    # unique keys to iterate over
    keys <- unique(vapply(plot_long, `[[`, character(1), "key"))
    
    # one list element per key; each element is either a list of pages or NULL
    pages_by_key <- lapply(keys, function(k) {
      grobs_k <- lapply(plot_long, function(x) {
        if (x$key == k) x$grob else NULL
      })
      grobs_k <- Filter(Negate(is.null), grobs_k)
      
      if (length(grobs_k) == 0) {
        return(NULL)
      }
      marrangeGrob(grobs = grobs_k, nrow = 3, ncol = 2)
    })
    
    # drop NULL keys
    pages_by_key <- Filter(Negate(is.null), pages_by_key)
    
    # flatten into a single list of pages (same as your c(all_pages, pages_k) loop)
    all_pages <- pages_by_key#do.call(c, pages_by_key)
    
    if (length(all_pages) > 0) {
      ggsave("tag_diagnostics_plots.pdf", all_pages, width = 10, height = 10)
    } else {
      warning("No plots available for perAttrib pdfMode.")
    }
  }
  
  return(data)
}