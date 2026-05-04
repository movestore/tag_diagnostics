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
# bat_attr_prov <- "eobs_battery_voltage"
# bat_attr <- NULL
# unts_fix_rate <- "min"
# use_local_time <- T
# attr_line <- "eobs_fix_battery_voltage,eobs_speed_accuracy_estimate,eobs_temperature,eobs_used_time_to_get_fix,gpsdop,gpssatellite_count"
# attr_boxplot <- ""# "eobs_fix_battery_voltage,eobs_speed_accuracy_estimate,eobstemperature,eobs_used_timeto_get_fix,gps_dop,gps_satellite_count"
# pdfMode <- "perAttrib" #"perTrack" "perAttrib"
# # pdf_file <- "plots_prt.pdf"
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
# plot_nb_lcs <- T
# bat_attr_prov <- "no_selection"
# plot_fix_rate <- T
# bat_attr <- NULL#"eobs_battery_voltage"
# unts_fix_rate <- "min"
# use_local_time <- T
# attr_line <- "sigfox_rssi"
# attr_boxplot <- "sigfox_rssi"
# pdfMode <- "perAttrib" #"perTrack" "perAttrib"
# pdf_file <- "plots_per_attribute.pdf"#"plots_per_attribute.pdf" plots_per_track.pdf

# voc <- movebank_get_vocabulary()
# names(voc[grep("voltage",voc)])
# 
# "tag_voltage"
# "eobs_battery_voltage"
# "solar_cell_voltage"  
# 
# "battery_charging_voltage"
# "eobs_fix_battery_voltage"
# "solar_voltage_percent"
# "tag_backup_voltage"
# "tinyfox_sunny_index_start_voltage"
# "tinyfox_sunny_index_voltage_increase"
# "voltage_resolution"
# 
# 
# names(voc[grep("mV",voc)])



rFunction <- function(data,
                      plot_nb_lcs,
                      bat_attr_prov,
                      bat_attr,
                      plot_fix_rate,
                      unts_fix_rate,
                      attr_line,
                      attr_boxplot,
                      use_local_time,
                      pdfMode = c("perTrack", "perAttrib")
                      ) {
  
  ## check bat attr name
  if(bat_attr_prov!="no_selection"){
  batvot_ok <- bat_attr_prov[bat_attr_prov %in% names(data)]
  batvot_error <- bat_attr_prov[!bat_attr_prov %in% names(data)]
  if (length(batvot_error) > 0) {logger.info(paste0("Warning! Your selected voltage attribute: ",'"',batvot_error,'"'," does not exist in the data set. It will not be plotted."))}
  }
  if(bat_attr_prov == "no_selection" & !is.null(bat_attr)){
    batvot_ok <- bat_attr[bat_attr %in% names(data)]  
    batvot_error <- bat_attr[!bat_attr %in% names(data)]
    if (length(batvot_error) > 0) {logger.info(paste0("Warning! Your provided voltage attribute: ",'"',batvot_error,'"'," does not exist in the data set. It will not be plotted."))}
  }
  if(bat_attr_prov == "no_selection" & is.null(bat_attr)){
    batvot_ok <- NULL
  }
  
  ## check line attr name  
  if(!is.null(attr_line)){
  attr_line_L <- strsplit(attr_line, ",")[[1]]
  attr_line_L <- gsub(" ", "", attr_line_L, fixed = TRUE)
  attr_line_ok <- attr_line_L[attr_line_L %in% names(data)]
  attr_line_error <- attr_line_L[!attr_line_L %in% names(data)]
  if (length(attr_line_error) > 0) {logger.info(paste0("Warning! Your defined attributes: ",paste0('"',attr_line_error,'"', collapse = ", ")," do not exist in the data set. They will not be plotted."))}
  }else{attr_line_ok <- NULL}
  ## check boxplot attr name 
  if(!is.null(attr_boxplot)){
  attr_boxplot_L <- strsplit(attr_boxplot, ",")[[1]]
  attr_boxplot_L <- gsub(" ", "", attr_boxplot_L, fixed = TRUE)
  attr_boxplot_ok <- attr_boxplot_L[attr_boxplot_L %in% names(data)]
  attr_boxplot_error <- attr_boxplot_L[!attr_boxplot_L %in% names(data)]
  if (length(attr_boxplot_error) > 0) {logger.info(paste0("Warning! Your defined attributes: ",paste0('"',attr_boxplot_error,'"', collapse = ", ")," do not exist in the data set. They will not be plotted."))}
  }else{attr_boxplot_ok <- NULL}
  
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
    
### removed for now -- alignment of middle of bar with noon
    # breaks_noon <- as.POSIXct(paste(seq(min(unique(data_daily$date)), max(unique(data_daily$date)), by = "day"), "12:00:00"), tz = attr(mt_time(trk), "tzone"))
    # if (length(breaks_noon) >= 2) {
    #   breaks_noon_subset <- breaks_noon[seq(2, length(breaks_noon), 2)]
    # } else {
    #   breaks_noon_subset <- breaks_noon
    # }
    # 
    # data_daily <- data_daily |> mutate(date_noon = as.POSIXct(paste(date, "12:00:00"), tz = attr(mt_time(trk), "tzone")))
    
    if(plot_nb_lcs){
    
    ## nb_volt
      if (length(batvot_ok) == 1) {
        if(length(trk[[batvot_ok]][is.na(trk[[batvot_ok]])])==length(trk[[batvot_ok]]))
          batvot_ok <- NULL
      }
    if (length(batvot_ok) == 1) {
      bat_units <- units(trk[[batvot_ok]])
      bat_label <- paste("Battery voltage", ifelse(is.null(bat_units), "", paste0("(", bat_units, ")")))
      
      coeff <- max(data_daily$n_fix, na.rm = TRUE) /
        as.numeric(max(trk[[batvot_ok]], na.rm = TRUE))
      
      # nb_volt <- ggplot() +
      #   # geom_bar(data = data_daily, aes(x = date_noon, y = n_fix), stat = "identity", fill = "grey70") +
      #   geom_bar(data = trk, aes(x = date), fill = "grey70") +
      #   geom_path(data = trk, aes(x = mt_time(trk), y = as.numeric(.data[[batvot_ok]]) * coeff), colour = "blue") +
      #   geom_point(data = trk, aes(x = mt_time(trk), y = as.numeric(.data[[batvot_ok]]) * coeff), shape = 4, colour = "blue") +
      #   # scale_x_datetime(breaks = breaks_noon_subset, name = "", date_labels = "%d %b") +
      #   scale_y_continuous(name = "Number GPS fixes per day", sec.axis = sec_axis(~ . / coeff, name = bat_label)) +
      #   labs(title = "Number of GPS fixes per day and battery voltage", subtitle = paste("Track: ", id))+
      #   theme_bw()+
      #   xlab("")
      
      nb_volt <- ggplot() +  
        geom_point(data = trk, aes(x = mt_time(trk), y = as.numeric(.data[[batvot_ok]]) * coeff), shape = 20, size=0, colour = "white") + ## workaround to get both plotted on the same xaxis
        geom_bar(data = trk, aes(x = date), fill = "grey70") +
        geom_path(data = trk, aes(x = mt_time(trk)-hours(12), y = as.numeric(.data[[batvot_ok]]) * coeff), colour = "blue") +
        geom_point(data = trk, aes(x = mt_time(trk)-hours(12), y = as.numeric(.data[[batvot_ok]]) * coeff), shape = 4, colour = "blue") +
        scale_y_continuous(name = "Number GPS fixes per day", sec.axis = sec_axis(~ . / coeff, name = bat_label)) +
        labs(title = paste0("Number of GPS fixes per day and ",batvot_ok), subtitle = paste("Track: ", id))+
        theme_bw()+
        xlab("")
      
      
    } else {
      nb_volt <- ggplot() +
        # geom_bar(data = data_daily, aes(x = date_noon, y = n_fix), stat = "identity", fill = "grey70") +
        geom_bar(data = data_daily, aes(x = date, y = n_fix), stat = "identity", fill = "grey70") +
        # scale_x_datetime(breaks = breaks_noon_subset, name = "", date_labels = "%d %b") +
        scale_y_continuous(name = "Number GPS fixes per day") +
        labs(title = "Number of GPS fixes", subtitle = paste("Track: ", id))+
        theme_bw()+
        xlab("")
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
    if(!is.null(attr_line_ok)){
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
    }else{ggtrk_all_ls <- NULL}
    
    ## other attr boxplot
    if(!is.null(attr_boxplot_ok)){
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
    }else{ggtrk_all_bx <- NULL}
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
      marrangeGrob(grobs = grobs_id, nrow = 2, ncol = 3)
    })
    
    # all_pages is a list of "grob lists"; bind them
    # all_pages <- do.call(c, all_pages)
    
    ggsave(appArtifactPath("tag_diagnostics_plots_by_Track.pdf"), all_pages, width = 20, height = 10)
    
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
      marrangeGrob(grobs = grobs_k, nrow = 2, ncol = 3)
    })
    
    # drop NULL keys
    pages_by_key <- Filter(Negate(is.null), pages_by_key)
    
    # flatten into a single list of pages (same as your c(all_pages, pages_k) loop)
    all_pages <- pages_by_key#do.call(c, pages_by_key)
    
    if (length(all_pages) > 0) {
      ggsave(appArtifactPath("tag_diagnostics_plots_by_Attribute.pdf"), all_pages, width = 20, height = 10)
    } else {
      warning("No plots available for perAttrib pdfMode.")
    }
  }
  
  return(data)
}