library(move2)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(sf)
library(lutz)
library(rlang)
library(units)
library(lubridate)
library(grid)

# dt <- readRDS("~/Downloads/test__Workflow_Instance_001__Movebank_Location__2026-04-24_22-01-30.rds")
# data <- filter_track_data(dt, .track_id = c("Lina.ABA63..ebos.9679."))
# data <- dt
# names(data)
# summary(data)

# plot_nb_lcs <- T
# bat_attr_prov <- "eobs_battery_voltage"
# bat_attr <- NULL
# plot_fix_rate <- T
# unts_fix_rate <- "min"
# use_local_time <- T
# attr_line <- "eobs_fix_battery_voltage,eobs_speed_accuracy_estimate,eobs_temperature,eobs_used_time_to_get_fix,gpsdop,gpssatellite_count"
# attr_boxplot <- "eobs_fix_battery_voltage,eobs_speed_accuracy_estimate,eobstemperature,eobs_used_timeto_get_fix,gps_dop,gps_satellite_count"
# pdfMode <- "perAttrib" #"perTrack" "perAttrib"
# # pdf_file <- "plots_prt.pdf"
# 
# 
# 
# dtt <- readRDS("~/Downloads/test__Workflow_Instance_002__Movebank_Location__2026-04-26_19-18-22.rds")
#  data <- filter_track_data(dtt, .track_id = c("Floreana_131_11964"))
# #data <- dtt
# 
# # names(data)
# # summary(data)
# 
# plot_nb_lcs <- T
# bat_attr_prov <- "tag_voltage"
# plot_fix_rate <- T
# bat_attr <- NULL #"tag_voltage"#"eobs_battery_voltage"
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

## ToDo plot all tags

# dtt <- readRDS("~/Downloads/test__Workflow_Instance_002__Movebank_Location__2026-04-26_19-18-22.rds")
# dtt$date <- as.Date(mt_time(dtt))
# data_daily <- dtt %>% group_by(mt_track_id()) |> count(date, name = "n_fix") |> sf::st_drop_geometry() %>% rename(TrackId=`mt_track_id()`)
# 
# ggplot() +
#   geom_path(data = data_daily, aes(x = date, y=n_fix,color=TrackId), linewidth=0.1, alpha=0.8)+
#   geom_point(data = data_daily, aes(x = date, y =n_fix, color=TrackId))+
#   scale_y_continuous(name = "Number GPS fixes per day") +
#   labs(title = "Number of GPS fixes", subtitle = paste("All track"))+
#   theme_bw()+
#   xlab("")



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
    
    if(plot_nb_lcs){
      ## nb_volt
      if (length(batvot_ok) == 1) {
        if(length(trk[[batvot_ok]][is.na(trk[[batvot_ok]])])==length(trk[[batvot_ok]]))
          batvot_ok <- NULL
      }
      if (length(batvot_ok) == 1) {
        bat_units <- units(trk[[batvot_ok]])
        bat_label <- paste("Battery voltage", ifelse(is.null(bat_units), "", paste0("(", bat_units, ")")))
        
        # ranges
        bat_max <- max(as.numeric(trk[[batvot_ok]]), na.rm = TRUE)
        y_max   <- max(data_daily$n_fix, na.rm = TRUE)  # or another chosen max
        bat_min <- min(as.numeric(trk[[batvot_ok]]), na.rm = TRUE)
        # if(bat_max<10000){bat_min <- 2000} else if(bat_max<10){bat_min <- 2}else{bat_min <- 0}
        
        # map: primary = a + b * battery
        b <- y_max / (bat_max - bat_min)
        a <- -b * bat_min
        
        color_voltage <- "limegreen"
        
        nb_volt <- ggplot() +  
          geom_point(data = trk, aes(x = mt_time(trk), y = a + b * as.numeric(.data[[batvot_ok]])), shape = 20, size=0, colour = "white") + ## workaround to get both plotted on the same xaxis
          geom_path(data = trk, aes(x = mt_time(trk)-hours(12), y = a + b * as.numeric(.data[[batvot_ok]])), colour = color_voltage, linewidth=0.1, alpha=0.8) +
          geom_point(data = trk, aes(x = mt_time(trk)-hours(12), y = a + b * as.numeric(.data[[batvot_ok]])), shape = 4, colour = color_voltage) +
          geom_path(data = data_daily, aes(x = date, y=n_fix), linewidth=0.1, alpha=0.8)+
          geom_point(data = data_daily, aes(x = date, y =n_fix))+
          scale_y_continuous(name   = "Number GPS fixes per day", limits = c(0, y_max), sec.axis = sec_axis(transform = ~ ( . - a ) / b, name  = bat_label)) +
          labs(title = paste0("Number of GPS fixes per day and ",batvot_ok), subtitle = paste("Track: ", id))+
          theme_bw()+
          theme(axis.title.y.right = element_text(colour = color_voltage),  axis.text.y.right=element_text(colour = color_voltage))+
          xlab("")
        
      } else {
        nb_volt <- ggplot() +
          geom_path(data = data_daily, aes(x = date, y=n_fix), linewidth=0.1, alpha=0.8)+
          geom_point(data = data_daily, aes(x = date, y =n_fix))+
          scale_y_continuous(name = "Number GPS fixes per day") +
          labs(title = "Number of GPS fixes", subtitle = paste("Track: ", id))+
          theme_bw()+
          xlab("")
      }
    }else{nb_volt <- NULL}
    ## fix rate
    if(plot_fix_rate){
      fixrt <- ggplot(trk) +
        geom_boxplot(aes(x = date, y = mt_time_lags(trk, unts_fix_rate), group = date), outliers = FALSE, na.rm = TRUE) +
        theme_bw() +
        labs(title = "Fix rate (approx)", subtitle = paste("Track: ", id))+
        xlab("") +
        ylab("")
    }else{fixrt <- NULL}
    
    ## other attr lines
    if(!is.null(attr_line_ok)){
      ggtrk_all_ls <- lapply(seq_along(attr_line_ok), function(i) {
        atr <- attr_line_ok[i]
        ggplot(trk) + 
          geom_line(aes(x = mt_time(trk), y = !!sym(atr)), linewidth=0.1, alpha=0.5 , show.legend = TRUE) +
          geom_point(aes(x = mt_time(trk), y = !!sym(atr)), show.legend = TRUE) +
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
  
  # remove NULL entries for tracks without plots, and plots set to FALSE
  track_plots_list <- Filter(Negate(is.null), track_plots_list)
  track_plots_list <- lapply(track_plots_list, function(x){if (is.list(x)) {Filter(Negate(is.null), x)} else {x}}) 
  
  ## add study name to top of pdf
  study_name <- as.character(unique(mt_track_data(data)$name))
  add_study_header <- function(page_grob, study_name) {
    grid.arrange(
      textGrob(
        label = paste("Study: ", study_name),
        x = 0.01, y = 0.99, just = c("left", "top"),
        gp = gpar(cex = 1.2, fontface = "bold")
      ),
      page_grob,
      ncol = 1,
      heights = c(0.08, 0.92)
    )
  }
  
  study_name_valid <- gsub("[^A-Za-z0-9]+", "_", study_name)
  study_name_valid <- gsub("_+", "_", study_name_valid)
  
  
  
  # --- helper functions ------------------------------------------------------
  
  get_pdf_dims <- function(n_plots) {
    if (n_plots == 1) {
      list(width = 21, height = 10, nrow = 2, ncol = 3)
    } else if (n_plots == 2) {
      list(width = 14, height = 5, nrow = 1, ncol = 2)
    } else if (n_plots == 3) {
      list(width = 21, height = 5, nrow = 1, ncol = 3)
    } else if (n_plots == 4) {
      list(width = 14, height = 10, nrow = 2, ncol = 2)
    } else if (n_plots %in% c(5, 6)) {
      list(width = 21, height = 10, nrow = 2, ncol = 3)
    } else {
      list(width = 21, height = 10, nrow = 2, ncol = 3)
    }
  }
  
  # flatten exactly one level, preserving grobs
  flatten_grob_list <- function(x) {
    unlist(x, recursive = FALSE, use.names = FALSE)
  }
  
  # this MUST return one grob/gtable, not a list
  add_study_header <- function(page_grob, study_name) {
    arrangeGrob(
      grobs = list(
        textGrob(
          label = paste("Study ID:", study_name),
          x = 0.01, y = 0.5,
          just = c("left", "center"),
          gp = gpar(fontsize = 16, fontface = "bold")
        ),
        page_grob
      ),
      ncol = 1,
      heights = c(0.06, 0.94)
    )
  }
  
  save_pages_with_header <- function(page_grobs, filename, study_name, width, height) {
    page_grobs <- flatten_grob_list(page_grobs)
    page_grobs <- Filter(Negate(is.null), page_grobs)
    
    pages_with_header <- lapply(page_grobs, function(pg) {
      add_study_header(pg, study_name = study_name)
    })
    
    pages_with_header <- Filter(Negate(is.null), pages_with_header)
    
    final_pages <- marrangeGrob(
      grobs = pages_with_header,
      nrow = 1,
      ncol = 1
    )
    
    ggsave(
      filename = appArtifactPath(filename),
      plot = final_pages,
      width = width,
      height = height
    )
  }
  
  # --- PDF creation logic ----------------------------------------------------
  if (pdfMode == "perTrack") {
    
    n_plots_track <- if (length(track_plots_list) > 0) {
      length(track_plots_list[[1]])
    } else {
      0
    }
    
    dims <- get_pdf_dims(n_plots_track)
    
    if (n_plots_track == 1) {
      # one plot per track -> 6 tracks per page
      all_track_plots <- lapply(track_plots_list, function(x) {
        if (is.null(x) || length(x) == 0) return(NULL)
        x[[1]]
      })
      all_track_plots <- Filter(Negate(is.null), all_track_plots)
      
      all_pages <- marrangeGrob(
        grobs = all_track_plots,
        nrow = 2,
        ncol = 3
      )
      
    } else {
      all_pages1 <- lapply(names(track_plots_list), function(id) {
        grobs_id <- track_plots_list[[id]]
        marrangeGrob(
          grobs = grobs_id,
          nrow = dims$nrow,
          ncol = dims$ncol
        )
      })
      
      all_pages <- flatten_grob_list(all_pages1)
    }
    
    save_pages_with_header(
      page_grobs = all_pages,
      filename   = paste0(study_name_valid, "__tag_diagnostics_plots_by_Track.pdf"),
      study_name = study_name,
      width      = dims$width,
      height     = dims$height
    )
    
  } else if (pdfMode == "perAttrib") {
    
    plot_long <- list()
    for (id in names(track_plots_list)) {
      pl <- track_plots_list[[id]]
      for (nm in names(pl)) {
        plot_long[[length(plot_long) + 1]] <- list(
          key = nm,
          id = id,
          grob = pl[[nm]]
        )
      }
    }
    
    keys <- unique(vapply(plot_long, `[[`, character(1), "key"))
    
    n_per_key <- sapply(keys, function(k) {
      sum(vapply(plot_long, function(x) x$key == k, logical(1)))
    })
    
    max_plots_group <- if (length(n_per_key) > 0) max(n_per_key) else 0
    dims <- get_pdf_dims(max_plots_group)
    
    pages_by_key <- lapply(keys, function(k) {
      grobs_k <- lapply(plot_long, function(x) {
        if (x$key == k) x$grob else NULL
      })
      grobs_k <- Filter(Negate(is.null), grobs_k)
      
      if (length(grobs_k) == 0) return(NULL)
      
      if (length(grobs_k) == 1) {
        marrangeGrob(grobs = grobs_k, nrow = 2, ncol = 3)
      } else {
        marrangeGrob(
          grobs = grobs_k,
          nrow = dims$nrow,
          ncol = dims$ncol
        )
      }
    })
    
    pages_by_key <- Filter(Negate(is.null), pages_by_key)
    all_pages <- flatten_grob_list(pages_by_key)
    
    if (length(all_pages) > 0) {
      save_pages_with_header(
        page_grobs = all_pages,
        filename   = paste0(study_name_valid, "__tag_diagnostics_plots_by_Attribute.pdf"),
        study_name = study_name,
        width      = dims$width,
        height     = dims$height
      )
    } else {
      warning("No plots available for perAttrib pdfMode.")
    }
  }
  return(data)
}