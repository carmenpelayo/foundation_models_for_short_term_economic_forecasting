# ============================================================
# DSA - Evaluacion univariante para RStudio local
# Version paralela: evita dependencias globales dentro de workers PSOCK.
# ------------------------------------------------------------
# Entrada esperada: CSV con dos columnas: timestamp y target.
# Salida: XLSX con metadata, windows, predictions y statistics,
# ademas de graficos de evaluacion y forecast de produccion.
#
# Ajustar la seccion CONFIGURACION antes de ejecutar.
# ============================================================

# -----------------------------
# 0) CONFIGURACION DEL USUARIO
# -----------------------------

# Ruta del CSV local (relativa a la raiz del proyecto).
DATA_PATH <- "data/data_euribor3M.csv"

# Si el archivo exportado tiene dos lineas iniciales de metadatos, dejar CSV_SKIP <- 2.
# Si el CSV empieza directamente con timestamp,target, usar CSV_SKIP <- 0.
CSV_SKIP <- 2
CSV_DECIMAL_MARK <- "."
CSV_SEPARATOR <- ";"
CSV_DATE_FORMAT <- "%Y-%m-%d"

# Carpeta de salida. Se crea si no existe.
OUTPUT_DIR <- "output_dsa"
PLOTS_DIR <- file.path(OUTPUT_DIR, "plots")

# Identificacion del target y transformacion.
TARGET_NAME <- "euribor_3M"
TRANSFORMATION <- "level"       # opciones: "log", "log1p", "level"
MONTHLY_VALUE <- "last"        # opciones: "sum", "mean", "last"
DAILY_RESULTS_SCALE <- "original"      # "original" (level) o "transformed" (transf=<TRANSFORMATION>)
MONTHLY_RESULTS_SCALE <- "original"    # "original" (level) o "transformed" (transf=<TRANSFORMATION>)
DATA_CALENDAR <- "business"    # opciones: "natural", "business" (lunes-viernes)
MISSING_METHOD <- "previous_week_mean"  # "previous_week_mean" o "linear"
MODEL_NAME <- "DSA"

# Meses de evaluacion. El entrenamiento siempre usa todo el historico disponible.
# EVALUATION_END es el ultimo mes evaluado (m3 para la ventana final).
EVALUATION_START <- "2022-01"
EVALUATION_END <- "2025-12"

# Horizonte de cada vendimia. Se evalúan todas las vendimias disponibles.
NEXT_FULL_MONTHS <- 3

# Paralelizacion del rolling / vendimias.
# - PARALLEL_ROLLING = TRUE paraleliza los cortes dentro de cada escenario.
# - PARALLEL_CORES permite elegir cuantos cores usar. DSA inicia Java en cada
#   worker: mas workers no siempre acelera y puede agotar RAM/temperatura.
# - Para volver al comportamiento secuencial: PARALLEL_ROLLING <- FALSE o PARALLEL_CORES <- 1L.
PARALLEL_ROLLING <- FALSE
.detected_cores <- parallel::detectCores(logical = TRUE)
if (is.na(.detected_cores) || .detected_cores < 1L) .detected_cores <- 1L
# Dos workers es el limite seguro por defecto para RStudio local. Aumentarlo solo
# tras comprobar en el administrador de tareas que queda RAM suficiente.
PARALLEL_CORES <- min(2L, max(1L, .detected_cores - 1L))
rm(.detected_cores)
PARALLEL_WORKER_LOGS <- FALSE  # TRUE muestra mensajes de los workers en consola

# DSA se vuelve muy costoso al reentrenarse 177 veces con toda la historia.
# Se usa una historia movil de los ultimos 8 anos, suficiente para estacionalidad
# anual y cambios recientes, y comparable en tamano a la serie de consumo.
# Usa NULL solamente si se dispone de mucha RAM y se desea toda la historia.
MAX_TRAINING_YEARS <- 8L

# Guarda el rolling tras cada corte. Si RStudio/Windows se cierra, la proxima
# ejecucion reutiliza los cortes ya terminados en vez de empezar de cero.
RESUME_ROLLING <- TRUE
ROLLING_CHECKPOINT_FILE <- file.path(OUTPUT_DIR, "rolling_checkpoint.rds")

# Graficos.
SHOW_PLOTS <- TRUE             # TRUE muestra graficos en RStudio
SAVE_PLOTS <- TRUE             # TRUE guarda PNG en PLOTS_DIR
PLOT_WIDTH <- 11
PLOT_HEIGHT <- 7
PLOT_DPI <- 150

# Instalacion automatica de paquetes faltantes.
INSTALL_MISSING_PACKAGES <- TRUE
CRAN_REPO <- "https://cloud.r-project.org"

# -----------------------------
# 1) PAQUETES
# -----------------------------

required_packages <- c(
  "dsa", "xts", "zoo", "lubridate", "dplyr", "timeDate",
  "ggplot2", "reshape2", "openxlsx"
)

install_and_load_packages <- function(packages, install_missing = TRUE, repos = CRAN_REPO) {
  if (!nzchar(Sys.which("java"))) {
    warning(
      "No se detecta Java en PATH. Si falla la instalacion/carga de dsa o rJava, ",
      "instala un JDK y configura JAVA_HOME. En macOS/Linux puede hacer falta ejecutar ",
      "R CMD javareconf desde una terminal."
    )
  }

  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    if (!install_missing) {
      stop("Faltan paquetes: ", paste(missing, collapse = ", "))
    }
    message("Instalando paquetes faltantes: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = repos, dependencies = TRUE)
  }

  invisible(lapply(packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}

install_and_load_packages(required_packages, INSTALL_MISSING_PACKAGES, CRAN_REPO)
options(warn = 1)

# -----------------------------
# 2) FUNCIONES AUXILIARES
# -----------------------------

# Fixed rolling-cutoff definitions shared with the other evaluators.
NATURAL_CUTOFF_DAYS <- c(7L, 14L, 21L, 28L)
BUSINESS_CUTOFF_DAYS <- c(5L, 10L, 15L, 20L)

transform_level <- function(x, transformation = c("log", "log1p", "level")) {
  transformation <- match.arg(transformation)
  if (transformation == "log") return(log(x))
  if (transformation == "log1p") return(log1p(x))
  x
}

inverse_transform <- function(x, transformation = c("log", "log1p", "level")) {
  transformation <- match.arg(transformation)
  if (transformation == "log") return(exp(x))
  if (transformation == "log1p") return(expm1(x))
  x
}

parse_target_numeric <- function(x, decimal_mark = CSV_DECIMAL_MARK) {
  if (is.numeric(x)) return(x)
  y <- trimws(as.character(x))
  y <- gsub("\\s+", "", y)
  if (identical(decimal_mark, ",")) {
    y <- gsub(".", "", y, fixed = TRUE)
    y <- sub(",", ".", y, fixed = TRUE)
  }
  suppressWarnings(as.numeric(y))
}

add_calendar_covariates <- function(df, date_col = "timestamp") {
  df <- df %>%
    dplyr::mutate(
      timestamp = as.Date(.data[[date_col]]),
      dow = lubridate::wday(timestamp, week_start = 1) - 1,
      month = lubridate::month(timestamp),
      day = lubridate::day(timestamp),
      week = lubridate::isoweek(timestamp),
      is_weekend = as.integer(dow >= 5)
    )

  years <- sort(unique(lubridate::year(df$timestamp)))
  years <- years[is.finite(years)]

  if (length(years) == 0) {
    return(df)
  }

  fixed_holidays <- do.call(c, lapply(years, function(y) {
    as.Date(c(
      paste0(y, "-01-01"), paste0(y, "-01-06"), paste0(y, "-05-01"),
      paste0(y, "-05-02"), paste0(y, "-06-24"), paste0(y, "-08-15"),
      paste0(y, "-10-12"), paste0(y, "-11-01"), paste0(y, "-12-06"),
      paste0(y, "-12-08"), paste0(y, "-12-25"), paste0(y, "-12-31")
    ))
  }))

  easter_sundays <- as.Date(timeDate::Easter(years))
  easter_related <- do.call(c, lapply(easter_sundays, function(e) {
    as.Date(e + lubridate::days(-7:1))
  }))

  holidays_all <- unique(c(fixed_holidays, easter_related))

  df %>%
    dplyr::mutate(
      is_holiday = as.integer(timestamp %in% holidays_all),
      is_pre_holiday = as.integer((timestamp + lubridate::days(1)) %in% holidays_all),
      is_post_holiday = as.integer((timestamp - lubridate::days(1)) %in% holidays_all),
      is_bridge_day = as.integer(
        (is_weekend == 0 & is_holiday == 0) &
          (is_pre_holiday == 1 | is_post_holiday == 1)
      ),
      is_easter_week = as.integer(timestamp %in% easter_related)
    )
}

DATE_COLUMN <- "timestamp"
LEVEL_TARGET_COLUMN <- "target"
TARGET_COLUMN <- "target_proc"

date_sequence_by_week_days <- function(start_date, end_date, week_days = 7) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
    return(as.Date(character()))
  }
  idx <- seq(start_date, end_date, by = "1 day")
  if (week_days == 5) {
    idx <- idx[as.integer(format(idx, "%u")) <= 5]
  } else if (week_days != 7) {
    stop("week_days debe ser 5 o 7.")
  }
  as.Date(idx)
}

range_has_leap_day <- function(a, b) {
  a <- as.Date(a)
  b <- as.Date(b)
  if (is.na(a) || is.na(b) || a > b) return(FALSE)
  d <- seq(a, b, by = "1 day")
  any(format(d, "%m-%d") == "02-29")
}

nth_business_day_of_month <- function(month_start, n) {
  month_start <- as.Date(month_start)
  days <- seq(
    lubridate::floor_date(month_start, "month"),
    lubridate::ceiling_date(month_start, "month") - lubridate::days(1),
    by = "day"
  )
  bdays <- days[as.integer(format(days, "%u")) <= 5]
  if (n > length(bdays)) return(as.Date(NA))
  bdays[n]
}

aggregate_level <- function(x, agg = "sum", empty_value = NA_real_) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(empty_value)
  if (agg == "sum") return(sum(x))
  if (agg == "mean") return(mean(x))
  if (agg == "last") return(tail(x, 1))
  stop("agg debe ser 'sum', 'mean' o 'last'.")
}

fill_weekends <- function(df, date_col, level_col, mode = c("mean", "interp")) {
  mode <- match.arg(mode)
  df <- df %>% dplyr::arrange(.data[[date_col]])
  dts <- as.Date(df[[date_col]])
  wd <- as.integer(format(dts, "%u"))
  is_wknd <- wd >= 6
  lvl <- as.numeric(df[[level_col]])

  if (mode == "interp") {
    lvl_na <- lvl
    lvl_na[is_wknd] <- NA_real_
    lvl_filled <- zoo::na.approx(lvl_na, x = as.numeric(dts), na.rm = FALSE)
  } else {
    bus_idx <- which(!is_wknd)
    lvl_filled <- lvl
    for (i in which(is_wknd)) {
      prev_bus <- bus_idx[bus_idx < i]
      if (length(prev_bus) >= 1) {
        lvl_filled[i] <- mean(lvl[tail(prev_bus, 5)], na.rm = TRUE)
      } else {
        nxt_bus <- bus_idx[bus_idx > i]
        lvl_filled[i] <- if (length(nxt_bus)) {
          mean(lvl[head(nxt_bus, 5)], na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    }
  }

  lvl_filled <- zoo::na.approx(lvl_filled, x = as.numeric(dts), na.rm = FALSE)
  lvl_filled <- zoo::na.locf(lvl_filled, na.rm = FALSE)
  lvl_filled <- zoo::na.locf(lvl_filled, fromLast = TRUE, na.rm = FALSE)
  df[[level_col]] <- lvl_filled
  df
}

complete_input_calendar <- function(df, data_calendar = DATA_CALENDAR,
                                    missing_method = MISSING_METHOD) {
  df <- df %>% dplyr::arrange(timestamp)
  expected_dates <- seq(min(df$timestamp), max(df$timestamp), by = "day")
  if (data_calendar == "business") {
    expected_dates <- expected_dates[as.integer(format(expected_dates, "%u")) <= 5]
    df <- df[as.integer(format(df$timestamp, "%u")) <= 5, , drop = FALSE]
  } else if (data_calendar != "natural") {
    stop("DATA_CALENDAR debe ser 'natural' o 'business'.")
  }
  completed <- merge(data.frame(timestamp = expected_dates), df, by = "timestamp", all.x = TRUE)
  completed <- completed[order(completed$timestamp), , drop = FALSE]
  values <- as.numeric(completed$target)
  period <- if (data_calendar == "business") 5L else 7L
  if (missing_method == "previous_week_mean") {
    for (i in seq_along(values)) {
      if (!is.finite(values[i])) {
        previous <- values[seq.int(max(1L, i - period), i - 1L)]
        previous <- previous[is.finite(previous)]
        if (!length(previous)) stop("No se puede imputar el hueco de ", completed$timestamp[i])
        values[i] <- mean(previous)
      }
    }
  } else if (missing_method == "linear") {
    values <- zoo::na.approx(values, x = as.numeric(completed$timestamp), na.rm = FALSE)
  } else {
    stop("MISSING_METHOD debe ser 'previous_week_mean' o 'linear'.")
  }
  if (any(!is.finite(values))) stop("Quedan huecos del target sin imputar.")
  completed$target <- values
  completed
}

build_aggregate_eval <- function(daily_eval_df, context_df,
                                 date_col = DATE_COLUMN,
                                 level_target_col = LEVEL_TARGET_COLUMN,
                                 unit = c("week", "month"),
                                 week_days = 7,
                                 aggregate_fun = MONTHLY_VALUE,
                                 transformation = TRANSFORMATION) {
  unit <- match.arg(unit)
  if (is.null(daily_eval_df) || nrow(daily_eval_df) == 0) return(data.frame())

  context_levels <- context_df %>%
    dplyr::transmute(
      .fecha = as.Date(.data[[date_col]]),
      .level = as.numeric(.data[[level_target_col]])
    ) %>%
    dplyr::filter(!is.na(.fecha))

  if (week_days == 5) {
    context_levels <- context_levels %>%
      dplyr::filter(as.integer(format(.fecha, "%u")) <= 5)
  }

  daily_aug <- daily_eval_df %>%
    dplyr::mutate(
      fecha = as.Date(fecha),
      cutoff_date = as.Date(cutoff_date)
    )

  if (!("id_ventana" %in% colnames(daily_aug))) {
    daily_aug <- daily_aug %>%
      dplyr::mutate(id_ventana = paste("Corte", cutoff_date))
  }

  if (unit == "week") {
    daily_aug <- daily_aug %>%
      dplyr::mutate(
        period_start = lubridate::floor_date(fecha, "week", week_start = 1),
        period_end = period_start + lubridate::days(6)
      )
  } else {
    daily_aug <- daily_aug %>%
      dplyr::mutate(
        period_start = lubridate::floor_date(fecha, "month"),
        period_end = lubridate::ceiling_date(period_start, "month") - lubridate::days(1)
      )
  }

  grouped <- daily_aug %>%
    dplyr::group_by(id_ventana, cutoff_date, cutoff_day, week_no, period_start, period_end) %>%
    dplyr::group_split()

  rows <- lapply(grouped, function(g) {
    g <- g %>% dplyr::arrange(fecha)
    cutoff_date <- as.Date(g$cutoff_date[1])
    period_start <- as.Date(g$period_start[1])
    period_end <- as.Date(g$period_end[1])
    period_dates <- date_sequence_by_week_days(period_start, period_end, week_days)

    observed_full <- context_levels %>% dplyr::filter(.fecha %in% period_dates)
    observed_partial <- observed_full %>% dplyr::filter(.fecha <= cutoff_date)

    available_dates <- unique(observed_full$.fecha[is.finite(observed_full$.level)])
    has_full_observed_period <- length(period_dates) > 0 && all(period_dates %in% available_dates)

    forecast_future_values <- as.numeric(g$yhat_level)
    observed_partial_values <- as.numeric(observed_partial$.level)

    real_level <- if (has_full_observed_period) {
      aggregate_level(observed_full$.level, aggregate_fun)
    } else {
      NA_real_
    }
    pred_level <- aggregate_level(
      c(observed_partial_values, forecast_future_values),
      aggregate_fun,
      empty_value = NA_real_
    )

    real_proc <- transform_level(real_level, transformation)
    pred_proc <- transform_level(pred_level, transformation)
    proc_error <- pred_proc - real_proc
    error_pp_level <- if (is.finite(real_level) && real_level != 0 && is.finite(pred_level)) {
      100 * (pred_level - real_level) / real_level
    } else NA_real_
    error_pp_proc <- if (is.finite(real_proc) && real_proc != 0 && is.finite(pred_proc)) {
      100 * (pred_proc - real_proc) / real_proc
    } else NA_real_
    error_pp <- if (MONTHLY_RESULTS_SCALE == "original") error_pp_level else error_pp_proc

    period_offset <- if (unit == "month") {
      (lubridate::year(period_start) - lubridate::year(cutoff_date)) * 12 +
        lubridate::month(period_start) - lubridate::month(cutoff_date)
    } else {
      as.integer(as.numeric(
        period_start - lubridate::floor_date(cutoff_date, "week", week_start = 1)
      ) / 7)
    }

    data.frame(
      id_ventana = g$id_ventana[1],
      cutoff_date = cutoff_date,
      cutoff_day = g$cutoff_day[1],
      week_no = g$week_no[1],
      unit = unit,
      period_start = period_start,
      period_end = period_end,
      period_offset = period_offset,
      observed_partial_level = aggregate_level(observed_partial_values, aggregate_fun, empty_value = 0),
      forecast_future_level = aggregate_level(forecast_future_values, aggregate_fun, empty_value = NA_real_),
      real_level = real_level,
      pred_level = pred_level,
      real_proc = real_proc,
      pred_proc = pred_proc,
      proc_error = proc_error,
      error_pp_level = error_pp_level,
      error_pp_proc = error_pp_proc,
      error_pp = error_pp,
      abs_error_pp = abs(error_pp),
      observed_partial_n = sum(is.finite(observed_partial_values)),
      forecast_n = sum(is.finite(forecast_future_values)),
      observed_full_n = sum(is.finite(observed_full$.level)),
      expected_n = length(period_dates),
      has_full_observed_period = has_full_observed_period,
      has_leap_day = range_has_leap_day(period_start, period_end)
    )
  })

  do.call(rbind, rows) %>% dplyr::arrange(cutoff_date, period_start)
}

forecast_end_from_cutoff <- function(cutoff_date, next_full_months) {
  month_start <- lubridate::floor_date(as.Date(cutoff_date), "month")
  next_boundary <- seq(
    from = month_start,
    by = "month",
    length.out = as.integer(next_full_months) + 2L
  )[as.integer(next_full_months) + 2L]
  as.Date(next_boundary - 1L)
}

# -----------------------------
# 3) MOTOR DE PREDICCION
# -----------------------------

run_growing_daily_forecast <- function(context_df, target_col,
                                       date_col = "timestamp",
                                       evaluation_start,
                                       evaluation_end,
                                       cutoff_days,
                                       next_full_months = 3,
                                       week_days = 7,
                                       weekend_fill = "mean",
                                       level_target_col = "target",
                                       aggregate_fun = "sum",
                                       transformation = "log",
                                       parallel_rolling = PARALLEL_ROLLING,
                                       parallel_cores = PARALLEL_CORES,
                                       parallel_worker_logs = PARALLEL_WORKER_LOGS,
                                       max_training_years = MAX_TRAINING_YEARS,
                                       checkpoint_file = ROLLING_CHECKPOINT_FILE,
                                       resume_rolling = RESUME_ROLLING,
                                       explicit_cutoff_date = NULL,
                                       forecast_days = NULL) {
  context_df[[date_col]] <- as.Date(context_df[[date_col]])
  evaluation_start <- as.Date(paste0(evaluation_start, "-01"))
  evaluation_end <- as.Date(paste0(evaluation_end, "-01"))
  # Use base R month arithmetic for compatibility with older lubridate releases.
  last_cutoff_month <- seq(evaluation_end, by = paste0("-", next_full_months, " months"), length.out = 2)[2]
  if (last_cutoff_month < evaluation_start) {
    stop("EVALUATION_END debe permitir todos los meses de prediccion hasta m3.")
  }

  if (!(target_col %in% colnames(context_df))) stop("No existe target_col: ", target_col)
  if (!(level_target_col %in% colnames(context_df))) stop("No existe level_target_col: ", level_target_col)

  keep_business <- (week_days == 5)

  context_fit <- context_df %>% dplyr::arrange(.data[[date_col]])
  context_fit[[date_col]] <- as.Date(context_fit[[date_col]])

  if (week_days == 5) {
    full_dates <- seq(min(context_fit[[date_col]]), max(context_fit[[date_col]]), by = "day")
    base_df <- stats::setNames(data.frame(full_dates), date_col)
    context_fit <- dplyr::left_join(
      base_df,
      context_fit[, c(date_col, level_target_col)],
      by = date_col
    )
    context_fit <- fill_weekends(context_fit, date_col, level_target_col, weekend_fill)
    context_fit[[target_col]] <- transform_level(context_fit[[level_target_col]], transformation)
  }

  context_fit <- add_calendar_covariates(context_fit, date_col = date_col)
  context_eval <- context_df %>% dplyr::arrange(.data[[date_col]])

  max_obs_date <- max(context_fit[[date_col]], na.rm = TRUE)
  last_evaluation_cutoff <- if (week_days == 5) {
    nth_business_day_of_month(last_cutoff_month, cutoff_days[1])
  } else {
    last_cutoff_month + (cutoff_days[1] - 1)
  }
  effective_last_cutoff <- min(last_evaluation_cutoff, max_obs_date)

  calendar_regressors <- setdiff(
    colnames(context_fit),
    unique(c(date_col, target_col, level_target_col))
  )
  calendar_regressors <- calendar_regressors[
    vapply(context_fit[, calendar_regressors, drop = FALSE], is.numeric, logical(1))
  ]

  sequence_months <- seq(
    evaluation_start,
    last_cutoff_month,
    by = "1 month"
  )

  cutoff_dates <- as.Date(character())
  cutoff_nominal <- integer()
  cutoff_weekno <- integer()

  for (m in sequence_months) {
    m_date <- as.Date(m)
    for (j in seq_along(cutoff_days)) {
      d <- cutoff_days[j]
      c_date <- if (week_days == 5) {
        nth_business_day_of_month(m_date, d)
      } else {
        m_date + lubridate::days(d - 1)
      }
      if (!is.na(c_date) && c_date >= evaluation_start && c_date <= effective_last_cutoff) {
        cutoff_dates <- c(cutoff_dates, c_date)
        cutoff_nominal <- c(cutoff_nominal, d)
        cutoff_weekno <- c(cutoff_weekno, j)
      }
    }
  }

  ord <- order(cutoff_dates)
  cutoff_dates <- cutoff_dates[ord]
  cutoff_nominal <- cutoff_nominal[ord]
  cutoff_weekno <- cutoff_weekno[ord]

  if (!is.null(explicit_cutoff_date)) {
    requested_cutoff <- as.Date(explicit_cutoff_date)
    available <- context_fit[[date_col]][context_fit[[date_col]] <= requested_cutoff]
    if (length(available) == 0) stop("No hay historia antes del corte de produccion.")
    cutoff_dates <- max(as.Date(available))
    cutoff_nominal <- NA_integer_
    cutoff_weekno <- NA_integer_
  }

  n_cutoffs <- length(cutoff_dates)

  if (n_cutoffs == 0) {
    daily_eval_df <- data.frame()
    cutoff_summary_df <- data.frame()
    return(list(
      daily_eval = daily_eval_df,
      weekly_eval = data.frame(),
      monthly_eval = data.frame(),
      cutoff_summary = cutoff_summary_df,
      cutoff_dates = cutoff_dates
    ))
  }

  requested_cores <- suppressWarnings(as.integer(parallel_cores[1]))
  if (is.na(requested_cores) || requested_cores < 1L) requested_cores <- 1L

  available_cores <- parallel::detectCores(logical = TRUE)
  if (is.na(available_cores) || available_cores < 1L) available_cores <- 1L

  effective_cores <- min(requested_cores, available_cores, n_cutoffs)
  use_parallel <- isTRUE(parallel_rolling) && effective_cores > 1L

  # Copias locales escalares para el rolling, especialmente importantes en modo PSOCK
  # en Windows/RStudio. Asi los workers no tienen que buscar objetos globales como
  # NEXT_FULL_MONTHS, TRANSFORMATION o MONTHLY_VALUE en su propia sesion.
  .next_full_months <- suppressWarnings(as.integer(next_full_months[1]))
  if (is.na(.next_full_months) || .next_full_months < 0L) {
    stop("next_full_months debe ser un entero >= 0.")
  }

  .week_days <- suppressWarnings(as.integer(week_days[1]))
  if (is.na(.week_days) || !(.week_days %in% c(5L, 7L))) {
    stop("week_days debe ser 5 o 7.")
  }

  .weekend_fill <- if (length(weekend_fill) == 0 || is.na(weekend_fill[1])) {
    NA_character_
  } else {
    as.character(weekend_fill[1])
  }
  .aggregate_fun <- as.character(aggregate_fun[1])
  .transformation <- as.character(transformation[1])
  .forecast_days <- if (is.null(forecast_days)) NULL else as.integer(forecast_days[1])
  if (!is.null(.forecast_days) && (is.na(.forecast_days) || .forecast_days < 1L)) {
    stop("forecast_days debe ser un entero positivo.")
  }

  .max_training_years <- if (is.null(max_training_years)) NA_integer_ else {
    suppressWarnings(as.integer(max_training_years[1]))
  }
  if (!is.na(.max_training_years) && .max_training_years < 1L) {
    stop("MAX_TRAINING_YEARS debe ser NULL o un entero positivo.")
  }
  forecast_key <- if (is.null(.forecast_days)) "monthly" else .forecast_days
  # El checkpoint solo es reutilizable si corresponde exactamente a esta
  # combinacion de cortes, historia y configuracion de entrenamiento.
  checkpoint_key <- paste(
    "dsa-rolling-v2", min(context_fit[[date_col]]), max(context_fit[[date_col]]),
    paste(cutoff_dates, collapse = ","), .max_training_years,
    .next_full_months, .week_days, forecast_key, .transformation,
    format(sum(as.numeric(context_fit[[level_target_col]]), na.rm = TRUE), digits = 16),
    sep = "|"
  )

  message(
    "Vendimias/cortes a ejecutar: ", n_cutoffs,
    " | modo: ", if (use_parallel) paste0("paralelo (", effective_cores, " cores)") else "secuencial"
  )

  # Referencias locales para que los workers PSOCK las serialicen correctamente.
  .fn_date_sequence_by_week_days <- date_sequence_by_week_days
  .fn_add_calendar_covariates <- add_calendar_covariates
  .fn_inverse_transform <- inverse_transform
  .fn_forecast_end_from_cutoff <- forecast_end_from_cutoff

  run_one_cutoff <- function(i) {
    cutoff_date <- cutoff_dates[i]
    nominal_d <- cutoff_nominal[i]
    week_no <- cutoff_weekno[i]

    message("Ejecutando corte ", i, "/", n_cutoffs, ": ", cutoff_date)

    forecast_end <- if (is.null(.forecast_days)) {
      .fn_forecast_end_from_cutoff(cutoff_date, .next_full_months)
    } else {
      cutoff_date + lubridate::days(.forecast_days)
    }

    horizon_index <- .fn_date_sequence_by_week_days(cutoff_date + lubridate::days(1), forecast_end, 7)
    if (length(horizon_index) == 0) return(NULL)

    train_keep <- context_fit[[date_col]] <= cutoff_date
    if (!is.na(.max_training_years)) {
      # Mantiene observaciones desde la misma fecha de hace N anos. Al trabajar
      # con calendario business, context_fit incluye fines de semana imputados;
      # sin este limite cada corte acababa ajustando toda la serie desde 2007.
      train_start_limit <- cutoff_date %m-% lubridate::years(.max_training_years)
      train_keep <- train_keep & context_fit[[date_col]] >= train_start_limit
    }
    train_df <- context_fit[train_keep, , drop = FALSE]
    if (nrow(train_df) == 0) return(NULL)

    future_df <- .fn_add_calendar_covariates(
      stats::setNames(data.frame(horizon_index), date_col),
      date_col = date_col
    )

    y_train_xts <- xts::xts(as.numeric(train_df[[target_col]]), order.by = train_df[[date_col]])

    reg_train_xts <- xts::xts(train_df[, calendar_regressors, drop = FALSE], order.by = train_df[[date_col]])
    reg_test_xts <- xts::xts(future_df[, calendar_regressors, drop = FALSE], order.by = horizon_index)

    reg_train <- dsa::multi_xts2ts(reg_train_xts)

    valid_cols <- apply(reg_train, 2, stats::var, na.rm = TRUE) > 0
    if (!any(valid_cols)) {
      stop("No hay regresores validos con varianza positiva para el corte ", cutoff_date)
    }

    reg_train <- reg_train[, valid_cols, drop = FALSE]
    freq_train <- stats::frequency(reg_train)
    # multi_xts2ts() removes 29 February for short series. That made a future
    # horizon crossing a leap day one observation shorter than h. Build the
    # future regressors directly so they retain every requested forecast date.
    reg_test <- stats::ts(
      as.matrix(reg_test_xts[, colnames(reg_train), drop = FALSE]),
      start = stats::end(reg_train)[1] + 1 / freq_train,
      frequency = freq_train
    )

    fit <- dsa::dsa(
      y_train_xts,
      regressor = reg_train,
      forecast_regressor = reg_test,
      h = length(horizon_index),
      automodel = "reduced",
      ic = "bic",
      include.constant = TRUE,
      progress_bar = FALSE
    )

    full_forecast <- dsa::get_original(fit, forecast = TRUE)
    pred_all <- as.numeric(tail(full_forecast, length(horizon_index)))

    df_all <- data.frame(fecha = horizon_index, yhat = pred_all)
    if (keep_business) {
      df_all <- df_all[as.integer(format(df_all$fecha, "%u")) <= 5, , drop = FALSE]
    }
    if (nrow(df_all) == 0) return(NULL)

    pred_proc_values <- df_all$yhat
    fcst_dates <- df_all$fecha
    pred_level_values <- .fn_inverse_transform(pred_proc_values, .transformation)

    test_actuals <- context_eval[, c(date_col, target_col, level_target_col), drop = FALSE]
    names(test_actuals) <- c("fecha", "y_true_proc", "y_true_level")

    fcst_daily <- data.frame(
      fecha = fcst_dates,
      yhat_proc = pred_proc_values,
      yhat_level = pred_level_values,
      cutoff_date = cutoff_date,
      cutoff_day = nominal_d,
      week_no = week_no,
      train_start = min(train_df[[date_col]]),
      train_end = max(train_df[[date_col]]),
      forecast_start = min(fcst_dates),
      forecast_end = max(fcst_dates),
      step = seq_along(fcst_dates),
      id_ventana = paste("Corte", cutoff_date)
    )

    fcst_daily <- dplyr::left_join(fcst_daily, test_actuals, by = "fecha")
    fcst_daily <- dplyr::mutate(
      fcst_daily,
      yhat = yhat_level,
      y_true = y_true_level,
      daily_proc_error = yhat_proc - y_true_proc,
      daily_level_error = yhat_level - y_true_level,
      daily_error_pct = 100 * daily_level_error / dplyr::na_if(y_true_level, 0)
    )
    fcst_daily <- dplyr::arrange(fcst_daily, fecha)
    scored <- fcst_daily[is.finite(fcst_daily$y_true_level), , drop = FALSE]
    daily_mae <- if (nrow(scored)) mean(abs(scored$daily_level_error)) else NA_real_
    daily_rmse <- if (nrow(scored)) sqrt(mean(scored$daily_level_error^2)) else NA_real_
    daily_mape <- if (nrow(scored)) mean(abs(scored$daily_error_pct), na.rm = TRUE) else NA_real_

    cutoff_row <- data.frame(
      cutoff_date = cutoff_date,
      cutoff_day = nominal_d,
      week_no = week_no,
      train_start = min(train_df[[date_col]]),
      train_end = max(train_df[[date_col]]),
      forecast_start = min(fcst_dates),
      forecast_end = max(fcst_dates),
      n_steps = length(fcst_dates),
      prediction_length = length(fcst_dates),
      n_observed_steps = nrow(scored),
      MAE = daily_mae,
      RMSE = daily_rmse,
      MAPE_pct = daily_mape,
      covariates = paste(calendar_regressors, collapse = ", "),
      train_rows = nrow(train_df),
      week_days = .week_days,
      weekend_fill = if (.week_days == 5L) .weekend_fill else NA_character_,
      reg_train_freq = stats::frequency(reg_train),
      reg_test_freq = stats::frequency(reg_test)
    )

    list(daily = fcst_daily, cutoff = cutoff_row)
  }

  safe_run_one_cutoff <- function(i) {
    tryCatch(
      run_one_cutoff(i),
      error = function(e) {
        list(
          .error = TRUE,
          i = i,
          cutoff_date = as.character(cutoff_dates[i]),
          message = conditionMessage(e)
        )
      }
    )
  }

  # En PSOCK el campo `tag` de recvOneResult() es un detalle interno del
  # planificador. Devolvemos tambien el indice desde el worker para que un tag
  # inesperado no pueda corromper ni detener la recoleccion de resultados.
  run_tagged_cutoff <- function(i) {
    list(i = as.integer(i)[1], value = safe_run_one_cutoff(i))
  }

  task_results <- vector("list", n_cutoffs)
  if (isTRUE(resume_rolling) && !is.null(checkpoint_file) && file.exists(checkpoint_file)) {
    saved_checkpoint <- tryCatch(readRDS(checkpoint_file), error = function(e) NULL)
    if (is.list(saved_checkpoint) && identical(saved_checkpoint$key, checkpoint_key) &&
        length(saved_checkpoint$results) == n_cutoffs) {
      task_results <- saved_checkpoint$results
      message("Checkpoint recuperado: ", sum(!vapply(task_results, is.null, logical(1))),
              "/", n_cutoffs, " cortes ya completados.")
    } else {
      message("Se ignora un checkpoint que no corresponde a esta ejecucion.")
    }
  }
  save_checkpoint <- function() {
    if (is.null(checkpoint_file) || !nzchar(checkpoint_file)) return(invisible(NULL))
    dir.create(dirname(checkpoint_file), recursive = TRUE, showWarnings = FALSE)
    # Un fichero temporal evita dejar un RDS corrupto si se interrumpe al guardar.
    tmp_file <- paste0(checkpoint_file, ".tmp")
    saveRDS(list(key = checkpoint_key, results = task_results), tmp_file)
    file.copy(tmp_file, checkpoint_file, overwrite = TRUE)
    unlink(tmp_file)
    invisible(NULL)
  }
  pending_indices <- which(vapply(task_results, is.null, logical(1)))
  if (!length(pending_indices)) message("Rolling completo recuperado del checkpoint.")

  if (use_parallel && length(pending_indices) > 0) {
    cluster_args <- list(spec = effective_cores, type = "PSOCK")
    if (isTRUE(parallel_worker_logs)) {
      cluster_args$outfile <- ""
    }

    cl <- do.call(parallel::makeCluster, cluster_args)
    on.exit({
      if (!is.null(cl)) {
        try(parallel::stopCluster(cl), silent = TRUE)
      }
    }, add = TRUE)

    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(dsa)
        library(xts)
        library(zoo)
        library(lubridate)
        library(dplyr)
        library(timeDate)
      })
      NULL
    })

    # Export explicito de los objetos que usa cada worker. Esto evita errores tipo
    # "objeto 'NEXT_FULL_MONTHS' no encontrado" cuando RStudio/Windows lanza
    # procesos PSOCK con una sesion limpia.
    parallel::clusterExport(
      cl,
      varlist = c(
        "safe_run_one_cutoff", "run_tagged_cutoff", "run_one_cutoff",
        "cutoff_dates", "cutoff_nominal", "cutoff_weekno", "n_cutoffs",
        "context_fit", "context_eval", "calendar_regressors",
        "target_col", "date_col", "level_target_col",
        "keep_business", ".next_full_months", ".week_days", ".weekend_fill", ".forecast_days",
        ".aggregate_fun", ".transformation", ".max_training_years",
        ".fn_date_sequence_by_week_days", ".fn_add_calendar_covariates",
        ".fn_inverse_transform", ".fn_forecast_end_from_cutoff"
      ),
      envir = environment()
    )

    indices <- pending_indices
    n_workers <- length(cl)
    initial_jobs <- min(n_workers, length(indices))
    next_job <- initial_jobs + 1L
    completed_jobs <- 0L

    for (k in seq_len(initial_jobs)) {
      parallel:::sendCall(cl[[k]], run_tagged_cutoff, list(indices[k]), tag = indices[k])
    }

    while (completed_jobs < length(indices)) {
      result <- parallel:::recvOneResult(cl)
      job_i <- result$value$i
      if (length(job_i) != 1L || is.na(job_i) || job_i < 1L || job_i > n_cutoffs) {
        stop("El worker devolvio un indice de corte invalido.")
      }
      task_results[[job_i]] <- result$value$value
      completed_jobs <- completed_jobs + 1L
      save_checkpoint()

      message(
        "Progreso rolling: ",
        sum(!vapply(task_results, is.null, logical(1))), "/", n_cutoffs,
        " cortes completados (ultimo: ", cutoff_dates[job_i], ")"
      )

      if (next_job <= length(indices)) {
        parallel:::sendCall(
          cl[[result$node]],
          run_tagged_cutoff,
          list(indices[next_job]),
          tag = indices[next_job]
        )
        next_job <- next_job + 1L
      }
    }

    parallel::stopCluster(cl)
    cl <- NULL
  } else if (length(pending_indices) > 0) {
    for (i in pending_indices) {
      task_results[[i]] <- safe_run_one_cutoff(i)
      save_checkpoint()
      message("Progreso rolling: ", sum(!vapply(task_results, is.null, logical(1))),
              "/", n_cutoffs, " cortes completados (ultimo: ", cutoff_dates[i], ")")
    }
  }

  has_error <- vapply(task_results, function(x) is.list(x) && isTRUE(x$.error), logical(1))
  if (any(has_error)) {
    error_msgs <- vapply(task_results[has_error], function(x) {
      paste0("corte ", x$i, " (", x$cutoff_date, "): ", x$message)
    }, character(1))
    stop(
      "Errores durante el rolling:\n",
      paste(error_msgs, collapse = "\n")
    )
  }

  task_results <- Filter(Negate(is.null), task_results)

  daily_rows <- lapply(task_results, function(x) x[["daily"]])
  cutoff_rows <- lapply(task_results, function(x) x[["cutoff"]])

  daily_eval_df <- if (length(daily_rows) > 0) do.call(rbind, daily_rows) else data.frame()
  cutoff_summary_df <- if (length(cutoff_rows) > 0) do.call(rbind, cutoff_rows) else data.frame()

  if (nrow(daily_eval_df) > 0) {
    daily_eval_df <- daily_eval_df[order(daily_eval_df$cutoff_date, daily_eval_df$fecha), , drop = FALSE]
  }
  if (nrow(cutoff_summary_df) > 0) {
    cutoff_summary_df <- cutoff_summary_df[order(cutoff_summary_df$cutoff_date), , drop = FALSE]
  }

  weekly_eval_df <- build_aggregate_eval(
    daily_eval_df, context_eval, date_col, level_target_col, "week",
    week_days, aggregate_fun, transformation
  )
  monthly_eval_df <- build_aggregate_eval(
    daily_eval_df, context_eval, date_col, level_target_col, "month",
    week_days, aggregate_fun, transformation
  )

  list(
    daily_eval = daily_eval_df,
    weekly_eval = weekly_eval_df,
    monthly_eval = monthly_eval_df,
    cutoff_summary = cutoff_summary_df,
    cutoff_dates = cutoff_dates
  )
}

# -----------------------------
# 4) METRICAS, GRAFICOS Y EXPORTACION
# -----------------------------

disaggregated_scores <- function(eval_df, by_offset = TRUE) {
  d <- eval_df %>%
    dplyr::filter(
      has_full_observed_period,
      is.finite(real_level),
      is.finite(pred_level),
      is.finite(error_pp)
    )

  if (nrow(d) == 0) return(data.frame())

  grp <- if (by_offset) c("week_no", "cutoff_day", "period_offset") else c("week_no", "cutoff_day")

  out <- d %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(
      n_periodos = dplyr::n(),
      ME_pp = mean(error_pp),
      MAE_pp = mean(abs_error_pp),
      MedAE_pp = stats::median(abs_error_pp),
      RMSE_pp = sqrt(mean(error_pp^2)),
      results_scale = results_scale_label(MONTHLY_RESULTS_SCALE),
      .groups = "drop"
    )

  if ("period_offset" %in% colnames(out)) {
    out <- out %>% dplyr::arrange(week_no, period_offset)
  } else {
    out <- out %>% dplyr::arrange(week_no)
  }
  out
}

validate_results_scale <- function(scale, setting_name) {
  if (!(scale %in% c("original", "transformed"))) {
    stop(setting_name, " debe ser 'original' o 'transformed'.")
  }
}

results_scale_label <- function(scale, transformation = TRANSFORMATION) {
  validate_results_scale(scale, "Escala de resultados")
  if (scale == "original") "level" else paste0("transf=", transformation)
}

daily_results_columns <- function(scale = DAILY_RESULTS_SCALE) {
  validate_results_scale(scale, "DAILY_RESULTS_SCALE")
  if (scale == "original") c("y_true_level", "yhat_level") else c("y_true_proc", "yhat_proc")
}

monthly_results_columns <- function(scale = MONTHLY_RESULTS_SCALE) {
  validate_results_scale(scale, "MONTHLY_RESULTS_SCALE")
  if (scale == "original") c("real_level", "pred_level") else c("real_proc", "pred_proc")
}

score_monthly <- function(res, next_full_months = NEXT_FULL_MONTHS) {
  res$monthly_eval %>%
    dplyr::filter(
      has_full_observed_period,
      is.finite(real_level),
      is.finite(pred_level),
      is.finite(error_pp),
      period_offset >= 0,
      period_offset <= next_full_months
    ) %>%
    dplyr::group_by(cutoff_week = week_no, pred_month = period_offset) %>%
    dplyr::summarise(
      n_periods = dplyr::n(),
      ME_pp = mean(error_pp),
      MAE_pp = mean(abs_error_pp),
      MedAE_pp = stats::median(abs_error_pp),
      RMSE_pp = sqrt(mean(error_pp^2)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(cutoff_week, pred_month)
}

plot_daily <- function(res, titulo, transformation = TRANSFORMATION) {
  if (is.null(res$daily_eval) || nrow(res$daily_eval) == 0) return(NULL)
  columns <- daily_results_columns()
  scale_label <- results_scale_label(DAILY_RESULTS_SCALE, transformation)
  d <- res$daily_eval %>%
    dplyr::mutate(fecha = as.Date(fecha), real_plot = .data[[columns[1]]], pred_plot = .data[[columns[2]]])

  ggplot2::ggplot(d, ggplot2::aes(fecha)) +
    ggplot2::geom_line(ggplot2::aes(y = real_plot, color = "Real"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = pred_plot, color = "Forecast"), linetype = "dashed", linewidth = 0.7) +
    ggplot2::facet_wrap(~ id_ventana, scales = "free", ncol = 2) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::scale_color_manual(values = c("Real" = "black", "Forecast" = "royalblue")) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0("Prediccion diaria por ventana de corte (", scale_label, ")"),
      x = "Fecha",
      y = paste0("Serie (", scale_label, ")"),
      color = "Serie"
    ) +
    ggplot2::theme(legend.position = "bottom")
}

plot_monthly <- function(res, titulo, transformation = TRANSFORMATION) {
  if (is.null(res$monthly_eval) || nrow(res$monthly_eval) == 0) return(NULL)
  columns <- monthly_results_columns()
  scale_label <- results_scale_label(MONTHLY_RESULTS_SCALE, transformation)
  d <- res$monthly_eval %>%
    dplyr::filter(has_full_observed_period, is.finite(.data[[columns[1]]]), is.finite(.data[[columns[2]]])) %>%
    dplyr::mutate(Escenario = paste0("Semana ", week_no, " (corte ", cutoff_day, ")"),
                  real_plot = .data[[columns[1]]], pred_plot = .data[[columns[2]]])

  if (nrow(d) == 0) return(NULL)

  ggplot2::ggplot(d, ggplot2::aes(period_start)) +
    ggplot2::geom_line(ggplot2::aes(y = real_plot, color = "Real"), linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(y = real_plot, color = "Real")) +
    ggplot2::geom_line(ggplot2::aes(y = pred_plot, color = "Forecast"), linetype = "dashed", linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(y = pred_plot, color = "Forecast")) +
    ggplot2::facet_wrap(~ Escenario, scales = "free_x") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::scale_color_manual(values = c("Real" = "black", "Forecast" = "royalblue")) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0("Valor mensual real vs forecast (", MONTHLY_VALUE, "; ", scale_label, ")"),
      x = "Mes",
      y = paste0("Agregado mensual (", scale_label, ")"),
      color = "Serie"
    ) +
    ggplot2::theme(legend.position = "bottom")
}

plot_monthly_rmse_grid <- function(res, titulo = "RMSE mensual por semana de corte y mes predicho") {
  statistics <- score_monthly(res)
  if (is.null(statistics) || nrow(statistics) == 0) return(NULL)

  plot_data <- expand.grid(cutoff_week = 1:4, pred_month = 0:3) %>%
    dplyr::left_join(statistics, by = c("cutoff_week", "pred_month")) %>%
    dplyr::mutate(
      cutoff_week_label = factor(paste0("w", cutoff_week), levels = paste0("w", 1:4)),
      pred_month_label = factor(paste0("m", pred_month), levels = paste0("m", 0:3))
    )

  ggplot2::ggplot(plot_data, ggplot2::aes(cutoff_week_label, RMSE_pp)) +
    ggplot2::geom_col(fill = "#7570b3", na.rm = TRUE) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(is.finite(RMSE_pp), sprintf("%.2f", RMSE_pp), "")),
      vjust = -0.35, size = 3, na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~ pred_month_label, ncol = 2) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0("Metrica calculada sobre MONTHLY_VALUE = '", MONTHLY_VALUE, "' (", results_scale_label(MONTHLY_RESULTS_SCALE), ")"),
      x = "Semana de corte", y = paste0("RMSE (pp; ", results_scale_label(MONTHLY_RESULTS_SCALE), ")")
    ) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
}

plot_monthly_actual_vs_predicted <- function(res, titulo = "Valor mensual real vs predicho") {
  if (is.null(res$monthly_eval) || nrow(res$monthly_eval) == 0) return(NULL)
  columns <- monthly_results_columns()
  scale_label <- results_scale_label(MONTHLY_RESULTS_SCALE)
  d <- res$monthly_eval %>%
    dplyr::filter(
      has_full_observed_period,
      is.finite(.data[[columns[1]]]), is.finite(.data[[columns[2]]]),
      period_offset >= 0, period_offset <= NEXT_FULL_MONTHS
    ) %>% dplyr::mutate(real_plot = .data[[columns[1]]], pred_plot = .data[[columns[2]]]) %>%
    dplyr::arrange(cutoff_date, period_start)
  if (nrow(d) == 0) return(NULL)
  actual <- d %>% dplyr::group_by(period_start) %>%
    dplyr::summarise(real_plot = dplyr::first(real_plot), .groups = "drop")

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = d,
      ggplot2::aes(period_start, pred_plot, group = id_ventana, color = "Predicho"),
      alpha = 0.16, linewidth = 0.7
    ) +
    ggplot2::geom_line(
      data = actual,
      ggplot2::aes(period_start, real_plot, color = "Real"), linewidth = 1.1
    ) +
    ggplot2::geom_point(
      data = actual,
      ggplot2::aes(period_start, real_plot, color = "Real"), size = 1.3
    ) +
    ggplot2::scale_color_manual(values = c("Real" = "black", "Predicho" = "#377eb8")) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0("Cada linea azul es una ventana; MONTHLY_VALUE = '", MONTHLY_VALUE, "' (", scale_label, ")"),
      x = "Mes", y = paste0("Valor mensual (", scale_label, ")"), color = "Serie"
    ) +
    ggplot2::theme(legend.position = "bottom")
}

save_or_show_plot <- function(p, file_name) {
  if (is.null(p)) return(invisible(NULL))
  if (isTRUE(SHOW_PLOTS)) print(p)
  if (isTRUE(SAVE_PLOTS)) {
    dir.create(PLOTS_DIR, showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(
      filename = file.path(PLOTS_DIR, file_name),
      plot = p,
      width = PLOT_WIDTH,
      height = PLOT_HEIGHT,
      dpi = PLOT_DPI
    )
  }
  invisible(NULL)
}

save_scenario_results <- function(res, calendar, weekend_treat, output_xlsx,
                                  target_name = TARGET_NAME,
                                  transformation = TRANSFORMATION,
                                  aggregate_fun = MONTHLY_VALUE,
                                  next_full_months = NEXT_FULL_MONTHS,
                                  output_dir = OUTPUT_DIR) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cs <- res$cutoff_summary
  de <- res$daily_eval
  me <- res$monthly_eval
  window_rows <- list()
  prediction_frames <- list()

  if (nrow(cs) > 0) {
    for (k in seq_len(nrow(cs))) {
      cutoff <- as.Date(cs$cutoff_date[k])
      id_v <- paste("Corte", cutoff)
      columns <- daily_results_columns()
      scale_label <- results_scale_label(DAILY_RESULTS_SCALE, transformation)
      prediction <- de[de$id_ventana == id_v, c("fecha", columns)]
      names(prediction) <- c("fecha", sprintf("w%d_true_%s", k, scale_label), sprintf("w%d_pred_%s", k, scale_label))
      prediction_frames[[length(prediction_frames) + 1L]] <- prediction
      months <- me[me$id_ventana == id_v, , drop = FALSE]
      if (nrow(months) > 0) {
        for (j in seq_len(nrow(months))) {
          window_rows[[length(window_rows) + 1L]] <- data.frame(
            evaluation_start_month = EVALUATION_START,
            evaluation_end_month = EVALUATION_END,
            next_full_months = next_full_months,
            window = k,
            cutoff_week = months$week_no[j],
            cutoff_date = format(cutoff),
            pred_month = months$period_offset[j],
            train_start = format(as.Date(cs$train_start[k])),
            test_start = format(as.Date(months$period_start[j])),
            test_end = format(as.Date(months$period_end[j])),
            horizon_start = format(as.Date(cs$forecast_start[k])),
            horizon_end = format(as.Date(cs$forecast_end[k])),
            prediction_length = cs$prediction_length[k],
            real_level = months$real_level[j],
            pred_level = months$pred_level[j],
            real_proc = months$real_proc[j],
            pred_proc = months$pred_proc[j],
            error_pp_level = months$error_pp_level[j],
            error_pp_proc = months$error_pp_proc[j],
            abs_error_pp = months$abs_error_pp[j],
            full_month_obs = months$has_full_observed_period[j],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  windows_df <- if (length(window_rows)) do.call(rbind, window_rows) else data.frame()
  predictions_df <- if (length(prediction_frames)) {
    Reduce(function(a, b) merge(a, b, by = "fecha", all = TRUE), prediction_frames)
  } else data.frame()
  if (nrow(predictions_df)) predictions_df <- predictions_df[order(predictions_df$fecha), ]
  statistics_df <- score_monthly(res, next_full_months)
  metadata_df <- data.frame(
    model_id = MODEL_NAME,
    transformation = transformation,
    target = target_name,
    calendar = calendar,
    missing_method = MISSING_METHOD,
    evaluation_start_month = EVALUATION_START,
    evaluation_end_month = EVALUATION_END,
    monthly_value = MONTHLY_VALUE,
    daily_results_scale = results_scale_label(DAILY_RESULTS_SCALE, transformation),
    monthly_results_scale = results_scale_label(MONTHLY_RESULTS_SCALE, transformation),
    monthly_metric_scale = paste0(results_scale_label(MONTHLY_RESULTS_SCALE, transformation), "; relative percentage points"),
    covariates = if (nrow(cs) && "covariates" %in% names(cs)) cs$covariates[1] else "none",
    evaluation_start = if (nrow(cs)) format(min(as.Date(cs$cutoff_date))) else NA_character_,
    evaluation_end = if (nrow(cs)) format(max(as.Date(cs$cutoff_date))) else NA_character_,
    evaluation_windows = nrow(cs),
    parallel_rolling = PARALLEL_ROLLING,
    parallel_cores = PARALLEL_CORES,
    max_training_years = if (is.null(MAX_TRAINING_YEARS)) NA_integer_ else MAX_TRAINING_YEARS,
    rolling_checkpoint = ROLLING_CHECKPOINT_FILE,
    weekend_treat = weekend_treat,
    stringsAsFactors = FALSE
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "metadata")
  openxlsx::writeData(wb, "metadata", metadata_df)
  openxlsx::addWorksheet(wb, "windows")
  openxlsx::writeData(wb, "windows", windows_df)
  openxlsx::addWorksheet(wb, "predictions")
  openxlsx::writeData(wb, "predictions", predictions_df, keepNA = FALSE)
  openxlsx::addWorksheet(wb, "statistics")
  openxlsx::writeData(wb, "statistics", statistics_df)
  out_path <- file.path(output_dir, output_xlsx)
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  message("Guardado: ", normalizePath(out_path, mustWork = FALSE))
  invisible(list(
    metadata = metadata_df, windows = windows_df, predictions = predictions_df,
    statistics = statistics_df, path = out_path
  ))
}

# -----------------------------
# 5) CARGA Y VALIDACION DEL DATO
# -----------------------------

load_input_data <- function(data_path = DATA_PATH,
                            csv_skip = CSV_SKIP,
                            csv_decimal_mark = CSV_DECIMAL_MARK,
                            csv_separator = CSV_SEPARATOR,
                            csv_date_format = CSV_DATE_FORMAT,
                            transformation = TRANSFORMATION) {
  if (!file.exists(data_path)) {
    local_candidates <- unique(c(
      file.path("data", basename(data_path)),
      file.path("modelos", "dsa", "data", basename(data_path))
    ))
    local_candidates <- local_candidates[file.exists(local_candidates)]
    if (length(local_candidates) > 0) {
      data_path <- local_candidates[1]
      message("Usando CSV localizado automaticamente: ", data_path)
    } else {
      message("No se encontro DATA_PATH: ", data_path)
      message("Selecciona manualmente el CSV en la ventana de RStudio...")
      data_path <- file.choose()
    }
  }

  required_cols <- c("timestamp", "target")
  normalize_names <- function(names) {
    names <- sub("^\\ufeff", "", names)
    tolower(trimws(names))
  }
  read_with <- function(separator) {
    read.csv(
      data_path,
      skip = csv_skip,
      header = TRUE,
      sep = separator,
      dec = csv_decimal_mark,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  # The selected separator is tried first, then common delimiters. This makes
  # exported CSVs with a different regional delimiter fail less opaquely.
  separators <- unique(c(csv_separator, ";", ",", "\t"))
  parsed <- lapply(separators, read_with)
  valid <- vapply(parsed, function(df) {
    all(required_cols %in% normalize_names(colnames(df)))
  }, logical(1))
  if (!any(valid)) {
    detected_headers <- vapply(parsed, function(df) {
      paste(colnames(df), collapse = ", ")
    }, character(1))
    stop(
      "No se localizaron las columnas timestamp y target tras probar los separadores ",
      paste(sQuote(separators), collapse = ", "), ". Cabeceras detectadas: ",
      paste(paste0(sQuote(separators), " -> [", detected_headers, "]"), collapse = "; "),
      ". Revisa CSV_SKIP o el fichero seleccionado."
    )
  }
  Data <- parsed[[which(valid)[1]]]
  colnames(Data) <- normalize_names(colnames(Data))
  # Spreadsheet exports often retain blank rows after the data range.
  Data <- Data[!is.na(Data$timestamp) & nzchar(trimws(as.character(Data$timestamp))), , drop = FALSE]
  if (nrow(Data) == 0) stop("El CSV no contiene filas de datos despues de la cabecera.")

  raw_timestamps <- trimws(as.character(Data$timestamp))
  Data$timestamp <- as.Date(raw_timestamps, format = csv_date_format)
  if (any(is.na(Data$timestamp))) {
    alternative_formats <- c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")
    for (format in setdiff(alternative_formats, csv_date_format)) {
      missing_dates <- is.na(Data$timestamp)
      Data$timestamp[missing_dates] <- as.Date(raw_timestamps[missing_dates], format = format)
    }
  }
  if (any(is.na(Data$timestamp))) {
    stop("Hay fechas no parseadas. Revisa CSV_DATE_FORMAT. Valor actual: ", csv_date_format)
  }

  Data$target <- parse_target_numeric(Data$target, csv_decimal_mark)
  if (any(!is.finite(Data$target))) {
    stop("Hay valores target no numericos o no finitos tras la conversion.")
  }

  if (transformation == "log" && any(Data$target <= 0, na.rm = TRUE)) {
    stop("TRANSFORMATION = 'log' requiere target > 0. Usa 'log1p' o 'level', o depura la serie.")
  }
  if (transformation == "log1p" && any(Data$target < -1, na.rm = TRUE)) {
    stop("TRANSFORMATION = 'log1p' requiere target >= -1.")
  }

  Data <- Data[, c("timestamp", "target")]
  if (anyDuplicated(Data$timestamp)) stop("Hay fechas duplicadas en el fichero de entrada.")
  Data <- complete_input_calendar(Data, DATA_CALENDAR, MISSING_METHOD)
  Data$target_proc <- transform_level(Data$target, transformation)
  Data <- add_calendar_covariates(Data, "timestamp") %>%
    dplyr::arrange(timestamp) %>%
    stats::na.omit()

  message(
    "Filas: ", nrow(Data),
    " | rango: ", format(min(Data$timestamp)),
    " a ", format(max(Data$timestamp))
  )

  Data
}

# -----------------------------
# 6) EVALUACION Y FORECAST DE PRODUCCION
# -----------------------------

run_scenario <- function(Data, week_days, weekend_fill,
                         parallel_rolling = PARALLEL_ROLLING,
                         parallel_cores = PARALLEL_CORES,
                         parallel_worker_logs = PARALLEL_WORKER_LOGS,
                         explicit_cutoff_date = NULL,
                         forecast_days = NULL) {
  cutoff_days <- if (week_days == 5) BUSINESS_CUTOFF_DAYS else NATURAL_CUTOFF_DAYS
  run_growing_daily_forecast(
    context_df = Data,
    target_col = TARGET_COLUMN,
    date_col = DATE_COLUMN,
    evaluation_start = EVALUATION_START,
    evaluation_end = EVALUATION_END,
    cutoff_days = cutoff_days,
    next_full_months = NEXT_FULL_MONTHS,
    week_days = week_days,
    weekend_fill = weekend_fill,
    level_target_col = LEVEL_TARGET_COLUMN,
    aggregate_fun = MONTHLY_VALUE,
    transformation = TRANSFORMATION,
    parallel_rolling = parallel_rolling,
    parallel_cores = parallel_cores,
    parallel_worker_logs = parallel_worker_logs,
    explicit_cutoff_date = explicit_cutoff_date,
    forecast_days = forecast_days
  )
}

selected_week_days <- if (DATA_CALENDAR == "business") 5L else 7L
selected_weekend_fill <- if (DATA_CALENDAR == "business") {
  if (MISSING_METHOD == "linear") "interp" else "mean"
} else {
  NA_character_
}
calendar_label <- if (DATA_CALENDAR == "business") "business" else "natural"

Data <- load_input_data()
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOTS_DIR, showWarnings = FALSE, recursive = TRUE)

# 6.a) Evaluacion rolling-origin
evaluation_result <- run_scenario(
  Data,
  week_days = selected_week_days,
  weekend_fill = selected_weekend_fill
)
evaluation_summary <- evaluation_result$cutoff_summary
monthly_comparison <- score_monthly(evaluation_result)
print(evaluation_summary)
print(monthly_comparison)

token <- function(value) {
  value <- gsub("[^0-9A-Za-z_.-]+", "-", tolower(trimws(as.character(value))))
  value <- gsub("^-+|-+$", "", value)
  if (nzchar(value)) value else "none"
}
output_file <- paste0(
  token(MODEL_NAME), "_", token(TARGET_NAME),
  "_transf-", token(TRANSFORMATION), "_monthly-", token(MONTHLY_VALUE),
  "_calendar-", token(DATA_CALENDAR), "_",
  substr(EVALUATION_START, 1, 4), "-", substr(EVALUATION_END, 1, 4), ".xlsx"
)
evaluation_export <- save_scenario_results(
  evaluation_result,
  calendar = calendar_label,
  weekend_treat = if (selected_week_days == 5L) selected_weekend_fill else "none",
  output_xlsx = output_file
)
save_or_show_plot(plot_daily(evaluation_result, "DSA - ventanas de evaluacion"), "evaluation_daily.png")
save_or_show_plot(plot_monthly(evaluation_result, "DSA - evaluacion mensual"), "evaluation_monthly.png")
save_or_show_plot(plot_monthly_actual_vs_predicted(evaluation_result), "evaluation_monthly_actual_vs_predicted.png")
save_or_show_plot(plot_monthly_rmse_grid(evaluation_result), "evaluation_monthly_rmse_grid.png")

# 6.b) Forecast de produccion con la misma configuracion
PRODUCTION_CUTOFF_DATE <- NA_character_  # NA usa toda la historia observada
FORECAST_DAYS <- 28L
production_cutoff <- if (is.na(PRODUCTION_CUTOFF_DATE)) {
  max(Data$timestamp)
} else {
  as.Date(PRODUCTION_CUTOFF_DATE)
}
production_result <- run_scenario(
  Data,
  week_days = selected_week_days,
  weekend_fill = selected_weekend_fill,
  parallel_rolling = FALSE,
  parallel_cores = 1L,
  explicit_cutoff_date = production_cutoff,
  forecast_days = FORECAST_DAYS
)
production_forecast <- production_result$daily_eval
if (nrow(production_forecast) > 0) {
  print(production_forecast[, c("fecha", "yhat_level")])
  history_plot <- Data[Data$timestamp >= production_cutoff - 90, c("timestamp", "target")]
  p_production <- ggplot2::ggplot() +
    ggplot2::geom_line(data = history_plot, ggplot2::aes(timestamp, target, color = "Real")) +
    ggplot2::geom_line(
      data = production_forecast,
      ggplot2::aes(fecha, yhat_level, color = "Forecast"),
      linewidth = 0.9
    ) +
    ggplot2::geom_vline(xintercept = as.numeric(production_cutoff), linetype = "dashed") +
    ggplot2::scale_color_manual(values = c("Real" = "black", "Forecast" = "#d95f02")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "DSA - forecast de produccion (level)", x = "Fecha", y = "Target (level)", color = "Serie")
  save_or_show_plot(p_production, "production_forecast.png")
} else {
  warning("No se pudo generar el forecast de produccion.")
}
