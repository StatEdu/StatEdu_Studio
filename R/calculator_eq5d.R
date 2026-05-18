# EQ-5D calculator module.

eq5d_item_specs <- function() {
  data.frame(
    id = paste0("eq5d_item_", 1:5),
    label = c("EQ1 (Mobility)", "EQ2 (Self-care)", "EQ3 (Usual activities)", "EQ4 (Pain/discomfort)", "EQ5 (Anxiety/depression)"),
    stringsAsFactors = FALSE
  )
}

eq5d_value_set_catalog <- function(type = "5L") {
  type <- toupper(as.character(type %||% "5L"))
  base_sets <- c("South Korea" = "KR", "Japan" = "JP", "China" = "CN")
  if (identical(type, "3L")) return(base_sets)
  c(
    base_sets,
    "United States" = "US", "England" = "EN", "Germany" = "DE", "Netherlands" = "NL",
    "Canada" = "CA",
    "France" = "FR", "Spain" = "ES", "Portugal" = "PT", "Ireland" = "IE",
    "Denmark" = "DK", "Poland" = "PL", "Hungary" = "HU", "Thailand" = "TH",
    "Uruguay" = "UY", "Mexico" = "MX", "Taiwan" = "TW", "Hong Kong" = "HK",
    "Indonesia" = "ID", "Malaysia" = "MY", "Vietnam" = "VN",
    "Belgium" = "BE", "India" = "IN", "Philippines" = "PH"
  )
}

eq5d_normalized_value_set <- function(type = "5L", value_set = "KR") {
  choices <- unname(eq5d_value_set_catalog(type))
  value_set <- toupper(as.character(value_set %||% "KR"))
  if (!value_set %in% choices) "KR" else value_set
}

eq5d_reference_values <- function(type = "5L", value_set = "KR") {
  type <- toupper(as.character(type %||% "5L"))
  value_set <- eq5d_normalized_value_set(type, value_set)
  if (identical(type, "3L")) {
    if (identical(value_set, "JP")) {
      return(list(
        country = "Japan",
        label = "Japan EQ-5D-3L (Tsuchiya TTO)",
        constant = 0.152,
        n = 0,
        labels = c("M2", "M3", "SC2", "SC3", "UA2", "UA3", "PD2", "PD3", "AD2", "AD3"),
        maps = list(
          c(`1` = 0, `2` = 0.075, `3` = 0.418),
          c(`1` = 0, `2` = 0.054, `3` = 0.102),
          c(`1` = 0, `2` = 0.044, `3` = 0.133),
          c(`1` = 0, `2` = 0.080, `3` = 0.194),
          c(`1` = 0, `2` = 0.063, `3` = 0.112)
        )
      ))
    }
    if (identical(value_set, "CN")) {
      return(list(
        country = "China",
        label = "China EQ-5D-3L (Zhuo TTO)",
        constant = 0,
        n = 0,
        labels = c("M2", "M3", "SC2", "SC3", "UA2", "UA3", "PD2", "PD3", "AD2", "AD3"),
        maps = list(
          c(`1` = 0, `2` = 0.0766, `3` = 0.2668),
          c(`1` = 0, `2` = 0.0441, `3` = 0.2912),
          c(`1` = 0, `2` = 0.0370, `3` = 0.0538),
          c(`1` = 0, `2` = 0.0274, `3` = 0.0409),
          c(`1` = 0, `2` = 0.0359, `3` = 0.1771)
        )
      ))
    }
    return(list(
      country = "South Korea",
      label = "South Korea EQ-5D-3L",
      constant = 0.050,
      n = 0.050,
      labels = c("M2", "M3", "SC2", "SC3", "UA2", "UA3", "PD2", "PD3", "AD2", "AD3"),
      maps = list(
        c(`1` = 0, `2` = 0.096, `3` = 0.418),
        c(`1` = 0, `2` = 0.046, `3` = 0.136),
        c(`1` = 0, `2` = 0.051, `3` = 0.208),
        c(`1` = 0, `2` = 0.037, `3` = 0.151),
        c(`1` = 0, `2` = 0.043, `3` = 0.158)
      )
    ))
  }
  if (identical(value_set, "JP")) {
    return(list(
      country = "Japan",
      label = "Japan EQ-5D-5L (Shiroiwa TTO/DCE)",
      constant = 0.0609,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.0639, `3` = 0.1126, `4` = 0.1790, `5` = 0.2429),
        c(`1` = 0, `2` = 0.0436, `3` = 0.0767, `4` = 0.1243, `5` = 0.1597),
        c(`1` = 0, `2` = 0.0504, `3` = 0.0911, `4` = 0.1479, `5` = 0.1748),
        c(`1` = 0, `2` = 0.0445, `3` = 0.0682, `4` = 0.1314, `5` = 0.1912),
        c(`1` = 0, `2` = 0.0718, `3` = 0.1105, `4` = 0.1682, `5` = 0.1960)
      )
    ))
  }
  if (identical(value_set, "CN")) {
    return(list(
      country = "China",
      label = "China EQ-5D-5L (Luo TTO)",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.066, `3` = 0.158, `4` = 0.287, `5` = 0.345),
        c(`1` = 0, `2` = 0.048, `3` = 0.116, `4` = 0.210, `5` = 0.253),
        c(`1` = 0, `2` = 0.045, `3` = 0.107, `4` = 0.194, `5` = 0.233),
        c(`1` = 0, `2` = 0.058, `3` = 0.138, `4` = 0.252, `5` = 0.302),
        c(`1` = 0, `2` = 0.049, `3` = 0.118, `4` = 0.215, `5` = 0.258)
      )
    ))
  }
  if (identical(value_set, "US")) {
    return(list(
      country = "United States",
      label = "United States EQ-5D-5L (Pickard TTO/DCE)",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.096, `3` = 0.122, `4` = 0.237, `5` = 0.322),
        c(`1` = 0, `2` = 0.089, `3` = 0.107, `4` = 0.220, `5` = 0.261),
        c(`1` = 0, `2` = 0.068, `3` = 0.101, `4` = 0.255, `5` = 0.255),
        c(`1` = 0, `2` = 0.060, `3` = 0.098, `4` = 0.318, `5` = 0.414),
        c(`1` = 0, `2` = 0.057, `3` = 0.123, `4` = 0.299, `5` = 0.321)
      )
    ))
  }
  if (identical(value_set, "EN")) {
    return(list(
      country = "England",
      label = "England EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.058, `3` = 0.076, `4` = 0.207, `5` = 0.274),
        c(`1` = 0, `2` = 0.050, `3` = 0.080, `4` = 0.164, `5` = 0.203),
        c(`1` = 0, `2` = 0.050, `3` = 0.063, `4` = 0.162, `5` = 0.184),
        c(`1` = 0, `2` = 0.063, `3` = 0.084, `4` = 0.276, `5` = 0.335),
        c(`1` = 0, `2` = 0.078, `3` = 0.104, `4` = 0.285, `5` = 0.289)
      )
    ))
  }
  if (identical(value_set, "DE")) {
    return(list(
      country = "Germany",
      label = "Germany EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.026, `3` = 0.042, `4` = 0.139, `5` = 0.224),
        c(`1` = 0, `2` = 0.050, `3` = 0.056, `4` = 0.169, `5` = 0.260),
        c(`1` = 0, `2` = 0.036, `3` = 0.049, `4` = 0.129, `5` = 0.209),
        c(`1` = 0, `2` = 0.057, `3` = 0.109, `4` = 0.404, `5` = 0.612),
        c(`1` = 0, `2` = 0.030, `3` = 0.082, `4` = 0.244, `5` = 0.356)
      )
    ))
  }
  if (identical(value_set, "NL")) {
    return(list(
      country = "Netherlands",
      label = "Netherlands EQ-5D-5L",
      constant = 0.047,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.035, `3` = 0.057, `4` = 0.166, `5` = 0.203),
        c(`1` = 0, `2` = 0.038, `3` = 0.061, `4` = 0.168, `5` = 0.168),
        c(`1` = 0, `2` = 0.039, `3` = 0.087, `4` = 0.192, `5` = 0.192),
        c(`1` = 0, `2` = 0.066, `3` = 0.092, `4` = 0.360, `5` = 0.415),
        c(`1` = 0, `2` = 0.070, `3` = 0.145, `4` = 0.356, `5` = 0.421)
      )
    ))
  }
  if (identical(value_set, "CA")) {
    return(list(
      country = "Canada",
      label = "Canada EQ-5D-5L (Xie TTO)",
      model = "canada_linear",
      intercept = 1.1351,
      constant = 0,
      n = 0,
      slopes = c(M = 0.0389, S = 0.0458, U = 0.0195, P = 0.0444, A = 0.0376),
      severe = c(M = 0.0510, S = 0.0584, U = 0.1103, P = 0.1409, A = 0.1277),
      num45sq = 0.0085,
      labels = c("M", "M45", "S", "S45", "U", "U45", "P", "P45", "A", "A45", "Num45sq"),
      maps = list(
        c(`1` = 0, `2` = 0.0778, `3` = 0.1167, `4` = 0.2066, `5` = 0.2455),
        c(`1` = 0, `2` = 0.0916, `3` = 0.1374, `4` = 0.2416, `5` = 0.2874),
        c(`1` = 0, `2` = 0.0390, `3` = 0.0585, `4` = 0.1883, `5` = 0.2078),
        c(`1` = 0, `2` = 0.0888, `3` = 0.1332, `4` = 0.3185, `5` = 0.3629),
        c(`1` = 0, `2` = 0.0752, `3` = 0.1128, `4` = 0.2781, `5` = 0.3157)
      )
    ))
  }
  if (identical(value_set, "FR")) {
    return(list(
      country = "France",
      label = "France EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.038, `3` = 0.048, `4` = 0.179, `5` = 0.325),
        c(`1` = 0, `2` = 0.037, `3` = 0.051, `4` = 0.172, `5` = 0.258),
        c(`1` = 0, `2` = 0.033, `3` = 0.040, `4` = 0.157, `5` = 0.240),
        c(`1` = 0, `2` = 0.022, `3` = 0.047, `4` = 0.264, `5` = 0.444),
        c(`1` = 0, `2` = 0.020, `3` = 0.047, `4` = 0.200, `5` = 0.258)
      )
    ))
  }
  if (identical(value_set, "ES")) {
    return(list(
      country = "Spain",
      label = "Spain EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.084, `3` = 0.099, `4` = 0.249, `5` = 0.337),
        c(`1` = 0, `2` = 0.050, `3` = 0.053, `4` = 0.164, `5` = 0.196),
        c(`1` = 0, `2` = 0.044, `3` = 0.049, `4` = 0.135, `5` = 0.153),
        c(`1` = 0, `2` = 0.078, `3` = 0.101, `4` = 0.245, `5` = 0.382),
        c(`1` = 0, `2` = 0.081, `3` = 0.128, `4` = 0.270, `5` = 0.348)
      )
    ))
  }
  if (identical(value_set, "PT")) {
    return(list(
      country = "Portugal",
      label = "Portugal EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.048, `3` = 0.092, `4` = 0.182, `5` = 0.356),
        c(`1` = 0, `2` = 0.048, `3` = 0.070, `4` = 0.156, `5` = 0.294),
        c(`1` = 0, `2` = 0.044, `3` = 0.063, `4` = 0.135, `5` = 0.263),
        c(`1` = 0, `2` = 0.041, `3` = 0.101, `4` = 0.254, `5` = 0.406),
        c(`1` = 0, `2` = 0.036, `3` = 0.085, `4` = 0.212, `5` = 0.284)
      )
    ))
  }
  if (identical(value_set, "IE")) {
    return(list(
      country = "Ireland",
      label = "Ireland EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.063, `3` = 0.097, `4` = 0.215, `5` = 0.344),
        c(`1` = 0, `2` = 0.055, `3` = 0.088, `4` = 0.229, `5` = 0.287),
        c(`1` = 0, `2` = 0.049, `3` = 0.072, `4` = 0.154, `5` = 0.187),
        c(`1` = 0, `2` = 0.068, `3` = 0.093, `4` = 0.373, `5` = 0.510),
        c(`1` = 0, `2` = 0.080, `3` = 0.202, `4` = 0.535, `5` = 0.646)
      )
    ))
  }
  if (identical(value_set, "DK")) {
    return(list(
      country = "Denmark",
      label = "Denmark EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.041, `3` = 0.054, `4` = 0.157, `5` = 0.220),
        c(`1` = 0, `2` = 0.035, `3` = 0.050, `4` = 0.144, `5` = 0.209),
        c(`1` = 0, `2` = 0.033, `3` = 0.040, `4` = 0.139, `5` = 0.174),
        c(`1` = 0, `2` = 0.048, `3` = 0.094, `4` = 0.381, `5` = 0.537),
        c(`1` = 0, `2` = 0.072, `3` = 0.191, `4` = 0.430, `5` = 0.618)
      )
    ))
  }
  if (identical(value_set, "PL")) {
    return(list(
      country = "Poland",
      label = "Poland EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.025, `3` = 0.034, `4` = 0.126, `5` = 0.314),
        c(`1` = 0, `2` = 0.031, `3` = 0.047, `4` = 0.111, `5` = 0.264),
        c(`1` = 0, `2` = 0.023, `3` = 0.040, `4` = 0.097, `5` = 0.205),
        c(`1` = 0, `2` = 0.030, `3` = 0.050, `4` = 0.261, `5` = 0.575),
        c(`1` = 0, `2` = 0.018, `3` = 0.029, `4` = 0.108, `5` = 0.232)
      )
    ))
  }
  if (identical(value_set, "HU")) {
    return(list(
      country = "Hungary",
      label = "Hungary EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.035, `3` = 0.089, `4` = 0.263, `5` = 0.455),
        c(`1` = 0, `2` = 0.045, `3` = 0.089, `4` = 0.241, `5` = 0.366),
        c(`1` = 0, `2` = 0.035, `3` = 0.085, `4` = 0.217, `5` = 0.276),
        c(`1` = 0, `2` = 0.043, `3` = 0.073, `4` = 0.288, `5` = 0.411),
        c(`1` = 0, `2` = 0.040, `3` = 0.093, `4` = 0.261, `5` = 0.340)
      )
    ))
  }
  if (identical(value_set, "TH")) {
    return(list(
      country = "Thailand",
      label = "Thailand EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.066, `3` = 0.087, `4` = 0.211, `5` = 0.371),
        c(`1` = 0, `2` = 0.058, `3` = 0.071, `4` = 0.193, `5` = 0.250),
        c(`1` = 0, `2` = 0.058, `3` = 0.071, `4` = 0.154, `5` = 0.248),
        c(`1` = 0, `2` = 0.056, `3` = 0.067, `4` = 0.207, `5` = 0.256),
        c(`1` = 0, `2` = 0.058, `3` = 0.096, `4` = 0.233, `5` = 0.295)
      )
    ))
  }
  if (identical(value_set, "UY")) {
    return(list(
      country = "Uruguay",
      label = "Uruguay EQ-5D-5L",
      constant = 0.013,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.014, `3` = 0.032, `4` = 0.108, `5` = 0.299),
        c(`1` = 0, `2` = 0.026, `3` = 0.061, `4` = 0.117, `5` = 0.273),
        c(`1` = 0, `2` = 0.042, `3` = 0.046, `4` = 0.118, `5` = 0.232),
        c(`1` = 0, `2` = 0.017, `3` = 0.061, `4` = 0.187, `5` = 0.271),
        c(`1` = 0, `2` = 0.010, `3` = 0.044, `4` = 0.104, `5` = 0.177)
      )
    ))
  }
  if (identical(value_set, "MX")) {
    return(list(
      country = "Mexico",
      label = "Mexico EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.0160, `3` = 0.0473, `4` = 0.1786, `5` = 0.2697),
        c(`1` = 0, `2` = 0.0476, `3` = 0.0819, `4` = 0.1697, `5` = 0.2589),
        c(`1` = 0, `2` = 0.0553, `3` = 0.0952, `4` = 0.1798, `5` = 0.2758),
        c(`1` = 0, `2` = 0.0531, `3` = 0.0808, `4` = 0.2283, `5` = 0.4579),
        c(`1` = 0, `2` = 0.0551, `3` = 0.0824, `4` = 0.1611, `5` = 0.3337)
      )
    ))
  }
  if (identical(value_set, "TW")) {
    return(list(
      country = "Taiwan",
      label = "Taiwan EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.108, `3` = 0.200, `4` = 0.365, `5` = 0.477),
        c(`1` = 0, `2` = 0.076, `3` = 0.132, `4` = 0.264, `5` = 0.324),
        c(`1` = 0, `2` = 0.073, `3` = 0.123, `4` = 0.280, `5` = 0.351),
        c(`1` = 0, `2` = 0.087, `3` = 0.158, `4` = 0.340, `5` = 0.453),
        c(`1` = 0, `2` = 0.064, `3` = 0.183, `4` = 0.340, `5` = 0.421)
      )
    ))
  }
  if (identical(value_set, "HK")) {
    return(list(
      country = "Hong Kong",
      label = "Hong Kong EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.109, `3` = 0.182, `4` = 0.371, `5` = 0.529),
        c(`1` = 0, `2` = 0.087, `3` = 0.113, `4` = 0.271, `5` = 0.352),
        c(`1` = 0, `2` = 0.067, `3` = 0.094, `4` = 0.234, `5` = 0.282),
        c(`1` = 0, `2` = 0.076, `3` = 0.147, `4` = 0.307, `5` = 0.354),
        c(`1` = 0, `2` = 0.080, `3` = 0.140, `4` = 0.293, `5` = 0.348)
      )
    ))
  }
  if (identical(value_set, "ID")) {
    return(list(
      country = "Indonesia",
      label = "Indonesia EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.119, `3` = 0.192, `4` = 0.410, `5` = 0.613),
        c(`1` = 0, `2` = 0.101, `3` = 0.140, `4` = 0.248, `5` = 0.316),
        c(`1` = 0, `2` = 0.090, `3` = 0.156, `4` = 0.301, `5` = 0.385),
        c(`1` = 0, `2` = 0.086, `3` = 0.095, `4` = 0.198, `5` = 0.246),
        c(`1` = 0, `2` = 0.079, `3` = 0.134, `4` = 0.227, `5` = 0.305)
      )
    ))
  }
  if (identical(value_set, "MY")) {
    return(list(
      country = "Malaysia",
      label = "Malaysia EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.081, `3` = 0.108, `4` = 0.261, `5` = 0.340),
        c(`1` = 0, `2` = 0.062, `3` = 0.083, `4` = 0.200, `5` = 0.261),
        c(`1` = 0, `2` = 0.048, `3` = 0.064, `4` = 0.155, `5` = 0.202),
        c(`1` = 0, `2` = 0.081, `3` = 0.107, `4` = 0.259, `5` = 0.338),
        c(`1` = 0, `2` = 0.072, `3` = 0.095, `4` = 0.230, `5` = 0.300)
      )
    ))
  }
  if (identical(value_set, "VN")) {
    return(list(
      country = "Vietnam",
      label = "Vietnam EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.069, `3` = 0.079, `4` = 0.206, `5` = 0.376),
        c(`1` = 0, `2` = 0.043, `3` = 0.046, `4` = 0.147, `5` = 0.231),
        c(`1` = 0, `2` = 0.046, `3` = 0.059, `4` = 0.174, `5` = 0.299),
        c(`1` = 0, `2` = 0.084, `3` = 0.152, `4` = 0.270, `5` = 0.367),
        c(`1` = 0, `2` = 0.064, `3` = 0.113, `4` = 0.171, `5` = 0.239)
      )
    ))
  }
  if (identical(value_set, "BE")) {
    return(list(
      country = "Belgium",
      label = "Belgium EQ-5D-5L",
      model = "conditional_constant",
      conditional_constant = TRUE,
      constant = 0.0376805,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.0315444, `3` = 0.05856131, `4` = 0.1786745, `5` = 0.2267916),
        c(`1` = 0, `2` = 0.02301333, `3` = 0.04272361, `4` = 0.1303526, `5` = 0.1654566),
        c(`1` = 0, `2` = 0.02514052, `3` = 0.04667269, `4` = 0.14240152, `5` = 0.1807503),
        c(`1` = 0, `2` = 0.06707767, `3` = 0.12452785, `4` = 0.37994286, `5` = 0.4822616),
        c(`1` = 0, `2` = 0.06101942, `3` = 0.11328088, `4` = 0.34562757, `5` = 0.4387052)
      )
    ))
  }
  if (identical(value_set, "IN")) {
    return(list(
      country = "India",
      label = "India EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.0496623, `3` = 0.0988915, `4` = 0.2541657, `5` = 0.3874732),
        c(`1` = 0, `2` = 0.0512558, `3` = 0.1305876, `4` = 0.3014405, `5` = 0.3798653),
        c(`1` = 0, `2` = 0.0454892, `3` = 0.0886009, `4` = 0.2415260, `5` = 0.3239096),
        c(`1` = 0, `2` = 0.0513593, `3` = 0.1255064, `4` = 0.3897716, `5` = 0.5842377),
        c(`1` = 0, `2` = 0.0162728, `3` = 0.0626321, `4` = 0.1635654, `5` = 0.2470492)
      )
    ))
  }
  if (identical(value_set, "PH")) {
    return(list(
      country = "Philippines",
      label = "Philippines EQ-5D-5L",
      constant = 0,
      n = 0,
      labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
      maps = list(
        c(`1` = 0, `2` = 0.04196569, `3` = 0.05257251, `4` = 0.21956972, `5` = 0.30855823),
        c(`1` = 0, `2` = 0.03999859, `3` = 0.05010821, `4` = 0.20927756, `5` = 0.29409479),
        c(`1` = 0, `2` = 0.03433007, `3` = 0.04300698, `4` = 0.17961917, `5` = 0.25241628),
        c(`1` = 0, `2` = 0.05108055, `3` = 0.06399114, `4` = 0.26725976, `5` = 0.37557637),
        c(`1` = 0, `2` = 0.0282, `3` = 0.03534685, `4` = 0.14762654, `5` = 0.20745748)
      )
    ))
  }
  list(
    country = "South Korea",
    label = "South Korea EQ-5D-5L",
    constant = 0.096,
    n = 0.078,
    labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
    maps = list(
      c(`1` = 0, `2` = 0.046, `3` = 0.058, `4` = 0.133, `5` = 0.251),
      c(`1` = 0, `2` = 0.032, `3` = 0.050, `4` = 0.078, `5` = 0.122),
      c(`1` = 0, `2` = 0.021, `3` = 0.051, `4` = 0.100, `5` = 0.175),
      c(`1` = 0, `2` = 0.042, `3` = 0.053, `4` = 0.166, `5` = 0.207),
      c(`1` = 0, `2` = 0.033, `3` = 0.046, `4` = 0.102, `5` = 0.137)
    )
  )
}

eq5d_variable_choices <- function(data, variable_info = NULL) {
  hint8_variable_choices(data, variable_info)
}

eq5d_selected_variables <- function(input) {
  specs <- eq5d_item_specs()
  vapply(specs$id, function(id) as.character(input[[id]] %||% ""), character(1))
}

eq5d_score <- function(items, type = "5L", value_set = "KR", profile_11111_as_one = TRUE) {
  items <- as.data.frame(items, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(items) != 5) stop("EQ-5D requires exactly 5 item columns.", call. = FALSE)

  reference <- eq5d_reference_values(type, value_set)
  max_level <- if (toupper(as.character(type %||% "5L")) == "3L") 3L else 5L
  values <- as.data.frame(lapply(items, function(column) suppressWarnings(as.integer(as.character(column)))), check.names = FALSE)
  if (identical(reference$model, "canada_linear")) {
    complete_values <- stats::complete.cases(values) & apply(values, 1, function(row) all(row %in% seq_len(max_level)))
    score <- rep(NA_real_, nrow(values))
    n45 <- rowSums(values >= 4L, na.rm = TRUE)
    slope_penalty <- as.matrix(values) %*% as.numeric(reference$slopes)
    severe_penalty <- (as.matrix(values) >= 4L) %*% as.numeric(reference$severe)
    score[complete_values] <- reference$intercept -
      slope_penalty[complete_values, 1] -
      severe_penalty[complete_values, 1] +
      reference$num45sq * pmax(n45[complete_values] - 1, 0)^2
    if (isTRUE(profile_11111_as_one)) {
      all_one <- stats::complete.cases(values) & rowSums(values == 1L, na.rm = TRUE) == 5L
      score[all_one] <- 1
    }
    return(score)
  }
  penalties <- lapply(seq_len(5), function(index) {
    valid <- !is.na(values[[index]]) & values[[index]] %in% seq_len(max_level)
    out <- rep(NA_real_, nrow(values))
    out[valid] <- unname(reference$maps[[index]][as.character(values[[index]][valid])])
    out
  })
  penalty_data <- as.data.frame(penalties, stringsAsFactors = FALSE)
  complete <- stats::complete.cases(penalty_data)
  score <- rep(NA_real_, nrow(items))
  any_highest <- rowSums(values == max_level, na.rm = TRUE) > 0
  any_problem <- rowSums(values > 1L, na.rm = TRUE) > 0
  constant_penalty <- if (isTRUE(reference$conditional_constant)) {
    ifelse(any_problem[complete], reference$constant, 0)
  } else {
    reference$constant
  }
  score[complete] <- 1 - (constant_penalty + rowSums(penalty_data[complete, , drop = FALSE]) + ifelse(any_highest[complete], reference$n, 0))
  if (isTRUE(profile_11111_as_one)) {
    all_one <- stats::complete.cases(values) & rowSums(values == 1L, na.rm = TRUE) == 5L
    score[all_one] <- 1
  }
  score
}

eq5d_calculator_result <- function(data, selected, type = "5L", value_set = "KR", variable_choices = NULL, profile_11111_as_one = TRUE) {
  selected <- as.character(selected)
  selected <- selected[nzchar(selected)]
  if (length(selected) != 5 || anyDuplicated(selected)) stop("Select 5 different EQ-5D item variables.", call. = FALSE)
  if (is.null(data) || !all(selected %in% names(data))) stop("Selected variables are not available in the loaded data.", call. = FALSE)
  if (!is.null(variable_choices) && !all(selected %in% variable_choices)) stop("Selected EQ-5D item variables are not available in the loaded data.", call. = FALSE)
  result <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  result[["eq5d_score"]] <- eq5d_score(result[, selected, drop = FALSE], type = type, value_set = value_set, profile_11111_as_one = profile_11111_as_one)
  result
}

eq5d_calculator_tab_panel <- function() {
  tabPanel(
    "EQ5D",
    value = "calculator_eq5d",
    div(
      class = "page-shell",
      div(class = "app-heading", h1("EQ-5D Calculator"), div("Select the 5 EQ-5D item variables and add eq5d_score to the current data.", class = "app-subtitle")),
      div(
        class = "workspace-panel frequencies-workspace-panel hint8-calculator-workspace",
        style = "min-width:980px;overflow-x:auto;",
        h3("EQ-5D"),
        div(class = "load-message", textOutput("eq5d_loaded_message")),
        uiOutput("eq5d_calculator_setup"),
        div(class = "analysis-action-row hint8-action-row", actionButton("run_eq5d_calculator", "Calculate", class = "btn btn-primary"), downloadButton("download_eq5d_calculator", "Download CSV", class = "btn btn-default")),
        uiOutput("eq5d_calculator_summary"),
        DT::DTOutput("eq5d_calculator_preview")
      )
    )
  )
}

eq5d_reference_table <- function(type = "5L", value_set = "KR") {
  reference <- eq5d_reference_values(type, value_set)
  if (identical(reference$model, "canada_linear")) {
    return(tags$table(
      class = "hint8-initial-table eq5d-initial-table",
      tags$thead(tags$tr(
        tags$th(""),
        tags$th("slope"),
        tags$th("level 4/5")
      )),
      tags$tbody(
        lapply(names(reference$slopes), function(dimension) {
          tags$tr(
            tags$td(dimension, class = "eq5d-dimension-label"),
            tags$td(sprintf("%.4f", unname(reference$slopes[[dimension]]))),
            tags$td(sprintf("%.4f", unname(reference$severe[[dimension]])))
          )
        }),
        tags$tr(
          tags$td("Intercept", class = "eq5d-dimension-label"),
          tags$td(sprintf("%.4f", reference$intercept), colspan = 2)
        ),
        tags$tr(
          tags$td("Num45sq", class = "eq5d-dimension-label"),
          tags$td(sprintf("%.4f", reference$num45sq), colspan = 2)
        )
      )
    ))
  }
  type <- toupper(as.character(type %||% "5L"))
  levels <- if (identical(type, "3L")) as.character(2:3) else as.character(2:5)
  dimensions <- c("M", "S", "U", "P", "A")
  tags$table(
    class = "hint8-initial-table eq5d-initial-table",
    tags$thead(tags$tr(
      tags$th(""),
      lapply(levels, tags$th)
    )),
    tags$tbody(
      lapply(seq_along(dimensions), function(index) {
        values <- reference$maps[[index]][levels]
        tags$tr(
          tags$td(dimensions[[index]], class = "eq5d-dimension-label"),
          lapply(values, function(value) tags$td(sprintf("%.3f", unname(value))))
        )
      }),
      tags$tr(
        tags$td(if (isTRUE(reference$conditional_constant)) "constant*" else "constant", class = "eq5d-dimension-label"),
        tags$td(sprintf("%.3f", reference$constant), colspan = length(levels))
      ),
      tags$tr(
        tags$td(if (identical(type, "3L")) "N3" else "N4", class = "eq5d-dimension-label"),
        tags$td(sprintf("%.3f", reference$n), colspan = length(levels))
      )
    )
  )
}

eq5d_output_table <- function() {
  tags$table(
    class = "hint8-initial-table eq5d-initial-table eq5d-output-table",
    tags$tbody(tags$tr(tags$td("Score"), tags$td("eq5d_score")))
  )
}

eq5d_setup_ui <- function(file, data, variable_info, input) {
  if (is.null(file)) return(setup_empty_message("Load a data file in the Data tab before using the EQ-5D calculator."))
  choices <- eq5d_variable_choices(data, variable_info)
  available_items <- analysis_variable_items(choices, variable_info, character(0))
  specs <- eq5d_item_specs()
  selected_type <- if (identical(input$eq5d_type, "3L")) "3L" else "5L"
  value_set_choices <- eq5d_value_set_catalog(selected_type)
  selected_value_set <- eq5d_normalized_value_set(selected_type, input$eq5d_value_set %||% "KR")
  reference <- eq5d_reference_values(selected_type, selected_value_set)
  profile_as_one <- isTRUE(input$eq5d_profile_11111_as_one %||% TRUE)
  initial_score <- if (isTRUE(profile_as_one)) 1 else 1 - reference$constant
  variable_inputs <- lapply(seq_len(nrow(specs)), function(index) {
    id <- specs$id[[index]]
    hint8_item_select_control(id, specs$label[[index]], choices, selected = input[[id]] %||% "")
  })
  div(
    class = "frequencies-setup-grid metabolic-setup-grid",
    div(class = "analysis-transfer-column analysis-transfer-panel", analysis_field_label_tag("Variables"), analysis_transfer_listbox_input("eq5d_available", available_items, selected = isolate(input$eq5d_available), size = 19)),
    div(class = "analysis-transfer-controls hint8-transfer-spacer"),
    div(
      class = "analysis-transfer-column analysis-transfer-panel metabolic-target-panel eq5d-target-panel",
      analysis_field_label_tag("EQ-5D variables"),
      div(
        class = "eq5d-type-control",
        selectInput("eq5d_type", "Type", choices = c("EQ-5D-5L" = "5L", "EQ-5D-3L" = "3L"), selected = selected_type, width = "100%")
      ),
      div(class = "metabolic-variable-input-grid", variable_inputs)
    ),
    div(
      class = "analysis-options-column analysis-options-panel metabolic-reference-panel eq5d-initial-panel",
      div(class = "analysis-option-title", "Initial values"),
      div(
        class = "hint8-initial-content eq5d-initial-content",
        selectInput(
          "eq5d_value_set",
          "Country / value set",
          choices = value_set_choices,
          selected = selected_value_set,
          width = "100%"
        ),
        div(
          class = "step-summary hint8-initial-summary eq5d-initial-summary",
          div(sprintf("Initial score: %.3f", initial_score), class = "step-summary-title"),
          div("When checked, profile 11111 is scored as 1.000. When unchecked, the formula score excludes only the constant.", class = "step-summary-detail")
        ),
        checkboxInput(
          "eq5d_profile_11111_as_one",
          "profile 11111 -> EQ5D = 1.0",
          value = profile_as_one
        ),
        div(sprintf("Value set: %s", reference$label), class = "eq5d-value-set-label"),
        eq5d_reference_table(selected_type, selected_value_set),
        div(class = "analysis-option-title calculator-output-title", "Output"),
        eq5d_output_table()
      )
    )
  )
}

register_eq5d_calculator_handlers <- function(input, output, session, dataset_fn, current_data_file_fn, variable_info_fn, add_calculated_variable_fn) {
  output$eq5d_loaded_message <- renderText({
    file <- current_data_file_fn()
    hint8_loaded_message_text(file, if (is.null(file)) NULL else dataset_fn())
  })
  output$eq5d_calculator_setup <- renderUI({
    file <- current_data_file_fn()
    data <- if (is.null(file)) NULL else dataset_fn()
    eq5d_setup_ui(file, data, if (is.null(file)) NULL else variable_info_fn(), input)
  })
  observeEvent(input$eq5d_available, {
    picked <- as.character(input$eq5d_available %||% "")
    if (!nzchar(picked)) return()
    selected <- eq5d_selected_variables(input)
    if (picked %in% selected) return()
    empty_index <- which(!nzchar(selected))[1]
    if (!is.na(empty_index)) updateSelectInput(session, eq5d_item_specs()$id[[empty_index]], selected = picked)
  }, ignoreInit = TRUE)
  result <- eventReactive(input$run_eq5d_calculator, {
    if (is.null(current_data_file_fn())) {
      showNotification("Load a data file before calculating EQ-5D.", type = "warning", duration = 5)
      return(NULL)
    }
    tryCatch({
      variable_choices <- eq5d_variable_choices(dataset_fn(), variable_info_fn())
      result_data <- eq5d_calculator_result(
        dataset_fn(),
        eq5d_selected_variables(input),
        type = input$eq5d_type %||% "5L",
        value_set = input$eq5d_value_set %||% "KR",
        variable_choices = variable_choices,
        profile_11111_as_one = isTRUE(input$eq5d_profile_11111_as_one)
      )
      add_calculated_variable_fn("eq5d_score", result_data[["eq5d_score"]], var_label = "EQ-5D score", measurement = "continuous")
      showNotification("eq5d_score was added to the current data.", type = "message", duration = 5)
      result_data
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "warning", duration = 6)
      NULL
    })
  }, ignoreInit = TRUE)
  output$eq5d_calculator_summary <- renderUI({
    data <- result()
    if (is.null(data)) return(div(class = "empty-message", div("Select variables and click Calculate.")))
    div(class = "empty-message", div(sprintf("Calculated eq5d_score for %s rows. The variable is available in analysis menus.", nrow(data))))
  })
  output$eq5d_calculator_preview <- DT::renderDT({
    data <- result()
    if (is.null(data)) return(DT::datatable(data.frame(Message = "No EQ-5D result yet.", stringsAsFactors = FALSE), rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    selected <- eq5d_selected_variables(input)
    preview_names <- intersect(c(selected, "eq5d_score"), names(data))
    DT::datatable(utils::head(data[, preview_names, drop = FALSE], 50), rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  output$download_eq5d_calculator <- downloadHandler(
    filename = function() paste0("easyflow_eq5d_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      data <- result()
      if (is.null(data)) {
        data <- eq5d_calculator_result(
          dataset_fn(),
          eq5d_selected_variables(input),
          type = input$eq5d_type %||% "5L",
          value_set = input$eq5d_value_set %||% "KR",
          profile_11111_as_one = isTRUE(input$eq5d_profile_11111_as_one)
        )
      }
      utils::write.csv(data, file, row.names = FALSE, na = "")
    }
  )
  invisible(TRUE)
}
