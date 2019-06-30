geography.theme <- theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position="bottom")

cbf.palette <- # color blind friendly palette
  c(black = "#000000",
    orange = "#E69F00",
    skyblue = "#56B4E9",
    bluishgreen = "#009E73",
    yellow = "#F0E442",
    blue = "#0072B2",
    vermillion = "#D55E00",
    reddishpurple = "#CC79A7")

pointer.icon <- makeIcon(
  iconUrl = "images/font-awesome_4-7-0_street-view_20_0_002340_none.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 16)



# Emergency package override ----
getMap <- function (lon = -122.329237, lat = 47.694534, domain.data = NULL,
                    radius = 5, extend.fraction = 0.05, make.symmetrical = TRUE,
                    bw = TRUE, type = "toner-2011", adjust.zoom = 0, messaging = FALSE)
{
  options(ggmap.file_drawer = "L:\\EBI\\Analytics\\GIS\\Maps\\Stamen\\ggmap")
  miles.per.degree.latitude <- 69.05422334
  color <- ifelse(bw == TRUE, "bw", "color")
  if (is.null(domain.data)) {
    bbox <- createBoundingBox(lon, lat, radius)
    longitude.range <- unlist(c(bbox[1, ]))
    latitude.range <- unlist(c(bbox[2, ]))
  }
  else {
    longitude.range <- extendrange(domain.data[, 1], f = extend.fraction)
    latitude.range <- extendrange(domain.data[, 2], f = extend.fraction)
    names(longitude.range) <- c("min", "max")
    names(latitude.range) <- c("min", "max")
  }
  if (make.symmetrical == TRUE) {
    distance.width <- geosphere::distGeo(c(longitude.range[1],
                                           mean(latitude.range)), c(longitude.range[2], mean(latitude.range))) *
      0.000621371
    distance.height <- diff(latitude.range) * miles.per.degree.latitude
    if (distance.height > distance.width) {
      degrees <- convertDistanceInMilesToDegreesLongitude(mean(latitude.range),
                                                          distance.height/2)
      longitude.range <- mean(longitude.range) + c(-degrees,
                                                   degrees)
    }
    else {
      degrees <- distance.width/miles.per.degree.latitude/2
      latitude.range <- mean(latitude.range) + c(-degrees,
                                                 degrees)
    }
  }
  distance.width <- geosphere::distGeo(c(longitude.range[1],
                                         mean(latitude.range)), c(longitude.range[2], mean(latitude.range))) *
    0.000621371
  distance.height <- diff(latitude.range) * miles.per.degree.latitude
  bbox <- data.frame(min = c(longitude.range[1], latitude.range[1]),
                     max = c(longitude.range[2], latitude.range[2]))
  rownames(bbox) <- c("longitude", "latitude")
  
  ##### NEED TO UPDATE PACKAGE
  ##### NEED TO UPDATE PACKAGE
  ##### NEED TO UPDATE PACKAGE
  
  zoom <- ggmap::calc_zoom(make_bbox(longitude.range, latitude.range))
  # zoom <- ggmap::calc_zoom(longitude.range, latitude.range)
  
  ##### NEED TO UPDATE PACKAGE
  ##### NEED TO UPDATE PACKAGE
  ##### NEED TO UPDATE PACKAGE
  
  zoom <- zoom + adjust.zoom
  warning.count <- 0
  map.loaded <- FALSE
  map <- NULL
  map <- ggmap::get_stamenmap(bbox = c(bbox["longitude",
                                            "min"], bbox["latitude", "min"], bbox["longitude",
                                                                                  "max"], bbox["latitude", "max"]), maptype = type,
                              color = color, zoom = zoom, messaging = messaging,
                              crop = TRUE)
  while (class(map)[1] != "ggmap") {
    map.loaded <- tryCatch({
      map <- ggmap::get_stamenmap(bbox = c(bbox["longitude",
                                                "min"], bbox["latitude", "min"], bbox["longitude",
                                                                                      "max"], bbox["latitude", "max"]), maptype = type,
                                  color = color, zoom = zoom, messaging = FALSE,
                                  crop = TRUE, force = TRUE)
    }, warning = function(x) {
      warning.count <- warning.count + 1
      message(paste0("get_stamenmap probably couldn't split the tiles evening at the current zoom level. Increasing ",
                     type, " zoom from ", zoom, " to ", zoom + 1))
      return(FALSE)
    })
    zoom <- zoom + 1
    if (zoom > 15) {
      stop("Processing stoped. ", zoom, " zoom exceed 15.")
    }
  }
  logo.scale <- 0.075
  logo.offset <- 0.025
  logo.right <- bbox["longitude", "max"] - convertDistanceInMilesToDegreesLongitude(mean(latitude.range),
                                                                                    logo.offset * distance.width)
  logo.left <- logo.right - convertDistanceInMilesToDegreesLongitude(mean(latitude.range),
                                                                     logo.scale * distance.width)
  logo.bottom <- bbox["latitude", "min"] + (logo.offset *
                                              distance.height)/miles.per.degree.latitude
  logo.top <- logo.bottom + (logo.scale * distance.height)/miles.per.degree.latitude
  logo <- inset_raster(ebi.logo.png, logo.left, logo.right,
                       logo.bottom, logo.top)
  return(list(map = map, ebi.logo = logo, bbox = bbox))
}


# Load data ----

odbc.connection <- RODBC::odbcConnect("app-p-ebisql")
sql.string <- "select
pd.PatientID,
AdmitDate,
pd.IsMedicare,
pd.IsMedicareAdvantage,
pd.HospitalAdmitsLast12M,
pd.ERVisitsLast12M,
pd.HospitalReAdmitsLast12M,
pd.HospitalLOSLast12M,
fhe.PatientClass,
fhe.AdmitDX,
fhe.ChiefComplaint,
fhe.ServiceType,
fhe.DischgDisposition as DischargeDisposition,
fhe.LengthOfStay,
df.FacilityName,
pd.Lon as PatientLongitude,
pd.Lat as PatientLatitude,
da.Lon as HospitalLongitude,
da.Lat as HospitalLatitude
from MasterDM.dbo.FactHospitalEncounter fhe
join MasterDM.dbo.DimFacility df on fhe.FacilityID = df.FacilityID
join JCSandbox.dbo.PatientDemographic pd on fhe.patientid = pd.patientid
join JCSandbox.dbo.DimAddress da on df.FacilityAddress + ' ' +  df.FacilityCity + ', ' + df.FacilityState + ' ' + df.FacilityZip = da.Address
where AdmitDate > dateadd(mm, -3, getdate())"

map.source.data <- RODBC::sqlQuery(odbc.connection, sql.string)
map.source.data <- as_tibble(map.source.data)

close(odbc.connection)

# Data cleanup ----

map.source.data$DischargeDisposition <- fct_recode(map.source.data$DischargeDisposition, "unknown" = "")
map.source.data$ChiefComplaint <- fct_recode(map.source.data$ChiefComplaint, "unknown" = "")
map.source.data$AdmitDX <- fct_recode(map.source.data$AdmitDX, "unknown" = "")
map.source.data$PatientClass <- fct_recode(map.source.data$PatientClass, "emergency" = "ER")

# failure to remove NA coordinates will cause leaflet to fail unexepectedly
map.source.data <-
  map.source.data %>%
  drop_na(matches("Longitude|Latitude"))

# Feature engineering ----

map.source.data$PatientType <- "other"
map.source.data$PatientType <- ifelse(map.source.data$IsMedicare == 1,
                                      "medicare", map.source.data$PatientType)
map.source.data$PatientType <- ifelse(map.source.data$IsMedicareAdvantage == 1,
                                      "medicare advantage", map.source.data$PatientType)
map.source.data$PatientType <- factor(map.source.data$PatientType)
# Sort factors based on frequency ----

factor.columns <- names(Filter(is.factor, map.source.data))
map.source.data[factor.columns] = lapply(map.source.data[factor.columns], tolower)
map.source.data[factor.columns] = lapply(map.source.data[factor.columns], fct_infreq)


map.date.range.footer <-
  paste(
    "Map data covers",
    format(min(map.source.data$AdmitDate), "%A %b %m, %Y"),
    "through",
    format(max(map.source.data$AdmitDate), "%A %b %m, %Y"),
    "\n"
  )



# Geographic medians ----
geographic.medians <- map.source.data %>%
  group_by(PatientType) %>%
  summarize(PatientLongitude = median(PatientLongitude, na.rm = TRUE),
            PatientLatitude = median(PatientLatitude, na.rm = TRUE),
            HospitalLongitude = median(HospitalLongitude, na.rm = TRUE),
            HospitalLatitude = median(HospitalLatitude, na.rm = TRUE))

map.object <- getMap(domain.data =
                       data.frame(lon = c(geographic.medians$PatientLongitude,
                                          geographic.medians$HospitalLongitude),
                                  lat = c(geographic.medians$PatientLatitude,
                                          geographic.medians$HospitalLatitude)),
                     make.symmetrical = TRUE, type = "toner-lite")

geographic.medians.map <-
  ggmap::ggmap(map.object$map) +
  geom_point(data = geographic.medians,
             aes(x = PatientLongitude, y = PatientLatitude,
                 color = PatientType,
                 shape = "patient geographic center"),
             size = 5, stroke = 1.25) +
  geom_point(data = geographic.medians,
             aes(x = HospitalLongitude, y = HospitalLatitude,
                 color = PatientType,
                 shape = "hospital geographic center"),
             size = 5, stroke = 1.25) +
  scale_color_manual(name = "Patient Type", values = c("medicare" = "#1b9e77",
                                                       "medicare advantage" = "#d95f02",
                                                       "other" = "#7570b3")) +
  scale_fill_manual(name = "Patient Type", values = c("medicare" = "#1b9e77",
                                                      "medicare advantage" = "#d95f02",
                                                      "other" = "#7570b3")) +
  scale_shape_manual(name = NULL,
                     values = c("patient geographic center" = 9,
                                "hospital geographic center" = 10)) +
  map.object$ebi.logo +
  geography.theme +
  guides(color = guide_legend(ncol = 1),
         shape = guide_legend(ncol = 1))


# Data all hospital ----

# length of degree longitude / length of degree latitude at 47.89N
map.projection.c <- 69.09 / 46.47

hospital.scale.c <- 0.0025
map.hospital.data <-
  map.source.data %>%
  group_by(FacilityName, HospitalLongitude, HospitalLatitude) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(Rank = row_number(),
         side = sqrt(count),
         lng1 = HospitalLongitude - side/2 * map.projection.c * hospital.scale.c,
         lng2 = HospitalLongitude + side/2 * map.projection.c * hospital.scale.c,
         lat1 = HospitalLatitude - side/2 * hospital.scale.c,
         lat2 = HospitalLatitude + side/2 * hospital.scale.c)


# Data MA ED hospital ----

hospital.scale.c <- 0.01
ma.ed.hospital.data <-
  map.source.data %>%
  filter(IsMedicareAdvantage == 1,
         PatientClass == "emergency") %>%
  group_by(FacilityName, HospitalLongitude, HospitalLatitude) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(Rank = row_number(),
         side = sqrt(count),
         lng1 = HospitalLongitude - side/2 * map.projection.c * hospital.scale.c,
         lng2 = HospitalLongitude + side/2 * map.projection.c * hospital.scale.c,
         lat1 = HospitalLatitude - side/2 * hospital.scale.c,
         lat2 = HospitalLatitude + side/2 * hospital.scale.c)


# Data MA inpatient hospital  ----

hospital.scale.c <- 0.01
ma.inpatient.hospital.data <-
  map.source.data %>%
  filter(IsMedicareAdvantage == 1,
         PatientClass == "inpatient") %>%
  group_by(FacilityName, HospitalLongitude, HospitalLatitude) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(Rank = row_number(),
         side = sqrt(count),
         lng1 = HospitalLongitude - side/2 * map.projection.c * hospital.scale.c,
         lng2 = HospitalLongitude + side/2 * map.projection.c * hospital.scale.c,
         lat1 = HospitalLatitude - side/2 * hospital.scale.c,
         lat2 = HospitalLatitude + side/2 * hospital.scale.c)


# Data all encounters ----

encounter.table.data <-
  map.source.data %>%
  # mutate(IsMedicareAdvantage = ifelse(IsMedicareAdvantage == 1, "yes", "no")) %>%
  group_by(
    FacilityName,
    PatientType,
    PatientClass) %>%
  summarize(
    Encounters = n())

# encounter.table.data$IsMedicareAdvantage <-
#   factor(encounter.table.data$IsMedicareAdvantage)

names(encounter.table.data) <- c("Facility Name",
                                 "Patient Type",
                                 "Patient Class",
                                 "Encounters")

encounter.map.data <-
  map.source.data %>%
  mutate(
    MedicareED = ifelse(PatientType == "medicare" &
                          PatientClass == "emergency", 1, 0),
    MedicareInpatient = ifelse(PatientType == "medicare" &
                                 PatientClass == "inpatient", 1, 0),
    MedicareAdvantageED = ifelse(PatientType == "medicare advantage" &
                                   PatientClass == "emergency", 1, 0),
    MedicareAdvantageInpatient = ifelse(PatientType == "medicare advantage" &
                                          PatientClass == "inpatient", 1, 0),
    OtherED = ifelse(PatientType == "other" &
                       PatientClass == "emergency", 1, 0),
    OtherInpatient = ifelse(PatientType == "other" &
                              PatientClass == "inpatient", 1, 0))


# Leaflet all encounters ----

m.ed <- encounter.map.data %>%
  filter(MedicareED == 1) %>%
  select(lng = PatientLongitude,
         lat = PatientLatitude)

m.ip <- encounter.map.data %>%
  filter(MedicareInpatient == 1) %>%
  select(lng = PatientLongitude,
         lat = PatientLatitude)

ma.ed <- encounter.map.data %>%
  filter(MedicareAdvantageED == 1) %>%
  select(lng = PatientLongitude,
         lat = PatientLatitude)

ma.ip <- encounter.map.data %>%
  filter(MedicareAdvantageInpatient == 1) %>%
  select(lng = PatientLongitude,
         lat = PatientLatitude)


other.ed <- encounter.map.data %>%
  filter(OtherED == 1) %>%
  select(lng = PatientLongitude,
         lat = PatientLatitude)

other.ip <- encounter.map.data %>%
  filter(OtherInpatient == 1) %>%
  select(lng = PatientLongitude,
         lat = PatientLatitude)


encounter.map <-
  leaflet() %>%
  setView(lng = -100, lat = 30, zoom = 3, ) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Base Layer") %>%
  # addProviderTiles("Esri.WorldTopoMap", group = "Base Layer") %>%
  # addProviderTiles("Stamen.TonerLite", group = "Base Layer") %>%
  addRectangles(data = map.hospital.data,
                lng1 = ~lng1, lat1 = ~lat1,
                lng2 = ~lng2, lat2 = ~lat2,
                weight = 2,
                opacity = 0.5, fillOpacity = 0.25,
                popup = ~ FacilityName,
                color = "#00467f", fillColor = "#00467f") %>%
  addMarkers(data = m.ed,
             group = "Medicare Emergency",
             icon = pointer.icon,
             clusterOptions = markerClusterOptions()) %>%
  addMarkers(data = m.ip,
             group = "Medicare Inpatient",
             icon = pointer.icon,
             clusterOptions = markerClusterOptions()) %>%
  addMarkers(data = ma.ed,
             group = "Medicare Advantage Emergency",
             icon = pointer.icon,
             clusterOptions = markerClusterOptions()) %>%
  addMarkers(data = ma.ip,
             group = "Medicare Advantage Inpatient",
             icon = pointer.icon,
             clusterOptions = markerClusterOptions()) %>%
  addMarkers(data = other.ed,
             group = "Other Emergency",
             icon = pointer.icon,
             clusterOptions = markerClusterOptions()) %>%
  addMarkers(data = other.ip,
             group = "Other Inpatient",
             icon = pointer.icon,
             clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    overlayGroups = c("Medicare Emergency", "Medicare Inpatient",
                      "Medicare Advantage Emergency", "Medicare Advantage Inpatient",
                      "Other Emergency", "Other Inpatient"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Medicare Emergency", "Medicare Inpatient",
              "Other Emergency", "Other Inpatient"))


# Build layers function ----

buildDataset <- function(name, prefix, df, patient.type, patient.class){
  tbl.name <- paste0(prefix, ".", gsub(" ", ".", name))
  tbl.subset <-
    df %>%
    filter(PatientType == patient.type,
           PatientClass == patient.class,
           FacilityName == name) %>%
    select(lng = PatientLongitude,
           lat = PatientLatitude)
  list(facility.name = name, tbl = tbl.subset)
  # assign(df.name, df.subset, envir = .GlobalEnv)
}

# Leaflet ma.ed.encounter.map ----

pt.type <- "medicare advantage"
pt.class <- "emergency"
pt.prefix <- "ma.ed"


number.of.hospitals <- 10

hospitals <- as.character(ma.ed.hospital.data$FacilityName[1:number.of.hospitals])

ma.ed.list <- map(hospitals,
                  buildDataset, prefix = pt.prefix,
                  df = map.source.data,
                  patient.type = pt.type,
                  patient.class = pt.class)

ma.ed.encounter.map <-
  leaflet() %>%
  setView(lng = -100, lat = 30, zoom = 3, ) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Base Layer") %>%
  # addProviderTiles("Esri.WorldTopoMap", group = "Base Layer") %>%
  # addProviderTiles("Stamen.TonerLite", group = "Base Layer") %>%
  addRectangles(data = ma.ed.hospital.data,
                lng1 = ~lng1, lat1 = ~lat1,
                lng2 = ~lng2, lat2 = ~lat2,
                weight = 2,
                opacity = 0.5, fillOpacity = 0.25,
                popup = ~ FacilityName,
                color = "#00467f", fillColor = "#00467f")

for (i in 1:number.of.hospitals) {
  ma.ed.encounter.map <-
    ma.ed.encounter.map %>%
    addMarkers(lng = ma.ed.list[[i]][[2]]$lng,
               lat = ma.ed.list[[i]][[2]]$lat,
               group = as.character(ma.ed.list[[i]][1]),
               icon = pointer.icon,
               clusterOptions = markerClusterOptions())
}

ma.ed.encounter.map <-
  ma.ed.encounter.map %>%
  addLayersControl(
    overlayGroups = map_chr(ma.ed.list, "facility.name"),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(map_chr(ma.ed.list, "facility.name")[2:number.of.hospitals])




# Map MA ED facet ----

ranked.medicare.advantage.hospitals <-
  map.source.data %>%
  filter(IsMedicareAdvantage == 1,
         PatientClass == "emergency") %>%
  group_by(FacilityName, HospitalLongitude, HospitalLatitude) %>%
  summarize(count = n(),
            emergency.visits = sum(PatientClass == "emergency"),
            inpatient.visits = sum(PatientClass == "inpatient")) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(Rank = row_number()) %>%
  top_n(9, count)

df <- filter(map.source.data,
             IsMedicareAdvantage == 1,
             PatientLongitude < -121.5,
             PatientLatitude > 47,
             FacilityName %in% ranked.medicare.advantage.hospitals$FacilityName)

df <- droplevels(df)

# levels(df$FacilityName) <- map_chr(levels(df$FacilityName), wrapLines)

df$FacilityName <- fct_infreq(df$FacilityName)



map.object <- getMap(domain.data = df[, c("PatientLongitude", "PatientLatitude")],
                     type = "toner")

ma.ed.facet.map <-
  ggmap::ggmap(map.object$map) +
  geom_jitter(data = df,
              aes(x = PatientLongitude, y = PatientLatitude),
              size = 0.5, shape = 19, stroke = 0.2,
              color = "blue",
              width = 0.05, height = 0.03) +
  geom_point(data = ranked.medicare.advantage.hospitals,
             aes(x = HospitalLongitude, y = HospitalLatitude,
                 size = count),
             fill = NA,
             shape = 21, stroke = 1.5,
             alpha = 0.8,
             color = "darkorange")  +
  geom_point(data = ranked.medicare.advantage.hospitals,
             aes(x = HospitalLongitude, y = HospitalLatitude),
             size = 1,
             shape = 3, stroke = 0.5,
             alpha = 0.8,
             color = "darkorange")  +
  scale_size_area(guide = FALSE, max_size = 15) +
  facet_wrap(~ FacilityName, ncol = 3, labeller = label_wrap_gen()) +
  map.object$ebi.logo +
  geography.theme

# ma.ed.facet.map <-
# cowplot::plot_grid(ma.ed.facet.map, scale = 1.1)

ggsave(filename = "figures/ma.ed.top9.pdf",
       plot = ma.ed.facet.map,
       width = 7, height = 7,
       device = "pdf")


# Leaflet ma.inpatient.encounter.map ----

pt.type <- "medicare advantage"
pt.class <- "inpatient"
pt.prefix <- "ma.inpatient"


number.of.hospitals <- 10

hospitals <- as.character(ma.inpatient.hospital.data$FacilityName[1:number.of.hospitals])

ma.inpatient.list <- map(hospitals,
                         buildDataset, prefix = pt.prefix,
                         df = map.source.data,
                         patient.type = pt.type,
                         patient.class = pt.class)

ma.inpatient.encounter.map <-
  leaflet() %>%
  setView(lng = -100, lat = 30, zoom = 3, ) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Base Layer") %>%
  # addProviderTiles("Esri.WorldTopoMap", group = "Base Layer") %>%
  # addProviderTiles("Stamen.TonerLite", group = "Base Layer") %>%
  addRectangles(data = ma.inpatient.hospital.data,
                lng1 = ~lng1, lat1 = ~lat1,
                lng2 = ~lng2, lat2 = ~lat2,
                weight = 2,
                opacity = 0.5, fillOpacity = 0.25,
                popup = ~ FacilityName,
                color = "#00467f", fillColor = "#00467f")

for (i in 1:number.of.hospitals) {
  ma.inpatient.encounter.map <-
    ma.inpatient.encounter.map %>%
    addMarkers(lng = ma.inpatient.list[[i]][[2]]$lng,
               lat = ma.inpatient.list[[i]][[2]]$lat,
               group = as.character(ma.inpatient.list[[i]][1]),
               icon = pointer.icon,
               clusterOptions = markerClusterOptions())
}

ma.inpatient.encounter.map <-
  ma.inpatient.encounter.map %>%
  addLayersControl(
    overlayGroups = map_chr(ma.inpatient.list, "facility.name"),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(map_chr(ma.inpatient.list, "facility.name")[2:number.of.hospitals])




# MA inpatient facet ----

ranked.medicare.advantage.hospitals <-
  map.source.data %>%
  filter(IsMedicareAdvantage == 1,
         PatientClass == "inpatient") %>%
  group_by(FacilityName, HospitalLongitude, HospitalLatitude) %>%
  summarize(count = n(),
            emergency.visits = sum(PatientClass == "emergency"),
            inpatient.visits = sum(PatientClass == "inpatient")) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(Rank = row_number()) %>%
  top_n(9, count)

df <- filter(map.source.data,
             IsMedicareAdvantage == 1,
             PatientLongitude < -121.5,
             PatientLatitude > 47,
             FacilityName %in% ranked.medicare.advantage.hospitals$FacilityName)

df <- droplevels(df)

# levels(df$FacilityName) <- map_chr(levels(df$FacilityName), wrapLines)

df$FacilityName <- fct_infreq(df$FacilityName)



map.object <- getMap(domain.data = df[, c("PatientLongitude", "PatientLatitude")],
                     type = "toner")

ma.inpatient.facet.map <-
  ggmap::ggmap(map.object$map) +
  geom_jitter(data = df,
              aes(x = PatientLongitude, y = PatientLatitude),
              size = 0.5, shape = 19, stroke = 0.2,
              color = "blue",
              width = 0.05, height = 0.03) +
  geom_point(data = ranked.medicare.advantage.hospitals,
             aes(x = HospitalLongitude, y = HospitalLatitude,
                 size = count),
             fill = NA,
             shape = 21, stroke = 1.5,
             alpha = 0.8,
             color = "darkorange")  +
  geom_point(data = ranked.medicare.advantage.hospitals,
             aes(x = HospitalLongitude, y = HospitalLatitude),
             size = 1,
             shape = 3, stroke = 0.5,
             alpha = 0.8,
             color = "darkorange")  +
  scale_size_area(guide = FALSE, max_size = 15) +
  facet_wrap(~ FacilityName, ncol = 3, labeller = label_wrap_gen()) +
  map.object$ebi.logo +
  geography.theme

# ma.inpatient.facet.map <-
# cowplot::plot_grid(ma.inpatient.facet.map, scale = 1.1)

ggsave(filename = "figures/ma.inpatient.top9.pdf",
       plot = ma.inpatient.facet.map,
       width = 7, height = 7,
       device = "pdf")

