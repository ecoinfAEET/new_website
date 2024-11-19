# 
# library(here)
# library(dplyr)
# library(OpenStreetMap)
# library(RJSONIO)

format_member_table <- function(member_table){
  member_table$inst <- gsub(" ", "+", member_table$Institución)
  member_table$inst <- gsub(",|-|\n", " ", member_table$Institución)
  member_table$city <- gsub(" ", "+", member_table$Ciudad)
  member_table$country <- gsub(" ", "+", member_table$País)
  member_table$afil_id <- paste(member_table$Institución, member_table$Ciudad, member_table$País, sep = "_")
  return(member_table)
}

look_up <- function(row) {
  # First URL including the 'inst' field
  url1 <- paste0(
    "http://nominatim.openstreetmap.org/search?street=", 
    row["inst"], 
    "&city=", 
    row["city"], 
    "&country=", 
    row["country"], 
    "&limit=9&format=json"
  )
  
  # Try the first API call
  resOSM <- tryCatch({
    fromJSON(url1)
  }, error = function(e) {
    list() # Return empty list on error
  })
  
  # If first search has results, return coordinates
  if (length(resOSM) > 0) {
    return(c(resOSM[[1]]$lon, resOSM[[1]]$lat))
  }
  
  # Secondary URL omitting the 'inst' field
  url2 <- paste0(
    "http://nominatim.openstreetmap.org/search?city=", 
    row["city"], 
    "&country=", 
    row["country"], 
    "&limit=9&format=json"
  )
  
  # Try the second API call
  resOSM2 <- tryCatch({
    fromJSON(url2)
  }, error = function(e) {
    list() # Return empty list on error
  })
  
  # If second search has results, return coordinates
  if (length(resOSM2) > 0) {
    return(c(resOSM2[[1]]$lon, resOSM2[[1]]$lat))
  }
  
  # If both searches fail, return NA
  return(rep(NA, 2))
}

member.table <- gsheet::gsheet2tbl("1tRlCcIxurHIDg-CCHsFFbkD3e4YkiDlA3PkFlTcBlk8") |>
  format_member_table()

# Apply the function row-wise
coords <- as.data.frame(t(apply(member.table, 1, look_up)))

colnames(coords)[colnames(coords) == "V1"] <- "lat"
colnames(coords)[colnames(coords) == "V2"] <- "lon"

coord_df <- cbind(member.table, coords)




