convertColTypes <- function(data, tool.survey){
  # select_multiple: numeric or factor?
  col.types <- data.frame(column=colnames(data)) %>% 
    left_join(select(tool.survey, name, type), by=c("column"="name")) %>% 
    mutate(type.edited = case_when(
      type %in% c("integer", "decimal", "calculate") ~ "numeric",
      str_starts(type, "select_") ~ "factor",
      str_detect(column, "/") ~ "factor",
      TRUE ~ "text"))
  
  cols <- col.types[col.types$type.edited=="numeric", "column"]
  data[,cols] <- lapply(data[,cols], as.numeric)
  cols <- col.types[col.types$type.edited=="text", "column"]
  data[,cols] <- lapply(data[,cols], as.character)
  cols <- col.types[col.types$type.edited=="factor", "column"]
  data[,cols] <- lapply(data[,cols], as.factor)
  
  return(data)
}

# for each survey, it finds the closest matching survey with the minimum number of different columns
find_similar_surveys <- function(data.main, tool.survey, uuid="_uuid"){
  
  data <- data.main
  
  # 1) store UUIDs
  uuids <- data[[uuid]]
  
  # 2) convert all columns to character and tolower
  data <- mutate_all(data, as.character)
  data <- mutate_all(data, tolower)
  
  # 3) remove columns that are naturally different in each survey:
  # - columns of type = "start", "end", etc.
  # - columns starting with "_"
  # - option columns for the select multiple -> keeping only the concatenation column
  types_to_remove <- c("start", "end", "today", "deviceid", "date", "geopoint", "audit", 
                       "note", "calculate")
  cols_to_keep <- data.frame(column=colnames(data)) %>% 
    left_join(select(tool.survey, name, type), by=c("column"="name")) %>% 
    filter(!(type %in% types_to_remove) & 
             !str_starts(column, "_") & !str_detect(column, "/") & 
             !str_ends(column, "_other") & !(column %in% c("audit_URL")))
  data <- data[, all_of(cols_to_keep$column)]
  
  # 4) remove columns with all NA; convert remaining NA to "NA"; convert all columns to factor
  data <- data[, colSums(is.na(data))<nrow(data)]
  data[is.na(data)] <- "NA"
  data <- data %>% mutate_if(is.character, factor)
  error.message <- "NAs detected, remove them before proceeding (it can happen when converting to factor)"
  if (sum(is.na(data))>0) stop(error.message)
  
  # 5) calculate gower distance
  gower_dist <- daisy(data, metric="gower", warnBin=F, warnAsym=F, warnConst=F)
  gower_mat <- as.matrix(gower_dist)
  
  # 6) convert distance to number of differences and determine closest matching survey
  r <- unlist(lapply(1:nrow(data), function(i){
    srv1 <- sort(gower_mat[i,]*ncol(data))[1]
    srv2 <- sort(gower_mat[i,]*ncol(data))[2]
    if (names(srv1)==as.character(i)) return(srv2)
    else return(srv1)
    }))
  
  # 7) add relevant columns
  data[["num_cols_not_NA"]] <- rowSums(data!="NA")
  data[[uuid]] <- uuids
  data[["_id_most_similar_survey"]] <- uuids[as.numeric(names(r))]
  data[["number_different_columns"]] <- as.numeric(r)
  data <- data %>% arrange(number_different_columns, uuid)
  
  return(data)
}

check.soft.duplicates <- function(data.main, cols, ids, uuid="_uuid", only_differences=F){
  data <- data.main
  check <- data %>% filter(!!sym(uuid) %in% ids) %>% t() %>% as.data.frame()
  check$num.unique <- unlist(lapply(1:nrow(check), function(r)
    length(unique(as.character(check[r, all_of(colnames(check))])))))
  check <- check[(rownames(check) %in% cols),]
  if (only_differences){
    check <- check %>% filter(num.unique!=1) %>% select(-num.unique)
  } else{
    check <- check %>% arrange(-num.unique)
  }
  return(check)
}

# silhouette analysis based on gower distance between surveys
# METHOD: check for anomalies using the silhouette function. We assume the dataset is clustered using the 
# enumerator IDs as the cluster IDs and we calculate the silhouette for this clustering scenario. A 
# silhouette value close to 1 indicates that the entries of the cluster are very similar to each other and 
# very dissimilar from entries of other clusters. Thus, we need to raise a flag if the silhouette value gets 
# close to 1 for any of the clusters/enumerators.
# https://en.wikipedia.org/wiki/Silhouette_(clustering)
# https://dpmartin42.github.io/posts/r/cluster-mixed-types
# https://medium.com/@rumman1988/clustering-categorical-and-numerical-datatype-using-gower-distance-ab89b3aa90d9
calculateEnumeratorSimilarity <- function(data, tool.survey, col_enum, col_admin="adm"){
  # convert columns using the tool
  data <- convertColTypes(data, tool.survey)
  # keep only relevant columns
  cols <- data.frame(column=colnames(data)) %>% 
    left_join(select(tool.survey, name, type), by=c("column"="name")) %>% 
    filter(!(type %in% c("date", "start", "end", "today", 
                         "audit", "note", "calculate", "deviceid", "geopoint")) &
             !str_starts(column, "_"))
  # convert character columns to factor and add enum.id
  data <- data[, all_of(cols$column)] %>% 
    mutate_if(is.character, factor) %>% 
    arrange(!!sym(col_enum)) %>%
    mutate(enum.id=as.numeric(!!sym(col_enum)), .after=!!sym(col_enum))
  # add "SY" column in case col_admin is not specified
  if (col_admin=="adm") data <- mutate(data, adm="SY", .before=cols$column[1])
  # calculate similarity (for enumerators who completed at least 5 surveys)
  res <- data %>% split(data[[col_admin]]) %>% 
    lapply(function(gov){
      df <- gov %>% 
        group_by(enum.id) %>% mutate(n=n()) %>% filter(n>=5) %>% ungroup() %>% 
        select_if(function(x) any(!is.na(x)))
      if (length(unique(df$enum.id)) > 1){
        # calculate gower distance
        gower_dist <- daisy(select(df, -c(!!sym(col_enum), enum.id)), 
                            metric = "gower", warnBin = F, warnAsym = F, warnConst = F)
        # gower_mat <- as.matrix(gower_dist)
        # calculate silhouette
        si <- silhouette(df$enum.id, gower_dist)
        res.si <- summary(si)
        # create output
        r <- data.frame(enum.id=as.numeric(names(res.si$clus.avg.widths)), si=res.si$clus.avg.widths) %>% 
          left_join(distinct(select(df, !!sym(col_admin), !!sym(col_enum), enum.id)), by="enum.id") %>% 
          left_join(group_by(df, enum.id) %>% summarise(num.surveys=n(), .groups="drop_last"), by="enum.id") %>% 
          select(!!sym(col_admin), !!sym(col_enum), num.surveys, si) %>% arrange(-si)
        return(r)}})
  return(do.call(rbind, res))
}