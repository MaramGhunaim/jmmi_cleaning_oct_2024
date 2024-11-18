add.to.cleaning.log <- function(checks, check.id, question.names=c(), issue=""){
  if (nrow(filter(checks, flag))>0){
    for(q.n in question.names){
      new.entries <- checks %>% dplyr::filter(flag) %>%
        dplyr::mutate(uuid=uuid,
               variable=q.n,
               issue=issue,
               old.value=!!sym(q.n),
               new.value=NA)

      new.entries[["check.id"]] <- check.id
      new.entries[["rel.index"]] <- NA
      new.entries <- new.entries %>% select(uuid, rel.index, check.id, variable, issue, old.value, new.value)
      cleaning.log <<- rbind(cleaning.log, new.entries)
    }
  }
}

add.to.cleaning.log.other.recode <- function(x){
  if (as.character(x$q.type)=="select_one")
    add.to.cleaning.log.other.recode.one(x)
  else if (!is.na(x$q.type) & as.character(x$q.type) == "select_multiple")
    add.to.cleaning.log.other.recode.multiple(x)
  else stop("q.type unknown")
}

add.to.cleaning.log.other.recode.one <- function(x){
  #stop("Check and update function if needed")
  choice <- trimws(str_split(x$existing.other, ";")[[1]][1])
  available.choices <- filter(tool.choices, list_name==x$list_name)
  other.name <- ifelse("Other" %in% available.choices$name, "Other", "other")
  new.code <- filter(tool.choices, list_name==x$list_name & `label::english`==choice)
  if (dim(new.code)[1]!=1)
    stop(paste0("Choice is not in the list. ID: ", x$uuid, "; recode.into: ", choice))
  else{
    # set arabic text to NA
    df <- data.frame(uuid=x$uuid, rel.index=x$rel.index, variable=x$name, old.value=x$response.ar, new.value=NA, issue="Recode of other response")
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    # set new choice
    df <- data.frame(uuid=x$uuid, rel.index=x$rel.index, variable=x$ref.question,
                     old.value=other.name, new.value=new.code$name, issue="Recode of other response")
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
}

add.to.cleaning.log.other.recode.multiple <- function(x){
  if (!(x$ref.question %in% colnames(raw))){
    stop("Recode other: the column is not in the raw file but in one of the loops. The funtion needs to be edited.")
  } else{
    # get list of available choices and name of other (either "other" or "Other")
    available.choices <- filter(tool.choices, list_name==x$list_name)
    other.name <- ifelse("Other" %in% available.choices$name, "Other", "other")
    # get list of choices from other response
    choices <- trimws(str_split(x$existing.other, ";")[[1]])
    choices <- choices[choices!=""]
    # set arabic text to NA
    df <- data.frame(uuid=x$uuid, rel.index=x$rel.index, variable=x$name, old.value=x$response.ar, new.value=NA, issue="Recode of other response")
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    # set variable/other to "0"
    df <- data.frame(uuid=x$uuid, rel.index=x$rel.index,
                     variable=paste0(x$ref.question, "/", other.name),
                     old.value="1", new.value="0", issue="Recode of other response")
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    # get list of choices already selected
    old.value <- as.character(raw[raw$uuid==as.character(x$uuid), x$ref.question])
    l <- str_split(old.value, " ")[[1]]
    l.cumulative <- l[l!=other.name]
    # add to the cleaning log each choice in the other response
    for (choice in choices){
      # set corresponding variable to "1" if not already "1"
      new.code <- filter(tool.choices, list_name==x$list_name & `label::english`==choice)
      if (nrow(new.code)!=1) stop(paste0("Choice is not in the list. ID: ", x$uuid,"; recode.into: ", choice))
      variable.name <- paste0(x$ref.question, "/", new.code$name)
      old.boolean <- as.character(raw[raw$uuid==as.character(x$uuid), variable.name])
      if (is.na(old.boolean) | old.boolean == "0"){
        df <- data.frame(uuid=x$uuid, rel.index=x$rel.index, variable=variable.name,
                         old.value=old.boolean, new.value="1", issue="Recode of other response")
        cleaning.log.other <<- rbind(cleaning.log.other, df)
      }
      # add choice to cumulative variable and remove duplicates
      l.cumulative <- unique(c(l.cumulative, new.code$name))
    }
    # update cumulative variable
    new.value <- paste(sort(l.cumulative), collapse=" ")
    df <- data.frame(uuid=x$uuid, rel.index=x$rel.index, variable=x$ref.question,
                     old.value=old.value, new.value=new.value, issue="Recode of other response")
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
}

add.to.cleaning.log.other.remove <- function(x){
  print(x$ref.question)
  if (!(x$ref.question %in% colnames(raw))){
    stop("Remove other: the column is not in the raw file but in one of the loops. The funtion needs to be edited.")
  } else{
    df <- data.frame(uuid=x$uuid, rel.index=x$rel.index, variable=x$name, old.value=x$response.ar, new.value=NA, issue="Remove of other response")
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    if (!is.na(x$q.type)){
      if (as.character(x$q.type)=="select_one"){
        df <- data.frame(uuid=x$uuid, rel.index=x$rel.index, variable=x$ref.question, old.value="Other", new.value=NA, issue="Remove of other response")
        cleaning.log.other <<- rbind(cleaning.log.other, df)
      } else if (as.character(x$q.type)=="select_multiple"){
        df <- data.frame(uuid=x$uuid, rel.index=x$rel.index, variable=paste0(x$ref.question, "/Other"),
                         old.value="1", new.value="0", issue="Remove of other response")
        cleaning.log.other <<- rbind(cleaning.log.other, df)
        old.value <- raw[[x$ref.question]][raw$uuid==as.character(x$uuid)]
        l <- str_split(old.value, " ")[[1]]
        new.value <- paste(l[l!="Other"], collapse=" ")
        df <- data.frame(uuid=x$uuid, rel.index=x$rel.index, variable=x$ref.question, old.value=old.value, new.value=new.value, issue="Remove of other response")
        cleaning.log.other <<- rbind(cleaning.log.other, df)
      }
    } else warning(paste0("q.type is NA for ", x$name))
  }
}

get.ref.question <- function(x){
  x.1 <- str_split(x, "\\{")[[1]][2]
  return(str_split(x.1, "\\}")[[1]][1])
}
get.choice.list.name <- function(x){
  x.1 <- str_split(x, " ")[[1]]
  if (length(x.1)==1) return(NA)
  else return(x.1[2])
}
get.q.type <- function(x) return(str_split(x, " ")[[1]][1])

get.select.db <- function(){
  # list of choices for each list_name (from TOOL_CHOICES)
  list.choices <- tool.choices %>% filter(!is.na(list_name)) %>% group_by(list_name) %>%
    dplyr::mutate(choices=paste(name, collapse=";\r\n"),
           choices.label=paste(`label::english`, collapse=";\r\n")) %>%
    dplyr::summarise(choices=choices[1], choices.label=choices.label[1])
  # list of choices for each question
  select.questions <- tool.survey %>% select(type, name) %>%
    mutate(q.type=as.character(lapply(type, get.q.type)),
           list_name=as.character(lapply(type, get.choice.list.name))) %>%
    filter(list_name!="NA" & list_name!="group" & list_name!="repeat") %>%
    left_join(list.choices, by="list_name") %>%
    filter(!is.na(choices))
  return(select.questions)
}

get.text.variables <- function(){
  text.vars <- tool.survey %>%
    filter(type=="text") %>%
    mutate(ref.question=as.character(lapply(relevant, get.ref.question))) %>%
    select(name, ref.question)

  return(text.vars)
}

get.other.db <- function(){
  select.questions <- get.select.db()
  notes <- tool.survey %>% select(name,type) %>% filter(str_detect(type, "note"))
  name_list <- list(notes$name)
  exclude.list <- c("enumerator","respondent_phone_number",
  "organisation_other"  )
  translate.only <- c(name_list
                      )

  # for each "other" question, get ref.question and list of choices
  other.db <- get.text.variables() %>%
    filter(!(name %in% exclude.list)) %>%
    left_join(select(select.questions, "name", "q.type", "list_name", "choices", "choices.label"),
              by=c("ref.question"="name")) %>%
    mutate_at(c("q.type", "list_name", "choices", "choices.label"),
              ~ifelse(name %in% translate.only, NA, .)) %>%
    select(name, ref.question, q.type, list_name, choices, choices.label)

  return(other.db)
}

get.label <- function(variable){
  return(var.labels[var.labels$name==variable, "label.full"])
}

choice.name2label <- function(list_name, name){
  return(as.character(tool.choices[tool.choices$list_name==list_name &
                                     tool.choices$name==name, "label::english"]))
}

choice.label2name <- function(list_name, label){
  return(as.character(tool.choices[tool.choices$list_name==list_name &
                                     tool.choices$`label::english`==label, "name"]))
}

get.old.value.label <- function(cl){
  for (r in 1:dim(cl)[1]){
    list.name <- as.character(cl[r, "list_name"])
    old.value <- as.character(cl[r, "old.value"])
    if (!is.na(list.name)){
      cl[r, "old.value"] <- choice.name2label(list.name, old.value)
    }
  }
  return(cl)
}

save.follow.up.requests <- function(){
  cols <- c("uuid", "submission.time", "Organization", "community",
            "Enumerator ID", "check.id", "variable", "variable.label",
            "issue", "old.value", "new.value", "feedback")
  # prepare cleaning log
  cl <- left_join(cleaning.log,
                  select(raw, uuid, "_submission_time",
                         Q1_1_1_enum_code, Q1_1_org_name, Q1_5_community),
                  by="uuid") %>%
    dplyr::rename(`Enumerator ID`=Q1_1_1_enum_code,
                  Organization=Q1_1_org_name,
                  community=Q1_5_community,
                  submission.time="_submission_time") %>%
    mutate(feedback="")
  # get list_name for select_one and select_multiple questions
  cl <- left_join(cl, select(get.select.db(), name, list_name), by=c("variable"="name"))
  cl <- get.old.value.label(cl)
  cl <- cl %>%
    left_join(select(tool.survey, "name", variable.label="label::english"), by=c("variable"="name")) %>%
    mutate(variable.label=ifelse(is.na(variable.label), "", variable.label)) %>%
    select(all_of(cols)) %>%
    arrange(community, submission.time, `Enumerator ID`, uuid)
  # save follow-up requests
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  input.style <- createStyle(fgFill="#DDFFDD", border="TopBottomLeftRight", borderColour="#000000")
  addStyle(wb, "Follow-up", style = input.style, rows = 2:(dim(cl)[1]+1), cols=(dim(cl)[2]-1))
  addStyle(wb, "Follow-up", style = input.style, rows = 2:(dim(cl)[1]+1), cols=dim(cl)[2])
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  setColWidths(wb, "Follow-up", cols=1, widths=35)
  setColWidths(wb, "Follow-up", cols=3, widths=18)
  setColWidths(wb, "Follow-up", cols=4, widths=14)
  setColWidths(wb, "Follow-up", cols=5, widths=14)
  setColWidths(wb, "Follow-up", cols=6, widths=10)
  setColWidths(wb, "Follow-up", cols=7, widths=25)
  setColWidths(wb, "Follow-up", cols=8, widths=50)
  setColWidths(wb, "Follow-up", cols=9, widths=50)
  setColWidths(wb, "Follow-up", cols=10, widths=25)
  setColWidths(wb, "Follow-up", cols=11, widths=20)
  setColWidths(wb, "Follow-up", cols=12, widths=30)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(dim(cl)[1]+1), cols=8)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(dim(cl)[1]+1), cols=9)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(dim(cl)[1]+1), cols=10)
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  col.id <- which(colnames(cl)=="old.value")
  random.color <- ""
  for (r in 2:dim(cl)[1]){
    if(as.character(cl[r, "check.id"])!="Outlier" &
       as.character(cl[r, "check.id"])!="Other.response" &
       as.character(cl[r, "check.id"])!="Typing" &
       as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) &
       as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"])){
      if (random.color == "") random.color <- randomColor(1, luminosity = "light")
      addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T),
               rows = r:(r+1), cols=col.id)
    } else random.color=""
  }
  saveWorkbook(wb,
               paste0("output/checking/", Sys.Date(), "_follow_up_requests.xlsx"),
               overwrite = TRUE)
}

save.deletion.log <- function(save.only.selected.submission.dates=F){
  if (save.only.selected.submission.dates)
    deletion.log <- filter(deletion.log, as.character(as.Date(`_submission_time`)) %in% submission.dates)
  wb <- createWorkbook()
  addWorksheet(wb, "Deletion.log")
  writeData(wb = wb, x = deletion.log, sheet = "Deletion.log", startRow = 1)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  addStyle(wb, "Deletion.log", style = col.style, rows = 1, cols=1:dim(deletion.log)[2])
  setColWidths(wb, "Deletion.log", cols=1:dim(deletion.log)[2], widths="auto")
  saveWorkbook(wb, filename.out.deletion.log, overwrite = TRUE)
}

save.other.responses <- function(){
  other.responses.j <- select(other.responses.j, -`_submission_time`)
  style.col.color <- createStyle(fgFill="#FFEEBB", border="TopBottomLeftRight", borderColour="#000000")
  style.col.color.first <- createStyle(textDecoration="bold", fgFill="#FFEEBB",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(wb = wb, x = other.responses.j, sheet = "Sheet1", startRow = 1)
  addStyle(wb, "Sheet1", style = style.col.color, rows = 1:(nrow(other.responses.j)+1), cols=11)
  idx.rows <- which(!is.na(other.responses.j$list_name))+1
  addStyle(wb, "Sheet1", style = style.col.color, rows = c(1, idx.rows), cols=12)
  addStyle(wb, "Sheet1", style = style.col.color, rows = c(1, idx.rows), cols=13)
  setColWidths(wb, "Sheet1", cols=1, widths=35)
  setColWidths(wb, "Sheet1", cols=3, widths=15)
  setColWidths(wb, "Sheet1", cols=4:5, widths=32)
  setColWidths(wb, "Sheet1", cols=6:7, widths=15)
  setColWidths(wb, "Sheet1", cols=8:10, widths=30)
  setColWidths(wb, "Sheet1", cols=11:13, widths=20)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T), rows = 1:(nrow(other.responses.j)+1), cols=8)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T), rows = 1:(nrow(other.responses.j)+1), cols=9)
  addStyle(wb, "Sheet1", style = createStyle(wrapText=T), rows = 1:(nrow(other.responses.j)+1), cols=10)
  addStyle(wb, "Sheet1", style = createStyle(textDecoration="bold"),
           rows = 1, cols=1:ncol(other.responses.j))
  addStyle(wb, "Sheet1", style = style.col.color.first, rows = 1, cols=11:13)
  saveWorkbook(wb,
               paste0("output/checking/", Sys.Date(), "_other_responses.xlsx"),
               overwrite = TRUE)
}

load_tool <- function(filename.tool){
  source("src/check_kobo.R")
  tool.survey <- read_excel(filename.tool, sheet="survey", col_types="text") %>% filter(!is.na(type))
  tool.choices <- read_excel(filename.tool, sheet="choices", col_types="text") %>%
    filter(!is.na(list_name)) %>% distinct() %>% select(list_name, name, `label::english`, `label::arabic`)
  t <- check_constraints(tool.survey, tool.choices)
  if (length(t)>0) warning(paste0("\n", "Issues with relevancies: ", t))
  tool.survey <- tool.survey %>%
    mutate(q.type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
           list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
           list_name=ifelse(str_starts(type, "select_"), list_name, NA))
  tool.survey <<- tool.survey
  tool.choices <<- tool.choices
}


detect.outliers <- function(df, method="sd", n.sd=3){
  res <- data.frame()

   colnames <-  get.col.values(df)

  method <- "sd-linear"
  n.sd=3
  df <- df %>%  select(uuid,all_of(colnames))
  for (col in colnames(df)[3:length(colnames(df))]){
    df.temp <- data.frame(uuid=df$uuid,value=as.numeric(df[[col]])) %>%
      filter(!is.na(value) & value>0) %>%
      mutate(value_log=log10(value))
    if (method=="sd-linear"){
      df.temp <- df.temp %>%
        mutate(is.outlier=ifelse(value > mean(value, na.rm=T) + n.sd*sd(value, na.rm=T) |
                                   value < mean(value, na.rm=T) - n.sd*sd(value, na.rm=T), T, F))

      } else if (method=="sd-log"){
      df.temp <- df.temp %>%
        dplyr::mutate(value_log_norm_abs=abs(value_log-mean(value_log))) %>%
        dplyr::mutate(exclude=value_log_norm_abs > quantile(value_log_norm_abs, 0.9))
      mean_log_adj <- mean(filter(df.temp, !exclude)$value_log)
      sd_log_adj <- sd(filter(df.temp, !exclude)$value_log)
      df.temp <- df.temp %>%
        dplyr::mutate(is.outlier=ifelse(value_log > mean_log_adj + n.sd*sd_log_adj |
                                   value_log < mean_log_adj - n.sd*sd_log_adj, T, F))
    } else stop("Method unknown")
    if(nrow(filter(df.temp, is.outlier))>0){
      df.temp <- filter(df.temp, is.outlier) %>%
        mutate(check.id="Outlier", issue="The issue is low or high",
               variable=col, old.value=value, new.value=NA) %>%
        select(uuid, check.id, variable, issue, old.value, new.value)
      res <- rbind(res, df.temp)

    }else{
      df.temp <- df.temp %>%
        dplyr::mutate(check.id="Outlier", issue="The issue is low or high",
               variable=col, old.value=value, new.value=NA) %>%
        dplyr::filter(is.outlier) %>%
        dplyr::select(uuid, check.id, variable, issue, old.value, new.value)
      res <- rbind(res, df.temp)
    }

  }
  return(res)

get.col.values <- function(df){



  currency.fertiliser <- c("currency_fertiliser")
  item.list.fertiliser <- c("expenditure_fertiliser")
  currency.pesticide <- c("currency_pesticide")
  item.list.pesticide <- c("expenditure_pesticide")
  currency.seeds <- c("currency_seeds")
  item.list.seeds <- c("expenditure_seeds")
  currency.labour <- c("currency_labour")
  item.list.labour <- c("expenditure_labour")
  currency.fuel <- c("currency_fuel")
  item.list.fuel <- c("expenditure_fuel")
  currency.electricity <- c("currency_electricity")
  item.list.electricity <- c("expenditure_electricity")
  currency.services <- c("currency_services")
  item.list.services <- c("expenditure_services")
  currency.productive_assets <- c("currency_productive_assets")
  item.list.productive_assets <- c("expenditure_productive_assets")
  currency.rent <- c("currency_rent")
  item.list.rent <- c("expenditure_rent")
  currency.fees_taxes <- c("currency_fees_taxes")
  item.list.fees_taxes <- c("expenditure_fees_taxes")
  currency.other <- c("currency_other")
  item.list.other <- c("expenditure_other")
  currency.other_2 <- c("currency_other_2")
  item.list.other_2 <- c("expenditure_other_2")

    col.values <- c (item.list.other_2,item.list.other,item.list.fees_taxes,
                     item.list.rent,item.list.productive_assets,item.list.fertiliser,
                     item.list.pesticide,item.list.seeds,item.list.labour,item.list.services,
                     item.list.fuel,item.list.electricity)
    print(col.values)
    return(col.values)
  }

}
check.outliers_new_boxplot <- function(df){


  col.values <- get.col.values(df)

  for (col in col.values){
    print(col)

    region.medians.northeast <- df %>%
      dplyr::filter(region == "northeast") %>% dplyr::summarise(median.price.comm=quantile(!!sym(col), 0.5, na.rm=TRUE))

    region.medians.northwest <- df %>%
      dplyr::filter(region == "northwest") %>% dplyr::summarise(median.price.comm=quantile(!!sym(col), 0.5, na.rm=TRUE))

    region.col <- df %>%
      dplyr::group_by(region) %>%dplyr::select((!!sym(col)),'region','X_uuid')%>%na.omit()
    outlier.northeast <- region.col %>% select((!!sym(col)),'region','X_uuid')  %>% dplyr::filter(region == "northeast")

    out_northeast  <-  boxplot.stats(outlier.northeast[[sym(col)]])$out

    outlier.northwest <- region.col %>% select((!!sym(col)),'region','X_uuid')  %>% dplyr::filter(region == "northwest")
    out_northwest  <-  boxplot.stats(outlier.northwest[[sym(col)]])$out

    region.col.northeast <- outlier.northeast[outlier.northeast[[sym(col)]] %in% out_northeast , ]
    region.col.northwest <- outlier.northwest[outlier.northwest[[sym(col)]] %in% out_northwest , ]

    if (!grepl("xrate",col)){
      region.col.northeast <- region.col.northeast %>%
        dplyr::mutate(issue.type = case_when(region.col.northeast[[sym(col)]] >= as.numeric(region.medians.northeast)~"Price seems too high",
                                             region.col.northeast[[sym(col)]] < as.numeric(region.medians.northeast)~"Price seems too low"))
      region.col.northwest <- region.col.northwest %>%
        dplyr::mutate(issue.type = case_when(region.col.northwest[[sym(col)]] >= as.numeric(region.medians.northwest)~"Price seems too high",
                                             region.col.northwest[[sym(col)]] < as.numeric(region.medians.northwest)~"Price seems too low"))
    }else {
      region.col.northeast <- region.col.northeast %>%
        dplyr::mutate(issue.type = case_when(region.col.northeast[[sym(col)]] > as.numeric(region.medians.northeast)~"Exchange rates seems too high",
                                             region.col.northeast[[sym(col)]] <= as.numeric(region.medians.northeast)~"Exchange rates seems too low"))
      region.col.northwest <- region.col.northwest %>%
        dplyr::mutate(issue.type = case_when(region.col.northwest[[sym(col)]] > as.numeric(region.medians.northwest)~"Exchange rates seems too high",
                                             region.col.northwest[[sym(col)]] <= as.numeric(region.medians.northwest)~"Exchange rates seems too low"))
    }

    region.col.allregions <- rbind(region.col.northeast,region.col.northwest)
    final_outlier_df <- df %>%  dplyr::select(X_index,
                                              q_orgname,
                                              q_enum_id,
                                              q_sbd,
                                              admin3Name_en,
                                              admin3Name_ar,
                                              q_town,
                                              admin4Name_en,
                                              admin4Name_ar,
                                              X_uuid,
                                              q_shop_currency_most_used) %>%  merge(region.col.allregions,by ='X_uuid')
    add.to.cleaning.log.outlier(final_outlier_df,col)




  }
}

check.outliers_new_boxplot_log <- function(df){


  col.values <- get.col.values(df)

  for (col in col.values){
    print(col)

    region.medians.northeast <- df %>%
      dplyr::filter(region == "northeast") %>% dplyr::summarise(median.price.comm=quantile(!!sym(col), 0.5, na.rm=TRUE))

    region.medians.northwest <- df %>%
      dplyr::filter(region == "northwest") %>% dplyr::summarise(median.price.comm=quantile(!!sym(col), 0.5, na.rm=TRUE))

    region.col <- df %>%
      dplyr::group_by(region) %>%dplyr::select((!!sym(col)),'region','X_uuid')%>%na.omit()
    outlier.northeast <- region.col %>% select((!!sym(col)),'region','X_uuid')  %>% dplyr::filter(region == "northeast")
    Log.variable_northeast <- log(as.numeric(outlier.northeast[[sym(col)]]))
    out_northeast  <-  boxplot.stats(Log.variable_northeast)$out
    newlist_northeast = list()
    for (i in out_northeast) {

      i = 10^i
      newlist_northeast = append(newlist_northeast,i)
    }

    out_northeast = newlist_northeast[]


    outlier.northwest <- region.col %>% select((!!sym(col)),'region','X_uuid')  %>% dplyr::filter(region == "northwest")
    Log.variable_northwest <- log(as.numeric(outlier.northwest[[sym(col)]]))
    out_northwest  <-  boxplot.stats(Log.variable_northwest)$out
    newlist = list()
    for (i in out_northwest) {

      i = 10^i
      newlist = append(newlist,i)
    }

    out_northwest = newlist[]

    region.col.northeast <- outlier.northeast[outlier.northeast[[sym(col)]] %in% out_northeast , ]
    region.col.northwest <- outlier.northwest[outlier.northwest[[sym(col)]] %in% out_northwest , ]

    if (!grepl("xrate",col)){
      region.col.northeast <- region.col.northeast %>%
        dplyr::mutate(issue.type = case_when(region.col.northeast[[sym(col)]] > as.numeric(region.medians.northeast)~"Price seems too high",
                                             region.col.northeast[[sym(col)]] <= as.numeric(region.medians.northeast)~"Price seems too low"))
      region.col.northwest <- region.col.northwest %>%
        dplyr::mutate(issue.type = case_when(region.col.northwest[[sym(col)]] > as.numeric(region.medians.northwest)~"Price seems too high",
                                             region.col.northwest[[sym(col)]] <= as.numeric(region.medians.northwest)~"Price seems too low"))
    }else {
      region.col.northeast <- region.col.northeast %>%
        dplyr::mutate(issue.type = case_when(region.col.northeast[[sym(col)]] > as.numeric(region.medians.northeast)~"Exchange rates seems too high",
                                             region.col.northeast[[sym(col)]] <= as.numeric(region.medians.northeast)~"Exchange rates seems too low"))
      region.col.northwest <- region.col.northwest %>%
        dplyr::mutate(issue.type = case_when(region.col.northwest[[sym(col)]] > as.numeric(region.medians.northwest)~"Exchange rates seems too high",
                                             region.col.northwest[[sym(col)]] <= as.numeric(region.medians.northwest)~"Exchange rates seems too high"))
    }

    region.col.allregions <- rbind(region.col.northeast,region.col.northwest)
    final_outlier_df <- df %>%  dplyr::select(X_index,
                                              q_orgname,
                                              q_enum_id,
                                              q_sbd,
                                              admin3Name_en,
                                              admin3Name_ar,
                                              q_town,
                                              admin4Name_en,
                                              admin4Name_ar,
                                              X_uuid,
                                              q_shop_currency_most_used) %>%  merge(region.col.allregions,by ='X_uuid')
    add.to.cleaning.log.outlier(final_outlier_df,col)




  }
}

check.outliers_new_hampel <- function(df){


  col.values <- get.col.values(df)

  for (col in col.values){
    print(col)

    region.medians.northeast <- df %>%
      dplyr::filter(region == "northeast") %>% dplyr::summarise(median.price.comm=quantile(!!sym(col), 0.5, na.rm=TRUE))

    region.medians.northwest <- df %>%
      dplyr::filter(region == "northwest") %>% dplyr::summarise(median.price.comm=quantile(!!sym(col), 0.5, na.rm=TRUE))

    region.col <- df %>%
      dplyr::group_by(region) %>%dplyr::select((!!sym(col)),'region','X_uuid')%>%na.omit()
    outlier.northeast <- region.col %>% select((!!sym(col)),'region','X_uuid')  %>% dplyr::filter(region == "northeast")

    lower_bound_northeast <- median(outlier.northeast[[sym(col)]]) - 3 * mad(outlier.northeast[[sym(col)]], constant = 1)
    upper_bound_northeast <- median(outlier.northeast[[sym(col)]]) + 3 * mad(outlier.northeast[[sym(col)]], constant = 1)

    out_northeast  <-  which(outlier.northeast[[sym(col)]] < lower_bound_northeast | outlier.northeast[[sym(col)]] > upper_bound_northeast)

    outlier.northwest <- region.col %>% select((!!sym(col)),'region','X_uuid')  %>% dplyr::filter(region == "northwest")
    lower_bound_northwest <- median(outlier.northwest[[sym(col)]]) - 3 * mad(outlier.northwest[[sym(col)]], constant = 1)
    upper_bound_northwest <- median(outlier.northwest[[sym(col)]]) + 3 * mad(outlier.northwest[[sym(col)]], constant = 1)

    out_northwest  <-  which(outlier.northwest[[sym(col)]] < lower_bound_northwest | outlier.northwest[[sym(col)]] > upper_bound_northwest)

    region.col.northeast <- outlier.northeast[outlier.northeast[[sym(col)]] %in% out_northeast , ]
    region.col.northwest <- outlier.northwest[outlier.northwest[[sym(col)]] %in% out_northwest , ]

    if (!grepl("xrate",col)){
      region.col.northeast <- region.col.northeast %>%
        dplyr::mutate(issue.type = case_when(region.col.northeast[[sym(col)]] > as.numeric(region.medians.northeast)~"Price seems too high",
                                             region.col.northeast[[sym(col)]] <= as.numeric(region.medians.northeast)~"Price seems too low"))
      region.col.northwest <- region.col.northwest %>%
        dplyr::mutate(issue.type = case_when(region.col.northwest[[sym(col)]] > as.numeric(region.medians.northwest)~"Price seems too high",
                                             region.col.northwest[[sym(col)]] <= as.numeric(region.medians.northwest)~"Price seems too low"))
    }else {
      region.col.northeast <- region.col.northeast %>%
        dplyr::mutate(issue.type = case_when(region.col.northeast[[sym(col)]] > as.numeric(region.medians.northeast)~"Exchange rates seems too high",
                                             region.col.northeast[[sym(col)]] <= as.numeric(region.medians.northeast)~"Exchange rates seems too low"))
      region.col.northwest <- region.col.northwest %>%
        dplyr::mutate(issue.type = case_when(region.col.northwest[[sym(col)]] > as.numeric(region.medians.northwest)~"Exchange rates seems too high",
                                             region.col.northwest[[sym(col)]] <= as.numeric(region.medians.northwest)~"Exchange rates seems too high"))
    }

    region.col.allregions <- rbind(region.col.northeast,region.col.northwest)
    final_outlier_df <- df %>%  dplyr::select(X_index,
                                              q_orgname,
                                              q_enum_id,
                                              q_sbd,
                                              admin3Name_en,
                                              admin3Name_ar,
                                              q_town,
                                              admin4Name_en,
                                              admin4Name_ar,
                                              X_uuid,
                                              q_shop_currency_most_used) %>%  merge(region.col.allregions,by ='X_uuid')
    add.to.cleaning.log.outlier(final_outlier_df,col)




  }
}

check.outliers_new_rosnertest <- function(df){


  col.values <- get.col.values(df)

  for (col in col.values){
    print(col)

    region.medians.northeast <- df %>%
      dplyr::filter(region == "northeast") %>% dplyr::summarise(median.price.comm=quantile(!!sym(col), 0.5, na.rm=TRUE))

    region.medians.northwest <- df %>%
      dplyr::filter(region == "northwest") %>% dplyr::summarise(median.price.comm=quantile(!!sym(col), 0.5, na.rm=TRUE))

    region.col <- df %>%
      dplyr::group_by(region) %>%dplyr::select((!!sym(col)),'region','X_uuid')%>%na.omit()
    outlier.northeast <- region.col %>% select((!!sym(col)),'region','X_uuid')  %>% dplyr::filter(region == "northeast")

    library(EnvStats)
    test_northeast <- rosnerTest(outlier.northeast[[sym(col)]],
                                 k = 3
    )
    result_northeast = data.frame(test_northeast$all.stats)




    outlier.northwest <- region.col %>% select((!!sym(col)),'region','X_uuid')  %>% dplyr::filter(region == "northwest")
    test_northwest <- rosnerTest(outlier.northwest[[sym(col)]],
                                 k = 3
    )

    result_northwest <- data.frame(test_northwest$all.stats)


    region.col.northeast <- outlier.northeast[outlier.northeast[[sym(col)]] %in% result_northeast$Value , ]
    region.col.northwest <- outlier.northwest[outlier.northwest[[sym(col)]] %in% result_northwest$Value , ]

    if (!grepl("xrate",col)){
      region.col.northeast <- region.col.northeast %>%
        dplyr::mutate(issue.type = case_when(region.col.northeast[[sym(col)]] > as.numeric(region.medians.northeast)~"Price seems too high",
                                             region.col.northeast[[sym(col)]] <= as.numeric(region.medians.northeast)~"Price seems too low"))
      region.col.northwest <- region.col.northwest %>%
        dplyr::mutate(issue.type = case_when(region.col.northwest[[sym(col)]] > as.numeric(region.medians.northwest)~"Price seems too high",
                                             region.col.northwest[[sym(col)]] <= as.numeric(region.medians.northwest)~"Price seems too low"))
    }else {
      region.col.northeast <- region.col.northeast %>%
        dplyr::mutate(issue.type = case_when(region.col.northeast[[sym(col)]] > as.numeric(region.medians.northeast)~"Exchange rates seems too high",
                                             region.col.northeast[[sym(col)]] <= as.numeric(region.medians.northeast)~"Exchange rates seems too low"))
      region.col.northwest <- region.col.northwest %>%
        dplyr::mutate(issue.type = case_when(region.col.northwest[[sym(col)]] > as.numeric(region.medians.northwest)~"Exchange rates seems too high",
                                             region.col.northwest[[sym(col)]] <= as.numeric(region.medians.northwest)~"Exchange rates seems too high"))
    }

    region.col.allregions <- rbind(region.col.northeast,region.col.northwest)
    region.col.allregions <- region.col.northwest
    final_outlier_df <- df %>%  dplyr::select(X_index,
                                              q_orgname,
                                              q_enum_id,
                                              q_sbd,
                                              admin3Name_en,
                                              admin3Name_ar,
                                              q_town,
                                              admin4Name_en,
                                              admin4Name_ar,
                                              X_uuid,
                                              q_shop_currency_most_used) %>%  merge(region.col.allregions,by ='X_uuid')
    add.to.cleaning.log.outlier(final_outlier_df,col)




  }
}
add.to.cleaning.log.outlier <- function(df, col.price){

  new.entries <- df %>%
    dplyr::mutate(index =X_index,
                  region = ifelse(region=="northeast", "NES", "NWS"),
                  uuid = X_uuid,
                  currency=q_shop_currency_most_used,
                  variable = col.price,
                  variable.explanation = "",
                  value = !!sym(col.price),
                  new.value = "",
                  explanation = "") %>%
    dplyr::select(index, region, q_orgname, q_enum_id, q_sbd, admin3Name_en, admin3Name_ar, q_town,
                  admin4Name_en, admin4Name_ar, uuid, currency,variable, variable.explanation, issue.type,
                  value, new.value, explanation)
  cleaning.log <<- rbind(cleaning.log, new.entries)
}
