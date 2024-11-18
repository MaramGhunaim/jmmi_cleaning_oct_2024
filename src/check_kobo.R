check_answer_in_list <- function(questions, choices, constraint){
  
  if(!str_detect(constraint, ",")) return(T)
  
  question_regex <- "\\{([^()]+)\\}"
  answer_regex <- "\\'([^()]+)\\'"
  
  question <- gsub(question_regex, "\\1", str_extract_all(constraint, question_regex)[[1]])
  answer <- gsub(answer_regex, "\\1", str_extract_all(constraint, answer_regex)[[1]])
  
  question_type <- questions %>% 
    filter(name==question) %>% 
    filter(!grepl("^(begin|end)\\s+group$",type)) %>% 
    pull(type)
  
  if (question_type=="calculate") {
    return(T)
  } else {
    listname <- gsub("^.*\\s", "", question_type)
    choices_list <- choices %>% filter(list_name==listname) %>% pull(name)
    return(answer %in% choices_list)
  }
  
}

check_constraints <- function(questions, choices){
  
  questions <- mutate_at(questions, c("name", "type"), ~str_trim(.))
  choices <- mutate_at(choices, c("list_name", "name"), ~str_trim(.))
  
  all_contraints <- questions %>% filter(grepl("selected",relevant)) %>% pull(relevant)
  all_contraints <- gsub('"',"'",all_contraints)
  rs_list <- map(all_contraints,~map_lgl(unlist(ex_default(.x, pattern = "selected\\s*\\([^\\)]*\\)")),
                                         ~check_answer_in_list(questions, choices, .)))
  
  map2(rs_list,seq_along(rs_list), 
       ~if(length(which(!.x))!=0){
         return(unlist(ex_default(all_contraints[.y], pattern = "selected\\s*\\([^\\)]*\\)"))[which(!.x)])
       }) %>% 
    unlist() %>% unique()
}
