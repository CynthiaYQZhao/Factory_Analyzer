getName <- function(glb) {
  if(glb$resume_f %>% is.null()) ""
  else str_split(glb$resume_f,",")[[1]][1]
}

scoreResult = function(glb) {
  # case <- read_csv("courseDescription.csv")
  # resume_f <- read_file("resume.txt")
  
  # make resume content a dataframe
  resume_fdf <- tibble(title = getName(glb), description = glb$resume_f)
  
  # combine resume and job description
  case_resume <- rbind(resume_fdf, glb$case)
  
  # data cleaning function
  prep_fun = function(x) {
    x %>%
      # make text lower case
      str_to_lower() %>%
      # remove non-alphanumeric symbols
      str_replace_all("[^[:alnum:]]", " ") %>%
      # remove numbers
      {
        gsub(patter = "\\d", replace = " ", .)
      } %>%
      # remove stopwords
      removeWords(stopwords()) %>%
      # remove single character
      {
        gsub(patter = "\\b[A-z]\\b{1}", replace = " ", .)
      } %>%
      # collapse multiple spaces
      str_replace_all("\\s+", " ") %>%
      # lemmatization
      lemmatize_strings()
  }
  
  # clean the job description data and create a new column
  case_resume$description_clean = lemmatize_words(prep_fun(case_resume$description))
  
  # use vocabulary_based vectorization
  it_resume <-
    itoken(case_resume$description_clean, progressbar = FALSE)
  v_resume <- create_vocabulary(it_resume)
  
  # eliminate very frequent and very infrequent terms
  # v_resume = prune_vocabulary(v_resume, doc_proportion_max = 0.1, term_count_min = 5)
  v_resume <- prune_vocabulary(v_resume)
  vectorizer_resume <- vocab_vectorizer(v_resume)
  
  # apply TF-IDF transformation
  dtm_resume <- create_dtm(it_resume, vectorizer_resume)
  tfidf <- TfIdf$new()
  dtm_tfidf_resume <- fit_transform(dtm_resume, tfidf)
  
  # compute similarity-score against each row
  resume_tfidf_cos_sim = sim2(x = dtm_tfidf_resume, method = "cosine", norm = "l2")
  
  # create a new column for similarity_score of dataframe
  case_resume <- case_resume %>%
    mutate(similarity_score = resume_tfidf_cos_sim[1:nrow(resume_tfidf_cos_sim)])
  
  # calculate Percentile_Rank
  case_resume <- case_resume %>%
    mutate(Percentile_Rank = trunc(rank(similarity_score)) / length(similarity_score))
  
  # sort the dataframe by similarity score from the lowest to the highest
  case_resume_result <- case_resume %>%
    arrange(Percentile_Rank %>% desc()) %>%
    filter(title != getName(glb)) 
  
  return(case_resume_result)
  
}

getRankBarPlot <- function(dat, input) {
  s = input$case_resume_result_rows_selected
  if (length(s))
    data <- dat[s, ]
  else
    data <- dat %>% head(10)
  data %>% plot_ly(
    x = ~ Percentile_Rank,
    y = ~ reorder(title, Percentile_Rank),
    name = "Similarity Percentile Rank",
    type = "bar"
  )
  
}

#########Shiny Server##########

shinyServer(function(input , output, session) {
  glb <- reactiveValues()
  
  restab <- eventReactive(input$go, {
    req(input$upload_resume)
    req(input$upload_syllabus)
    glb$case <- read.csv(input$upload_syllabus$datapath, stringsAsFactors = F)
    glb$resume_f <- read_file(input$upload_resume$datapath)
    scoreResult(glb)
  })
  
  output$candidate_name <- renderText({
    getName(glb)
  })
  
  output$case_resume_result <- renderDataTable({
    restab() %>%
      select(c(title, similarity_score, Percentile_Rank)) %>%
      datatable(
        extensions = 'Buttons',
        options = list(
          pageLength = -1, 
          lengthMenu = list(c(10, 20, 50, 100, -1), c("10", "20", "50", "100", "All")) 
        )
      ) %>%
      formatPercentage("Percentile_Rank", 2)
  },
  server = TRUE)
  
  output$rankBarChart <- renderPlotly({
    restab() %>%
      select(c(title, similarity_score, Percentile_Rank)) %>% 
      getRankBarPlot(input)
  })
  
  output$reportTitle <-
    renderUI(expr = if (!is.null(glb$resume_f)) {
      tags$h2("Course Matching Report for ", textOutput("candidate_name"))
    } else {
      NULL
    })
  
  output$formatBtn <-
    renderUI(expr = if (!is.null(glb$resume_f)) {
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word', 'Excel'), inline = TRUE)
    } else {
      NULL
    })
  
  output$downloadBtn <-
    renderUI(expr = if (!is.null(glb$resume_f)) {
      downloadButton("downloadReport", paste("Download ", input$format  ," Scoring Result"))
    } else {
      NULL
    })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(getName(glb), "_Scoring_Result.", switch(
      input$format,
      PDF = 'pdf',
      HTML = 'html',
      Word = 'docx',
      Excel = 'xlsx'
      ), sep = '') %>% {
      gsub(" ", "_", .)
    }},
    content = function(file1) {
      if (input$format == "Excel") {
        write_xlsx(restab(), file1)
      } else{
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        src <- normalizePath('report_temp.Rmd')
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report_temp.Rmd', overwrite = TRUE)
        
        params <- list(
          studentName = getName(glb), 
          today = Sys.Date(),
          resultData = restab(), 
          outputFormat = input$format)
        
        out <- render('report_temp.Rmd', switch(
          input$format,
          PDF = pdf_document(),
          # HTML = html_document(),
          HTML = downcute(),
          Word = word_document()
        ), 
        params = params,
        envir = new.env(parent = globalenv())
        )
        file.rename(out, file1)
      }
    }, 
    contentType = NA
  )
  
})