process_file <- function(x) {
  result <- xml2::read_xml(x)
  articles <- xml_find_all(result, ".//article")

  stopifnot(inherits(result, "xml_document"))
  message(stringr::str_glue("Processing {length(articles)} articles"))

  fields <- c(
    date = "publishedDate",
    title = "title",
    txt = "content",
    word_count = "wordCount",
    lang = "languageCode",
    author = "author/name",
    sequenceId = "sequenceId",
    id = "id",
    source_name = "source/name",
    source_id = "source/id",
    source_url = "source/homeUrl",
    source_category = "source/category",
    sentiment = "sentiment/score"
  )

  out <- map(fields, function(f) {
    search <- stringr::str_glue("//article/{f}")
    xml2::xml_text(xml2::xml_find_all(articles, search))
  })

  out <- dplyr::as_tibble(out) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("word_count", "sentiment")),
      as.numeric
    )) |>
    dplyr::mutate(date = as.Date(date))

  out$indexTerms <- articles |>
    xml2::xml_find_all("//article/indexTerms") |>
    purrr::map(function(it) {
      term <- it |>
        xml2::xml_find_all("indexTerm/name") |>
        xml2::xml_text()
      score <- it |>
        xml2::xml_find_all("indexTerm/score") |>
        xml2::xml_text() |>
        as.integer()
      tibble::tibble(term, score)
    })

  out$topics <- articles |>
    xml2::xml_find_all("//article/topics") |>
    purrr::map(function(to) {
      name <- to |>
        xml2::xml_find_all("topic/name") |>
        xml2::xml_text()
      group <- to |>
        xml2::xml_find_all("topic/group") |>
        xml2::xml_text()
      tibble::tibble(name, group)
    })

  out$entities <- articles |>
    xml2::xml_find_all("//article/semantics/entities") |>
    purrr::map(function(ent) {
      ent |>
        xml2::xml_find_all("entity/properties") |>
        purrr::map(function(x) {
          nms <- x |>
            xml2::xml_find_all("property/name") |>
            xml2::xml_text()
          val <- x |>
            xml2::xml_find_all("property/value") |>
            xml2::xml_text()
          names(val) <- nms
          return(val)
        }) |>
        dplyr::bind_rows()
    })

  out$sent_ents <- articles |>
    xml2::xml_find_all("//article/sentiment/entities") |>
    purrr::map(function(s) {
      nms <- c("type", "value", "mentions", "score", "evidence", "confident")
      sent_ents <- xml2::xml_find_all(s, "entity")
      nms |>
        purrr::map(\(x) xml2::xml_text(xml2::xml_find_all(sent_ents, x))) |>
        purrr::set_names(nms) |>
        dplyr::as_tibble()
    })

  return(out)
}
