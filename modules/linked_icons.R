# Warning: need to use {{}} fa somewhere on page to use these functions
import("htmltools", "tags")

linked_icon <- function(icon_name, href,brands = TRUE, colour = NA) {
  brands_text <- if(brands) "fa-brands" else "fa-solid"
  inline_style = if(is.na(colour)) "" else paste0("color:", colour)
  tags$a(
    tags$i(class = paste0(brands_text, " fa-", icon_name)),
    href = href, 
    style = inline_style,
    target = "_blank"
  )
}

linked_facebook_icon <- function(href) {
  linked_icon("facebook", href, TRUE, "#4267B2")
}

linked_twitter_icon <- function(href) {
  linked_icon("twitter", href, TRUE, "#1DA1F2")
  # linked_icon("square-x-twitter", href, TRUE, "#1DA1F2")
  # linked_icon("x-twitter", href, TRUE, "#1DA1F2")
}

linked_youtube_icon <- function(href) {
  linked_icon("youtube", href, TRUE, "#FF0000")
}

linked_globe_icon <- function(href) {
  linked_icon("globe", href, FALSE)
}

linked_spotify_icon <- function(href) {
  linked_icon("spotify", href, TRUE, "#1DB954")
}

linked_podcast_icon <- function(href) {
  linked_icon("podcast", href, FALSE)
}

linked_link_icon <- function(href) {
  linked_icon("link", href, FALSE)
}