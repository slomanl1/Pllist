#Identify the url from where you want to extract data
base_url='https://www.intporn.org/threads/my-hot-video-collection-exclusive-and-very-hard-to-find-part-2.1031152/page-1612'
webpage <- read_html(base_url)

# Get the artist name
artist <- html_nodes(webpage, ".chart-row__artist")
artist <- as.character(html_text(artist))

# Get the artist rank
rank <- html_nodes(webpage, ".chart-row__rank")
rank <- as.numeric(html_text(rank))

# Save it to a tibble
top_artists <- tibble('Artist' = gsub("\n", "", artist),   #remove the \n character in the artist's name
                      'Rank' = rank) %>%
  filter(rank <= 10)
