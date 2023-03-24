library(rvest)
library(tidyverse)


urls <- c("https://sylabus.amu.edu.pl/pl/1/19/3/2",
          "https://sylabus.amu.edu.pl/pl/1/19/3/3",
          "https://sylabus.amu.edu.pl/pl/1/19/3/4",
          "https://sylabus.amu.edu.pl/pl/1/19/3/6",
          "https://sylabus.amu.edu.pl/pl/1/19/4/3",
          "https://sylabus.amu.edu.pl/pl/1/19/4/4")

forma_i_poziom <- c("stacjonarne I stopnia",
                    "stacjonarne II stopnia",
                    "stacjonarne I stopnia (inzynierskie)",
                    "stacjonarne II stopnia (inzynierskie)",
                    "niestacjonarne II stopnia",
                    "niestacjonarne I stopnia (inzynierskie)")

pages <- vector("list", length(urls))
links <- vector("list", length(urls))
linki_wydzialow <- vector("list", length(urls))


for (i in seq_along(urls)) {
  pages[[i]] <- read_html(urls[i])
  
  links[[i]] <- pages[[i]] %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep(paste0(substr(urls[i], 27, 38),"/"), ., value = TRUE)
  
  linki_wydzialow[[i]] <- paste0("https://sylabus.amu.edu.pl", links[[i]])
}

names(linki_wydzialow) <- forma_i_poziom
linki_wydzialow <- as.data.frame(stack(linki_wydzialow))
names(linki_wydzialow) <- c("links","forma_i_poziom")

forma_i_poziom <- linki_wydzialow$forma_i_poziom
linki_wydzialow <- as.list(linki_wydzialow$links)
names(linki_wydzialow) <- forma_i_poziom

strony_wydzialow <- vector("list", length(linki_wydzialow))
linki_kierunkow <- vector("list", length(linki_wydzialow))
full_links <- vector("list", length(linki_wydzialow))

for (i in seq_along(linki_wydzialow)) {
  strony_wydzialow[[i]] <- read_html(linki_wydzialow[[i]])
  
  linki_kierunkow[[i]] <- strony_wydzialow[[i]] %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep(paste0(substr(linki_wydzialow[[i]], 27, 41),"/"), ., value = TRUE)
 
  full_links[[i]] <- paste0("https://sylabus.amu.edu.pl", linki_kierunkow[[i]])
}


names(full_links) <- forma_i_poziom
full_links <- full_links[full_links!= "https://sylabus.amu.edu.pl"]
full_links <- as.data.frame(stack(full_links))
names(full_links) <- c("links","forma_i_poziom")

links_list <- as.list(full_links$links)

UAM <- vector("list", length(links_list))


for (i in seq_along(links_list)) {
  UAM[[i]] <- read_html(links_list[[i]])
  
}

sylabus_UAM <- vector("list", length(links_list))
names <- vector("list", length(links_list))

for (i in seq_along(links_list)) {
  tryCatch({
    sylabus_UAM[i] <- UAM[[i]] %>% html_elements(css = "#pills-tabContent") %>% 
      html_table()
    
    names[i] <- UAM[[i]] %>% html_elements(css = ".syl-major-name-label") %>%
      html_text() %>% 
      str_trim() %>% 
      str_replace_all("\\s+", " ")
  }, error = function(e) {
    sylabus_UAM[[i]] <- "Not found"
    names[i] <- "Not found"
  })
}

names(sylabus_UAM) <- paste0(names," (",full_links$forma_i_poziom,")")

sylabus_UAM <- sylabus_UAM[!grepl("NULL",names(sylabus_UAM))]

sylabus_UAM_exp <- bind_rows(sylabus_UAM, .id = "Forma_i_poziom")

library(writexl)

setwd("D:\\Education Data Solutions")
write.csv(sylabus_UAM_exp, file = "Sylabus_UAM.csv")
writexl::write_xlsx(sylabus_UAM_exp, path ="Sylabus_UAM.xlsx")



