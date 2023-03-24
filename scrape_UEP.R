library(rvest)
library(tidyverse)


urls <- c("https://www.e-sylabus.ue.poznan.pl/pl/10/1/1",
          "https://www.e-sylabus.ue.poznan.pl/pl/10/1/2",
          "https://www.e-sylabus.ue.poznan.pl/pl/10/1/3",
          "https://www.e-sylabus.ue.poznan.pl/pl/10/1/4",
          "https://www.e-sylabus.ue.poznan.pl/pl/10/2/1",
          "https://www.e-sylabus.ue.poznan.pl/pl/10/2/2",
          "https://www.e-sylabus.ue.poznan.pl/pl/10/2/3",
          "https://www.e-sylabus.ue.poznan.pl/pl/10/2/4")

forma_i_poziom <- c("stacjonarne I stopnia",
                    "stacjonarne II stopnia",
                    "stacjonarne I stopnia (inzynierskie)",
                    "stacjonarne II stopnia (inzynierskie)",
                    "niestacjonarne I stopnia",
                    "niestacjonarne II stopnia",
                    "niestacjonarne I stopnia (inzynierskie)",
                    "niestacjonarne II stopnia (inzynierskie)")

pages <- vector("list", length(urls))
links <- vector("list", length(urls))
full_links <- vector("list", length(urls))

for (i in seq_along(urls)) {
  pages[[i]] <- read_html(urls[i])
  
  links[[i]] <- pages[[i]] %>%
    html_nodes("a") %>%
    html_attr("href")%>%
    grep(paste0(substr(urls[i], 35, 44),"/"), ., value = TRUE)

  full_links[[i]] <- paste0("https://www.e-sylabus.ue.poznan.pl", links[[i]])  
}

names(full_links) <- forma_i_poziom
full_links <- as.data.frame(stack(full_links))
names(full_links) <- c("links","forma_i_poziom")

links_list <- as.list(full_links$links)

UEP <- vector("list", length(links_list))


for (i in seq_along(links_list)) {
  UEP[[i]] <- read_html(links_list[[i]])
  
}

sylabus_UEP <- vector("list", length(links_list))
names <- vector("list", length(links_list))

for (i in seq_along(links_list)) {
  sylabus_UEP[i] <- UEP[[i]] %>% html_elements(css = "#pills-tabContent") %>% 
    html_table()
  
  names[i] <- UEP[[i]] %>% html_elements(css = ".syl-major-name-label") %>%
    html_text() %>% 
    str_trim() %>% 
    str_replace_all("\\s+", " ")
  
}

names(sylabus_UEP) <- names
names(sylabus_UEP) <- paste0(names(sylabus_UEP)," (",full_links$forma_i_poziom,")")
sylabus_UEP

sylabus_UEP_exp <- bind_rows(sylabus_UEP, .id = "Forma_i_poziom")
sylabus_UEP_exp

library(writexl)

setwd("D:\\Education Data Solutions")
write.csv(sylabus_UEP_exp, file = "Sylabus_UEP.csv")

writexl::write_xlsx(sylabus_UEP_exp, path ="Sylabus_UEP.xlsx")
writexl::write_xlsx(sylabus_UEP, path ="Sylabus_UEP_Sep.xlsx")




