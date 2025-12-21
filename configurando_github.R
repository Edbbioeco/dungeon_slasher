# Pacotes ----

library(usethis)

# Dados ----

usethis::use_git_config(user.name = "Edbbioeco",
                        user.email = "edsonbbiologia@gmail.com")

# Configurando token ----

usethis::create_github_token()

# Inicializando ----

usethis::use_git()

usethis::use_github_links()

# Setando o repositório ----

usethis::use_git_remote(name = "origin",
                        url = "https://github.com/Edbbioeco/dungeon_slasher",
                        overwrite = TRUE)

# Sincronizando os arquivos ----

# no terminal: git pull origin main --allow-unrelated-histories

# no terminal: git push -u origin main
