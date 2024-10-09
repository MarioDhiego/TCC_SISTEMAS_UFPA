###### Como configurar o Git e GitHub no RStudio #############

# Instalar o usethis - Caso não tenha instalado
install.packages("usethis")

# Carregar o pacote usethis
library(usethis)

# Se apresentar para o git
usethis::use_git_config(user.name = "MarioDhiego",
                        user.email = "mario.valente@detran.pa.gov.br")

# Abrir o  arquivo .Renviron
usethis::edit_r_environ()

# Criar o token do github
#usethis::browse_github_token()
usethis::create_github_token()

# Salvar o token no arquivo .Renviron, usando este padrão (sem o #):
# GITHUB_PAT=
# Certifique-se que o arquivo .Renviron termine com uma
# linha em branco

# Reinicie o RStudio: CTRL + SHIFT + F10
# Pronto :)

# Versionar e configurar o Rstudio com o githtb
usethis::use_git()

# Conectar ao github
usethis::use_github()








