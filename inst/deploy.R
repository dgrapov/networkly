#deploy
devtools::install_bitbucket("dmitry_grapov/networkly.git" , password = '')
#cp
system('sudo cp -R /usr/local/lib/R/site-library/networkly/. /srv/apps/networkly')
