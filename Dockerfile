FROM rocker/shiny-verse:3.6.1

# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "install.packages(c('rvest', 'rjson', 'scales'), repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('abresler/nbastatR')"

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/

# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

# COPY test_script.R /srv/shiny-server/test_script.R
# RUN Rscript /srv/shiny-server/test_script.R

CMD ["/usr/bin/shiny-server.sh"]