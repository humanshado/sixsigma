# Six Sigma Studio in a container:
#   docker build -t six-sigma-studio .
#   docker run -p 3838:3838 six-sigma-studio      then open http://localhost:3838
FROM rocker/r-ver:4.4.1
RUN apt-get update && apt-get install -y --no-install-recommends pandoc libcurl4-openssl-dev libssl-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY R/requirements.R R/requirements.R
COPY install.R install.R
RUN Rscript install.R && Rscript -e 'install.packages("rmarkdown")'
COPY . .
RUN useradd --create-home shiny && chown -R shiny /app
USER shiny
EXPOSE 3838
HEALTHCHECK --interval=60s --timeout=10s CMD Rscript -e 'invisible(readLines("http://127.0.0.1:3838", n = 1))' || exit 1
CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
