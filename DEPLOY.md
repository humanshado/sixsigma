# Deploying Six Sigma Studio

Six Sigma Studio is a standard [Shiny](https://shiny.posit.co/) app, so it runs
anywhere Shiny apps run. It has **no user accounts**: anyone who can open the
address can use it. Each browser session has its own worksheets; nothing is
written to the server's disk (uploads and projects stay in memory until the
session ends). On a shared server, put it behind your organisation's login
(a reverse proxy with single sign-on, Posit Connect, or similar).

## Docker

```bash
docker build -t six-sigma-studio .
```

```bash
docker run -d -p 3838:3838 --name six-sigma-studio six-sigma-studio
```

Then open http://localhost:3838. With Docker Compose: `docker compose up -d`.
The image is based on `rocker/r-ver:4.4.1`, installs the packages with
`install.R`, runs as a non-root user and has a health check.

## Shiny Server (open source)

1. Install R, then the packages: `sudo Rscript install.R` (from this folder).
2. Copy the folder to `/srv/shiny-server/six-sigma-studio`.
3. Open `http://<server>:3838/six-sigma-studio/`.

Shiny Server's default upload limit is set by the app (200 MB).

## Posit Connect or shinyapps.io

```r
install.packages("rsconnect")
rsconnect::deployApp(".", appName = "six-sigma-studio")
```

`rsconnect` finds the packages from the code. On shinyapps.io choose an
instance with at least 1 GB of memory if you expect large worksheets.

## Behind a reverse proxy

Shiny uses WebSockets, so the proxy must pass `Upgrade` and `Connection`
headers. For nginx:

```nginx
location /six-sigma-studio/ {
  proxy_pass http://127.0.0.1:3838/;
  proxy_http_version 1.1;
  proxy_set_header Upgrade $http_upgrade;
  proxy_set_header Connection "upgrade";
  proxy_read_timeout 600s;
  client_max_body_size 200m;
}
```

## Sizing

Memory per active session is roughly 3-5 times the size of its worksheets
(including undo history, capped at 300 MB per worksheet). A 100,000-row,
10-column worksheet uses about 8 MB of data.
