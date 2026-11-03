# R environment for the lmmresample simulations and analyses
FROM rocker/r2u:jammy
RUN apt-get update && apt-get install -y --no-install-recommends zip \
    && rm -rf /var/lib/apt/lists/*
RUN install.r lme4 lmerTest future future.apply mgcv reformulas remotes
ARG LMM_REF=7219851
RUN Rscript -e "remotes::install_github('ricardo-martinez-flores/lmmresample@${LMM_REF}', upgrade = 'never')" \
    && Rscript -e 'library(lmmresample); cat("lmmresample", as.character(packageVersion("lmmresample")), packageDescription("lmmresample")$RemoteSha, "\n")'
