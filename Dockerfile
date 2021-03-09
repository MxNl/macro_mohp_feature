FROM ubuntu:focal

ARG USERNAME=apprunner

ENV LC_ALL=C.UTF-8 \
    LANG=C.UTF- \
    UBUNTU_CODENAME=focal \
    USER_HOME=/home/$USERNAME \
    R_VERSION=4.0.4 \
    RENV_VERSION=0.12.3 \
    DEBIAN_FRONTEND=noninteractive \
    POSTGRES_VERSION=13 \
    POSTGIS_VERSION=3 \
    DATABASE_NAME=postgis

RUN set -xe \
    && useradd $USERNAME \
	&& mkdir $USER_HOME \
	&& chown $USERNAME:$USERNAME $USER_HOME \
	&& addgroup $USERNAME staff \
    && apt-get update \
    && apt-get install -yqq --no-install-recommends \
        sudo \
        wget \
        curl \
        gnupg2 \
        ca-certificates \
        apt-utils \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
    && echo "deb https://cloud.r-project.org/bin/linux/ubuntu ${UBUNTU_CODENAME}-cran40/" > /etc/apt/sources.list.d/r-language.list \
    && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
    && echo "deb http://apt.postgresql.org/pub/repos/apt ${UBUNTU_CODENAME}-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get update \
    && apt-get install -yqq \
        postgresql-${POSTGRES_VERSION} \
        postgresql-${POSTGRES_VERSION}-postgis-${POSTGIS_VERSION} \
        r-base=${R_VERSION}-* \
        r-base-dev=${R_VERSION}-* \
		r-recommended=${R_VERSION}-* \
    && R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
    && R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')" \
    && sed -i "s|^host *all *all *\(.*\) md5|host all all \1 trust|" /etc/postgresql/${POSTGRES_VERSION}/main/pg_hba.conf \
    && /etc/init.d/postgresql start \
    && su - postgres -c "createuser --superuser ${USERNAME}" \
    && su - postgres -c "createdb ${DATABASE_NAME}" \
    && su - postgres -c "psql -d ${DATABASE_NAME} -c 'CREATE EXTENSION postgis; CREATE EXTENSION postgis_raster;'" \
    && echo "${USERNAME} ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers \
    && rm -rf /var/lib/apt/lists/*

WORKDIR $USER_HOME
USER $USER_NAME
COPY --chown=$USER_NAME renv.lock renv.lock
RUN R --vanilla -s -e 'renv::restore()'

COPY . .
VOLUME /docker-mount