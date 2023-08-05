FROM ubuntu:latest

COPY . /english2logic

WORKDIR /english2logic

    # Update apt-get
RUN apt-get -y update \
    && apt-get -y upgrade \
    # Install Python
    && apt-get install -y python3 \
    && apt-get install -y python3-pip \
    # Install dependencies for GHC and cabal
    && apt-get install -y clang \
    && apt-get install -y make \
    && apt-get install -y curl \
    && apt-get install -y libgmp-dev

# Install GHC and cabal
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH=${PATH}:/root/.ghcup/bin
RUN ghcup install ghc && ghcup install cabal

# Need to explicitly set to UTF-8 to write logic symbols to stdout
ENV LANG=C.UTF-8

# Install other dependencies
RUN ./install.sh
