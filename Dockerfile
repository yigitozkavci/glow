FROM ubuntu:16.04

RUN apt-get update && apt-get upgrade
RUN apt-get install -y wget sudo git gcc libgmp3-dev build-essential software-properties-common
RUN wget https://haskell.org/platform/download/8.0.2/haskell-platform-8.0.2-unknown-posix--minimal-x86_64.tar.gz
RUN tar -xzvf haskell-platform-8.0.2-unknown-posix--minimal-x86_64.tar.gz 
RUN ./install-haskell-platform.sh 
RUN add-apt-repository -s "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main" 
RUN apt-get update 
RUN apt-get install -y llvm-4.0-dev --allow-unauthenticated 
RUN rm *.tar.gz install-haskell-platform.sh 
RUN git clone https://github.com/yigitozkavci/glow.git /root/glow
WORKDIR /root/glow
RUN cabal sandbox init
RUN cabal update
RUN cabal install
