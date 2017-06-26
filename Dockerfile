FROM ubuntu:16.04

RUN wget https://haskell.org/platform/download/8.0.2/haskell-platform-8.0.2-unknown-posix--minimal-x86_64.tar.gz
RUN tar -xzvf haskell-platform-8.0.2-unknown-posix--minimal-x86_64.tar.gz                                       
RUN sudo ./install-haskell-platform.sh                                                                          
RUN sudo add-apt-repository -s "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main"                 
RUN sudo apt-get update                                                                                         
RUN sudo apt-get install -y llvm-4.0-dev --allow-unauthenticated                                                
RUN sudo rm *.tar.gz install-haskell-platform.sh                                                                
RUN sudo apt-get install -y git                                                                                 
RUN sudo apt-get install -y gcc                                                                                 
RUN sudo apt-get install -y libgmp3-dev                                                                         
RUN sudo apt-get install build-essential                                                                        
RUN git clone https://github.com/yigitozkavci/glow.git                                                          
RUN cd glow                                                                                                     
RUN cabal update && cabal sandbox init && cabal install
