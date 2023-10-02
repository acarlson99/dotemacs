# Use an Ubuntu-based image as the base image
FROM ubuntu:20.04

# Set environment variables for non-interactive installation
ENV DEBIAN_FRONTEND noninteractive

# Install build dependencies
RUN apt update && apt install -y \
    build-essential \
    wget \
    xz-utils \
	libxpm-dev \
    libgif-dev \
    libjpeg-dev \
    libtiff5-dev \
    libgtk-3-dev \
    libncurses5-dev \
    libgnutls28-dev \
	libwebkit2gtk-4.0-dev

# Download Emacs source code (adjust the version as needed)
WORKDIR /src
RUN wget https://ftp.gnu.org/gnu/emacs/emacs-29.1.tar.gz && \
    tar -xzvf emacs-29.1.tar.gz

# Build and install Emacs
WORKDIR /src/emacs-29.1
RUN ./configure --with-cairo --with-xwidgets --with-x-toolkit=gtk3 && \
    make -j4 && \
    make install

# RUN git clone https://github.com/acarlson99/dotemacs.git ~/.emacs.d && \
#     emacs --script ~/.emacs.d/lisp/web-server-org/setup.el

WORKDIR ~/
RUN mkdir -p ~/.emacs.d/lisp/web-server-org/
RUN cd ~/.emacs.d/lisp/web-server-org/ && \
    wget https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/web-server-org.el && \
    wget https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/setup.el
RUN cd ~/.emacs.d/lisp/ &&  \
    wget https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/el-log.el

# cleanup

# RUN apt install -y git
RUN apt autoremove -y

# Clean up
RUN apt clean && rm -rf /var/lib/apt/lists/* /src

# RUN rm -rf /src/emacs-29.1

EXPOSE 8080
RUN mkdir -p /tmp/test/
WORKDIR /tmp/test/

CMD ["emacs", "--batch", "-l", "~/.emacs.d/lisp/web-server-org/web-server-org.el", "-f", "web-server-org-main"]
