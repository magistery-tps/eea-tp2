FROM rocker/rstudio

RUN sudo apt update
RUN sudo apt -y install libglpk-dev
