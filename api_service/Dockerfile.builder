FROM ubuntu:18.04

CMD apt update && \
    apt install -y  \
        plantuml graphviz python3 build-essential