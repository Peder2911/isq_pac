FROM rocker/geospatial:3.6.3

RUN apt update
RUN apt install python3-pip -y

RUN pip3 install --upgrade pip
COPY ./requirements.txt /requirements.txt
RUN pip3 install -r requirements.txt 

COPY . /replication
WORKDIR /replication

RUN /replication/findDeps.sh
CMD /replication/run.sh
