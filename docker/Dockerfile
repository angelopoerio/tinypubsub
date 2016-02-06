FROM ubuntu
MAINTAINER Angelo Poerio <angelo.poerio@gmail.com>
RUN apt-get update
RUN apt-get -y git erlang-base
RUN adduser tinypubsub
RUN cd /home/tinypubsub;git clone https://github.com/angelopoerio/mystorage.git
RUN chmod +x compile.sh;./compile.sh
EXPOSE 3000
CMD ["./start.sh"]