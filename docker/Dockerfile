FROM ubuntu
MAINTAINER Angelo Poerio <angelo.poerio@gmail.com>
RUN apt-get update
RUN apt-get install -y git erlang-base
RUN adduser tinypubsub
RUN cd /home/tinypubsub;git clone https://github.com/angelopoerio/tinypubsub.git
RUN cd /home/tinypubsub/tinypubsub;chmod +x compile.sh;chmod +x start.sh;./compile.sh
EXPOSE 4444
CMD ["/home/tinypubsub/tinypubsub/start.sh"]