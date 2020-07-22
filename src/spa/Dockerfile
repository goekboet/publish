FROM node:lts

RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin
RUN npm install -g uglify-js@3.10.0
RUN npm install -g browserify@16.5.1

WORKDIR /home/publish_spa
COPY package.json package-lock.json ./
RUN npm install

COPY . .
RUN ./build.sh

CMD mkdir -p ./pub/publish ; rm -r ./pub/publish/* 2>/dev/null ; cp ./dist/* ./pub/publish


