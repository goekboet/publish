FROM node:12 AS build-env

WORKDIR /home/elm
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin

WORKDIR /home/spa
COPY package.json package-lock.json ./
RUN npm install
RUN npm install uglify-js -g

COPY . .
RUN ./build.sh

FROM ego/aspnet:3.1
WORKDIR /home/publish_spa

COPY --from=build-env /home/spa/dist ./build

CMD cp ./build/* ./pub 


