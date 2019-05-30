FROM node:12

WORKDIR /app

RUN npm i elm
RUN npm i serve

COPY . /app

EXPOSE 8000

RUN ./node_modules/elm/bin/elm make ./src/Main.elm --debug

CMD ["make", "serve"]
