FROM node:12

WORKDIR /app

RUN npm i elm
RUN mv node_modules/elm/bin/elm /bin/
RUN npm i -g elm-live

COPY . /app

EXPOSE 8000

CMD ["make", "dev"]
