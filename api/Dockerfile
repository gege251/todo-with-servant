FROM haskell:8.2.2

WORKDIR /app

COPY ./stack.yaml /app/stack.yaml
COPY ./package.yaml /app/package.yaml
RUN stack build --only-dependencies

COPY . /app
RUN stack build
RUN stack install

EXPOSE 3000

CMD ["stack", "exec" ,"todoapi"]
