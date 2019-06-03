# Todo app with Servant and Elm

This is an example app using the Servant framework (Haskell) on the server side and Elm on the client side. It utilizes the servant-elm library to create [decoders and request functions](https://github.com/gege251/todo-with-servant/blob/master/client/src/Requests/Todo.elm) on the client side. This makes your application type safe on the boundaries.

## Get started

### Database migrations

1. Start postgresql service (for use with Docker, run `docker-compose up -d "db"`)

2. Run migrations: `make db-migrations`

### Run with Docker

1. Start docker with `docker-compose up`

### Run without Docker

1. Start postgresql service (to use only the db service of docker, run `docker-compose up -d "db"`)

1. Run Servant (backend)
```
$ cd api && make dev
```
2. Run Elm compiler (frontend)
```
$ cd client && make dev
```

### Generate code with servant-elm

```
$ cd api && make gen-elm
```

### Generate api-docs with servant-docs

```
$ cd api && make gen-docs
```
