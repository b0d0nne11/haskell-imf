# mailbin

This tool is meant to simplify the testing of smtp clients. It spins up a local
SMTP client that saves all incoming messages to a SQLite DB as well as an API
that provides details about those messages.

## Configuration

An optional configuration file may be provided at `./mailbin.cfg` using the
[Configurator](http://hackage.haskell.org/packages/archive/configurator/latest/doc/html/Data-Configurator.html) file format.

* `db.file` - path to SQLite database, defaults to `":memory:"`
* `db.idle_timeout` - DB connection idle timeout in seconds, defaults to `86400` (1 day)
* `db.max_conns` - maximum number of DB connections, defaults to `1`

* `mta.host` - MTA host, defaults to `"127.0.0.1"`
* `mta.port` - MTA port, defaults to `"2525"`
* `mta.certificate` - path to certificate, defaults to `"localhost.crt"`
* `mta.private_key` - path to private key, defaults to `"localhost.key"`
* `mta.credentials` - list of usernames and passwords for authentication, defaults to `[]`
* `mta.max_recipients` - maximum number of recipients per message, defaults to `10`
* `mta.max_message_size` - maximum size of message, defaults to `4096`
* `mta.require_tls` - is opportunistic TLS required, defaults to `False`
* `mta.require_auth` - is authentication required, defaults to `False`

* `api.host` - API host, defaults to `"127.0.0.1"`
* `api.port` - API port, defaults to `"3000"`
