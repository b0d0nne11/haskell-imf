# mailbin

This tool is meant to simplify the testing of smtp clients. It spins up a local
SMTP client that saves all incoming messages to a SQLite DB as well as an API
that provides details about those messages.

## Configuration

An optional configuration file may be provided at `./mailbin.cfg` using the
[Configurator](http://hackage.haskell.org/packages/archive/configurator/latest/doc/html/Data-Configurator.html) file format.

* `db.uri` - SQLite database URI, defaults to `:memory:`
* `db.idle_timeout` - DB connection idle timeout in seconds, defaults to 86400 (1 day)
* `db.max_conns` - maximum number of DB connections, defaults to `1`
* `mta.host` - MTA host, defaults to `127.0.0.1`
* `mta.port` - MTA port, defaults to `2525`
* `mta.certificate_file` - MTA certificate, defaults to embedded self-signed credentials
* `mta.private_key_file` - MTA private key, defaults to embedded self-signed credentials
* `api.host` - API host, defaults to `127.0.0.1`
* `api.port` - API port, defaults to `8080`

