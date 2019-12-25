# json-rpc-server-example

# Run

```bash
$ stack run
```

# Examples

```bash
$ curl -X POST -d '{"jsonrpc":"2.0","method":"add_point","params":[1,2],"id":1}' localhost:3000/api/v1
{"result":[1],"jsonrpc":"2.0","id":1}

$ curl -X POST -d '{"jsonrpc":"2.0","method":"add_point","params":[3,4],"id":2}' localhost:3000/api/v1
{"result":[2],"jsonrpc":"2.0","id":2}

$ curl -X POST -d '{"jsonrpc":"2.0","method":"get_all_points","params":[],"id":3}' localhost:3000/api/v1
{"result":[{"_id":1,"x":1,"y":2},{"_id":2,"x":3,"y":4}],"jsonrpc":"2.0","id":3}
```
