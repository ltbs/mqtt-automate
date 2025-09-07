# Dynamic Reload Example

This project is a minimal reactive MQTT client. It uses the
[`hint`](https://hackage.haskell.org/package/hint) interpreter to reload
code whenever `Dynamic.hs` changes. The `Dynamic` module specifies which
topics to subscribe to and how to react to incoming messages; the running
client updates its subscriptions and callbacks on the fly.

## Running

```bash
stack build
stack run
```

Modify `Dynamic.hs` while the program runs to change subscriptions or
message handling behaviour. Each save triggers a reload and the new
definitions take effect without restarting the client.
