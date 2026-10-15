On macOS, build and run from the Lazarus root:

```sh
lazbuild --lazarusdir="$PWD" --ws=cocoa test/manual/cocoa/queuedfocus/queuedfocus.lpi
./test/manual/cocoa/queuedfocus/queuedfocus
./test/manual/cocoa/queuedfocus/queuedfocus retain
```

OnEnter queues focus back to the first edit. The default case gives a button
focus before the queue drains and checks that it keeps native and LCL focus.
`retain` checks that the queued request runs when nothing supersedes it.
