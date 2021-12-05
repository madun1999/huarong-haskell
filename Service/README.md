# Service

## build & install

```bash
$ stack build
$ stack install
```

## start

`$ ./Service-exe `

## API

### createRoom

- create a game room
- return the uuid of the game room

```json
// - request sample
// - from client A
{
  "reqService": "createRoom",
  "reqPayload": ""
}
```

```json
// - response sample
// - to client A
{
  "resService": "createRoom",
  "resPayload": "{\"status\":true,\"detail\":\"6905c091-3d5d-4334-9824-0c0b8637a1b6\"}"
}
```

### leaveRoom

- leave a game room by uuid
- broadcast a player leaving infomation in the game room

```json
// - request sample
// - from client A
{
  "reqService": "leaveRoom",
  "reqPayload": ""
}
```

```json
// - response sample
// - to client A and to client B
{
  "resService": "Info",
  "resPayload": "{\"status\":true,\"detail\":\"Leave 6905c091-3d5d-4334-9824-0c0b8637a1b6\"}" //json
}
```

### joinRoom

- join a game room marked by uuid
- leave the current game room by default
- broadcast a player joining infomation in the game room

```json
// - request sample
// - from client A
{
  "reqService": "joinRoom",
  "reqPayload": "6905c091-3d5d-4334-9824-0c0b8637a1b6"
}
```

```json
// - response sample
// - (currently in a game room)
// - to client A and to client C
{
  "resService": "Info",
  "resPayload": "{\"status\":true,\"detail\":\"Leave c302c282-111e-4205-aba6-c5b747449a0c\"}" //json
}
```

```json
// - response sample
// - to client A and to client B
{
  "resService": "Info",
  "resPayload": "{\"status\":true,\"detail\":\"Join 6905c091-3d5d-4334-9824-0c0b8637a1b6\"}" // json
}
```

### move

- broadcast the moves in the game room

```json
// - request sample
// - from client A
{
  "reqService": "move",
  "reqPayload": "player1, caocao, (1, 1), (2, 2)"
}
```

```json
// - response sample
// - to client A and to client B
{
  "resService": "move",
  "resPayload": "player1, caocao, (1, 1), (2, 2)"
}
```

## Schematic diagram of API call

```plain

client A - createRoom -> (waiting Join Info) - move -> (waiting move Info)
      |                     ^                                   ^
      |                     |                                   |
  room uuid               server                              server
      |                     |                                   |
      |                     |                                   |
      V                     V                                   V
client B - joinRoom -> (waiting Join Info)  - move ->  (waiting move Info)
```
