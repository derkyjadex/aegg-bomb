# Aegg Bomb

A game that lets you throw eggs at each other.

Build with `stack build`, run with `stack exec aegg-bomb`.

## Connect As A Player

Connect as a player on TCP port 4242, using something like `netcat`. First you need to provide your player name, then you can interact with the game.

Using `rlwrap` makes this a little more pleasant.

```
$ rlwrap netcat localhost 4242
```

```
> Hi, what's your name?
< bob
> Welcome, bob!
```

You will receive frequent status reports that tell you what is going on around you.

```
> Current position (0.0,0.0)
> Health 1.0
> Can see players []
> Can see eggs []
```

You can issue a `Move` command to move your player.

```
< Move (1.0, 0.0)
> Current position (0.31666666666666665,0.0)
> Health 1.0
> Can see players []
> Can see eggs []
> Current position (0.8333333333333343,0.0)
...
```

`Throw` will throw an egg.

```
< Throw (10.0, 5.0)
> Current position (0.0,0.0)
> Health 1.0
> Can see players []
> Can see eggs [(1.8333333333333335,0.9166666666666667)]
> Current position (0.0,0.0)
> Health 1.0
> Can see players []
> Can see eggs [(7.0000000000000036,3.5000000000000018)]
...
```

This is still very much a work-in-progress and precisely all the details are subject to change.


## Connect As A Renderer

The render output is sent over WebSockets. The server runs on port 2424. Each message sent to the client is a JSON array containing a list of objects in the scene.

Each object has `type`, `pos` and `bounds` properties. For example:

```json
{
  "type": "player",
  "pos": [32.5, -46.7],
  "bounds": [[-0.5, -0.5], [0.5, 0.5]]
}
```

The `pos` contains the `x` and `y` coordinates of the object, the `bounds` contains the minimum and maximum coordinates of the axis-aligned bounding box around the object, relative to `pos`. Some objects contain further properties which can be used to make the rendered output more interesting.

A full message looks something like the following (once nice formatting has been applied):

```json
[
  { "type": "wall", "pos": [0.0, 0.0], "bounds": [[-11, -11], [-10, 10]] },
  { "type": "wall", "pos": [0.0, 0.0], "bounds": [[-11, 10], [10, 11]] },
  { "type": "wall", "pos": [0.0, 0.0], "bounds": [[10, -10], [11, 11]] },
  { "type": "wall", "pos": [0.0, 0.0], "bounds": [[-10, -11], [11, -10]] },
  { "type": "wall", "pos": [0.0, 0.0], "bounds": [[-6, -6], [-5, 6]] },
  {
    "type": "player",
    "pos": [3.19999, 6.39999],
    "bounds": [[-0.5, -0.5], [0.5, 0.5]],
    "name": "alice"
  },
  {
    "type": "egg",
    "pos": [-2.40000, 2.66666],
    "bounds": [[-0.25, -0.25], [0.25, 0.25]],
    "height": 10.96666
  },
  {
    "type": "explosion",
    "pos": [-4.74999, 1.09999],
    "bounds": [[-0.95061, -0.95061], [0.95061, 0.95061]]
  }
]
```

`wscat` is a useful tool for debugging this.


## License

Copyright 2015 James Deery, Kris Jenkins. Distributed under the MIT license, see LICENSE for details.
