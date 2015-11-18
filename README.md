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

The render output is currently written to `stdout`. This will change to also be available over a TCP port and WebSockets.

Currently the output looks something like the following, with every object in the scene written out:

```
Scene
  { _walls =
      [ ( ( -11.0 , -11.0 ) , ( -10.0 , 10.0 ) )
      , ( ( -11.0 , 10.0 ) , ( 10.0 , 11.0 ) )
      , ( ( 10.0 , -10.0 ) , ( 11.0 , 11.0 ) )
      , ( ( -10.0 , -11.0 ) , ( 11.0 , -10.0 ) )
      , ( ( -6.0 , -6.0 ) , ( -5.0 , 6.0 ) )
      ]
  , _players = []
  , _eggs = []
  , _explosions = []
  }
```

## License

Copyright 2015 James Deery. Distributed under the MIT license, see LICENSE for details.
