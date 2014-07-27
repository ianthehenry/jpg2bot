# jpg2bot

`jpg2bot` is an alternative to [jpgtobot](https://github.com/hlian/jpgtobot), intended to demonstrate the power of [Tightrope](https://github.com/ianthehenry/tightrope) relative to [Linklater](https://github.com/hlian/linklater).

The functionality is the same but for one crucial difference: `jpg2bot` has a random animal as the emoji, whereas `jpgtobot` is always just a 🎁.

# Features

- randomized response emoji
- echoes input back
- works

# How do I use it

Right now you have to clone [Tightrope](https://github.com/ianthehenry/tightrope) separately because it isn't on Hackage yet. These steps will be much simpler soon.

    $ git clone https://github.com/ianthehenry/tightrope.git
    $ git clone https://github.com/ianthehenry/jpg2bot.git
    $ cd jpg2bot
    $ cabal sandbox init
    $ cabal sandbox add-source ../tightrope
    $ cabal run
