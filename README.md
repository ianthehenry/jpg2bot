# jpg2bot

`jpg2bot` is an alternative to [jpgtobot](https://github.com/hlian/jpgtobot), intended to demonstrate the power of [Tightrope](https://github.com/ianthehenry/tightrope) relative to [Linklater](https://github.com/hlian/linklater).

The functionality is the same but for one crucial difference: `jpg2bot` has a random animal as the emoji, whereas `jpgtobot` is always just a 🎁.

# Features

- randomized response emoji
- echoes input back
- works

# How do I use it

It's really easy!

    $ git clone https://github.com/ianthehenry/jpg2bot.git
    $ cd jpg2bot
    $ printf "your-token" > token
    $ ... edit the file that hardcodes trello.slack, whoops ...
    $ cabal sandbox init
    $ cabal install -j --only-dependencies
    $ cabal run
