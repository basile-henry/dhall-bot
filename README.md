# dhall-bot

## Building and running

- Using `nix`:

  ```sh
  $(nix-build --no-out-link)/bin/dhall-bot
  ```
- Using `stack`:

  Install `zlib`, on `Ubuntu`:

  ```sh
  sudo apt update
  sudo apt install libghc-zlib-dev
  ```

  ```sh
  stack build
  stack run
  ```

## Note

*Running this bot can be a massive security issue!!!*

Dhall let's users import local files and environment variables. No effort is made in this bot to prevent that,
so it's up to the user to provide a safe environment for the bot to run in.

I am personally running the bot on a cheap VPS with nothing else on it and from a non sudo account. I'll be doing that until someone convinces me otherwise!

# License

[MIT Licence](/LICENSE)

```
Copyright (c) 2019 Basile Henry
```
