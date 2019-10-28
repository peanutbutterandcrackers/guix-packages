# My [GNU Guix](https://guix.gnu.org/) Packages
This is a [channel](https://guix.gnu.org/manual/en/html_node/Channels.html) containing packages not yet in available in the [GNU Distribution](https://guix.gnu.org/manual/en/guix.html#GNU-Distribution).

## Adding this channel:
Add the following lines to `~/.config/guix/channels.scm` (run `guix pull` first, if the path does not exist):
```scheme
;; Add peanutbutterandcrackers/guix-packages
(cons (channel
  (name 'peanutbutterandcrackers-guix-packages)
  (url "https://github.com/peanutbutterandcrackers/guix-packages"))
 %default-channels)
```
