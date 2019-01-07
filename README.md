# dotfiles

## Miscellaneous Notes

```shell
# generate wallpaper, requires imagemagick
convert -size $(xdpyinfo \
                | grep 'dimensions:' \
                | sed -e 's/.* \([0-9]\+x[0-9]\+\) pixels.*/\1/') \
    xc:#201a10 -gravity southwest -stroke none -fill '#c08340' -pointsize 32 \
    -annotate 0 $(whoami)@$(hostname) .wallpaper.png
```
