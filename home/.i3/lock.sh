#!/bin/sh
# Turns on DPMS, mutes all output, locks the screen.
# Reverts all settings on unlock, or when killed.
# Courtesy of stapelberg.
revert() {
  xset dpms 0 0 0
  # for i in $(echo Master Headphone Speaker PCM)
  # do
  #   amixer sset $i unmute
  # done
}
trap revert SIGHUP SIGINT SIGTERM
xset +dpms dpms 5 5 5
# for i in $(echo Master Headphone Speaker PCM)
# do
#   amixer sset $i mute
# done
echo "screen locked at $(date)" >> /tmp/locklog.log
i3lock -i ~/.wallpaper.png -n
revert
