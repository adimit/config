#!/bin/sh

gdbus call --session --dest org.mpris.MediaPlayer2.Lollypop --object-path /org/mpris/MediaPlayer2 --method org.mpris.MediaPlayer2.Player.Pause
i3lock -i /home/aleks/.lockscreen.png
