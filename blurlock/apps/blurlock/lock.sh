#! /bin/sh

# OG: https://github.com/my-arch/dots/blob/main/3_i3lock/blurlock.sh
# Pixelation idea from : https://reddit.invak.id/r/unixporn/comments/7iddwn/i3lock_faster_and_better_lockscreen/dqy143t/?context=3
LOCK=/home/rohit/apps/i3lock-fancy/lock.png
RES=$(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/')
IMAGE=$(mktemp).png
ffmpeg -probesize 100M -thread_queue_size 32 -f x11grab -video_size $RES \
  -y -i $DISPLAY -i $LOCK -i $LOCK \
  -filter_complex \
  "eq=gamma=0.75,scale=iw/10:ih/10:flags=area,scale=10*iw:10*ih:flags=neighbor,overlay=(main_w-overlay_w)/4:(main_h-overlay_h)/2,overlay=3*(main_w-overlay_w)/4:(main_h-overlay_h)/2" \
  -vframes 1 \
  $IMAGE

# xdg-open $IMAGE
i3lock -n -i "$IMAGE"