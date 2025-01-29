#!/bin/bash

FILE="$HOME/.sharing"

if [ -f "$FILE" ]; then
    echo '{"class": "sharing", "text": "👁️‍🗨️" }'
else
    echo '{"class": "notsharing", "text": "not sharing" }'
fi
