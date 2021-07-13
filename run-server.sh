#!/bin/bash

# run Xephyr server on display ":1"
exec Xephyr -br -ac -noreset -screen 800x600 :1 &
