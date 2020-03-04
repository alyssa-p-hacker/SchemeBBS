#!/bin/sh
#mit-scheme --band "init.com" --interactive --batch-mode --args $* < myscript.scm
mit-scheme --quiet --args $* < bbs.scm
