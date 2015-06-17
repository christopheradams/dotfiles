#!/bin/bash
perl -pi -w -e "s/\s?\d(.*)DONE//g" "$1"

