#!/bin/bash

ls |sort|awk -F. '{$NF=""; print $0}' |uniq -c|grep -v '2 '
