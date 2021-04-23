#!/usr/bin/env bash
set -eux

BACKUP_NAME=`date -u +'app-list-backup.%Y-%m-%dT%H%M%SZ.txt'`
BACKUP_PATH="{{ backup_dir }}/$BACKUP_NAME"
API_UPLOAD_URL="https://api-content.dropbox.com/2/files/upload"
export HOME="{{ home_dir }}"

# Dump lists to a file in the backup dir
date -u +'App list backup at %Y-%m-%dT%H%M%SZ' > $BACKUP_PATH

echo -e "\n=== /Applications ===" >> $BACKUP_PATH
ls /Applications >> $BACKUP_PATH

echo -e "\n=== {{ home_dir }}/Applications ===" >> $BACKUP_PATH
ls "{{ home_dir }}/Applications/" >> $BACKUP_PATH

echo -e "\n=== brew list ===" >> $BACKUP_PATH
sudo -u "{{user}}" /opt/homebrew/bin/bash list >> $BACKUP_PATH

echo -e "\n=== brew info ===" >> $BACKUP_PATH
sudo -u "{{user}}" /opt/homebrew/bin/bash info --json=v1 --installed | python -m json.tool >> $BACKUP_PATH
