#!/usr/bin/env bash

BACKUP_NAME=`date -u +'app-list-backup.%Y-%m-%dT%H%M%SZ.txt'`
BACKUP_PATH="{{ backup_dir }}/$BACKUP_NAME"
API_UPLOAD_URL="https://api-content.dropbox.com/1/files_put"

# Dump lists to a file in the backup dir
date -u +'App list backup at %Y-%m-%dT%H%M%SZ' > $BACKUP_PATH

echo -e "\n=== /Applications ===" >> $BACKUP_PATH
ls /Applications >> $BACKUP_PATH

echo -e "\n=== {{ home_dir }}/Applications ===" >> $BACKUP_PATH
ls "{{ home_dir }}/Applications/" >> $BACKUP_PATH

echo -e "\n=== brew list ===" >> $BACKUP_PATH
brew list >> $BACKUP_PATH

echo -e "\n=== brew info ===" >> $BACKUP_PATH
brew info --json=v1 --installed | python -m json.tool >> $BACKUP_PATH

# Upload to Dropbox
/usr/bin/curl -s -i --globoff --upload-file "$BACKUP_PATH" \
          -H "Authorization:Bearer {{ dropbox_access_token }}" \
          "$API_UPLOAD_URL/sandbox/$BACKUP_NAME"
