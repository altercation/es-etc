#!/bin/bash

# index new mail, apply "new" tag to new mail
notmuch new

# get list of mailboxes
MAILBOXES=$(sed 's/[^"]*"+\([^"]*\)"/\1\n/g' ~/var/mail/mailboxes)

# add tags to each folder matching folder name (IMAP path trimmed)
# remove tags for messages that have been removed
for MAILBOX in $MAILBOXES; do
    MAILTAG=${MAILBOX##*/} # trim to last path component
    notmuch tag -$MAILTAG -- tag:$MAILTAG NOT folder:$MAILBOX NOT folder:trash
    notmuch tag +$MAILTAG -- folder:$MAILBOX NOT tag:$MAILTAG
done

notmuch tag -unread -- folder:sent
