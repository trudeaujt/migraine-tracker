# Migraine Tracker

Small personal application to track when my migraines occur, the weather at the time and the forecast for the next few days, and when I need to use medicine.

## Building

1. (push #P"~/code/migraine-tracker/" asdf:*central-registry*) ; Or wherever you cloned the repo to
2. (ql:quickload :migraine-tracker)
3. (asdf:make :migraine-tracker)

## Deploying

### Setup user

1. sudo useradd --system --shell /bin/false migraine-tracker-user

### Copy binary

1. sudo cp migraine-tracker /usr/local/bin/
2. sudo chown root:root /usr/local/bin/migraine-tracker
3. sudo chmod 755 /usr/local/bin/migraine-tracker

### Configure systemd service

1. sudo cp migraine-tracker.service /etc/systemd/system/
2. sudo chmod 644 /etc/systemd/system/migraine-tracker.service
3. sudo systemctl daemon-reload
4. sudo systemctl enable migraine-tracker.service
5. sudo systemctl start migraine-tracker.service

### Verify

1. systemctl status migraine-tracker.service

The database file will be stored in the [XDG data-home](https://specifications.freedesktop.org/basedir-spec/latest/) direcotry.
