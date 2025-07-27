# Migraine Tracker

Small personal application to track when my migraines occur, the weather at the time and the forecast for the next few days, and when I need to use medicine.

## Building

```Lisp
(push #P"~/code/migraine-tracker/" asdf:*central-registry*) ; Or wherever you cloned the repo to
(ql:quickload :migraine-tracker)
(asdf:make :migraine-tracker)
```

## Deploying

### Setup user

```shell
sudo useradd --system --shell /bin/false migraine-tracker-user
```

### Move binary

```shell
sudo mv migraine-tracker /usr/local/bin/
sudo chown root:root /usr/local/bin/migraine-tracker
sudo chmod 755 /usr/local/bin/migraine-tracker
```

### Configure systemd service

```shell
sudo cp migraine-tracker.service /etc/systemd/system/
sudo chmod 644 /etc/systemd/system/migraine-tracker.service
sudo systemctl daemon-reload
sudo systemctl enable migraine-tracker.service
sudo systemctl start migraine-tracker.service
```

### Verify

```shell
systemctl status migraine-tracker.service
```

The database file will be stored in the [XDG data-home](https://specifications.freedesktop.org/basedir-spec/latest/) direcotry.
