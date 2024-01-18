# ical-cli

`ical-cli` -- A command line interface for iCal files specified by RFC 5545 (https://datatracker.ietf.org/doc/html/rfc5545).#Newline The events are being stored in ~/.event_database.

## Usage

``` shell
ical-cli [global-options] [<command>] [command-options] [arguments ...]
```

## Options

`ical-cli` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

## Sub Commands

`ical-cli` provides the following sub commands:

``` shell
  import   Import an ical file into the database
  show     Show upcoming events from the database for today
  delete   Clear whole event database
  inspect  Inspect an ical file for its events

```

## Authors

* Arda Köcer <arda.koecer@st.oth-regensburg.de>

# ical-cli import

`ical-cli import` -- Import an ical file into the database

## Usage

``` shell
ical-cli [global-options] import [options] [arguments ...]
```

## Options

`ical-cli import` accepts the following options:

``` shell
      --help         display usage information and exit
      --version      display version and exit
  -f, --file <PATH>  (REQUIRED) The ical file to import

```

## Examples

Import Standup.ics file:

``` shell
ical-cli import -f Standup.ics
```

# ical-cli show

`ical-cli show` -- Show upcoming events from the database for today

## Usage

``` shell
ical-cli [global-options] show [options] [arguments ...]
```

## Options

`ical-cli show` accepts the following options:

``` shell
      --help          display usage information and exit
      --version       display version and exit
  -a, --all           Show all events
  -d, --date <VALUE>  Show events for given date (dd.mm.yyyy)
  -t, --tomorrow      Show events for tomorrow

```

## Examples

Show events for 2nd of March 2024

``` shell
ical-cli show -d 02.03.2024
```

# ical-cli delete

`ical-cli delete` -- Clear whole event database

## Usage

``` shell
ical-cli [global-options] delete [options] [arguments ...]
```

## Options

`ical-cli delete` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit
  -f, --force    Force-skip prompt

```

# ical-cli inspect

`ical-cli inspect` -- Inspect an ical file for its events

## Usage

``` shell
ical-cli [global-options] inspect [options] [arguments ...]
```

## Options

`ical-cli inspect` accepts the following options:

``` shell
      --help         display usage information and exit
      --version      display version and exit
  -f, --file <PATH>  (REQUIRED) The ical file to inspect

```

## Examples

Inspect Standup.ics file:

``` shell
ical-cli inspect -f Standup.ics
```

