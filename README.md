# lisp-ical-cli
iCalendar parser and cli-tool written in CommonLisp for iCal files specified after RFC 5545.
https://datatracker.ietf.org/doc/html/rfc5545

## Missing Features
- Timezones are being ignored atm
- Reoccurences are being ignored atm

## Installation
Three different libraries are needed.
Requirement: Quicklisp is already installed on your system (https://www.quicklisp.org/beta/index.html)
- `:local-time` and `:iterate` are already part of Quicklisp.
- `:clingon` needs to be added manually.

### Installing `clingon`
1. Download the `clingon` repository (https://github.com/dnaeon/clingon)
2. Place the repository into your Quicklisp local projects folder (https://www.quicklisp.org/beta/faq.html)

### Installing `ical-cli`
1. Download the `lisp-ical-cli` repository (https://github.com/suptower/lisp-ical-cli)
2. Place the repository into your Quicklisp local projects folder (https://www.quicklisp.org/beta/faq.html)
3. To build the binary, execute the `buildapp.sh` script from the root directory of this repository.
4. The binary will be located under `bin/ical-cli` in the project folder.

## Usage
Use `./ical-cli --help` for usage insructions. Alternatively, there is a short documentation in `ical-cli.md`.
