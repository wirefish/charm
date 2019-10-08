# Charm

Charm is a framework for creating text-based multiplayer role-playing games. You
could say it is an updated take on a MUD. Charm is designed to be played in a
modern browser and includes an integrated HTML5 client.

## Installation

Charm is written in [Common Lisp](https://common-lisp.net). Specifically it is
developed using the [SBCL](http://sbcl.org) implementation of the language.

Charm also requires the following:

* [sqlite3](https://www.sqlite.org/) version 3.11 or later.

* [nginx](https://www.nginx.com/) version 1.10.3 or later.

## Aside: Getting Started with Common Lisp

Here's a quick guide to getting a Common Lisp development environment running on
macOS, along with the dependencies needed by Charm. Although the steps would
differ slightly for other OSes, the general idea is the same.

* Install [homebrew](https://brew.sh).

* Install [SBCL]: `brew install sbcl`.

* Install [Emacs](https://emacsformacosx.com).

* Install [Slime](https://common-lisp.net/project/slime/), which is a Common
  Lisp development environment for Emacs. You should be able to install it from
  within Emacs with `M-x package-install RET slime RET`.

* Configure Slime by adding the following to your Emacs init file:

    (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (require 'slime-autoloads)
    (setq slime-contribs '(slime-asdf slime-repl slime-fancy))

* By default, ASDF (which is the de facto build system for Common Lisp) expects
  to find projects in a subdirectory of "~/common-lisp". Create a symlink with
  that name to a directory where you prefer to keep your projects.

* You can now fire up SBCL within Emacs using `M-x slime`. This will dump you
  into the Slime REPL where you can evaluate Lisp forms.

* Install [Quicklisp](https://www.quicklisp.org/beta/), which is a library
  manager for common lisp. Download the `quicklisp.lisp` file and then, from
  within the REPL, type:

    (load "quicklisp.lisp")
    (quicklisp-quickstart:install)
    (ql:add-to-init-file)

The following steps are specific to Charm:

* Install [libuv](https://libuv.org): `brew install libuv`.

* Install the libraries upon which charm depends. From within the REPL, type:

    (ql:quickload "cl-ppcre")
    (ql:quickload "cl-async")
    (ql:quickload "cl-base64")
    (ql:quickload "ironclad")
    (ql:quickload "sqlite")

That's a lot of steps! But you only have to do them once.

## Usage

In this section, `$CHARM` is assumed to refer to the root directory of the Charm
source repository.

### Create the Data Directory

Charm uses `/var/charm` as a root directory for data used by the running server,
and as a place to put server logs. Start by creating this directory and linking
it to the client resources:

    mkdir /var/charm
    cd /var/charm
    ln -s $CHARM/client/

### Start NGINX

Charm uses NGINX as a webserver that sits in front of the game server to serve
static content and provide TLS functionality.

After installing NGINX, you can start it as follows:

    sudo nginx -c $CHARM/config/nginx.conf

By default, NGINX listens on ports 80/443. If you cannot use the standard ports
you will need to modify the configuration file.

### Create the Database

Before running the server, create the database used to store account
information, player avatars, etc.

    sqlite3 /var/charm/charm.db < $charm/config/schema.sql

### Run the Server

During development, the most convenient way to run the server is within the
Slime REPL:

    M-x slime ; starts the REPL
    ,load-system charm ; loads charm.asd
    C-c M-p charm ; changes to the charm package
    (run-server) ; starts the game server

### Connect as a Client

Connect to the game as follows:

    http://localhost/

Follow the instructions to create a new account and you'll be dropped into the
game.
