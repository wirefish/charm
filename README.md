# Charm

Charm is a framework for creating text-based multiplayer role-playing games. You
could say it is an updated take on a MUD. Charm is designed to be played in a
modern browser and includes an integrated HTML5 client.

## Installation

Charm is written in [Common Lisp](https://common-lisp.net). Specifically it is
developed using the [SBCL](http://sbcl.org) implementation of the language. If
you're not familiar with Common Lisp,
[this page](https://lisp-lang.org/learn/getting-started/) provides a good guide
to getting started.

Charm also requires the following:

* [sqlite3](https://www.sqlite.org/) version 3.11 or later.

* [nginx](https://www.nginx.com/) version 1.10.3 or later.

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

During development, the most convenient way to run the server is within the SBCL
REPL. The easiest way to do this for Emacs users is via
[Slime](https://github.com/slime/slime).

    M-x slime ; starts the REPL
    ,load-system charm ; loads charm.asd
    C-c M-p charm ; changes to the charm package
    (run-server) ; starts the game server

### Connect as a Client

Connect to the game as follows:

    http://localhost/

Follow the instructions to create a new account and you'll be dropped into the
game.
