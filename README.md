# nokee

About
=====

Nokee is a simple note keeper (CLI) application (hence the name). It
is written by Moritz Schulte and licensed under a BSD3 license.

At the moment Nokee uses SQLite as its data store backend. Actually I
would have liked to use a plaintext data store backend like e.g. GNU
recutils, but I didn't find the binding situation for recutils
satisfying (a plaintext data store would make it easy to combine Nokee
with an SCM like Git). Thus, at least for now, Nokee uses SQLite3.

Installation Instructions
=========================

    $ stack build

resp.

    $ stack install

should be sufficient.

Status
======

This is work in progress! Although Nokee may work well, it is not
considered stable yet. The code needs a cleanup/reorganization.
