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
nokee - (Simple & Stupid) Note Manager

Usage
=====

The general syntax is this:

    nokee [-s|--store STORENAME] COMMAND

If nokee is to be used for the first time, it needs to initialize its
stores. If no store is specified, the default store named 'default' is used.
To initialize the default store, use:

    $ nokee init

This creates an SQL store file in ~/.nokee/stores. Nokee can be told
to use other stores via the option --store (resp. -s). Therefore, to
initialize the store "recipes", you can use:

    $ nokee -s recipes init

The commands understood by Nokee are:

    edit NOTE-ID             Edit a note
    add                      Add a note
    delete NOTE-ID           Delete a note
    list [-t|--tags TAGS]    List notes
    list-stores              List Stores
    list-tags                List Tags
    init                     Initialize store
    search PATTERN           Search notes
    retrieve NOTE-ID         Retrieve a note

Thus, to add a note, use:

    $ nokee add

This will cause your $EDITOR to be spawned, opening a new note. If
this is the first note in the store, the file will contain some
editing guidelines explaining the file format for notes. In
particular, the file format allows you to attach tags to notes.

After saving and leaving the editor, the new note will be added to the
note store. A list of saved notes can be retrieves via

    $ nokee list [--tags tag1,tag2]

This displays the note summary. In particular it displays the note IDs
associated to each note. These are (integer) IDs referencing notes
uniquely (in each store). A full note can be retrieved with the
command

    $ nokee retrieve <note ID>

You can search in your note store using the "search" command, as in:

    $ nokee --store recipes search waffles

[to be continued]
