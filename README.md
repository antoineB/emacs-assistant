Emacs-assistant
===============

Add some usefull for emacs (or other) throught an external server process

Start the server
----------------

    $ racket -l errortrace -t server.rkt

run the elisp code:

```elisp
(load-file "connect.el")
```

then

    M-x ab-start


Sql
---

Provide basic autocompletion for simple select sql.

Change _emacs-assistant.crkt_ to point to the your sql schema, ensure it is only
the schema other wise it will probably not work.

Run

```elisp
(load-file "sql/sql.el")
```

then if you have auto-complete

    M-: (ac-ab-sql-setup)

You can also try

    M-: (ab-sql-terminate-sync)

it should complete the _from_ part of your select sql statement


Php etags
---------

Generate an etags file from php files, and select your relevant data through an
[helm](https://github.com/emacs-helm/helm) interface.
