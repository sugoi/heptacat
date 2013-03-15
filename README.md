heptacat
========

Git-based worker array for version-controlled experiments.

How to test heptacat
--------------------

1. Create a directory for the test. Clone the record repository, so that you can write to it.

~~~~ sh
[~]$ mkdir heptacat-test; cd heptacat-test
[heptacat-test]$ mkdir remote; cd remote  # let's pretend that this is the master record repository
[remote]$ git clone git@github.com:sugoi/heptacat-example-record.git
Cloning into 'heptacat-example-record'...
remote: Counting objects: 14, done.
remote: Compressing objects: 100% (11/11), done.
remote: Total 14 (delta 2), reused 10 (delta 1)
Receiving objects: 100% (14/14), done.
Resolving deltas: 100% (2/2), done.
[remote]$
~~~~
