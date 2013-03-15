heptacat
========

Git-based worker array for version-controlled experiments.

heptacat interactive tutorial
-----------------------------

**1.** Create a directory for the test. Clone the subject and record repository, so that you can locally test them.

~~~~ bash
~$ mkdir heptacat-test; cd heptacat-test
heptacat-test$ mkdir node-m; cd node-m  # let's pretend that this is the master node
node-m$ git clone  git@github.com:sugoi/heptacat-example-subject.git
Cloning into 'heptacat-example-subject'...
remote: Counting objects: 30, done.
remote: Compressing objects: 100% (25/25), done.
remote: Total 30 (delta 12), reused 17 (delta 3)
Receiving objects: 100% (30/30), 4.79 KiB, done.
Resolving deltas: 100% (12/12), done.
node-m$ git clone git@github.com:sugoi/heptacat-example-record.git
Cloning into 'heptacat-example-record'...
remote: Counting objects: 14, done.
remote: Compressing objects: 100% (11/11), done.
remote: Total 14 (delta 2), reused 10 (delta 1)
Receiving objects: 100% (14/14), done.
Resolving deltas: 100% (2/2), done.
node-m$ cd ..
~~~~

**2.** Copy the project file, edit it to point to the local repository.

~~~~ bash
heptacat-test$ cp node-m/heptacat-example-record/project.yml node-w0/
heptacat-test$ cd node-w0/
~~~~
