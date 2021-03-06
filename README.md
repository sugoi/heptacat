heptacat
========

Git-based worker array for version-controlled experiments.

heptacat interactive tutorial
-----------------------------

**1.** Create a directory for the test. Clone the subject and record repository, so that you can locally test them.

~~~~ bash
~$ mkdir heptacat-test; cd heptacat-test
heptacat-test$ mkdir node-m; cd node-m  # let's pretend that this is the master node
node-m$ git clone --bare git@github.com:sugoi/heptacat-example-subject.git
Cloning into 'heptacat-example-subject.git'...
remote: Counting objects: 30, done.
remote: Compressing objects: 100% (25/25), done.
remote: Total 30 (delta 12), reused 17 (delta 3)
Receiving objects: 100% (30/30), 4.79 KiB, done.
Resolving deltas: 100% (12/12), done.
node-m$ git clone --bare git@github.com:sugoi/heptacat-example-record.git
Cloning into 'heptacat-example-record.git'...
remote: Counting objects: 14, done.
remote: Compressing objects: 100% (11/11), done.
remote: Total 14 (delta 2), reused 10 (delta 1)
Receiving objects: 100% (14/14), done.
Resolving deltas: 100% (2/2), done.
node-m$ cd ..
~~~~

**2.** Copy the project file, edit it to point to the local
repository.  Also let us specify the worker name `worker0` for
it. Worker name must not contain whitespace characters.

~~~~ bash
heptacat-test$ mkdir node-w0
heptacat-test$ cp node-m/heptacat-example-record/project.yml node-w0/
heptacat-test$ cd node-w0/
node-w0$ emacs project.yml  
node-w0$ cat project.yml
workerNameInCharge: 'worker0'
recordRepo:
  taskProgressDir: progress
  workerStateDir: worker
  recordRepoUrl: /home/nushio/heptacat-test/node-m/heptacat-example-record.git
  taskListDir: task
  resultDir: result
subjectRepo:
  startUpScript: start
  outputDir: output
  subjectRepoUrl: /home/nushio/heptacat-test/node-m/heptacat-example-subject.git
node-w0$ cd ..
~~~~

**3.** Start the worker. Since `project.yml` exists there, we can just type `heptacat worker` . 
Also, you can optionally change the worker name if you want.

~~~~ bash
heptacat-test$ heptacat worker
~~~~
