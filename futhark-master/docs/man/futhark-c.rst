.. role:: ref(emphasis)

.. _futhark-c(1):

=========
futhark-c
=========

SYNOPSIS
========

futhark c [options...] <program.fut>

DESCRIPTION
===========

``futhark c`` translates a Futhark program to sequential C code, and
either compiles that C code with a C compiler (see below) to an
executable binary program, or produces a ``.h`` and ``.c`` file that
can be linked with other code..  The standard Futhark optimisation
pipeline is used, and

The resulting program will read the arguments to the entry point
(``main`` by default) from standard input and print its return value
on standard output.  The arguments are read and printed in Futhark
syntax.

OPTIONS
=======

-h
  Print help text to standard output and exit.

--entry-point NAME
  Treat this top-level function as an entry point.

--library
  Generate a library instead of an executable.  Appends ``.c``/``.h``
  to the name indicated by the ``-o`` option to determine output
  file names.

-o outfile
  Where to write the result.  If the source program is named
  ``foo.fut``, this defaults to ``foo``.

--safe
  Ignore ``unsafe`` in program and perform safety checks unconditionally.

--server
  Generate a server-mode executable that reads commands from stdin.

-v verbose
  Enable debugging output.  If compilation fails due to a compiler
  error, the result of the last successful compiler step will be
  printed to standard error.

-V
  Print version information on standard output and exit.

-W
  Do not print any warnings.

--Werror
  Treat warnings as errors.

ENVIRONMENT VARIABLES
=====================

``CC``

  The C compiler used to compile the program.  Defaults to ``cc`` if
  unset.

``CFLAGS``

  Space-separated list of options passed to the C compiler.  Defaults
  to ``-O3 -std=c99`` if unset.

EXECUTABLE OPTIONS
==================

The following options are accepted by executables generated by ``futhark c``.

-h, --help

  Print help text to standard output and exit.

-b, --binary-output

  Print the program result in the binary output format.  The default
  is human-readable text, which is very slow.  Not accepted by
  server-mode executables.

--cache-file=FILE

  Store any reusable initialisation data in this file, possibly
  speeding up subsequent launches.

-D, --debugging

  Perform possibly expensive internal correctness checks and verbose
  logging.  Implies ``-L``.

-e, --entry-point=FUN

  The entry point to run.  Defaults to ``main``.  Not accepted by
  server-mode executables.

-L, --log

  Print various low-overhead logging information to stderr while
  running.

-n, --no-print-result

  Do not print the program result.  Not accepted by server-mode
  executables.

-r, --runs=NUM

  Perform NUM runs of the program.  With ``-t``, the runtime for each
  individual run will be printed.  Additionally, a single leading
  warmup run will be performed (not counted).  Only the final run will
  have its result written to stdout.  Not accepted by server-mode
  executables.

-t, --write-runtime-to=FILE

  Print the time taken to execute the program to the indicated file,
  an integral number of microseconds.  Not accepted by server-mode
  executables.


SEE ALSO
========

:ref:`futhark-opencl(1)`, :ref:`futhark-cuda(1)`, :ref:`futhark-test(1)`
