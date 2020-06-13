
## `(arew scheme process-context)`

### `(command-line)`

Returns the command line passed to the process as a list of
strings. The first string corresponds to the command name, and is
implementation-dependent. It is an error to mutate any of these
strings.

### `(emergency-exit [obj])`

Terminates the program without running any outstanding dynamic-wind
after procedures and communicates an exit value to the operating
system in the same manner as exit.

### `(exit [obj])`

Runs all outstanding dynamic-wind after procedures, terminates the
running program, and communicates an exit value to the operating
system. If no argument is supplied, or if obj is #t, the exit
procedure should communicate to the operating system that the program
exited normally. If obj is #f, the exit procedure should communicate
to the operating system that the program exited abnormally. Otherwise,
exit should translate obj into an appropriate exit value for the
operating system, if possible.

### `(get-environment-variable name)`

Many operating systems provide each running process with an
environment consisting of environment variables. Both the name and
value of an environment variable are strings. The procedure
get-environment-variable returns the value of the environment variable
name, or #f if the named environment variable is not found. It may use
locale information to encode the name and decode the value of the
environment variable. It is an error if get-environment-variable can’t
decode the value. It is also an error to mutate the resulting string.

### `(get-environment-variables)`

Returns the names and values of all the environment variables as an
alist, where the car of each entry is the name of an environment
variable and the cdr is its value, both as strings. The order of the
list is unspecified. It is an error to mutate any of these strings or
the alist itself.
