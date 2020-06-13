
## `(arew scheme time)`

### `(current-jiffy)`

Returns the number of jiffies as an exact integer that have elapsed
since an arbitrary, implementation-defined epoch. A jiffy is an
implementation-defined fraction of a second which is defined by the
return value of the jiffies-per-second procedure. The starting epoch
is guaranteed to be constant during a run of the program, but may vary
between runs.

### `(current-second)`

Returns an inexact number representing the current time on the
International Atomic Time (TAI) scale. The value 0.0 represents
midnight on January 1, 1970 TAI (equivalent to ten seconds before
midnight Universal Time) and the value 1.0 represents one TAI second
later. Neither high accuracy nor high precision are required; in
particular, returning Coordinated Universal Time plus a suitable
constant might be the best an implementation can do.

### `(jiffies-per-second)`

Returns an exact integer representing the number of jiffies per SI
second. This value is an implementation-specified constant.
