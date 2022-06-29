# llrt

llrl relies on a runtime named `llrt` (implemented in C) for some of its language features and standard library implementation.

`llrt` is designed to be as follows:

* As small as possible; keep dependencies on C implementations to a minimum.
* Re-export implementation-dependent definitions, such as `errno` in the C standard, as C functions explicitly.
