racket-abstract-interpretation
==============================

TODO
- change primitive-lookup to be less ad-hoc and classify primitives according to their pertinent behavior
- native functions (such as for-each) will choke on static- and dynamic-closures passed to them; wrap these arguments in native functions that will continue the interpretation
- the modifications made in interpreter-structs.rkt are expedient, but we should make sure we're not discarding useful information
- make dynamic-closures capture only the pertinent environment variables
