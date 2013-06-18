racket-abstract-interpretation
==============================

TODO
- change primitive-lookup to be less ad-hoc and classify primitives according to their pertinent behavior
- native functions (such as for-each) will choke on static- and dynamic-closures passed to them; wrap these arguments in native functions that will continue the interpretation
- the toplevel environment is broken; environments are not persistent; because all toplevel variables are known in advance, this should be able to be fixed in a clean way
- the modifications made in interpreter-structs.rkt are expedient, but we should make sure we're not discarding useful information
