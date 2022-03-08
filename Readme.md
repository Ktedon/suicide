## Suicide Programming Language

Suicide is a programming language designed for writing mission critical applications. So far it looks like this:

```suicide
pkg suicide.std.io

accum suicide.std.encryption._
accum suicide.std.util._._

\ space space0
# Move up to namespace space0
 \ space space1
 # Move up to namespace space0.space1
// space space2
# Move down to namespace space2
```
