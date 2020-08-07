(import (scheme base))
(import (scheme write))
(import (scheme process-context))


(display (string-append "hello" " " "world" "!\n"))

(pk "command-line return value is: " (command-line))
