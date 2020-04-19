(library (scheme ephemeron)
  (export ephemeron?
          make-ephemeron
          ephemeron-broken?
          ephemeron-key
          ephemeron-value)
  (import (srfi srfi-124)))
