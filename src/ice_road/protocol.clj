(ns ice-road.protocol)

(defprotocol Road
  "A protocol for operations on paths."
  (to-string [this] "Generate a string suitable for storage or printing")
  (as-object [this] "Generate the apropriate corresponding jvm object")
  (under? [this that] "Is that path under this one?")
  (absolute? [this] "Is this path an absolute path?")
  (root? [this] "Is this path at the root?")
  (component [this] "Final component of this path.")
  (extension [this] "Extension of the component, if any.")
  (parameters [this] "Parameters after the final component, if any.")
  (append [this that] "New path with that under this.")
  (equivalent? [this that] "Are the paths equivalent?")
  (parent [this] "The parent of this path."))
