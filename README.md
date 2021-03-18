# mandane
Functional Lisp Hypervisor for Apple Silicon M*

[![Build Status](https://travis-ci.com/arbace/mandane.svg?branch=main)](https://travis-ci.com/github/arbace/mandane)
## bare metal research playground, a synergetic aggregation of prior pathfinder studies, including
- [arbace](https://github.com/bodza/arbace)
- [beagle](https://github.com/bodza/beagle)
- [bitclojn](https://github.com/bodza/bitclojn)
- [cloiure](https://github.com/bodza/cloiure)
- [cortex](https://github.com/bodza/cortex)
- [ermine](https://github.com/bodza/ermine)
- [vijure](https://github.com/bodza/vijure)
- [waltraud](https://github.com/bodza/waltraud)
## meta-circular evaluator prototyped praising Clojure, soon to be transduced to advanced Armv8 idioms
```
$ lein mandane
Mandane => (def ζ (λ [f] ((λ [x] (x x)) (λ [x] (f (λ [& s] (apply (x x) s)))))))
...
Mandane => (defn mandane-is [c] (str "Mandane is a minimal " c " interpreter expressed entirely in " c))
Mandane => (mandane-is 'Clojure)
"Mandane is a minimal Clojure interpreter expressed entirely in Clojure"
...
Mandane => (repl)
Mandane => (let [m 'Mandane] (str m " to be expressed entirely in " m " complected from AArch64 primitives"))
"asap!"
```
## literature and credits
- [Asahi Linux](https://github.com/AsahiLinux/m1n1)
- [clojure](https://github.com/clojure/clojure)
- [exactstep](https://github.com/ultraembedded/exactstep)
- [ferret](https://github.com/nakkaya/ferret)
- [graalvm](https://github.com/graalvm/graal)
- [openjdk](https://github.com/AdoptOpenJDK/openjdk-jdk11u)
- [rrb-vector](https://github.com/clojure/core.rrb-vector)
