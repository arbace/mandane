(defn #_"bits(M * N)" replicate [#_"bits(M)" x, #_"integer" N]
    (throw! "UNIMPLEMENTED")
)

(defn #_"bits(N)" ones [#_"integer" N]
    (replicate (bits '1), N)
)

(defn #_"boolean" is-ones? [#_"bits(N)" x]
    (= x (ones N))
)

(defn #_"bits(N)" zeros [#_"integer" N]
    (replicate (bits '0), N)
)

(defn #_"boolean" is-zero? [#_"bits(N)" x]
    (= x (zeros N))
)

(defn #_"bits(1)" is-zero-bit? [#_"bits(N)" x]
    (if (is-zero? x) (bits '1) (bits '0))
)

(defn #_"integer" unsigned [#_"bits(N)" x]
    (let [
        #_"integer" result 0
    ]
        (loop [i 0]
            (when (<= i (- N 1))
                (when (= (at x i) (bits '1))
                    ( ass result (+ result (^ 2 i)))
                )
                (recur (inc i))
            )
        )
        result
    )
)

(defn #_"integer" signed [#_"bits(N)" x]
    (let [
        #_"integer" result (unsigned x)
    ]
        (when (= (at x (- N 1)) (bits '1))
            ( ass result (- result (^ 2 N)))
        )
        result
    )
)

(defn #_"bits(N)" sign-extend [#_"bits(M)" x, #_"integer" N]
    (assert (<= M N))
    (cat (replicate (at x (- M 1)), (- N M)) x)
)

(defn #_"bits(N)" zero-extend [#_"bits(M)" x, #_"integer" N]
    (assert (<= M N))
    (cat (zeros (- N M)) x)
)

;; Return the ID of the currently executing PE.

(defn #_"integer" processor-id []
    0
)

(defn #_"bits(64)" PC []
    (throw! "UNIMPLEMENTED")
)

(defn #_"void" pc! [#_"bits(64)" value]
    (throw! "UNIMPLEMENTED")
)

(defn #_"bits(width)" SP []
    (assert (contains? #{8, 16, 32, 64} width))
    (at _R[31] (- width 1) 0)
)

(defn #_"void" sp! [#_"bits(width)" value]
    (assert (contains? #{32, 64} width))
    ( ass _R[31] (zero-extend value, 64))
    nil
)

(defn #_"bits(width)" X [#_"integer" n]
    (assert (and (<= 0 n 31) (contains? #{8, 16, 32, 64} width)))
    (if (not= n 31)
        (at _R[n] (- width 1) 0)
        (zeros width)
    )
)

(defn #_"void" X! [#_"integer" n, #_"bits(width)" value]
    (assert (and (<= 0 n 31) (contains? #{32, 64} width)))
    (when (not= n 31)
        ( ass _R[n] (zero-extend value, 64))
    )
    nil
)

(defn #_"bits(52)" translate-address [#_"bits(64)" address]
    (throw! "UNIMPLEMENTED")
)

;; Sets the Exclusives monitors for the current PE to record the addresses associated
;; with the virtual address region of size bytes starting at address.

(defn #_"void" set-exclusive-monitors [#_"bits(64)" address, #_"integer" size]
    (throw! "UNIMPLEMENTED")
)

;; Return true if the local Exclusives monitor for processorid includes all of
;; the physical address region of size bytes starting at paddress.

(defn #_"boolean" is-exclusive-local? [#_"bits(52)" paddress, #_"integer" processorid, #_"integer" size]
    (throw! "UNIMPLEMENTED")
)

;; Clear the local Exclusives monitor for the specified processorid.

(defn #_"void" clear-exclusive-local [#_"integer" processorid]
    (throw! "UNIMPLEMENTED")
)

;; Return true if the Exclusives monitors for the current PE include all of the addresses
;; associated with the virtual address region of size bytes starting at address.
;; The immediately following memory write must be to the same addresses.

(defn #_"boolean" exclusive-monitors-pass? [#_"bits(64)" address, #_"integer" size]
    (let [
        #_"bits(52)" paddress (translate-address address)
        #_"boolean" passed? (is-exclusive-local? paddress, (processor-id), size)
    ]
        (clear-exclusive-local (processor-id))

        passed?
    )
)

;; Returns '0' to indicate success if the last memory write by this PE was to
;; the same physical address region endorsed by ExclusiveMonitorsPass().
;; Returns '1' to indicate failure if address translation resulted in a different
;; physical address.

(defn #_"bits(1)" exclusive-monitors-status []
    (throw! "UNIMPLEMENTED")
)

;; Clear the global Exclusives monitors for all PEs EXCEPT processorid if they
;; record any part of the physical address region of size bytes starting at paddress.
;; It is IMPLEMENTATION DEFINED whether the global Exclusives monitor for processorid
;; is also cleared if it records any part of the address region.

(defn #_"void" clear-exclusive-by-address [#_"bits(52)" paddress, #_"integer" processorid, #_"integer" size]
    (throw! "UNIMPLEMENTED")
)

(defn #_"bits(size * 8)" mem [#_"bits(64)" address, #_"integer" size]
    (assert (contains? #{1, 2, 4, 8, 16} size))
    (let [
        #_"bits(52)" paddress (translate-address address)
    ]
        ;; Memory array access
        (throw! "UNIMPLEMENTED")
    )
)

(defn #_"void" mem! [#_"bits(64)" address, #_"integer" size, #_"bits(size * 8)" value]
    (assert (contains? #{1, 2, 4, 8, 16} size))
    (let [
        #_"bits(52)" paddress (translate-address address)
    ]
        ;; Effect on exclusives
        (clear-exclusive-by-address paddress, (processor-id), size)

        ;; Memory array access
        (throw! "UNIMPLEMENTED")
    )
    nil
)

;; Return true if UBFX or SBFX is the preferred disassembly of a UBFM or SBFM bitfield instruction.
;; Must exclude more specific aliases UBFIZ, SBFIZ, UXT[BH], SXT[BHW], LSL, LSR and ASR.

(defn #_"boolean" bfx-preferred? [#_"bits(1)" sf, #_"bits(1)" uns, #_"bits(6)" imms, #_"bits(6)" immr]
    (not
        (or
            (< (unsigned imms) (unsigned immr))     ;; must not match UBFIZ/SBFIX alias
            (= imms (cat sf (bits '11111)))         ;; must not match LSR/ASR/LSL alias (imms == 31 or 63)
            (and (= immr (bits '000000))            ;; must not match UXTx/SXTx alias
                (or
                    (and (= sf (bits '0)) (contains? #{(bits '000111), (bits '001111)} imms))                            ;; must not match 32-bit UXT[BH] or SXT[BH]
                    (and (= (cat sf uns) (bits '10)) (contains? #{(bits '000111), (bits '001111), (bits '011111)} imms)) ;; must not match 64-bit SXT[BHW]
                )
            )
            ;; must be UBFX/SBFX alias
        )
    )
)

;; Return true if a bitmask immediate encoding would generate an immediate
;; value that could also be represented by a single MOVZ or MOVN instruction.
;; Used as a condition for the preferred MOV<-ORR alias.

(defn #_"boolean" move-wide-preferred? [#_"bits(1)" sf, #_"bits(1)" immN, #_"bits(6)" imms, #_"bits(6)" immr]
    (let [
        #_"integer" S (unsigned imms)
        #_"integer" R (unsigned immr)
        #_"integer" width (if (= sf (bits '1)) 64 32)
    ]
        (cond
            ;; element size must equal total immediate size
            (and (= sf (bits '1)) (not= (cat immN imms) (bits '1 xxxxxx))) false
            (and (= sf (bits '0)) (not= (cat immN imms) (bits '0 0xxxxx))) false

            ;; for MOVZ must contain no more than 16 ones
            ;; ones must not span halfword boundary when rotated
            (< S 16) (<= (MOD (- R) 16) (- 15 S))

            ;; for MOVN must contain no more than 16 zeros
            ;; zeros must not span halfword boundary when rotated
            (<= (- width 15) S) (<= (MOD R 16) (- S (- width 15)))

            :else false
        )
    )
)

(defn #_"integer" highest-set-bit [#_"bits(N)" x]
    (loop [i (- N 1)]
        (if (and (<= 0 i) (not= (at x i) (bits '1)))
            (recur (dec i))
            i
        )
    )
)

(defn #_"integer" count-leading-zero-bits [#_"bits(N)" x]
    (- N (+ (highest-set-bit x) 1))
)

(defn #_"integer" count-leading-sign-bits [#_"bits(N)" x]
    (count-leading-zero-bits (EOR (at x (- N 1) 1) (at x (- N 2) 0)))
)

;; Decode AArch64 bitfield and logical immediate masks which use a similar encoding structure.

(defn [#_"bits(M)", #_"bits(M)"] decode-bit-masks [#_"bits(1)" immN, #_"bits(6)" imms, #_"bits(6)" immr, #_"boolean" immediate?]
    (let [
        ;; Compute log2 of element size -- 2^len must be in range [2, M]
        #_"integer" len (highest-set-bit (cat immN (NOT imms)))
    ]
        (when (< len 1)
            (throw! "UNDEFINED")
        )
        (assert (<= (<< 1 len) M))
        (let [
            ;; Determine S, R and S - R parameters
            #_"bits(6)" levels (zero-extend (ones len), 6)
        ]
            ;; For logical immediates an all-ones value of S is reserved
            ;; since it would generate a useless all-ones result (many times)
            (when (and immediate? (= (AND imms levels) levels))
                (throw! "UNDEFINED")
            )
            (let [
                #_"integer" S (unsigned (AND imms levels))
                #_"integer" R (unsigned (AND immr levels))
                #_"integer" diff (- S R)   ;; 6-bit subtract with borrow

                ;; From a software perspective, the remaining code is equivalant to:
                ;;   esize = 1 << len;
                ;;   d = UInt(diff<len-1:0>);
                ;;   welem = ZeroExtend(Ones(S + 1), esize);
                ;;   telem = ZeroExtend(Ones(d + 1), esize);
                ;;   wmask = Replicate(ROR(welem, R));
                ;;   tmask = Replicate(telem);
                ;;   return (wmask, tmask);

                ;; Compute "top mask"
                #_"bits(6)" tmask_and (OR (at diff 5 0) (NOT levels))
                #_"bits(6)" tmask_or (AND (at diff 5 0) levels)

                #_"bits(64)" tmask (ones 64)
                tmask (OR (AND tmask (replicate (cat (replicate (at tmask_and 0),  1) (ones  1)), 32)) (replicate (cat (zeros  1) (replicate (at tmask_or 0),  1)), 32))
                ;; optimization of first step:
                ;; tmask = Replicate(tmask_and<0> : '1', 32);
                tmask (OR (AND tmask (replicate (cat (replicate (at tmask_and 1),  2) (ones  2)), 16)) (replicate (cat (zeros  2) (replicate (at tmask_or 1),  2)), 16))
                tmask (OR (AND tmask (replicate (cat (replicate (at tmask_and 2),  4) (ones  4)),  8)) (replicate (cat (zeros  4) (replicate (at tmask_or 2),  4)),  8))
                tmask (OR (AND tmask (replicate (cat (replicate (at tmask_and 3),  8) (ones  8)),  4)) (replicate (cat (zeros  8) (replicate (at tmask_or 3),  8)),  4))
                tmask (OR (AND tmask (replicate (cat (replicate (at tmask_and 4), 16) (ones 16)),  2)) (replicate (cat (zeros 16) (replicate (at tmask_or 4), 16)),  2))
                tmask (OR (AND tmask (replicate (cat (replicate (at tmask_and 5), 32) (ones 32)),  1)) (replicate (cat (zeros 32) (replicate (at tmask_or 5), 32)),  1))

                ;; Compute "wraparound mask"
                #_"bits(6)" wmask_and (OR immr (NOT levels))
                #_"bits(6)" wmask_or (AND immr levels)

                #_"bits(64)" wmask (zeros 64)
                wmask (OR (AND wmask (replicate (cat (ones  1) (replicate (at wmask_and 0),  1)), 32)) (replicate (cat (replicate (at wmask_or 0),  1) (zeros  1)), 32))
                ;; optimization of first step:
                ;; wmask = Replicate(wmask_or<0> : '0', 32);
                wmask (OR (AND wmask (replicate (cat (ones  2) (replicate (at wmask_and 1),  2)), 16)) (replicate (cat (replicate (at wmask_or 1),  2) (zeros  2)), 16))
                wmask (OR (AND wmask (replicate (cat (ones  4) (replicate (at wmask_and 2),  4)),  8)) (replicate (cat (replicate (at wmask_or 2),  4) (zeros  4)),  8))
                wmask (OR (AND wmask (replicate (cat (ones  8) (replicate (at wmask_and 3),  8)),  4)) (replicate (cat (replicate (at wmask_or 3),  8) (zeros  8)),  4))
                wmask (OR (AND wmask (replicate (cat (ones 16) (replicate (at wmask_and 4), 16)),  2)) (replicate (cat (replicate (at wmask_or 4), 16) (zeros 16)),  2))
                wmask (OR (AND wmask (replicate (cat (ones 32) (replicate (at wmask_and 5), 32)),  1)) (replicate (cat (replicate (at wmask_or 5), 32) (zeros 32)),  1))

                wmask
                    (if (not= (at diff 6) (bits '0))      ;; borrow from S - R
                        (AND wmask tmask)
                        (OR wmask tmask)
                    )
            ]
                [(at wmask (- M 1) 0), (at tmask (- M 1) 0)]
            )
        )
    )
)

;; Perform a register extension and shift

(defn #_"bits(N)" extend-reg [#_"integer" reg, #_"bits(3)" exttype, #_"integer" shift]
    (assert (<= 0 shift 4))
    (let [
        #_"bits(N)" val (X reg)
        [#_"boolean" signed?, #_"integer" len]
            (case exttype
                (bits '000) [false, 8]
                (bits '001) [false, 16]
                (bits '010) [false, 32]
                (bits '011) [false, 64]
                (bits '100) [true, 8]
                (bits '101) [true, 16]
                (bits '110) [true, 32]
                (bits '111) [true, 64]
            )

        ;; Note the extended width of the intermediate value and that sign extension occurs
        ;; from bit <len+shift-1>, not from bit <len-1>. This is equivalent to the instruction
        ;;     [SU]BFIZ Rtmp, Rreg, #shift, #len
        ;; It may also be seen as a sign/zero extend followed by a shift:
        ;;     LSL(Extend(val<len-1:0>, N, unsigned), shift);

        len (min len (- N shift))
    ]
        ((if signed? sign-extend zero-extend) (cat (at val (- len 1) 0) (zeros shift)), N)
    )
)

(defn #_"bits(N)" lsl [#_"bits(N)" x, #_"integer" shift]
    (assert (<= 0 shift))
    (if (= shift 0)
        x
        (at (cat x (zeros shift)) (- N 1) 0)
    )
)

(defn #_"bits(N)" lsr [#_"bits(N)" x, #_"integer" shift]
    (assert (<= 0 shift))
    (if (= shift 0)
        x
        (at (zero-extend x, (+ shift N)) (- (+ shift N) 1) shift)
    )
)

(defn #_"bits(N)" asr [#_"bits(N)" x, #_"integer" shift]
    (assert (<= 0 shift))
    (if (= shift 0)
        x
        (at (sign-extend x, (+ shift N)) (- (+ shift N) 1) shift)
    )
)

(defn #_"bits(N)" ror [#_"bits(N)" x, #_"integer" shift]
    (assert (<= 0 shift))
    (if (= shift 0)
        x
        (let [
            m (MOD shift N)
        ]
            (OR (lsr x, m) (lsl x, (- N m)))
        )
    )
)

;; Perform shift of a register operand

(defn #_"bits(N)" shift-reg [#_"integer" reg, #_"bits(2)" shiftype, #_"integer" amount]
    (let [
        #_"bits(N)" result (X reg)
    ]
        (case shiftype
            (bits '00) (lsl result, amount)
            (bits '01) (lsr result, amount)
            (bits '10) (asr result, amount)
            (bits '11) (ror result, amount)
        )
    )
)

;; Integer addition with carry input, returning result and NZCV flags

(defn [#_"bits(N)", #_"bits(4)"] add-with-carry [#_"bits(N)" x, #_"bits(N)" y, #_"bits(1)" carry_in]
    (let [
        #_"integer" unsigned_sum (+ (unsigned x) (unsigned y) (unsigned carry_in))
        #_"integer" signed_sum (+ (signed x) (signed y) (unsigned carry_in))
        #_"bits(N)" result (at unsigned_sum (- N 1) 0)     ;; same value as signed_sum<N-1:0>
        #_"bits(1)" n (at result (- N 1))
        #_"bits(1)" z (if (is-zero? result) (bits '1) (bits '0))
        #_"bits(1)" c (if (= (unsigned result) unsigned_sum) (bits '0) (bits '1))
        #_"bits(1)" v (if (= (signed result) signed_sum) (bits '0) (bits '1))
    ]
        [result, (cat n z c v)]
    )
)

;; Return the virtual address with tag bits removed for storing to the program counter.

(defn #_"bits(64)" branch-addr [#_"bits(64)" vaddress]
    (let [
        #_"integer" msbit 55
    ]
        (if (= (at vaddress msbit) (bits '1))
            (sign-extend (at vaddress msbit 0), 64)
            (zero-extend (at vaddress msbit 0), 64)
        )
    )
)

;; Set program counter to a new address

(defn #_"void" branch-to [#_"bits(N)" target]
    (assert (= N 64))
    (pc! (branch-addr (at target 63 0)))
    nil
)

;; Return true iff COND currently holds

(defn #_"boolean" condition-holds? [#_"bits(4)" cond]
    (let [
        ;; Evaluate base condition.
        ?
            (case (at cond 3 1)
                (bits '000) (= PSTATE'Z (bits '1))                              ;; EQ or NE
                (bits '001) (= PSTATE'C (bits '1))                              ;; CS or CC
                (bits '010) (= PSTATE'N (bits '1))                              ;; MI or PL
                (bits '011) (= PSTATE'V (bits '1))                              ;; VS or VC
                (bits '100) (and (= PSTATE'C (bits '1)) (= PSTATE'Z (bits '0))) ;; HI or LS
                (bits '101) (= PSTATE'N PSTATE'V)                               ;; GE or LT
                (bits '110) (and (= PSTATE'N PSTATE'V) (= PSTATE'Z (bits '0)))  ;; GT or LE
                (bits '111) true                                                ;; AL
            )
    ]
        ;; Condition flag values in the set '111x' indicate always true
        ;; Otherwise, invert condition if necessary.
        (if (and (= (at cond 0) (bits '1)) (not= cond (bits '1111)))
            (not ?)
            ?
        )
    )
)


;;;; Top-level encodings for A64
;;
;;; 31 30 29 | 28 27 26 25 | 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;                op0

(defn top-level-encodings-for-a64! [#_"bits(32)" I]
    (let [
        #_"bits(4)" op0 (at I 28 25)
    ]
        (case op0
            (bits '0000) (reserved! I)
            (bits '100x) (data-processing-immediate! I)
            (bits '101x) (branches-exception-generating-and-system-instructions! I)
            (bits 'x100) (loads-and-stores! I)
            (bits 'x101) (data-processing-register! I)
        )
    )
)


;;;; Reserved
;;
;;; 31 30 29 | 28 27 26 25 | 24 23 22 21 20 19 18 17 16 | 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;   op0       0  0  0  0               op1

(defn reserved! [#_"bits(32)" I]
    (let [
        #_"bits(3)" op0 (at I 31 29)
        #_"bits(9)" op1 (at I 24 16)
    ]
        (case (cat op0 op1)
            (bits '000000000000) (udf! I)
        )
    )
)

;;;; UDF
;;
;;; Permanently Undefined generates an Undefined Instruction exception (ESR_ELx.EC = 0b000000). The encodings for
;;; UDF used in this section are defined as permanently UNDEFINED in the Armv8-A architecture.
;;
;;; 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 | 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0                        imm16
;;
;;; "UDF #<imm>"
;;
;;; The imm16 field is ignored by hardware.
;;
;;;; Assembler Symbols
;;
;;; <imm>   is a 16-bit unsigned immediate, in the range 0 to 65535, encoded in the "imm16" field. The PE ignores
;;;         the value of this constant.

(defn udf! [#_"bits(32)" I]
    (let [
        #_"bits(16)" imm16 (at I 15 0)
    ]
        ;; No operation.
    )
)


;;;; Data Processing -- Immediate
;;
;;; 31 30 29 | 28 27 26 | 25 24 23 | 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;             1  0  0      op0

(defn data-processing-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(3)" op0 (at I 25 23)
    ]
        (case op0
            (bits '00x) (pc-rel-addressing! I)
            (bits '010) (add-subtract-immediate! I)
            (bits '100) (logical-immediate! I)
            (bits '101) (move-wide-immediate! I)
            (bits '110) (bitfield! I)
            (bits '111) (extract! I)
        )
    )
)


;;;; PC-rel. addressing
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  P            1  0  0  0  0

(defn pc-rel-addressing! [#_"bits(32)" I]
    (let [
        #_"bits(1)" P (at I 31)
    ]
        (case P
            (bits '0) (adr! I)
            (bits '1) (adrp! I)
        )
    )
)

;;;; ADR
;;
;;; Form PC-relative address adds an immediate value to the PC value to form a PC-relative address, and writes the result
;;; to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;;  0   immlo    1  0  0  0  0                             immhi                                  Rd
;;;  P
;;
;;; "ADR <Xd>, <label>"
;;
;;;; Assembler Symbols
;;
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <label>     Is the program label whose address is to be calculated. Its offset from the address of this instruction,
;;;             in the range +/-1MB, is encoded in "immhi:immlo".

(defn adr! [#_"bits(32)" I]
    (let [
        #_"bits(1)" P (at I 31)
        #_"bits(2)" immlo (at I 30 29)
        #_"bits(19)" immhi (at I 23 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"bits(64)" imm (sign-extend (cat immhi immlo), 64)

        #_"bits(64)" base (pc)
    ]
        (X! d (+ base imm))
    )
)

;;;; ADRP
;;
;;; Form PC-relative address to 4KB page adds an immediate value that is shifted left by 12 bits, to the PC value to form a
;;; PC-relative address, with the bottom 12 bits masked out, and writes the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;;  1   immlo    1  0  0  0  0                             immhi                                  Rd
;;;  P
;;
;;; "ADRP <Xd>, <label>"
;;
;;;; Assembler Symbols
;;
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <label>     Is the program label whose 4KB page address is to be calculated. Its offset from the page address of this instruction,
;;;             in the range +/-4GB, is encoded as "immhi:immlo" times 4096.

(defn adrp! [#_"bits(32)" I]
    (let [
        #_"bits(1)" P (at I 31)
        #_"bits(2)" immlo (at I 30 29)
        #_"bits(19)" immhi (at I 23 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"bits(64)" imm (sign-extend (cat immhi immlo (zeros 12)), 64)

        #_"bits(64)" base (pc)
    ]
        ( ass (at base 11 0) (zeros 12))

        (X! d (+ base imm))
    )
)


;;;; Add/subtract (immediate)
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 | 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;       N    S    1  0  0  0  1  0

(defn add-subtract-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
    ]
        (case (cat N S)
            (bits '00) (add-immediate! I)
            (bits '01) (adds-immediate! I)
            (bits '10) (sub-immediate! I)
            (bits '11) (subs-immediate! I)
        )
    )
)

;;;; ADD (immediate)
;;
;;; Add (immediate) adds a register value and an optionally-shifted immediate value,
;;; and writes the result to the destination register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    1  0  0  0  1  0   sh                  imm12                        Rn               Rd
;;;       N    S
;;
;;; "ADD <Wd|WSP>, <Wn|WSP>, #<imm>{, <shift>}" [32-bit (sf == 0)]
;;; "ADD <Xd|SP>, <Xn|SP>, #<imm>{, <shift>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd|WSP>    Is the 32-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Wn|WSP>    Is the 32-bit name of the source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <Xd|SP>     Is the 64-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Xn|SP>     Is the 64-bit name of the source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <imm>       Is an unsigned immediate, in the range 0 to 4095, encoded in the "imm12" field.
;;; <shift>     Is the optional left shift to apply to the immediate, defaulting to LSL #0 and encoded in “sh”:
;;;                  sh   <shift>
;;;                   0   LSL #0
;;;                   1   LSL #12

;;;; MOV (to/from SP) [alias for (sh == '0' && imm12 == '000000000000' && (Rd == '11111' || Rn == '11111'))]
;;
;;; Move between register and stack pointer: Rd = Rn.
;;
;;; "MOV <Wd|WSP>, <Wn|WSP>" === "ADD <Wd|WSP>, <Wn|WSP>, #0" [32-bit (sf == 0)]
;;; "MOV <Xd|SP>, <Xn|SP>" === "ADD <Xd|SP>, <Xn|SP>, #0" [64-bit (sf == 1)]

(defn add-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(1)" sh (at I 22)
        #_"bits(12)" imm12 (at I 21 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"bits(width)" operand2
            (case sh
                (bits '0) (zero-extend imm12, width)
                (bits '1) (zero-extend (cat imm12 (zeros 12)), width)
            )

        #_"bits(width)" operand1 (if (= n 31) (sp) (X n))
        [#_"bits(width)" result, _] (add-with-carry operand1, operand2, (bits '0))
    ]
        (if (= d 31)
            (sp! result)
            (X! d result)
        )
    )
)

(def #_"alias" mov-to-from-sp! add-immediate!)

;;;; ADDS (immediate)
;;
;;; Add (immediate), setting flags, adds a register value and an optionally-shifted immediate value, and writes the result
;;; to the destination register. It updates the condition flags based on the result.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    1    1  0  0  0  1  0   sh                  imm12                        Rn               Rd
;;;       N    S
;;
;;; "ADDS <Wd>, <Wn|WSP>, #<imm>{, <shift>}" [32-bit (sf == 0)]
;;; "ADDS <Xd>, <Xn|SP>, #<imm>{, <shift>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn|WSP>    Is the 32-bit name of the source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn|SP>     Is the 64-bit name of the source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <imm>       Is an unsigned immediate, in the range 0 to 4095, encoded in the "imm12" field.
;;; <shift>     Is the optional left shift to apply to the immediate, defaulting to LSL #0 and encoded in “sh”:
;;;                  sh   <shift>
;;;                   0   LSL #0
;;;                   1   LSL #12

;;;; CMN (immediate) [alias for (Rd == '11111')]
;;
;;; Compare Negative (immediate) adds a register value and an optionally-shifted immediate value.
;;; It updates the condition flags based on the result, and discards the result.
;;
;;; "CMN <Wn|WSP>, #<imm>{, <shift>}" === "ADDS WZR, <Wn|WSP>, #<imm> {, <shift>}" [32-bit (sf == 0)]
;;; "CMN <Xn|SP>, #<imm>{, <shift>}" === "ADDS XZR, <Xn|SP>, #<imm> {, <shift>}" [64-bit (sf == 1)]

(defn adds-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(1)" sh (at I 22)
        #_"bits(12)" imm12 (at I 21 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"bits(width)" operand2
            (case sh
                (bits '0) (zero-extend imm12, width)
                (bits '1) (zero-extend (cat imm12 (zeros 12)), width)
            )

        #_"bits(width)" operand1 (if (= n 31) (sp) (X n))
        [#_"bits(width)" result, #_"bits(4)" nzcv] (add-with-carry operand1, operand2, (bits '0))
    ]
        ( ass PSTATE'NZCV nzcv)
        (X! d result)
    )
)

(def #_"alias" cmn-immediate! adds-immediate!)

;;;; SUB (immediate)
;;
;;; Subtract (immediate) subtracts an optionally-shifted immediate value from a register value,
;;; and writes the result to the destination register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    1  0  0  0  1  0   sh                  imm12                        Rn               Rd
;;;       N    S
;;
;;; "SUB <Wd|WSP>, <Wn|WSP>, #<imm>{, <shift>}" [32-bit (sf == 0)]
;;; "SUB <Xd|SP>, <Xn|SP>, #<imm>{, <shift>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd|WSP>    Is the 32-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Wn|WSP>    Is the 32-bit name of the source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <Xd|SP>     Is the 64-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Xn|SP>     Is the 64-bit name of the source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <imm>       Is an unsigned immediate, in the range 0 to 4095, encoded in the "imm12" field.
;;; <shift>     Is the optional left shift to apply to the immediate, defaulting to LSL #0 and encoded in “sh”:
;;;                  sh   <shift>
;;;                   0   LSL #0
;;;                   1   LSL #12

(defn sub-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(1)" sh (at I 22)
        #_"bits(12)" imm12 (at I 21 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"bits(width)" operand2
            (case sh
                (bits '0) (zero-extend imm12, width)
                (bits '1) (zero-extend (cat imm12 (zeros 12)), width)
            )

        #_"bits(width)" operand1 (if (= n 31) (sp) (X n))
        [#_"bits(width)" result, _] (add-with-carry operand1, (NOT operand2), (bits '1))
    ]
        (if (= d 31)
            (sp! result)
            (X! d result)
        )
    )
)

;;;; SUBS (immediate)
;;
;;; Subtract (immediate), setting flags, subtracts an optionally-shifted immediate value from a register value, and writes
;;; the result to the destination register. It updates the condition flags based on the result.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    1    1  0  0  0  1  0   sh                  imm12                        Rn               Rd
;;;       N    S
;;
;;; "SUBS <Wd>, <Wn|WSP>, #<imm>{, <shift>}" [32-bit (sf == 0)]
;;; "SUBS <Xd>, <Xn|SP>, #<imm>{, <shift>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn|WSP>    Is the 32-bit name of the source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn|SP>     Is the 64-bit name of the source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <imm>       Is an unsigned immediate, in the range 0 to 4095, encoded in the "imm12" field.
;;; <shift>     Is the optional left shift to apply to the immediate, defaulting to LSL #0 and encoded in “sh”:
;;;                  sh   <shift>
;;;                   0   LSL #0
;;;                   1   LSL #12

;;;; CMP (immediate) [alias for (Rd == '11111')]
;;
;;; Compare (immediate) subtracts an optionally-shifted immediate value from a register value. It updates the condition
;;; flags based on the result, and discards the result.
;;
;;; "CMP <Wn|WSP>, #<imm>{, <shift>}" === "SUBS WZR, <Wn|WSP>, #<imm> {, <shift>}" [32-bit (sf == 0)]
;;; "CMP <Xn|SP>, #<imm>{, <shift>}" === "SUBS XZR, <Xn|SP>, #<imm> {, <shift>}" [64-bit (sf == 1)]

(defn subs-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(1)" sh (at I 22)
        #_"bits(12)" imm12 (at I 21 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"bits(width)" operand2
            (case sh
                (bits '0) (zero-extend imm12, width)
                (bits '1) (zero-extend (cat imm12 (zeros 12)), width)
            )

        #_"bits(width)" operand1 (if (= n 31) (sp) (X n))
        [#_"bits(width)" result, #_"bits(4)" nzcv] (add-with-carry operand1, (NOT operand2), (bits '1))
    ]
        ( ass PSTATE'NZCV nzcv)
        (X! d result)
    )
)

(def #_"alias" cmp-immediate! subs-immediate!)


;;;; Logical (immediate)
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;       opc     1  0  0  1  0  0

(defn logical-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 30 29)
    ]
        (case opc
            (bits '00) (and-immediate! I)
            (bits '01) (orr-immediate! I)
            (bits '10) (eor-immediate! I)
            (bits '11) (ands-immediate! I)
        )
    )
)

;;;; AND (immediate)
;;
;;; Bitwise AND (immediate) performs a bitwise AND of a register value and an immediate value, and writes the result to
;;; the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  0    1  0  0  1  0  0    N          immr                imms               Rn               Rd
;;;       opc
;;
;;; "AND <Wd|WSP>, <Wn>, #<imm>" [32-bit (sf == 0 && N == 0)]
;;; "AND <Xd|SP>, <Xn>, #<imm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd|WSP>    Is the 32-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd|SP>     Is the 64-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <imm>       For the 32-bit variant: is the bitmask immediate, encoded in "imms:immr".
;;;             For the 64-bit variant: is the bitmask immediate, encoded in "N:imms:immr".

(defn and-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(1)" N (at I 22)
        #_"bits(6)" immr (at I 21 16)
        #_"bits(6)" imms (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (not= N (bits '0)))
                (throw! "UNDEFINED")
            )

        [#_"bits(width)" imm, _] (decode-bit-masks N, imms, immr, true)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" result (AND operand1 imm)
    ]
        (if (= d 31)
            (sp! result)
            (X! d result)
        )
    )
)

;;;; ORR (immediate)
;;
;;; Bitwise OR (immediate) performs a bitwise (inclusive) OR of a register value and an immediate register value,
;;; and writes the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  1    1  0  0  1  0  0    N          immr                imms               Rn               Rd
;;;       opc
;;
;;; "ORR <Wd|WSP>, <Wn>, #<imm>" [32-bit (sf == 0 && N == 0)]
;;; "ORR <Xd|SP>, <Xn>, #<imm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd|WSP>    Is the 32-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd|SP>     Is the 64-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <imm>       For the 32-bit variant: is the bitmask immediate, encoded in "imms:immr".
;;;             For the 64-bit variant: is the bitmask immediate, encoded in "N:imms:immr".

;;;; MOV (bitmask immediate) [alias for (Rn == '11111' && ! MoveWidePreferred(sf, N, imms, immr))]
;;
;;; Move (bitmask immediate) writes a bitmask immediate value to a register.
;;
;;; "MOV <Wd|WSP>, #<imm>" === "ORR <Wd|WSP>, WZR, #<imm>" [32-bit (sf == 0 && N == 0)]
;;; "MOV <Xd|SP>, #<imm>" === "ORR <Xd|SP>, XZR, #<imm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <imm>       For the 32-bit variant: is the bitmask immediate, encoded in "imms:immr",
;;;             but excluding values which could be encoded by MOVZ or MOVN.
;;;             For the 64-bit variant: is the bitmask immediate, encoded in "N:imms:immr",
;;;             but excluding values which could be encoded by MOVZ or MOVN.

(defn orr-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(1)" N (at I 22)
        #_"bits(6)" immr (at I 21 16)
        #_"bits(6)" imms (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (not= N (bits '0)))
                (throw! "UNDEFINED")
            )

        [#_"bits(width)" imm, _] (decode-bit-masks N, imms, immr, true)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" result (OR operand1 imm)
    ]
        (if (= d 31)
            (sp! result)
            (X! d result)
        )
    )
)

(def #_"alias" mov-bitmask-immediate! orr-immediate!)

;;;; EOR (immediate)
;;
;;; Bitwise Exclusive OR (immediate) performs a bitwise Exclusive OR of a register value and an immediate value, and writes
;;; the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1  0    1  0  0  1  0  0    N          immr                imms               Rn               Rd
;;;       opc
;;
;;; "EOR <Wd|WSP>, <Wn>, #<imm>" [32-bit (sf == 0 && N == 0)]
;;; "EOR <Xd|SP>, <Xn>, #<imm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd|WSP>    Is the 32-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd|SP>     Is the 64-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <imm>       For the 32-bit variant: is the bitmask immediate, encoded in "imms:immr".
;;;             For the 64-bit variant: is the bitmask immediate, encoded in "N:imms:immr".

(defn eor-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(1)" N (at I 22)
        #_"bits(6)" immr (at I 21 16)
        #_"bits(6)" imms (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (not= N (bits '0)))
                (throw! "UNDEFINED")
            )

        [#_"bits(width)" imm, _] (decode-bit-masks N, imms, immr, true)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" result (EOR operand1 imm)
    ]
        (if (= d 31)
            (sp! result)
            (X! d result)
        )
    )
)

;;;; ANDS (immediate)
;;
;;; Bitwise AND (immediate), setting flags, performs a bitwise AND of a register value and an immediate value, and writes
;;; the result to the destination register. It updates the condition flags based on the result.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1  1    1  0  0  1  0  0    N          immr                imms               Rn               Rd
;;;       opc
;;
;;; "ANDS <Wd>, <Wn>, #<imm>" [32-bit (sf == 0 && N == 0)]
;;; "ANDS <Xd>, <Xn>, #<imm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <imm>       For the 32-bit variant: is the bitmask immediate, encoded in "imms:immr".
;;;             For the 64-bit variant: is the bitmask immediate, encoded in "N:imms:immr".

;;;; TST (immediate) [alias for (Rd == '11111')]
;;
;;; Test bits (immediate), setting the condition flags and discarding the result: Rn AND imm.
;;
;;; "TST <Wn>, #<imm>" === "ANDS WZR, <Wn>, #<imm>" [32-bit (sf == 0 && N == 0)]
;;; "TST <Xn>, #<imm>" === "ANDS XZR, <Xn>, #<imm>" [64-bit (sf == 1)]

(defn ands-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(1)" N (at I 22)
        #_"bits(6)" immr (at I 21 16)
        #_"bits(6)" imms (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (not= N (bits '0)))
                (throw! "UNDEFINED")
            )

        [#_"bits(width)" imm, _] (decode-bit-masks N, imms, immr, true)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" result (AND operand1 imm)
    ]
        ( ass PSTATE'NZCV (cat (at result (- width 1)) (is-zero-bit? result) (bits '00)))
        (X! d result)
    )
)

(def #_"alias" tst-immediate! ands-immediate!)


;;;; Move wide (immediate)
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;       opc     1  0  0  1  0  1

(defn move-wide-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 30 29)
    ]
        (case opc
            (bits '00) (movn! I)
            (bits '10) (movz! I)
            (bits '11) (movk! I)
        )
    )
)

;;;; MOVN
;;
;;; Move wide with NOT moves the inverse of an optionally-shifted 16-bit immediate value to a register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 21 | 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  0    1  0  0  1  0  1     hw                         imm16                              Rd
;;;       opc
;;
;;; "MOVN <Wd>, #<imm>{, LSL #<shift>}" [32-bit (sf == 0 && hw == 0x)]
;;; "MOVN <Xd>, #<imm>{, LSL #<shift>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <imm>       Is the 16-bit unsigned immediate, in the range 0 to 65535, encoded in the "imm16" field.
;;; <shift>     For the 32-bit variant: is the amount by which to shift the immediate left, either 0 (the default) or 16,
;;;             encoded in the "hw" field as <shift>/16.
;;;             For the 64-bit variant: is the amount by which to shift the immediate left, either 0 (the default), 16, 32 or 48,
;;;             encoded in the "hw" field as <shift>/16.

;;;; MOV (inverted wide immediate) [alias for (32-bit (! (IsZero(imm16) && hw != '00') && ! IsOnes(imm16)))] [alias for (64-bit (! (IsZero(imm16) && hw != '00')))]
;;
;;; Move (inverted wide immediate) moves an inverted 16-bit immediate value to a register.
;;
;;; "MOV <Wd>, #<imm>" === "MOVN <Wd>, #<imm16>, LSL #<shift>" [32-bit (sf == 0 && hw == 0x)]
;;; "MOV <Xd>, #<imm>" === "MOVN <Xd>, #<imm16>, LSL #<shift>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <imm>       For the 32-bit variant: is a 32-bit immediate, the bitwise inverse of which can be encoded in "imm16:hw",
;;;             but excluding 0xffff0000 and 0x0000ffff
;;;             For the 64-bit variant: is a 64-bit immediate, the bitwise inverse of which can be encoded in "imm16:hw".

(defn movn! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" hw (at I 22 21)
        #_"bits(16)" imm16 (at I 20 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at hw 1) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" pos (unsigned (cat hw (bits '0000)))

        #_"bits(width)" result (zeros width)
    ]
        ( ass (at result (+ pos 15) pos) imm16)
        (X! d (NOT result))
    )
)

(def #_"alias" mov-inverted-wide-immediate! movn!)

;;;; MOVZ
;;
;;; Move wide with zero moves an optionally-shifted 16-bit immediate value to a register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 21 | 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1  0    1  0  0  1  0  1     hw                         imm16                              Rd
;;;       opc
;;
;;; "MOVZ <Wd>, #<imm>{, LSL #<shift>}" [32-bit (sf == 0 && hw == 0x)]
;;; "MOVZ <Xd>, #<imm>{, LSL #<shift>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <imm>       Is the 16-bit unsigned immediate, in the range 0 to 65535, encoded in the "imm16" field.
;;; <shift>     For the 32-bit variant: is the amount by which to shift the immediate left, either 0 (the default) or 16,
;;;             encoded in the "hw" field as <shift>/16.
;;;             For the 64-bit variant: is the amount by which to shift the immediate left, either 0 (the default), 16, 32 or 48,
;;;             encoded in the "hw" field as <shift>/16.

;;;; MOV (wide immediate) [alias for (! (IsZero(imm16) && hw != '00'))]
;;
;;; Move (wide immediate) moves a 16-bit immediate value to a register.
;;
;;; "MOV <Wd>, #<imm>" === "MOVZ <Wd>, #<imm16>, LSL #<shift>" [32-bit (sf == 0 && hw == 0x)]
;;; "MOV <Xd>, #<imm>" === "MOVZ <Xd>, #<imm16>, LSL #<shift>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <imm>       For the 32-bit variant: is a 32-bit immediate which can be encoded in "imm16:hw".
;;;             For the 64-bit variant: is a 64-bit immediate which can be encoded in "imm16:hw".

(defn movz! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" hw (at I 22 21)
        #_"bits(16)" imm16 (at I 20 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at hw 1) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" pos (unsigned (cat hw (bits '0000)))

        #_"bits(width)" result (zeros width)
    ]
        ( ass (at result (+ pos 15) pos) imm16)
        (X! d result)
    )
)

(def #_"alias" mov-wide-immediate! movz!)

;;;; MOVK
;;
;;; Move wide with keep moves an optionally-shifted 16-bit immediate value into a register, keeping other bits unchanged.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 21 | 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1  1    1  0  0  1  0  1     hw                         imm16                              Rd
;;;       opc
;;
;;; "MOVK <Wd>, #<imm>{, LSL #<shift>}" [32-bit (sf == 0 && hw == 0x)]
;;; "MOVK <Xd>, #<imm>{, LSL #<shift>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <imm>       Is the 16-bit unsigned immediate, in the range 0 to 65535, encoded in the "imm16" field.
;;; <shift>     For the 32-bit variant: is the amount by which to shift the immediate left, either 0 (the default) or 16,
;;;             encoded in the "hw" field as <shift>/16.
;;;             For the 64-bit variant: is the amount by which to shift the immediate left, either 0 (the default), 16, 32 or 48,
;;;             encoded in the "hw" field as <shift>/16.

(defn movk! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" hw (at I 22 21)
        #_"bits(16)" imm16 (at I 20 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at hw 1) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" pos (unsigned (cat hw (bits '0000)))

        #_"bits(width)" result (X d)
    ]
        ( ass (at result (+ pos 15) pos) imm16)
        (X! d result)
    )
)


;;;; Bitfield
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;       opc     1  0  0  1  1  0

(defn bitfield! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 30 29)
    ]
        (case opc
            (bits '00) (sbfm! I)
            (bits '01) (bfm! I)
            (bits '10) (ubfm! I)
        )
    )
)

;;;; SBFM
;;
;;; Signed Bitfield Move is usually accessed via one of its aliases, which are always preferred for disassembly.
;;; If <imms> is greater than or equal to <immr>, this copies a bitfield of (<imms>-<immr>+1) bits starting from
;;; bit position <immr> in the source register to the least significant bits of the destination register.
;;; If <imms> is less than <immr>, this copies a bitfield of (<imms>+1) bits from the least significant bits of
;;; the source register to bit position (regsize-<immr>) of the destination register, where regsize is the destination
;;; register size of 32 or 64 bits.
;;; In both cases the destination bits below the bitfield are set to zero, and the bits above the bitfield are set to
;;; a copy of the most significant bit of the bitfield.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  0    1  0  0  1  1  0    N          immr                imms               Rn               Rd
;;;       opc
;;
;;; "SBFM <Wd>, <Wn>, #<immr>, #<imms>" [32-bit (sf == 0 && N == 0)]
;;; "SBFM <Xd>, <Xn>, #<immr>, #<imms>" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <immr>      For the 32-bit variant: is the right rotate amount, in the range 0 to 31, encoded in the "immr" field.
;;;             For the 64-bit variant: is the right rotate amount, in the range 0 to 63, encoded in the "immr" field.
;;; <imms>      For the 32-bit variant: is the leftmost bit number to be moved from the source, in the range 0 to 31,
;;;             encoded in the "imms" field.
;;;             For the 64-bit variant: is the leftmost bit number to be moved from the source, in the range 0 to 63,
;;;             encoded in the "imms" field.

;;;; ASR (immediate) [alias for (32-bit (imms == '011111'))] [alias for (64-bit (imms == '111111'))]
;;
;;; Arithmetic Shift Right (immediate) shifts a register value right by an immediate number of bits, shifting in copies of
;;; the sign bit in the upper bits and zeros in the lower bits, and writes the result to the destination register.
;;
;;; "ASR <Wd>, <Wn>, #<shift>" === "SBFM <Wd>, <Wn>, #<shift>, #31" [32-bit (sf == 0 && N == 0 && imms == 011111)]
;;; "ASR <Xd>, <Xn>, #<shift>" === "SBFM <Xd>, <Xn>, #<shift>, #63" [64-bit (sf == 1 && N == 1 && imms == 111111)]
;;
;;;; Assembler Symbols
;;
;;; <shift>     For the 32-bit variant: is the shift amount, in the range 0 to 31, encoded in the "immr" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, encoded in the "immr" field.

;;;; SBFIZ [alias for (unsigned(imms) < unsigned(immr))]
;;
;;; Signed Bitfield Insert in Zeros copies a bitfield of <width> bits from the least significant bits of the source register to
;;; bit position <lsb> of the destination register, setting the destination bits below the bitfield to zero, and the bits above
;;; the bitfield to a copy of the most significant bit of the bitfield.
;;
;;; "SBFIZ <Wd>, <Wn>, #<lsb>, #<width>" === "SBFM <Wd>, <Wn>, #(-<lsb> MOD 32), #(<width>-1)" [32-bit (sf == 0 && N == 0)]
;;; "SBFIZ <Xd>, <Xn>, #<lsb>, #<width>" === "SBFM <Xd>, <Xn>, #(-<lsb> MOD 64), #(<width>-1)" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <lsb>       For the 32-bit variant: is the bit number of the lsb of the destination bitfield, in the range 0 to 31.
;;;             For the 64-bit variant: is the bit number of the lsb of the destination bitfield, in the range 0 to 63.
;;; <width>     For the 32-bit variant: is the width of the bitfield, in the range 1 to 32-<lsb>.
;;;             For the 64-bit variant: is the width of the bitfield, in the range 1 to 64-<lsb>.

;;;; SBFX [alias for (BFXPreferred(sf, opc<1>, imms, immr))]
;;
;;; Signed Bitfield Extract copies a bitfield of <width> bits starting from bit position <lsb> in the source register to the
;;; least significant bits of the destination register, and sets destination bits above the bitfield to a copy of the most
;;; significant bit of the bitfield.
;;
;;; "SBFX <Wd>, <Wn>, #<lsb>, #<width>" === "SBFM <Wd>, <Wn>, #<lsb>, #(<lsb>+<width>-1)" [32-bit (sf == 0 && N == 0)]
;;; "SBFX <Xd>, <Xn>, #<lsb>, #<width>" === "SBFM <Xd>, <Xn>, #<lsb>, #(<lsb>+<width>-1)" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <lsb>       For the 32-bit variant: is the bit number of the lsb of the source bitfield, in the range 0 to 31.
;;;             For the 64-bit variant: is the bit number of the lsb of the source bitfield, in the range 0 to 63.
;;; <width>     For the 32-bit variant: is the width of the bitfield, in the range 1 to 32-<lsb>.
;;;             For the 64-bit variant: is the width of the bitfield, in the range 1 to 64-<lsb>.

;;;; SXTB [alias for (immr == '000000' && imms == '000111')]
;;
;;; Signed Extend Byte extracts an 8-bit value from a register, sign-extends it to the size of the register,
;;; and writes the result to the destination register.
;;
;;; "SXTB <Wd>, <Wn>" === "SBFM <Wd>, <Wn>, #0, #7" [32-bit (sf == 0 && N == 0)]
;;; "SXTB <Xd>, <Wn>" === "SBFM <Xd>, <Xn>, #0, #7" [64-bit (sf == 1 && N == 1)]

;;;; SXTH [alias for (immr == '000000' && imms == '001111')]
;;
;;; Sign Extend Halfword extracts a 16-bit value, sign-extends it to the size of the register,
;;; and writes the result to the destination register.
;;
;;; "SXTH <Wd>, <Wn>" === "SBFM <Wd>, <Wn>, #0, #15" [32-bit (sf == 0 && N == 0)]
;;; "SXTH <Xd>, <Wn>" === "SBFM <Xd>, <Xn>, #0, #15" [64-bit (sf == 1 && N == 1)]

;;;; SXTW [alias for (immr == '000000' && imms == '011111')]
;;
;;; Sign Extend Word sign-extends a word to the size of the register, and writes the result to the destination register.
;;
;;; "SXTW <Xd>, <Wn>" === "SBFM <Xd>, <Xn>, #0, #31" [64-bit (sf == 1 && N == 1)]

(defn sbfm! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(1)" N (at I 22)
        #_"bits(6)" immr (at I 21 16)
        #_"bits(6)" imms (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (cond
                (and (= sf (bits '1)) (not= N (bits '1)))                                                                (throw! "UNDEFINED")
                (and (= sf (bits '0)) (or (not= N (bits '0)) (not= (at immr 5) (bits '0)) (not= (at imms 5) (bits '0)))) (throw! "UNDEFINED")
            )

        #_"integer" R (unsigned immr)
        #_"integer" S (unsigned imms)
        [#_"bits(width)" wmask, #_"bits(width)" tmask] (decode-bit-masks N, imms, immr, false)

        #_"bits(width)" src (X n)

        ;; perform bitfield move on low bits
        #_"bits(width)" bot (AND (ror src, R) wmask)
        ;; determine extension bits (sign, zero or dest register)
        #_"bits(width)" top (replicate (at src S), width)
    ]
        ;; combine extension bits and result bits
        (X! d (OR (AND top (NOT tmask)) (AND bot tmask)))
    )
)

(def #_"alias" asr-immediate! sbfm!)

(def #_"alias" sbfiz! sbfm!)

(def #_"alias" sbfx! sbfm!)

(def #_"alias" sxtb! sbfm!)

(def #_"alias" sxth! sbfm!)

(def #_"alias" sxtw! sbfm!)

;;;; BFM
;;
;;; Bitfield Move is usually accessed via one of its aliases, which are always preferred for disassembly.
;;; If <imms> is greater than or equal to <immr>, this copies a bitfield of (<imms>-<immr>+1) bits starting from bit
;;; position <immr> in the source register to the least significant bits of the destination register.
;;; If <imms> is less than <immr>, this copies a bitfield of (<imms>+1) bits from the least significant bits of the
;;; source register to bit position (regsize-<immr>) of the destination register, where regsize is the destination
;;; register size of 32 or 64 bits.
;;; In both cases the other bits of the destination register remain unchanged.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  1    1  0  0  1  1  0    N          immr                imms               Rn               Rd
;;;       opc
;;
;;; "BFM <Wd>, <Wn>, #<immr>, #<imms>" [32-bit (sf == 0 && N == 0)]
;;; "BFM <Xd>, <Xn>, #<immr>, #<imms>" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <immr>      For the 32-bit variant: is the right rotate amount, in the range 0 to 31, encoded in the "immr" field.
;;;             For the 64-bit variant: is the right rotate amount, in the range 0 to 63, encoded in the "immr" field.
;;; <imms>      For the 32-bit variant: is the leftmost bit number to be moved from the source, in the range 0 to 31,
;;;             encoded in the "imms" field.
;;;             For the 64-bit variant: is the leftmost bit number to be moved from the source, in the range 0 to 63,
;;;             encoded in the "imms" field.

;;;; BFC [alias for (Rn == '11111' && unsigned(imms) < unsigned(immr))]

;;;; BFI [alias for (Rn != '11111' && unsigned(imms) < unsigned(immr))]
;;
;;; Bitfield Insert copies a bitfield of <width> bits from the least significant bits of the source register to bit position
;;; <lsb> of the destination register, leaving the other destination bits unchanged.
;;
;;; "BFI <Wd>, <Wn>, #<lsb>, #<width>" === "BFM <Wd>, <Wn>, #(-<lsb> MOD 32), #(<width>-1)" [32-bit (sf == 0 && N == 0)]
;;; "BFI <Xd>, <Xn>, #<lsb>, #<width>" === "BFM <Xd>, <Xn>, #(-<lsb> MOD 64), #(<width>-1)" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <lsb>       For the 32-bit variant: is the bit number of the lsb of the destination bitfield, in the range 0 to 31.
;;;             For the 64-bit variant: is the bit number of the lsb of the destination bitfield, in the range 0 to 63.
;;; <width>     For the 32-bit variant: is the width of the bitfield, in the range 1 to 32-<lsb>.
;;;             For the 64-bit variant: is the width of the bitfield, in the range 1 to 64-<lsb>.

;;;; BFXIL [alias for (unsigned(imms) >= unsigned(immr))]
;;
;;; Bitfield Extract and Insert Low copies a bitfield of <width> bits starting from bit position <lsb> in the source register
;;; to the least significant bits of the destination register, leaving the other destination bits unchanged.
;;
;;; "BFXIL <Wd>, <Wn>, #<lsb>, #<width>" === "BFM <Wd>, <Wn>, #<lsb>, #(<lsb>+<width>-1)" [32-bit (sf == 0 && N == 0)]
;;; "BFXIL <Xd>, <Xn>, #<lsb>, #<width>" === "BFM <Xd>, <Xn>, #<lsb>, #(<lsb>+<width>-1)" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <lsb>       For the 32-bit variant: is the bit number of the lsb of the source bitfield, in the range 0 to 31.
;;;             For the 64-bit variant: is the bit number of the lsb of the source bitfield, in the range 0 to 63.
;;; <width>     For the 32-bit variant: is the width of the bitfield, in the range 1 to 32-<lsb>.
;;;             For the 64-bit variant: is the width of the bitfield, in the range 1 to 64-<lsb>.

(defn bfm! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(1)" N (at I 22)
        #_"bits(6)" immr (at I 21 16)
        #_"bits(6)" imms (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (cond
                (and (= sf (bits '1)) (not= N (bits '1)))                                                                (throw! "UNDEFINED")
                (and (= sf (bits '0)) (or (not= N (bits '0)) (not= (at immr 5) (bits '0)) (not= (at imms 5) (bits '0)))) (throw! "UNDEFINED")
            )

        #_"integer" R (unsigned immr)
        [#_"bits(width)" wmask, #_"bits(width)" tmask] (decode-bit-masks N, imms, immr, false)

        #_"bits(width)" dst (X d)
        #_"bits(width)" src (X n)

        ;; perform bitfield move on low bits
        #_"bits(width)" bot (OR (AND dst (NOT wmask)) (AND (ror src, R) wmask))
    ]
        ;; combine extension bits and result bits
        (X! d (OR (AND dst (NOT tmask)) (AND bot tmask)))
    )
)

(def #_"alias" bfc! bfm!)

(def #_"alias" bfi! bfm!)

(def #_"alias" bfxil! bfm!)

;;;; UBFM
;;
;;; Unsigned Bitfield Move is usually accessed via one of its aliases, which are always preferred for disassembly.
;;; If <imms> is greater than or equal to <immr>, this copies a bitfield of (<imms>-<immr>+1) bits starting from
;;; bit position <immr> in the source register to the least significant bits of the destination register.
;;; If <imms> is less than <immr>, this copies a bitfield of (<imms>+1) bits from the least significant bits of
;;; the source register to bit position (regsize-<immr>) of the destination register, where regsize is the destination
;;; register size of 32 or 64 bits.
;;; In both cases the destination bits below and above the bitfield are set to zero.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1  0    1  0  0  1  1  0    N          immr                imms               Rn               Rd
;;;       opc
;;
;;; "UBFM <Wd>, <Wn>, #<immr>, #<imms>" [32-bit (sf == 0 && N == 0)]
;;; "UBFM <Xd>, <Xn>, #<immr>, #<imms>" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <immr>      For the 32-bit variant: is the right rotate amount, in the range 0 to 31, encoded in the "immr" field.
;;;             For the 64-bit variant: is the right rotate amount, in the range 0 to 63, encoded in the "immr" field.
;;; <imms>      For the 32-bit variant: is the leftmost bit number to be moved from the source, in the range 0 to 31,
;;;             encoded in the "imms" field.
;;;             For the 64-bit variant: is the leftmost bit number to be moved from the source, in the range 0 to 63,
;;;             encoded in the "imms" field.

;;;; LSL (immediate) [alias for (32-bit (imms != '011111' && imms + 1 == immr))] [alias for (64-bit (imms != '111111' && imms + 1 == immr))]
;;
;;; Logical Shift Left (immediate) shifts a register value left by an immediate number of bits, shifting in zeros, and writes
;;; the result to the destination register.
;;
;;; "LSL <Wd>, <Wn>, #<shift>" === "UBFM <Wd>, <Wn>, #(-<shift> MOD 32), #(31-<shift>)" [32-bit (sf == 0 && N == 0 && imms != 011111)]
;;; "LSL <Xd>, <Xn>, #<shift>" === "UBFM <Xd>, <Xn>, #(-<shift> MOD 64), #(63-<shift>)" [64-bit (sf == 1 && N == 1 && imms != 111111)]
;;
;;;; Assembler Symbols
;;
;;; <shift>     For the 32-bit variant: is the shift amount, in the range 0 to 31.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63.

;;;; LSR (immediate) [alias for (32-bit (imms == '011111'))] [alias for (64-bit (imms == '111111'))]
;;
;;; Logical Shift Right (immediate) shifts a register value right by an immediate number of bits, shifting in zeros, and
;;; writes the result to the destination register.
;;
;;; "LSR <Wd>, <Wn>, #<shift>" === "UBFM <Wd>, <Wn>, #<shift>, #31" [32-bit (sf == 0 && N == 0 && imms == 011111)]
;;; "LSR <Xd>, <Xn>, #<shift>" === "UBFM <Xd>, <Xn>, #<shift>, #63" [64-bit (sf == 1 && N == 1 && imms == 111111)]
;;
;;;; Assembler Symbols
;;
;;; <shift>     For the 32-bit variant: is the shift amount, in the range 0 to 31, encoded in the "immr" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, encoded in the "immr" field.

;;;; UBFIZ [alias for (unsigned(imms) < unsigned(immr))]
;;
;;; Unsigned Bitfield Insert in Zeros copies a bitfield of <width> bits from the least significant bits of the source register
;;; to bit position <lsb> of the destination register, setting the destination bits above and below the bitfield to zero.
;;
;;; "UBFIZ <Wd>, <Wn>, #<lsb>, #<width>" === "UBFM <Wd>, <Wn>, #(-<lsb> MOD 32), #(<width>-1)" [32-bit (sf == 0 && N == 0)]
;;; "UBFIZ <Xd>, <Xn>, #<lsb>, #<width>" === "UBFM <Xd>, <Xn>, #(-<lsb> MOD 64), #(<width>-1)" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <lsb>       For the 32-bit variant: is the bit number of the lsb of the destination bitfield, in the range 0 to 31.
;;;             For the 64-bit variant: is the bit number of the lsb of the destination bitfield, in the range 0 to 63.
;;; <width>     For the 32-bit variant: is the width of the bitfield, in the range 1 to 32-<lsb>.
;;;             For the 64-bit variant: is the width of the bitfield, in the range 1 to 64-<lsb>.

;;;; UBFX [alias for (BFXPreferred(sf, opc<1>, imms, immr))]
;;
;;; Unsigned Bitfield Extract copies a bitfield of <width> bits starting from bit position <lsb> in the source register to the
;;; least significant bits of the destination register, and sets destination bits above the bitfield to zero.
;;
;;; "UBFX <Wd>, <Wn>, #<lsb>, #<width>" === "UBFM <Wd>, <Wn>, #<lsb>, #(<lsb>+<width>-1)" [32-bit (sf == 0 && N == 0)]
;;; "UBFX <Xd>, <Xn>, #<lsb>, #<width>" === "UBFM <Xd>, <Xn>, #<lsb>, #(<lsb>+<width>-1)" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <lsb>       For the 32-bit variant: is the bit number of the lsb of the source bitfield, in the range 0 to 31.
;;;             For the 64-bit variant: is the bit number of the lsb of the source bitfield, in the range 0 to 63.
;;; <width>     For the 32-bit variant: is the width of the bitfield, in the range 1 to 32-<lsb>.
;;;             For the 64-bit variant: is the width of the bitfield, in the range 1 to 64-<lsb>.

;;;; UXTB [alias for (32-bit (immr == '000000' && imms == '000111'))]
;;
;;; Unsigned Extend Byte extracts an 8-bit value from a register, zero-extends it to the size of the register,
;;; and writes the result to the destination register.
;;
;;; "UXTB <Wd>, <Wn>" === "UBFM <Wd>, <Wn>, #0, #7" [32-bit (sf == 0 && N == 0)]

;;;; UXTH [alias for (32-bit (immr == '000000' && imms == '001111'))]
;;
;;; Unsigned Extend Halfword extracts a 16-bit value from a register, zero-extends it to the size of the register,
;;; and writes the result to the destination register.
;;
;;; "UXTH <Wd>, <Wn>" === "UBFM <Wd>, <Wn>, #0, #15" [32-bit (sf == 0 && N == 0)]

(defn ubfm! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(1)" N (at I 22)
        #_"bits(6)" immr (at I 21 16)
        #_"bits(6)" imms (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (cond
                (and (= sf (bits '1)) (not= N (bits '1)))                                                                (throw! "UNDEFINED")
                (and (= sf (bits '0)) (or (not= N (bits '0)) (not= (at immr 5) (bits '0)) (not= (at imms 5) (bits '0)))) (throw! "UNDEFINED")
            )

        #_"integer" R (unsigned immr)
        [#_"bits(width)" wmask, #_"bits(width)" tmask] (decode-bit-masks N, imms, immr, false)

        #_"bits(width)" src (X n)

        ;; perform bitfield move on low bits
        #_"bits(width)" bot (AND (ror src, R) wmask)
    ]
        ;; combine extension bits and result bits
        (X! d (AND bot tmask))
    )
)

(def #_"alias" lsl-immediate! ubfm!)

(def #_"alias" lsr-immediate! ubfm!)

(def #_"alias" ubfiz! ubfm!)

(def #_"alias" ubfx! ubfm!)

(def #_"alias" uxtb! ubfm!)

(def #_"alias" uxth! ubfm!)


;;;; Extract
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 | 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;       op21    1  0  0  1  1  1        o0

(defn extract! [#_"bits(32)" I]
    (let [
        #_"bits(2)" op21 (at I 30 29)
        #_"bits(1)" o0 (at I 21)
    ]
        (case (cat op21 o0)
            (bits '000) (extr! I)
        )
    )
)

;;;; EXTR
;;
;;; Extract register extracts a register from a pair of registers.
;;
;;; 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  0    1  0  0  1  1  1    N    0         Rm                imms               Rn               Rd
;;
;;; "EXTR <Wd>, <Wn>, <Wm>, #<lsb>" [32-bit (sf == 0 && N == 0 && imms == 0xxxxx)]
;;; "EXTR <Xd>, <Xn>, <Xm>, #<lsb>" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <lsb>   For the 32-bit variant: is the least significant bit position from which to extract, in the range 0 to 31,
;;;         encoded in the "imms" field.
;;;         For the 64-bit variant: is the least significant bit position from which to extract, in the range 0 to 63,
;;;         encoded in the "imms" field.

;;;; ROR (immediate) [alias for (Rn == Rm)]
;;
;;; Rotate right (immediate) provides the value of the contents of a register rotated by a variable number of bits. The bits
;;; that are rotated off the right end are inserted into the vacated bit positions on the left.
;;
;;; "ROR <Wd>, <Ws>, #<shift>" === "EXTR <Wd>, <Ws>, <Ws>, #<shift>" [32-bit (sf == 0 && N == 0 && imms == 0xxxxx)]
;;; "ROR <Xd>, <Xs>, #<shift>" === "EXTR <Xd>, <Xs>, <Xs>, #<shift>" [64-bit (sf == 1 && N == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Ws>        Is the 32-bit name of the general-purpose source register, encoded in the "Rn" and "Rm" fields.
;;; <Xs>        Is the 64-bit name of the general-purpose source register, encoded in the "Rn" and "Rm" fields.
;;; <shift>     For the 32-bit variant: is the amount by which to rotate, in the range 0 to 31, encoded in the "imms" field.
;;;             For the 64-bit variant: is the amount by which to rotate, in the range 0 to 63, encoded in the "imms" field.

(defn extr! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imms (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (cond
                (not= N sf)                                      (throw! "UNDEFINED")
                (and (= sf (bits '0)) (= (at imms 5) (bits '1))) (throw! "UNDEFINED")
            )

        #_"integer" lsb (unsigned imms)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        #_"bits(2 * width)" concat (cat operand1 operand2)
        #_"bits(width)" result (at concat (- (+ lsb width) 1) lsb)
    ]
        (X! d result)
    )
)

(def #_"alias" ror-immediate! extr!)


;;;; Branches, Exception Generating and System instructions
;;
;;; 31 30 29 | 28 27 26 | 25 24 23 22 21 20 19 18 17 16 15 14 13 12 | 11 10  9  8  7  6  5 |  4  3  2  1  0
;;;    op0      1  0  1                     op1                                                    op2

(defn branches-exception-generating-and-system-instructions! [#_"bits(32)" I]
    (let [
        #_"bits(3)" op0 (at I 31 29)
        #_"bits(14)" op1 (at I 25 12)
        #_"bits(5)" op2 (at I 4 0)
    ]
        (case (cat op0     op1                 op2)
            (bits '010     0xxxxxxxxxxxxx      xxxxx) (conditional-branch-immediate! I)
            (bits '110     00xxxxxxxxxxxx      xxxxx) (exception-generation! I)
            (bits '110     01000000110010      11111) (hints! I)
            (bits '110     01000000110011      xxxxx) (barriers! I)
            (bits '110     0100x1xxxxxxxx      xxxxx) (system-register-move! I)
            (bits '110     1xxxxxxxxxxxxx      xxxxx) (unconditional-branch-register! I)
            (bits 'x00     xxxxxxxxxxxxxx      xxxxx) (unconditional-branch-immediate! I)
            (bits 'x01     0xxxxxxxxxxxxx      xxxxx) (compare-and-branch-immediate! I)
            (bits 'x01     1xxxxxxxxxxxxx      xxxxx) (test-and-branch-immediate! I)
        )
    )
)


;;;; Conditional branch (immediate)
;;
;;; 31 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4 |  3  2  1  0
;;;  0  1  0  1  0  1  0   o1                                                              o0

(defn conditional-branch-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" o1 (at I 24)
        #_"bits(1)" o0 (at I 4)
    ]
        (case (cat o1  o0)
            (bits '0   0) (b-cond! I)
        )
    )
)

;;;; B.cond
;;
;;; Branch conditionally to a label at a PC-relative offset, with a hint that this is not a subroutine call or return.
;;
;;; 31 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4 |  3  2  1  0
;;;  0  1  0  1  0  1  0    0                              imm19                            0       cond
;;
;;; "B.<cond> <label>"
;;
;;;; Assembler Symbols
;;
;;; <cond>      Is one of the standard conditions, encoded in the "cond" field in the standard way.
;;; <label>     Is the program label to be conditionally branched to. Its offset from the address of this instruction,
;;;             in the range +/-1MB, is encoded as "imm19" times 4.

(defn b-cond! [#_"bits(32)" I]
    (let [
        #_"bits(19)" imm19 (at I 23 5)
        #_"bits(4)" cond (at I 3 0)

        #_"bits(64)" offset (sign-extend (cat imm19 (bits '00)), 64)
    ]
        (when (condition-holds? cond)
            (branch-to (+ (pc) offset))
        )
    )
)


;;;; Exception generation
;;
;;; 31 30 29 28 27 26 25 24 | 23 22 21 | 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2 |  1  0
;;;  1  1  0  1  0  1  0  0     opc                                                           op2       LL

(defn exception-generation! [#_"bits(32)" I]
    (let [
        #_"bits(3)" opc (at I 23 21)
        #_"bits(3)" op2 (at I 4 2)
        #_"bits(2)" LL (at I 1 0)
    ]
        (case (cat opc     op2     LL)
            (bits '000     000     01) (svc! I)
        )
    )
)

;;;; SVC
;;
;;; Supervisor Call causes an exception to be taken to EL1.
;;; On executing an SVC instruction, the PE records the exception as a Supervisor Call exception in ESR_ELx, using the
;;; EC value 0x15, and the value of the immediate argument.
;;
;;; 31 30 29 28 27 26 25 24 | 23 22 21 | 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2 |  1  0
;;;  1  1  0  1  0  1  0  0    0  0  0                        imm16                         0  0  0    0  1
;;
;;; "SVC #<imm>"
;;
;;;; Assembler Symbols
;;
;;; <imm>   Is a 16-bit unsigned immediate, in the range 0 to 65535, encoded in the "imm16" field.

(defn svc! [#_"bits(32)" I]
    (let [
        #_"bits(16)" imm16 (at I 20 5)
    ]
        (CallSupervisor imm16)
    )
)


;;;; Hints
;;
;;; 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 | 11 10  9  8 |  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0  0  0  0  0  1  1  0  0  1  0       CRm          op2      1  1  1  1  1

(defn hints! [#_"bits(32)" I]
    (let [
        #_"bits(4)" CRm (at I 11 8)
        #_"bits(3)" op2 (at I 7 5)
    ]
        (when (= CRm (bits '0000))
            (case op2
                (bits '000) (nop! I)
                (bits '001) (yield! I)
                (bits '011) (wfi! I)
            )
        )
    )
)

;;;; NOP
;;
;;; No Operation does nothing, other than advance the value of the program counter by 4. This instruction can be used for
;;; instruction alignment purposes.
;;; The timing effects of including a NOP instruction in a program are not guaranteed. It can increase execution time, leave
;;; it unchanged, or even reduce it. Therefore, NOP instructions are not suitable for timing loops.
;;
;;; 31 30 29 28 27 26 25 24 23 22 | 21 | 20 19 | 18 17 16 | 15 14 13 12 | 11 10  9  8 |  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0    0    0  0    0  1  1    0  0  1  0    0  0  0  0    0  0  0    1  1  1  1  1
;;;                                                                           CRm          op2
;;
;;; "NOP"

(defn nop! [#_"bits(32)" I]
    (let [
        #_"bits(4)" CRm (at I 11 8)
        #_"bits(3)" op2 (at I 7 5)
    ]
        ;; do nothing
    )
)

;;;; YIELD
;;
;;; YIELD is a hint instruction. Software with a multithreading capability can use a YIELD instruction to indicate to the PE
;;; that it is performing a task, for example a spin-lock, that could be swapped out to improve overall system performance.
;;; The PE can use this hint to suspend and resume multiple software threads if it supports the capability.
;;
;;; 31 30 29 28 27 26 25 24 23 22 | 21 | 20 19 | 18 17 16 | 15 14 13 12 | 11 10  9  8 |  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0    0    0  0    0  1  1    0  0  1  0    0  0  0  0    0  0  1    1  1  1  1  1
;;;                                                                           CRm          op2
;;
;;; "YIELD"

(defn yield! [#_"bits(32)" I]
    (let [
        #_"bits(4)" CRm (at I 11 8)
        #_"bits(3)" op2 (at I 7 5)
    ]
        ;; not implemented
    )
)

;;;; WFI
;;
;;; Wait For Interrupt is a hint instruction that indicates that the PE can enter a low-power state and remain there until a
;;; wakeup event occurs.
;;
;;; 31 30 29 28 27 26 25 24 23 22 | 21 | 20 19 | 18 17 16 | 15 14 13 12 | 11 10  9  8 |  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0    0    0  0    0  1  1    0  0  1  0    0  0  0  0    0  1  1    1  1  1  1  1
;;;                                                                           CRm          op2
;;
;;; "WFI"

(defn wfi! [#_"bits(32)" I]
    (let [
        #_"bits(4)" CRm (at I 11 8)
        #_"bits(3)" op2 (at I 7 5)
    ]
        ;; not implemented
    )
)


;;;; Barriers
;;
;;; 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 | 11 10  9  8 |  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0  0  0  0  0  1  1  0  0  1  1       CRm          op2           Rt

(defn barriers! [#_"bits(32)" I]
    (let [
        #_"bits(4)" CRm (at I 11 8)
        #_"bits(3)" op2 (at I 7 5)
        #_"bits(5)" Rt (at I 4 0)
    ]
        (when (= Rt (bits '11111))
            (case (cat CRm     op2)
                (bits 'xxxx    010) (clrex! I)
                (bits 'xxxx    101) (dmb! I)
                (bits 'xxxx    110) (isb! I)
                (bits '!= 0x00    100) (dsb! I)
            )
        )
    )
)

;;;; CLREX
;;
;;; Clear Exclusive clears the local monitor of the executing PE.
;;
;;; 31 30 29 28 27 26 25 24 23 22 | 21 | 20 19 | 18 17 16 | 15 14 13 12 | 11 10  9  8 |  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0    0    0  0    0  1  1    0  0  1  1       CRm        0  1  0    1  1  1  1  1
;;
;;; "CLREX {#<imm>}"
;;
;;; CRm field is ignored
;;
;;;; Assembler Symbols
;;
;;; <imm>   Is an optional 4-bit unsigned immediate, in the range 0 to 15, defaulting to 15 and encoded in the "CRm" field.

(defn clrex! [#_"bits(32)" I]
    (let [
        #_"bits(4)" CRm (at I 11 8)
    ]
        (clear-exclusive-local (processor-id))
    )
)

;;;; DMB
;;
;;; Data Memory Barrier is a memory barrier that ensures the ordering of observations of memory accesses.
;;
;;; 31 30 29 28 27 26 25 24 23 22 | 21 | 20 19 | 18 17 16 | 15 14 13 12 | 11 10  9  8 |  7 |  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0    0    0  0    0  1  1    0  0  1  1       CRm        1    0  1    1  1  1  1  1
;;;                                                                                           opc
;;
;;; "DMB <option>|#<imm>"
;;
;;;; Assembler Symbols
;;
;;; <option>    Specifies the limitation on the barrier operation. Values are:
;;;             SY
;;;                 Full system is the required shareability domain, reads and writes are the required access types,
;;;                 both before and after the barrier instruction. This option is referred to as the full system barrier.
;;;                 Encoded as CRm = 0b1111.
;;;             ST
;;;                 Full system is the required shareability domain, writes are the required access type, both before
;;;                 and after the barrier instruction. Encoded as CRm = 0b1110.
;;;             LD
;;;                 Full system is the required shareability domain, reads are the required access type before the
;;;                 barrier instruction, and reads and writes are the required access types after the barrier
;;;                 instruction. Encoded as CRm = 0b1101.
;;;             ISH
;;;                 Inner Shareable is the required shareability domain, reads and writes are the required access
;;;                 types, both before and after the barrier instruction. Encoded as CRm = 0b1011.
;;;             ISHST
;;;                 Inner Shareable is the required shareability domain, writes are the required access type, both
;;;                 before and after the barrier instruction. Encoded as CRm = 0b1010.
;;;             ISHLD
;;;                 Inner Shareable is the required shareability domain, reads are the required access type before the
;;;                 barrier instruction, and reads and writes are the required access types after the barrier
;;;                 instruction. Encoded as CRm = 0b1001.
;;;             NSH
;;;                 Non-shareable is the required shareability domain, reads and writes are the required access, both
;;;                 before and after the barrier instruction. Encoded as CRm = 0b0111.
;;;             NSHST
;;;                 Non-shareable is the required shareability domain, writes are the required access type, both
;;;                 before and after the barrier instruction. Encoded as CRm = 0b0110.
;;;             NSHLD
;;;                 Non-shareable is the required shareability domain, reads are the required access type before the
;;;                 barrier instruction, and reads and writes are the required access types after the barrier
;;;                 instruction. Encoded as CRm = 0b0101.
;;;             OSH
;;;                 Outer Shareable is the required shareability domain, reads and writes are the required access
;;;                 types, both before and after the barrier instruction. Encoded as CRm = 0b0011.
;;;             OSHST
;;;                 Outer Shareable is the required shareability domain, writes are the required access type, both
;;;                 before and after the barrier instruction. Encoded as CRm = 0b0010.
;;;             OSHLD
;;;                 Outer Shareable is the required shareability domain, reads are the required access type before the
;;;                 barrier instruction, and reads and writes are the required access types after the barrier
;;;                 instruction. Encoded as CRm = 0b0001.
;;;
;;;             All other encodings of CRm that are not listed above are reserved, and can be encoded using the
;;;             #<imm> syntax. All unsupported and reserved options must execute as a full system barrier operation,
;;;             but software must not rely on this behavior.
;;;
;;; <imm>       Is a 4-bit unsigned immediate, in the range 0 to 15, encoded in the "CRm" field.

(defn dmb! [#_"bits(32)" I]
    (let [
        #_"bits(4)" CRm (at I 11 8)
        #_"bits(2)" opc (at I 6 5)
    ]
        ;; not implemented
    )
)

;;;; ISB
;;
;;; Instruction Synchronization Barrier flushes the pipeline in the PE and is a context synchronization event.
;;
;;; 31 30 29 28 27 26 25 24 23 22 | 21 | 20 19 | 18 17 16 | 15 14 13 12 | 11 10  9  8 |  7 |  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0    0    0  0    0  1  1    0  0  1  1       CRm        1    1  0    1  1  1  1  1
;;;                                                                                           opc
;;
;;; "ISB {<option>|#<imm>}"
;;
;;;; Assembler Symbols
;;
;;; <option>    Specifies an optional limitation on the barrier operation. Values are:
;;;             SY
;;;                 Full system barrier operation, encoded as CRm = 0b1111. Can be omitted.
;;;
;;;             All other encodings of CRm are reserved. The corresponding instructions execute as full system barrier operations,
;;;             but must not be relied upon by software.
;;;
;;; <imm>       Is an optional 4-bit unsigned immediate, in the range 0 to 15, defaulting to 15 and encoded in the "CRm" field.

(defn isb! [#_"bits(32)" I]
    (let [
        #_"bits(4)" CRm (at I 11 8)
        #_"bits(2)" opc (at I 6 5)
    ]
        ;; not implemented
    )
)

;;;; DSB
;;
;;; Data Synchronization Barrier is a memory barrier that ensures the completion of memory accesses.
;;
;;; 31 30 29 28 27 26 25 24 23 22 | 21 | 20 19 | 18 17 16 | 15 14 13 12 | 11 10  9  8 |  7 |  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0    0    0  0    0  1  1    0  0  1  1     != 0x00      1    0  0    1  1  1  1  1
;;;                                                                           CRm             opc
;;
;;; "DSB <option>|#<imm>"
;;
;;;; Assembler Symbols
;;
;;; <option>    For the memory barrier variant: specifies the limitation on the barrier operation. Values are:
;;;             SY
;;;                 Full system is the required shareability domain, reads and writes are the required access types,
;;;                 both before and after the barrier instruction. This option is referred to as the full system barrier.
;;;                 Encoded as CRm = 0b1111.
;;;             ST
;;;                 Full system is the required shareability domain, writes are the required access type, both before
;;;                 and after the barrier instruction. Encoded as CRm = 0b1110.
;;;             LD
;;;                 Full system is the required shareability domain, reads are the required access type before the
;;;                 barrier instruction, and reads and writes are the required access types after the barrier
;;;                 instruction. Encoded as CRm = 0b1101.
;;;             ISH
;;;                 Inner Shareable is the required shareability domain, reads and writes are the required access
;;;                 types, both before and after the barrier instruction. Encoded as CRm = 0b1011.
;;;             ISHST
;;;                 Inner Shareable is the required shareability domain, writes are the required access type, both
;;;                 before and after the barrier instruction. Encoded as CRm = 0b1010.
;;;             ISHLD
;;;                 Inner Shareable is the required shareability domain, reads are the required access type before the
;;;                 barrier instruction, and reads and writes are the required access types after the barrier
;;;                 instruction. Encoded as CRm = 0b1001.
;;;             NSH
;;;                 Non-shareable is the required shareability domain, reads and writes are the required access, both
;;;                 before and after the barrier instruction. Encoded as CRm = 0b0111.
;;;             NSHST
;;;                 Non-shareable is the required shareability domain, writes are the required access type, both
;;;                 before and after the barrier instruction. Encoded as CRm = 0b0110.
;;;             NSHLD
;;;                 Non-shareable is the required shareability domain, reads are the required access type before the
;;;                 barrier instruction, and reads and writes are the required access types after the barrier
;;;                 instruction. Encoded as CRm = 0b0101.
;;;             OSH
;;;                 Outer Shareable is the required shareability domain, reads and writes are the required access
;;;                 types, both before and after the barrier instruction. Encoded as CRm = 0b0011.
;;;             OSHST
;;;                 Outer Shareable is the required shareability domain, writes are the required access type, both
;;;                 before and after the barrier instruction. Encoded as CRm = 0b0010.
;;;             OSHLD
;;;                 Outer Shareable is the required shareability domain, reads are the required access type before the
;;;                 barrier instruction, and reads and writes are the required access types after the barrier
;;;                 instruction. Encoded as CRm = 0b0001.
;;;
;;;             All other encodings of CRm, other than the values 0b0000 and 0b0100, that are not listed above are
;;;             reserved, and can be encoded using the #<imm> syntax. All unsupported and reserved options must
;;;             execute as a full system barrier operation, but software must not rely on this behavior.
;;;
;;; <imm>       For the memory barrier variant: is a 4-bit unsigned immediate, in the range 0 to 15, encoded in the "CRm" field.

(defn dsb! [#_"bits(32)" I]
    (let [
        #_"bits(4)" CRm (at I 11 8)
        #_"bits(2)" opc (at I 6 5)
    ]
        ;; not implemented
    )
)


;;;; System register move
;;
;;; 31 30 29 28 27 26 25 24 23 22 | 21 | 20 | 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0    L    1

(defn system-register-move! [#_"bits(32)" I]
    (let [
        #_"bits(1)" L (at I 21)
    ]
        (case L
            (bits '0) (msr-register! I)
            (bits '1) (mrs! I)
        )
    )
)

;;;; MSR (register)
;;
;;; Move general-purpose register to System Register allows the PE to write an AArch64 System register from a general- purpose register.
;;
;;; 31 30 29 28 27 26 25 24 23 22 | 21 | 20 | 19 | 18 17 16 | 15 14 13 12 | 11 10  9  8 |  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0    0    1   o0      op1         CRn           CRm          op2           Rt
;;;                                  L
;;
;;; "MSR (<systemreg>|S<op0>_<op1>_<Cn>_<Cm>_<op2>), <Xt>"
;;
;;;; Assembler Symbols
;;
;;; <systemreg>     Is a System register name, encoded in the "o0:op1:CRn:CRm:op2".
;;;                 The System register names are defined in 'AArch64 System Registers' in the System Register XML.
;;; <op0>           Is an unsigned immediate, encoded in “o0”:
;;;                    o0     <op0>
;;;                     0       2
;;;                     1       3
;;; <op1>           Is a 3-bit unsigned immediate, in the range 0 to 7, encoded in the "op1" field.
;;; <Cn>            Is a name 'Cn', with 'n' in the range 0 to 15, encoded in the "CRn" field.
;;; <Cm>            Is a name 'Cm', with 'm' in the range 0 to 15, encoded in the "CRm" field.
;;; <op2>           Is a 3-bit unsigned immediate, in the range 0 to 7, encoded in the "op2" field.
;;; <Xt>            Is the 64-bit name of the general-purpose source register, encoded in the "Rt" field.

(defn msr-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" L (at I 21)
        #_"bits(1)" o0 (at I 19)
        #_"bits(3)" op1 (at I 18 16)
        #_"bits(4)" CRn (at I 15 12)
        #_"bits(4)" CRm (at I 11 8)
        #_"bits(3)" op2 (at I 7 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" t (unsigned Rt)

        #_"integer" sys_op0 (+ 2 (unsigned o0))
        #_"integer" sys_op1 (unsigned op1)
        #_"integer" sys_op2 (unsigned op2)
        #_"integer" sys_crn (unsigned CRn)
        #_"integer" sys_crm (unsigned CRm)
    ]
        (SysRegWrite sys_op0, sys_op1, sys_crn, sys_crm, sys_op2, (X t))
    )
)

;;;; MRS
;;
;;; Move System Register allows the PE to read an AArch64 System register into a general-purpose register.
;;
;;; 31 30 29 28 27 26 25 24 23 22 | 21 | 20 | 19 | 18 17 16 | 15 14 13 12 | 11 10  9  8 |  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  0  1  0  0    1    1   o0      op1         CRn           CRm          op2           Rt
;;;                                  L
;;
;;; "MRS <Xt>, (<systemreg>|S<op0>_<op1>_<Cn>_<Cm>_<op2>)"
;;
;;;; Assembler Symbols
;;
;;; <Xt>            Is the 64-bit name of the general-purpose destination register, encoded in the "Rt" field.
;;; <systemreg>     Is a System register name, encoded in the "o0:op1:CRn:CRm:op2".
;;;                 The System register names are defined in 'AArch64 System Registers' in the System Register XML.
;;; <op0>           Is an unsigned immediate, encoded in “o0”:
;;;                    o0     <op0>
;;;                     0       2
;;;                     1       3
;;; <op1>           Is a 3-bit unsigned immediate, in the range 0 to 7, encoded in the "op1" field.
;;; <Cn>            Is a name 'Cn', with 'n' in the range 0 to 15, encoded in the "CRn" field.
;;; <Cm>            Is a name 'Cm', with 'm' in the range 0 to 15, encoded in the "CRm" field.
;;; <op2>           Is a 3-bit unsigned immediate, in the range 0 to 7, encoded in the "op2" field.

(defn mrs! [#_"bits(32)" I]
    (let [
        #_"bits(1)" L (at I 21)
        #_"bits(1)" o0 (at I 19)
        #_"bits(3)" op1 (at I 18 16)
        #_"bits(4)" CRn (at I 15 12)
        #_"bits(4)" CRm (at I 11 8)
        #_"bits(3)" op2 (at I 7 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" t (unsigned Rt)

        #_"integer" sys_op0 (+ 2 (unsigned o0))
        #_"integer" sys_op1 (unsigned op1)
        #_"integer" sys_op2 (unsigned op2)
        #_"integer" sys_crn (unsigned CRn)
        #_"integer" sys_crm (unsigned CRm)
    ]
        (X! t (SysRegRead sys_op0, sys_op1, sys_crn, sys_crm, sys_op2))
    )
)


;;;; Unconditional branch (register)
;;
;;; 31 30 29 28 27 26 25 | 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  1       opc             op2               op3                                 op4

(defn unconditional-branch-register! [#_"bits(32)" I]
    (let [
        #_"bits(4)" opc (at I 24 21)
        #_"bits(5)" op2 (at I 20 16)
        #_"bits(6)" op3 (at I 15 10)
        #_"bits(5)" op4 (at I 4 0)
    ]
        (when (and (= op2 (bits '11111)) (= op3 (bits '000000)) (= op4 (bits '00000)))
            (case opc
                (bits '0000) (br! I)
                (bits '0001) (blr! I)
                (bits '0010) (ret! I)
            )
        )
    )
)

;;;; BR
;;
;;; Branch to Register branches unconditionally to an address in a register, with a hint that this is not a subroutine return.
;;
;;; 31 30 29 28 27 26 25 | 24 | 23 | 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  1    0    0    0  0    1  1  1  1  1    0  0  0  0    0    0         Rn          0  0  0  0  0
;;;                         Z          op                                    A    M                          Rm
;;
;;; "BR <Xn>"
;;
;;;; Assembler Symbols
;;
;;; <Xn>    Is the 64-bit name of the general-purpose register holding the address to be branched to, encoded in the "Rn" field.

(defn br! [#_"bits(32)" I]
    (let [
        #_"bits(1)" Z (at I 24)
        #_"bits(2)" op (at I 22 21)
        #_"bits(1)" A (at I 11)
        #_"bits(1)" M (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rm (at I 4 0)

        #_"integer" n (unsigned Rn)

        #_"bits(64)" target (X n)
    ]
        (branch-to target)
    )
)

;;;; BLR
;;
;;; Branch with Link to Register calls a subroutine at an address in a register, setting register X30 to PC+4.
;;
;;; 31 30 29 28 27 26 25 | 24 | 23 | 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  1    0    0    0  1    1  1  1  1  1    0  0  0  0    0    0         Rn          0  0  0  0  0
;;;                         Z          op                                    A    M                          Rm
;;
;;; "BLR <Xn>"
;;
;;;; Assembler Symbols
;;
;;; <Xn>    Is the 64-bit name of the general-purpose register holding the address to be branched to, encoded in the "Rn" field.

(defn blr! [#_"bits(32)" I]
    (let [
        #_"bits(1)" Z (at I 24)
        #_"bits(2)" op (at I 22 21)
        #_"bits(1)" A (at I 11)
        #_"bits(1)" M (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rm (at I 4 0)

        #_"integer" n (unsigned Rn)

        #_"bits(64)" target (X n)
    ]
        (X! 30 (+ (pc) 4))

        (branch-to target)
    )
)

;;;; RET
;;
;;; Return from subroutine branches unconditionally to an address in a register, with a hint that this is a subroutine return.
;;
;;; 31 30 29 28 27 26 25 | 24 | 23 | 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  1  0  1  0  1  1    0    0    1  0    1  1  1  1  1    0  0  0  0    0    0         Rn          0  0  0  0  0
;;;                         Z         op                                     A    M                          Rm
;;
;;; "RET {<Xn>}"
;;
;;;; Assembler Symbols
;;
;;; <Xn>    Is the 64-bit name of the general-purpose register holding the address to be branched to, encoded in the "Rn" field.
;;;         Defaults to X30 if absent.

(defn ret! [#_"bits(32)" I]
    (let [
        #_"bits(1)" Z (at I 24)
        #_"bits(2)" op (at I 22 21)
        #_"bits(1)" A (at I 11)
        #_"bits(1)" M (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rm (at I 4 0)

        #_"integer" n (unsigned Rn)

        #_"bits(64)" target (X n)
    ]
        (branch-to target)
    )
)


;;;; Unconditional branch (immediate)
;;
;;; 31 | 30 29 28 27 26 | 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;; op    0  0  1  0  1

(defn unconditional-branch-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" op (at I 31)
    ]
        (case op
            (bits '0) (b! I)
            (bits '1) (bl! I)
        )
    )
)

;;;; B
;;
;;; Branch causes an unconditional branch to a label at a PC-relative offset, with a hint that this is not a subroutine call or return.
;;
;;; 31 | 30 29 28 27 26 | 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  0    0  0  1  0  1                                      imm26
;;; op
;;
;;; "B <label>"
;;
;;;; Assembler Symbols
;;
;;; <label>     Is the program label to be unconditionally branched to. Its offset from the address of this instruction,
;;;             in the range +/-128MB, is encoded as "imm26" times 4.

(defn b! [#_"bits(32)" I]
    (let [
        #_"bits(1)" op (at I 31)
        #_"bits(26)" imm26 (at I 25 0)

        #_"bits(64)" offset (sign-extend (cat imm26 (bits '00)), 64)
    ]
        (branch-to (+ (pc) offset))
    )
)

;;;; BL
;;
;;; Branch with Link branches to a PC-relative offset, setting the register X30 to PC+4.
;;; It provides a hint that this is a subroutine call.
;;
;;; 31 | 30 29 28 27 26 | 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  1    0  0  1  0  1                                      imm26
;;; op
;;
;;; "BL <label>"
;;
;;;; Assembler Symbols
;;
;;; <label>     Is the program label to be unconditionally branched to. Its offset from the address of this instruction,
;;;             in the range +/-128MB, is encoded as "imm26" times 4.

(defn bl! [#_"bits(32)" I]
    (let [
        #_"bits(1)" op (at I 31)
        #_"bits(26)" imm26 (at I 25 0)

        #_"bits(64)" offset (sign-extend (cat imm26 (bits '00)), 64)
    ]
        (X! 30 (+ (pc) 4))

        (branch-to (+ (pc) offset))
    )
)


;;;; Compare and branch (immediate)
;;
;;; 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;       0  1  1  0  1  0   op

(defn compare-and-branch-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" op (at I 24)
    ]
        (case op
            (bits '0) (cbz! I)
            (bits '1) (cbnz! I)
        )
    )
)

;;;; CBZ
;;
;;; Compare and Branch on Zero compares the value in a register with zero, and conditionally branches to a label at a PC-
;;; relative offset if the comparison is equal. It provides a hint that this is not a subroutine call or return. This instruction
;;; does not affect condition flags.
;;
;;; 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  1  1  0  1  0    0                              imm19                                 Rt
;;;                          op
;;
;;; "CBZ <Wt>, <label>" [32-bit (sf == 0)]
;;; "CBZ <Xt>, <label>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be tested, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be tested, encoded in the "Rt" field.
;;; <label>     Is the program label to be conditionally branched to. Its offset from the address of this instruction,
;;;             in the range +/-1MB, is encoded as "imm19" times 4.

(defn cbz! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 24)
        #_"bits(19)" imm19 (at I 23 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" t (unsigned Rt)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"bits(64)" offset (sign-extend (cat imm19 (bits '00)), 64)

        #_"bits(width)" operand1 (X t)
    ]
        (when (= (is-zero? operand1) true)
            (branch-to (+ (pc) offset))
        )
    )
)

;;;; CBNZ
;;
;;; Compare and Branch on Nonzero compares the value in a register with zero, and conditionally branches to a label at a
;;; PC-relative offset if the comparison is not equal. It provides a hint that this is not a subroutine call or return. This
;;; instruction does not affect the condition flags.
;;
;;; 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  1  1  0  1  0    1                              imm19                                 Rt
;;;                          op
;;
;;; "CBNZ <Wt>, <label>" [32-bit (sf == 0)]
;;; "CBNZ <Xt>, <label>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be tested, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be tested, encoded in the "Rt" field.
;;; <label>     Is the program label to be conditionally branched to. Its offset from the address of this instruction,
;;;             in the range +/-1MB, is encoded as "imm19" times 4.

(defn cbnz! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 24)
        #_"bits(19)" imm19 (at I 23 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" t (unsigned Rt)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"bits(64)" offset (sign-extend (cat imm19 (bits '00)), 64)

        #_"bits(width)" operand1 (X t)
    ]
        (when (= (is-zero? operand1) false)
            (branch-to (+ (pc) offset))
        )
    )
)


;;;; Test and branch (immediate)
;;
;;; 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;       0  1  1  0  1  1   op

(defn test-and-branch-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" op (at I 24)
    ]
        (case op
            (bits '0) (tbz! I)
            (bits '1) (tbnz! I)
        )
    )
)

;;;; TBZ
;;
;;; Test bit and Branch if Zero compares the value of a test bit with zero, and conditionally branches to a label at a PC-
;;; relative offset if the comparison is equal. It provides a hint that this is not a subroutine call or return. This instruction
;;; does not affect condition flags.
;;
;;; 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 | 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;; b5    0  1  1  0  1  1    0        b40                           imm14                           Rt
;;;                          op
;;
;;; "TBZ <R><t>, #<imm>, <label>"
;;
;;;; Assembler Symbols
;;
;;; <R>         Is a width specifier, encoded in “b5”:
;;;                 b5 <R>
;;;                 0   W
;;;                 1   X
;;;             In assembler source code an 'X' specifier is always permitted, but a 'W' specifier is only permitted
;;;             when the bit number is less than 32.
;;; <t>         Is the number [0-30] of the general-purpose register to be tested or the name ZR (31), encoded in the "Rt" field.
;;; <imm>       Is the bit number to be tested, in the range 0 to 63, encoded in "b5:b40".
;;; <label>     Is the program label to be conditionally branched to. Its offset from the address of this instruction,
;;;             in the range +/-32KB, is encoded as "imm14" times 4.

(defn tbz! [#_"bits(32)" I]
    (let [
        #_"bits(1)" b5 (at I 31)
        #_"bits(1)" op (at I 24)
        #_"bits(5)" b40 (at I 23 19)
        #_"bits(14)" imm14 (at I 18 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" t (unsigned Rt)

        #_"integer" width (if (= b5 (bits '1)) 64 32)
        #_"integer" bit_pos (unsigned (cat b5 b40))
        #_"bits(64)" offset (sign-extend (cat imm14 (bits '00)), 64)

        #_"bits(width)" operand (X t)
    ]
        (when (= (at operand bit_pos) op)
            (branch-to (+ (pc) offset))
        )
    )
)

;;;; TBNZ
;;
;;; Test bit and Branch if Nonzero compares the value of a bit in a general-purpose register with zero, and conditionally
;;; branches to a label at a PC-relative offset if the comparison is not equal. It provides a hint that this is not a subroutine
;;; call or return. This instruction does not affect condition flags.
;;
;;; 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 | 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;; b5    0  1  1  0  1  1    1        b40                           imm14                           Rt
;;;                          op
;;
;;; "TBNZ <R><t>, #<imm>, <label>"
;;
;;;; Assembler Symbols
;;
;;; <R>         Is a width specifier, encoded in “b5”:
;;;                 b5 <R>
;;;                 0   W
;;;                 1   X
;;;             In assembler source code an 'X' specifier is always permitted, but a 'W' specifier is only permitted
;;;             when the bit number is less than 32.
;;; <t>         Is the number [0-30] of the general-purpose register to be tested or the name ZR (31), encoded in the "Rt" field.
;;; <imm>       Is the bit number to be tested, in the range 0 to 63, encoded in "b5:b40".
;;; <label>     Is the program label to be conditionally branched to. Its offset from the address of this instruction,
;;;             in the range +/-32KB, is encoded as "imm14" times 4.

(defn tbnz! [#_"bits(32)" I]
    (let [
        #_"bits(1)" b5 (at I 31)
        #_"bits(1)" op (at I 24)
        #_"bits(5)" b40 (at I 23 19)
        #_"bits(14)" imm14 (at I 18 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" t (unsigned Rt)

        #_"integer" width (if (= b5 (bits '1)) 64 32)
        #_"integer" bit_pos (unsigned (cat b5 b40))
        #_"bits(64)" offset (sign-extend (cat imm14 (bits '00)), 64)

        #_"bits(width)" operand (X t)
    ]
        (when (= (at operand bit_pos) op)
            (branch-to (+ (pc) offset))
        )
    )
)


;;;; Loads and Stores
;;
;;; 31 30 29 28 | 27 26 25 | 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 | 11 10 |  9  8  7  6  5  4  3  2  1  0
;;;     op0        1  0  0    op2                op3                         op4

(defn loads-and-stores! [#_"bits(32)" I]
    (let [
        #_"bits(4)" op0 (at I 31 28)
        #_"bits(2)" op2 (at I 24 23)
        #_"bits(6)" op3 (at I 21 16)
        #_"bits(2)" op4 (at I 11 10)
    ]
        (case (cat op0     op2     op3     op4)
            (bits 'xx00    0x      xxxxxx  xx) (load-store-exclusive! I)
            (bits 'xx01    0x      xxxxxx  xx) (load-register-literal! I)
            (bits 'xx10    00      xxxxxx  xx) (load-store-no-allocate-pair-offset! I)
            (bits 'xx10    01      xxxxxx  xx) (load-store-register-pair-post-indexed! I)
            (bits 'xx10    10      xxxxxx  xx) (load-store-register-pair-offset! I)
            (bits 'xx10    11      xxxxxx  xx) (load-store-register-pair-pre-indexed! I)
            (bits 'xx11    0x      0xxxxx  00) (load-store-register-unscaled-immediate! I)
            (bits 'xx11    0x      0xxxxx  x1) (load-store-register-immediate-indexed! I)
            (bits 'xx11    0x      1xxxxx  10) (load-store-register-register-offset! I)
            (bits 'xx11    1x      xxxxxx  xx) (load-store-register-unsigned-immediate! I)
        )
    )
)


;;;; Load/store exclusive
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  size    0  0  1  0  0  0   o2    L   o1                    o0

(defn load-store-exclusive! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" o2 (at I 23)
        #_"bits(1)" L (at I 22)
        #_"bits(1)" o1 (at I 21)
        #_"bits(1)" o0 (at I 15)
    ]
        (case (cat size    o2  L   o1  o0)
            (bits '00      0   0   0   0) (stxrb! I)
            (bits '00      0   0   0   1) (stlxrb! I)
            (bits '00      0   1   0   0) (ldxrb! I)
            (bits '00      0   1   0   1) (ldaxrb! I)
            (bits '00      1   0   0   1) (stlrb! I)
            (bits '00      1   1   0   1) (ldarb! I)
            (bits '01      0   0   0   0) (stxrh! I)
            (bits '01      0   0   0   1) (stlxrh! I)
            (bits '01      0   1   0   0) (ldxrh! I)
            (bits '01      0   1   0   1) (ldaxrh! I)
            (bits '01      1   0   0   1) (stlrh! I)
            (bits '01      1   1   0   1) (ldarh! I)
            (bits '1x      0   0   0   0) (stxr! I)
            (bits '1x      0   0   0   1) (stlxr! I)
            (bits '1x      0   0   1   0) (stxp! I)
            (bits '1x      0   0   1   1) (stlxp! I)
            (bits '1x      0   1   0   0) (ldxr! I)
            (bits '1x      0   1   0   1) (ldaxr! I)
            (bits '1x      0   1   1   0) (ldxp! I)
            (bits '1x      0   1   1   1) (ldaxp! I)
            (bits '1x      1   0   0   1) (stlr! I)
            (bits '1x      1   1   0   1) (ldar! I)
        )
    )
)

;;;; STXRB
;;
;;; Store Exclusive Register Byte stores a byte from a register to memory if the PE has exclusive access to the memory
;;; address, and returns a status value of 0 if the store was successful, or of 1 if no store was performed.
;;; The memory access is atomic.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    0  0  1  0  0  0    0    0    0         Rs          0 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L                         o0        Rt2
;;
;;; "STXRB <Ws>, <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Ws>        Is the 32-bit name of the general-purpose register into which the status result of the store exclusive is
;;;             written, encoded in the "Rs" field. The value returned is:
;;;             0
;;;                 If the operation updates memory.
;;;             1
;;;                 If the operation fails to update memory.
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stxrb! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" s (unsigned Rs)     ;; ignored by all loads and store-release

        #_"bits(64)" address (if (= n 31) (sp) (X n))

        #_"bits(8)" data (X t)
        #_"bits(1)" status (bits '1)
    ]
        ;; Check whether the Exclusives monitors are set to include the
        ;; physical memory locations corresponding to virtual address
        ;; range [address, address+dbytes-1].
        (when (exclusive-monitors-pass? address, 1)
            ;; This atomic write will be rejected if it does not refer
            ;; to the same physical locations after address translation.
            (mem! address, 1, data)
            ( ass status (exclusive-monitors-status))
        )
        (X! s (zero-extend status, 32))
    )
)

;;;; STLXRB
;;
;;; Store-Release Exclusive Register Byte stores a byte from a 32-bit register to memory if the PE has exclusive access to
;;; the memory address, and returns a status value of 0 if the store was successful, or of 1 if no store was performed.
;;; The memory access is atomic. The instruction also has memory ordering semantics as described in
;;; Load-Acquire, Store-Release.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    0  0  1  0  0  0    0    0    0         Rs          1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L                         o0        Rt2
;;
;;; "STLXRB <Ws>, <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Ws>        Is the 32-bit name of the general-purpose register into which the status result of the store exclusive is
;;;             written, encoded in the "Rs" field. The value returned is:
;;;             0
;;;                 If the operation updates memory.
;;;             1
;;;                 If the operation fails to update memory.
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stlxrb! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" s (unsigned Rs)     ;; ignored by all loads and store-release

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(8)" data (X t)
        #_"bits(1)" status (bits '1)
    ]
        ;; Check whether the Exclusives monitors are set to include the
        ;; physical memory locations corresponding to virtual address
        ;; range [address, address+dbytes-1].
        (when (exclusive-monitors-pass? address, 1)
            ;; This atomic write will be rejected if it does not refer
            ;; to the same physical locations after address translation.
            (mem! address, 1, data)
            ( ass status (exclusive-monitors-status))
        )
        (X! s (zero-extend status, 32))
    )
)

;;;; LDXRB
;;
;;; Load Exclusive Register Byte derives an address from a base register value, loads a byte from memory, zero-extends it
;;; and writes it to a register. The memory access is atomic. The PE marks the physical address being accessed as an
;;; exclusive access. This exclusive access mark is checked by Store Exclusive instructions.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    0  0  1  0  0  0    0    1    0 (1) (1) (1) (1) (1) 0 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "LDXRB <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldxrb! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
    ]
        ;; Tell the Exclusives monitors to record a sequence of one or more atomic
        ;; memory reads from virtual address range [address, address+dbytes-1].
        ;; The Exclusives monitor will only be set if all the reads are from the
        ;; same dbytes-aligned physical address, to allow for the possibility of
        ;; an atomicity break if the translation is changed between reads.
        (set-exclusive-monitors address, 1)

        (let [
            #_"bits(8)" data (mem address, 1)
        ]
            (X! t (zero-extend data, 32))
        )
    )
)

;;;; LDAXRB
;;
;;; Load-Acquire Exclusive Register Byte derives an address from a base register value, loads a byte from memory, zero-
;;; extends it and writes it to a register. The memory access is atomic. The PE marks the physical address being accessed
;;; as an exclusive access. This exclusive access mark is checked by Store Exclusive instructions.
;;; The instruction also has memory ordering semantics as described in Load-Acquire, Store-Release.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    0  0  1  0  0  0    0    1    0 (1) (1) (1) (1) (1) 1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "LDAXRB <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldaxrb! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
    ]
        ;; Tell the Exclusives monitors to record a sequence of one or more atomic
        ;; memory reads from virtual address range [address, address+dbytes-1].
        ;; The Exclusives monitor will only be set if all the reads are from the
        ;; same dbytes-aligned physical address, to allow for the possibility of
        ;; an atomicity break if the translation is changed between reads.
        (set-exclusive-monitors address, 1)

        (let [
            #_"bits(8)" data (mem address, 1)
        ]
            (X! t (zero-extend data, 32))
        )
    )
)

;;;; STLRB
;;
;;; Store-Release Register Byte stores a byte from a 32-bit register to a memory location. The instruction also has memory
;;; ordering semantics as described in Load-Acquire, Store-Release.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    0  0  1  0  0  0    1    0    0 (1) (1) (1) (1) (1) 1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "STLRB <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stlrb! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(8)" data (X t)
    ]
        (mem! address, 1, data)
    )
)

;;;; LDARB
;;
;;; Load-Acquire Register Byte derives an address from a base register value, loads a byte from memory, zero-extends it
;;; and writes it to a register. The instruction also has memory ordering semantics as described in Load-Acquire, Store-
;;; Release.
;;; For this instruction, if the destination is WZR/XZR, it is impossible for software to observe the presence of the
;;; acquire semantic other than its effect on the arrival at endpoints.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    0  0  1  0  0  0    1    1    0 (1) (1) (1) (1) (1) 1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "LDARB <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldarb! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(8)" data (mem address, 1)
    ]
        (X! t (zero-extend data, 32))
    )
)

;;;; STXRH
;;
;;; Store Exclusive Register Halfword stores a halfword from a register to memory if the PE has exclusive access to the
;;; memory address, and returns a status value of 0 if the store was successful, or of 1 if no store was performed.
;;; The memory access is atomic.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    0  0  1  0  0  0    0    0    0         Rs          0 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L                         o0        Rt2
;;
;;; "STXRH <Ws>, <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Ws>        Is the 32-bit name of the general-purpose register into which the status result of the store exclusive is
;;;             written, encoded in the "Rs" field. The value returned is:
;;;             0
;;;                 If the operation updates memory.
;;;             1
;;;                 If the operation fails to update memory.
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stxrh! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" s (unsigned Rs)     ;; ignored by all loads and store-release

        #_"bits(64)" address (if (= n 31) (sp) (X n))

        #_"bits(16)" data (X t)
        #_"bits(1)" status (bits '1)
    ]
        ;; Check whether the Exclusives monitors are set to include the
        ;; physical memory locations corresponding to virtual address
        ;; range [address, address+dbytes-1].
        (when (exclusive-monitors-pass? address, 2)
            ;; This atomic write will be rejected if it does not refer
            ;; to the same physical locations after address translation.
            (mem! address, 2, data)
            ( ass status (exclusive-monitors-status))
        )
        (X! s (zero-extend status, 32))
    )
)

;;;; STLXRH
;;
;;; Store-Release Exclusive Register Halfword stores a halfword from a 32-bit register to memory if the PE has exclusive
;;; access to the memory address, and returns a status value of 0 if the store was successful, or of 1 if no store was
;;; performed. The memory access is atomic. The instruction also has memory ordering semantics as described in
;;; Load-Acquire, Store-Release.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    0  0  1  0  0  0    0    0    0         Rs          1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L                         o0        Rt2
;;
;;; "STLXRH <Ws>, <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Ws>        Is the 32-bit name of the general-purpose register into which the status result of the store exclusive is
;;;             written, encoded in the "Rs" field. The value returned is:
;;;             0
;;;                 If the operation updates memory.
;;;             1
;;;                 If the operation fails to update memory.
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stlxrh! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" s (unsigned Rs)     ;; ignored by all loads and store-release

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(16)" data (X t)
        #_"bits(1)" status (bits '1)
    ]
        ;; Check whether the Exclusives monitors are set to include the
        ;; physical memory locations corresponding to virtual address
        ;; range [address, address+dbytes-1].
        (when (exclusive-monitors-pass? address, 2)
            ;; This atomic write will be rejected if it does not refer
            ;; to the same physical locations after address translation.
            (mem! address, 2, data)
            ( ass status (exclusive-monitors-status))
        )
        (X! s (zero-extend status, 32))
    )
)

;;;; LDXRH
;;
;;; Load Exclusive Register Halfword derives an address from a base register value, loads a halfword from memory, zero-
;;; extends it and writes it to a register. The memory access is atomic. The PE marks the physical address being accessed
;;; as an exclusive access. This exclusive access mark is checked by Store Exclusive instructions.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    0  0  1  0  0  0    0    1    0 (1) (1) (1) (1) (1) 0 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "LDXRH <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldxrh! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
    ]
        ;; Tell the Exclusives monitors to record a sequence of one or more atomic
        ;; memory reads from virtual address range [address, address+dbytes-1].
        ;; The Exclusives monitor will only be set if all the reads are from the
        ;; same dbytes-aligned physical address, to allow for the possibility of
        ;; an atomicity break if the translation is changed between reads.
        (set-exclusive-monitors address, 2)

        (let [
            #_"bits(16)" data (mem address, 2)
        ]
            (X! t (zero-extend data, 32))
        )
    )
)

;;;; LDAXRH
;;
;;; Load-Acquire Exclusive Register Halfword derives an address from a base register value, loads a halfword from
;;; memory, zero-extends it and writes it to a register. The memory access is atomic. The PE marks the physical address
;;; being accessed as an exclusive access. This exclusive access mark is checked by Store Exclusive instructions.
;;; The instruction also has memory ordering semantics as described in Load-Acquire, Store-Release.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    0  0  1  0  0  0    0    1    0 (1) (1) (1) (1) (1) 1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "LDAXRH <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldaxrh! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
    ]
        ;; Tell the Exclusives monitors to record a sequence of one or more atomic
        ;; memory reads from virtual address range [address, address+dbytes-1].
        ;; The Exclusives monitor will only be set if all the reads are from the
        ;; same dbytes-aligned physical address, to allow for the possibility of
        ;; an atomicity break if the translation is changed between reads.
        (set-exclusive-monitors address, 2)

        (let [
            #_"bits(16)" data (mem address, 2)
        ]
            (X! t (zero-extend data, 32))
        )
    )
)

;;;; STLRH
;;
;;; Store-Release Register Halfword stores a halfword from a 32-bit register to a memory location. The instruction also
;;; has memory ordering semantics as described in Load-Acquire, Store-Release.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    0  0  1  0  0  0    1    0    0 (1) (1) (1) (1) (1) 1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "STLRH <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stlrh! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(16)" data (X t)
    ]
        (mem! address, 2, data)
    )
)

;;;; LDARH
;;
;;; Load-Acquire Register Halfword derives an address from a base register value, loads a halfword from memory, zero-
;;; extends it, and writes it to a register. The instruction also has memory ordering semantics as described in Load-
;;; Acquire, Store-Release.
;;; For this instruction, if the destination is WZR/XZR, it is impossible for software to observe the presence of the
;;; acquire semantic other than its effect on the arrival at endpoints.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    0  0  1  0  0  0    1    1    0 (1) (1) (1) (1) (1) 1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "LDARH <Wt>, [<Xn|SP>{,#0}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldarh! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(16)" data (mem address, 2)
    ]
        (X! t (zero-extend data, 32))
    )
)

;;;; STXR
;;
;;; Store Exclusive Register stores a 32-bit word or a 64-bit doubleword from a register to memory if the PE has exclusive
;;; access to the memory address, and returns a status value of 0 if the store was successful, or of 1 if no store was
;;; performed.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    0  0  1  0  0  0    0    0    0         Rs          0 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L                         o0        Rt2
;;
;;; "STXR <Ws>, <Wt>, [<Xn|SP>{,#0}]" [32-bit (size == 10)]
;;; "STXR <Ws>, <Xt>, [<Xn|SP>{,#0}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Ws>        Is the 32-bit name of the general-purpose register into which the status result of the store exclusive is
;;;             written, encoded in the "Rs" field. The value returned is:
;;;             0
;;;                 If the operation updates memory.
;;;             1
;;;                 If the operation fails to update memory.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stxr! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" s (unsigned Rs)     ;; ignored by all loads and store-release

        #_"integer" elsize (<< 8 (unsigned size))

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"integer" dbytes (quot elsize 8)

        #_"bits(elsize)" data (X t)
        #_"bits(1)" status (bits '1)
    ]
        ;; Check whether the Exclusives monitors are set to include the
        ;; physical memory locations corresponding to virtual address
        ;; range [address, address+dbytes-1].
        (when (exclusive-monitors-pass? address, dbytes)
            ;; This atomic write will be rejected if it does not refer
            ;; to the same physical locations after address translation.
            (mem! address, dbytes, data)
            ( ass status (exclusive-monitors-status))
        )
        (X! s (zero-extend status, 32))
    )
)

;;;; STLXR
;;
;;; Store-Release Exclusive Register stores a 32-bit word or a 64-bit doubleword to memory if the PE has exclusive access
;;; to the memory address, from two registers, and returns a status value of 0 if the store was successful, or of 1 if no
;;; store was performed. The memory access is atomic. The instruction also has memory ordering semantics as described in
;;; Load-Acquire, Store-Release.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    0  0  1  0  0  0    0    0    0         Rs          1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L                         o0        Rt2
;;
;;; "STLXR <Ws>, <Wt>, [<Xn|SP>{,#0}]" [32-bit (size == 10)]
;;; "STLXR <Ws>, <Xt>, [<Xn|SP>{,#0}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Ws>        Is the 32-bit name of the general-purpose register into which the status result of the store exclusive is
;;;             written, encoded in the "Rs" field. The value returned is:
;;;             0
;;;                 If the operation updates memory.
;;;             1
;;;                 If the operation fails to update memory.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stlxr! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" s (unsigned Rs)     ;; ignored by all loads and store-release

        #_"integer" elsize (<< 8 (unsigned size))

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"integer" dbytes (quot elsize 8)
        #_"bits(elsize)" data (X t)
        #_"bits(1)" status (bits '1)
    ]
        ;; Check whether the Exclusives monitors are set to include the
        ;; physical memory locations corresponding to virtual address
        ;; range [address, address+dbytes-1].
        (when (exclusive-monitors-pass? address, dbytes)
            ;; This atomic write will be rejected if it does not refer
            ;; to the same physical locations after address translation.
            (mem! address, dbytes, data)
            ( ass status (exclusive-monitors-status))
        )
        (X! s (zero-extend status, 32))
    )
)

;;;; STXP
;;
;;; Store Exclusive Pair of registers stores two 32-bit words or two 64-bit doublewords from two registers to a memory
;;; location if the PE has exclusive access to the memory address, and returns a status value of 0 if the store was
;;; successful, or of 1 if no store was performed. A 32-bit pair requires the address to be doubleword aligned and
;;; is single-copy atomic at doubleword granularity. A 64-bit pair requires the address to be quadword aligned and,
;;; if the Store-Exclusive succeeds, it causes a single-copy atomic update of the 128-bit memory location being updated.
;;
;;; 31 | 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1   sz    0  0  1  0  0  0    0    0    1         Rs          0        Rt2               Rn               Rt
;;;                                     L                         o0
;;
;;; "STXP <Ws>, <Wt1>, <Wt2>, [<Xn|SP>{,#0}]" [32-bit (sz == 0)]
;;; "STXP <Ws>, <Xt1>, <Xt2>, [<Xn|SP>{,#0}]" [64-bit (sz == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Ws>        Is the 32-bit name of the general-purpose register into which the status result of the store exclusive is
;;;             written, encoded in the "Rs" field. The value returned is:
;;;             0
;;;                 If the operation updates memory.
;;;             1
;;;                 If the operation fails to update memory.
;;; <Xt1>       Is the 64-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt2>       Is the 64-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Wt1>       Is the 32-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Wt2>       Is the 32-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stxp! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sz (at I 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" t2 (unsigned Rt2)   ;; ignored by load/store single register
        #_"integer" s (unsigned Rs)     ;; ignored by all loads and store-release

        #_"integer" elsize (<< 32 (unsigned sz))
        #_"integer" width (* elsize 2)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"integer" dbytes (quot width 8)
        #_"bits(width quot 2)" el1 (X t)
        #_"bits(width quot 2)" el2 (X t2)

        #_"bits(width)" data (cat el2 el1)
        #_"bits(1)" status (bits '1)
    ]
        ;; Check whether the Exclusives monitors are set to include the
        ;; physical memory locations corresponding to virtual address
        ;; range [address, address+dbytes-1].
        (when (exclusive-monitors-pass? address, dbytes)
            ;; This atomic write will be rejected if it does not refer
            ;; to the same physical locations after address translation.
            (mem! address, dbytes, data)
            ( ass status (exclusive-monitors-status))
        )
        (X! s (zero-extend status, 32))
    )
)

;;;; STLXP
;;
;;; Store-Release Exclusive Pair of registers stores two 32-bit words or two 64-bit doublewords to a memory location if the
;;; PE has exclusive access to the memory address, from two registers, and returns a status value of 0 if the store was
;;; successful, or of 1 if no store was performed. A 32-bit pair requires the address to be doubleword aligned and
;;; is single-copy atomic at doubleword granularity. A 64-bit pair requires the address to be quadword aligned and,
;;; if the Store-Exclusive succeeds, it causes a single-copy atomic update of the 128-bit memory location being updated.
;;; The instruction also has memory ordering semantics as described in Load-Acquire, Store-Release.
;;
;;; 31 | 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1   sz    0  0  1  0  0  0    0    0    1         Rs          1        Rt2               Rn               Rt
;;;                                     L                         o0
;;
;;; "STLXP <Ws>, <Wt1>, <Wt2>, [<Xn|SP>{,#0}]" [32-bit (sz == 0)]
;;; "STLXP <Ws>, <Xt1>, <Xt2>, [<Xn|SP>{,#0}]" [64-bit (sz == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Ws>        Is the 32-bit name of the general-purpose register into which the status result of the store exclusive is
;;;             written, encoded in the "Rs" field. The value returned is:
;;;             0
;;;                 If the operation updates memory.
;;;             1
;;;                 If the operation fails to update memory.
;;; <Xt1>       Is the 64-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt2>       Is the 64-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Wt1>       Is the 32-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Wt2>       Is the 32-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stlxp! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sz (at I 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" t2 (unsigned Rt2)   ;; ignored by load/store single register
        #_"integer" s (unsigned Rs)     ;; ignored by all loads and store-release

        #_"integer" elsize (<< 32 (unsigned sz))
        #_"integer" width (* elsize 2)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"integer" dbytes (quot width 8)
        #_"bits(width quot 2)" el1 (X t)
        #_"bits(width quot 2)" el2 (X t2)
        #_"bits(width)" data (cat el2 el1)
        #_"bits(1)" status (bits '1)
    ]
        ;; Check whether the Exclusives monitors are set to include the
        ;; physical memory locations corresponding to virtual address
        ;; range [address, address+dbytes-1].
        (when (exclusive-monitors-pass? address, dbytes)
            ;; This atomic write will be rejected if it does not refer
            ;; to the same physical locations after address translation.
            (mem! address, dbytes, data)
            ( ass status (exclusive-monitors-status))
        )
        (X! s (zero-extend status, 32))
    )
)

;;;; LDXR
;;
;;; Load Exclusive Register derives an address from a base register value, loads a 32-bit word or a 64-bit doubleword
;;; from memory, and writes it to a register. The memory access is atomic. The PE marks the physical address being
;;; accessed as an exclusive access. This exclusive access mark is checked by Store Exclusive instructions.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    0  0  1  0  0  0    0    1    0 (1) (1) (1) (1) (1) 0 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "LDXR <Wt>, [<Xn|SP>{,#0}]" [32-bit (size == 10)]
;;; "LDXR <Xt>, [<Xn|SP>{,#0}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldxr! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"integer" elsize (<< 8 (unsigned size))
        #_"integer" regsize (if (= elsize 64) 64 32)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"integer" dbytes (quot elsize 8)
    ]
        ;; Tell the Exclusives monitors to record a sequence of one or more atomic
        ;; memory reads from virtual address range [address, address+dbytes-1].
        ;; The Exclusives monitor will only be set if all the reads are from the
        ;; same dbytes-aligned physical address, to allow for the possibility of
        ;; an atomicity break if the translation is changed between reads.
        (set-exclusive-monitors address, dbytes)

        (let [
            #_"bits(elsize)" data (mem address, dbytes)
        ]
            (X! t (zero-extend data, regsize))
        )
    )
)

;;;; LDAXR
;;
;;; Load-Acquire Exclusive Register derives an address from a base register value, loads a 32-bit word or 64-bit
;;; doubleword from memory, and writes it to a register. The memory access is atomic. The PE marks the physical address
;;; being accessed as an exclusive access. This exclusive access mark is checked by Store Exclusive instructions.
;;; The instruction also has memory ordering semantics as described in Load-Acquire, Store-Release.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    0  0  1  0  0  0    0    1    0 (1) (1) (1) (1) (1) 1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "LDAXR <Wt>, [<Xn|SP>{,#0}]" [32-bit (size == 10)]
;;; "LDAXR <Xt>, [<Xn|SP>{,#0}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldaxr! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"integer" elsize (<< 8 (unsigned size))
        #_"integer" regsize (if (= elsize 64) 64 32)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"integer" dbytes (quot elsize 8)
    ]
        ;; Tell the Exclusives monitors to record a sequence of one or more atomic
        ;; memory reads from virtual address range [address, address+dbytes-1].
        ;; The Exclusives monitor will only be set if all the reads are from the
        ;; same dbytes-aligned physical address, to allow for the possibility of
        ;; an atomicity break if the translation is changed between reads.
        (set-exclusive-monitors address, dbytes)

        (let [
            #_"bits(elsize)" data (mem address, dbytes)
        ]
            (X! t (zero-extend data, regsize))
        )
    )
)

;;;; LDXP
;;
;;; Load Exclusive Pair of Registers derives an address from a base register value, loads two 32-bit words or two 64-bit
;;; doublewords from memory, and writes them to two registers. A 32-bit pair requires the address to be doubleword
;;; aligned and is single-copy atomic at doubleword granularity. A 64-bit pair requires the address to be quadword aligned
;;; and is single-copy atomic for each doubleword at doubleword granularity. The PE marks the physical address being
;;; accessed as an exclusive access. This exclusive access mark is checked by Store Exclusive instructions.
;;
;;; 31 | 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1   sz    0  0  1  0  0  0    0    1    1 (1) (1) (1) (1) (1) 0        Rt2               Rn               Rt
;;;                                     L              Rs         o0
;;
;;; "LDXP <Wt1>, <Wt2>, [<Xn|SP>{,#0}]" [32-bit (sz == 0)]
;;; "LDXP <Xt1>, <Xt2>, [<Xn|SP>{,#0}]" [64-bit (sz == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wt1>       Is the 32-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Wt2>       Is the 32-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xt1>       Is the 64-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt2>       Is the 64-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldxp! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sz (at I 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" t2 (unsigned Rt2)

        #_"integer" elsize (<< 32 (unsigned sz))
        #_"integer" width (* elsize 2)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"integer" dbytes (quot width 8)
    ]
        ;; Tell the Exclusives monitors to record a sequence of one or more atomic
        ;; memory reads from virtual address range [address, address+dbytes-1].
        ;; The Exclusives monitor will only be set if all the reads are from the
        ;; same dbytes-aligned physical address, to allow for the possibility of
        ;; an atomicity break if the translation is changed between reads.
        (set-exclusive-monitors address, dbytes)

        (if (= elsize 32)
            (let [
                ;; 32-bit load exclusive pair (atomic)
                #_"bits(width)" data (mem address, dbytes)
            ]
                (X! t (at data (- elsize 1) 0))
                (X! t2 (at data (- width 1) elsize))
            )
            (do
                ;; 64-bit load exclusive pair (not atomic), but must be 128-bit aligned
                (X! t (mem address, 8))
                (X! t2 (mem (+ address 8), 8))
            )
        )
    )
)

;;;; LDAXP
;;
;;; Load-Acquire Exclusive Pair of Registers derives an address from a base register value, loads two 32-bit words or
;;; two 64-bit doublewords from memory, and writes them to two registers. A 32-bit pair requires the address to be
;;; doubleword aligned and is single-copy atomic at doubleword granularity. A 64-bit pair requires the address to be
;;; quadword aligned and is single-copy atomic for each doubleword at doubleword granularity. The PE marks the
;;; physical address being accessed as an exclusive access. This exclusive access mark is checked by Store Exclusive
;;; instructions. The instruction also has memory ordering semantics as described in Load-Acquire, Store-Release.
;;
;;; 31 | 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1   sz    0  0  1  0  0  0    0    1    1 (1) (1) (1) (1) (1) 1        Rt2               Rn               Rt
;;;                                     L              Rs         o0
;;
;;; "LDAXP <Wt1>, <Wt2>, [<Xn|SP>{,#0}]" [32-bit (sz == 0)]
;;; "LDAXP <Xt1>, <Xt2>, [<Xn|SP>{,#0}]" [64-bit (sz == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wt1>       Is the 32-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Wt2>       Is the 32-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xt1>       Is the 64-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt2>       Is the 64-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldaxp! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sz (at I 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" t2 (unsigned Rt2)

        #_"integer" elsize (<< 32 (unsigned sz))
        #_"integer" width (* elsize 2)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"integer" dbytes (quot width 8)
    ]
        ;; Tell the Exclusives monitors to record a sequence of one or more atomic
        ;; memory reads from virtual address range [address, address+dbytes-1].
        ;; The Exclusives monitor will only be set if all the reads are from the
        ;; same dbytes-aligned physical address, to allow for the possibility of
        ;; an atomicity break if the translation is changed between reads.
        (set-exclusive-monitors address, dbytes)

        (if (= elsize 32)
            (let [
                ;; 32-bit load exclusive pair (atomic)
                #_"bits(width)" data (mem address, dbytes)
            ]
                (X! t (at data (- elsize 1) 0))
                (X! t2 (at data (- width 1) elsize))
            )
            (do
                ;; 64-bit load exclusive pair (not atomic), but must be 128-bit aligned
                (X! t (mem address, 8))
                (X! t2 (mem (+ address 8), 8))
            )
        )
    )
)

;;;; STLR
;;
;;; Store-Release Register stores a 32-bit word or a 64-bit doubleword to a memory location, from a register. The
;;; instruction also has memory ordering semantics as described in Load-Acquire, Store-Release.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    0  0  1  0  0  0    1    0    0 (1) (1) (1) (1) (1) 1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "STLR <Wt>, [<Xn|SP>{,#0}]" [32-bit (size == 10)]
;;; "STLR <Xt>, [<Xn|SP>{,#0}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn stlr! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" elsize (<< 8 (unsigned size))

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"integer" dbytes (quot elsize 8)
        #_"bits(elsize)" data (X t)
    ]
        (mem! address, dbytes, data)
    )
)

;;;; LDAR
;;
;;; Load-Acquire Register derives an address from a base register value, loads a 32-bit word or 64-bit doubleword from
;;; memory, and writes it to a register. The instruction also has memory ordering semantics as described in Load-Acquire,
;;; Store-Release.
;;; For this instruction, if the destination is WZR/XZR, it is impossible for software to observe the presence of the
;;; acquire semantic other than its effect on the arrival at endpoints.
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    0  0  1  0  0  0    1    1    0 (1) (1) (1) (1) (1) 1 (1) (1) (1) (1) (1)      Rn               Rt
;;;  size                             L              Rs         o0        Rt2
;;
;;; "LDAR <Wt>, [<Xn|SP>{,#0}]" [32-bit (size == 10)]
;;; "LDAR <Xt>, [<Xn|SP>{,#0}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.

(defn ldar! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(5)" Rs (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"integer" elsize (<< 8 (unsigned size))
        #_"integer" regsize (if (= elsize 64) 64 32)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"integer" dbytes (quot elsize 8)
        #_"bits(elsize)" data (mem address, dbytes)
    ]
        (X! t (zero-extend data, regsize))
    )
)


;;;; Load register (literal)
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  opc     0  1  1  0  0  0

(defn load-register-literal! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
    ]
        (case opc
            (bits '0x) (ldr-literal! I)
            (bits '10) (ldrsw-literal! I)
        )
    )
)

;;;; LDR (literal)
;;
;;; Load Register (literal) calculates an address from the PC value and an immediate offset, loads a word from memory,
;;; and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;;  0  x    0  1  1    0    0  0                              imm19                                 Rt
;;;  opc
;;
;;; "LDR <Wt>, <label>" [32-bit (opc == 00)]
;;; "LDR <Xt>, <label>" [64-bit (opc == 01)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be loaded, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be loaded, encoded in the "Rt" field.
;;; <label>     Is the program label from which the data is to be loaded. Its offset from the address of this instruction,
;;;             in the range +/-1MB, is encoded as "imm19" times 4.

(defn ldr-literal! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(19)" imm19 (at I 23 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" t (unsigned Rt)
        #_"bits(64)" offset (sign-extend (cat imm19 (bits '00)), 64)

        [#_"boolean" signed?, #_"integer" size]
            (case opc
                (bits '00) [false, 4]
                (bits '01) [false, 8]
                (bits '10) [true, 4]
                (bits '11) [true, 8]
            )
        #_"boolean" prefetch? (and signed? (= size 8))
    ]
        (when-not prefetch?
            (let [
                #_"bits(64)" address (+ (pc) offset)
                #_"bits(size * 8)" data (mem address, size)
            ]
                (X! t (if signed? (sign-extend data, 64) data))
            )
        )
    )
)

;;;; LDRSW (literal)
;;
;;; Load Register Signed Word (literal) calculates an address from the PC value and an immediate offset, loads a word
;;; from memory, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5 |  4  3  2  1  0
;;;  1  0    0  1  1    0    0  0                              imm19                                 Rt
;;;  opc
;;
;;; "LDRSW <Xt>, <label>"
;;
;;;; Assembler Symbols
;;
;;; <Xt>        Is the 64-bit name of the general-purpose register to be loaded, encoded in the "Rt" field.
;;; <label>     Is the program label from which the data is to be loaded. Its offset from the address of this instruction,
;;;             in the range +/-1MB, is encoded as "imm19" times 4.

(defn ldrsw-literal! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(19)" imm19 (at I 23 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" t (unsigned Rt)
        #_"bits(64)" offset (sign-extend (cat imm19 (bits '00)), 64)

        #_"bits(64)" address (+ (pc) offset)
        #_"bits(32)" data (mem address, 4)
    ]
        (X! t (sign-extend data, 64))
    )
)


;;;; Load/store no-allocate pair (offset)
;;
;;; 31 30 | 29 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  opc     1  0  1  0  0  0  0    L

(defn load-store-no-allocate-pair-offset! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(1)" L (at I 22)
    ]
        (case (cat opc     L)
            (bits 'x0      0) (stnp! I)
            (bits 'x0      1) (ldnp! I)
        )
    )
)

;;;; STNP
;;
;;; Store Pair of Registers, with non-temporal hint, calculates an address from a base register value and an immediate
;;; offset, and stores two 32-bit words or two 64-bit doublewords to the calculated address, from two registers.
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  x  0    1  0  1    0    0    0  0    0           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "STNP <Wt1>, <Wt2>, [<Xn|SP>{, #<imm>}]" [32-bit (opc == 00)]
;;; "STNP <Xt1>, <Xt2>, [<Xn|SP>{, #<imm>}]" [64-bit (opc == 10)]
;;
;;;; Assembler Symbols
;;
;;; <Wt1>       Is the 32-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Wt2>       Is the 32-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xt1>       Is the 64-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt2>       Is the 64-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <imm>       For the 32-bit variant: is the optional signed immediate byte offset, a multiple of 4 in the range -256 to 252,
;;;             defaulting to 0 and encoded in the "imm7" field as <imm>/4.
;;;             For the 64-bit variant: is the optional signed immediate byte offset, a multiple of 8 in the range -512 to 504,
;;;             defaulting to 0 and encoded in the "imm7" field as <imm>/8.

(defn stnp! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(7)" imm7 (at I 21 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" t2 (unsigned Rt2)
        _
            (when (= (at opc 0) (bits '1))
                (throw! "UNDEFINED")
            )

        #_"integer" scale (+ 2 (unsigned (at opc 1)))
        #_"integer" width (<< 8 scale)
        #_"bits(64)" offset (lsl (sign-extend imm7, 64), scale)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)
        #_"integer" dbytes (quot width 8)

        #_"bits(width)" data1 (X t)
        #_"bits(width)" data2 (X t2)
    ]
        (mem! address, dbytes, data1)
        (mem! (+ address dbytes), dbytes, data2)
    )
)

;;;; LDNP
;;
;;; Load Pair of Registers, with non-temporal hint, calculates an address from a base register value and an immediate
;;; offset, loads two 32-bit words or two 64-bit doublewords from memory, and writes them to two registers.
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  x  0    1  0  1    0    0    0  0    1           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "LDNP <Wt1>, <Wt2>, [<Xn|SP>{, #<imm>}]" [32-bit (opc == 00)]
;;; "LDNP <Xt1>, <Xt2>, [<Xn|SP>{, #<imm>}]" [64-bit (opc == 10)]
;;
;;;; Assembler Symbols
;;
;;; <Wt1>       Is the 32-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Wt2>       Is the 32-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xt1>       Is the 64-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt2>       Is the 64-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <imm>       For the 32-bit variant: is the optional signed immediate byte offset, a multiple of 4 in the range -256 to 252,
;;;             defaulting to 0 and encoded in the "imm7" field as <imm>/4.
;;;             For the 64-bit variant: is the optional signed immediate byte offset, a multiple of 8 in the range -512 to 504,
;;;             defaulting to 0 and encoded in the "imm7" field as <imm>/8.

(defn ldnp! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(7)" imm7 (at I 21 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" t2 (unsigned Rt2)
        _
            (when (= (at opc 0) (bits '1))
                (throw! "UNDEFINED")
            )

        #_"integer" scale (+ 2 (unsigned (at opc 1)))
        #_"integer" width (<< 8 scale)
        #_"bits(64)" offset (lsl (sign-extend imm7, 64), scale)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)
        #_"integer" dbytes (quot width 8)

        #_"bits(width)" data1 (mem address, dbytes)
        #_"bits(width)" data2 (mem (+ address dbytes), dbytes)
    ]
        (X! t data1)
        (X! t2 data2)
    )
)


;;;; Load/store register pair (post-indexed)
;;
;;; 31 30 | 29 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  opc     1  0  1  0  0  0  1    L

(defn load-store-register-pair-post-indexed! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(1)" L (at I 22)
    ]
        (case (cat opc     L)
            (bits 'x0      0) (stp! I)
            (bits 'x0      1) (ldp! I)
            (bits '01      1) (ldpsw! I)
        )
    )
)

;;;; Load/store register pair (offset)
;;
;;; 31 30 | 29 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  opc     1  0  1  0  0  1  0    L

(defn load-store-register-pair-offset! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(1)" L (at I 22)
    ]
        (case (cat opc     L)
            (bits 'x0      0) (stp! I)
            (bits 'x0      1) (ldp! I)
            (bits '01      1) (ldpsw! I)
        )
    )
)

;;;; Load/store register pair (pre-indexed)
;;
;;; 31 30 | 29 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  opc     1  0  1  0  0  1  1    L

(defn load-store-register-pair-pre-indexed! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(1)" L (at I 22)
    ]
        (case (cat opc     L)
            (bits 'x0      0) (stp! I)
            (bits 'x0      1) (ldp! I)
            (bits '01      1) (ldpsw! I)
        )
    )
)

;;;; STP
;;
;;; Store Pair of Registers calculates an address from a base register value and an immediate offset, and stores two 32-bit
;;; words or two 64-bit doublewords to the calculated address, from two registers.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Signed offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  x  0    1  0  1    0    0    0  1    0           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "STP <Wt1>, <Wt2>, [<Xn|SP>], #<imm>" [32-bit (opc == 00)]
;;; "STP <Xt1>, <Xt2>, [<Xn|SP>], #<imm>" [64-bit (opc == 10)]
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  x  0    1  0  1    0    0    1  1    0           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "STP <Wt1>, <Wt2>, [<Xn|SP>, #<imm>]!" [32-bit (opc == 00)]
;;; "STP <Xt1>, <Xt2>, [<Xn|SP>, #<imm>]!" [64-bit (opc == 10)]
;;
;;;; Signed offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  x  0    1  0  1    0    0    1  0    0           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "STP <Wt1>, <Wt2>, [<Xn|SP>{, #<imm>}]" [32-bit (opc == 00)]
;;; "STP <Xt1>, <Xt2>, [<Xn|SP>{, #<imm>}]" [64-bit (opc == 10)]
;;
;;;; Assembler Symbols
;;
;;; <Wt1>       Is the 32-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Wt2>       Is the 32-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xt1>       Is the 64-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt2>       Is the 64-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <imm>       For the 32-bit post-index and 32-bit pre-index variant: is the signed immediate byte offset, a multiple of
;;;             4 in the range -256 to 252, encoded in the "imm7" field as <imm>/4.
;;;             For the 32-bit signed offset variant: is the optional signed immediate byte offset, a multiple of 4 in the
;;;             range -256 to 252, defaulting to 0 and encoded in the "imm7" field as <imm>/4.
;;;             For the 64-bit post-index and 64-bit pre-index variant: is the signed immediate byte offset, a multiple of
;;;             8 in the range -512 to 504, encoded in the "imm7" field as <imm>/8.
;;;             For the 64-bit signed offset variant: is the optional signed immediate byte offset, a multiple of 8 in the
;;;             range -512 to 504, defaulting to 0 and encoded in the "imm7" field as <imm>/8.

(defn stp! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(7)" imm7 (at I 21 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        [#_"boolean" wback?, #_"boolean" postindex?]
            (case 
                Post-index [true, true]
                Pre-index [true, false]
                Signed offset [false, false]
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" t2 (unsigned Rt2)
        _
            (when (or (= (cat L (at opc 0)) (bits '01)) (= opc (bits '11)))
                (throw! "UNDEFINED")
            )

        #_"integer" scale (+ 2 (unsigned (at opc 1)))
        #_"integer" width (<< 8 scale)
        #_"bits(64)" offset (lsl (sign-extend imm7, 64), scale)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)
        #_"integer" dbytes (quot width 8)

        #_"bits(width)" data1 (X t)
        #_"bits(width)" data2 (X t2)
    ]
        (mem! address, dbytes, data1)
        (mem! (+ address dbytes), dbytes, data2)

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)

;;;; LDP
;;
;;; Load Pair of Registers calculates an address from a base register value and an immediate offset, loads two 32-bit
;;; words or two 64-bit doublewords from memory, and writes them to two registers.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Signed offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  x  0    1  0  1    0    0    0  1    1           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "LDP <Wt1>, <Wt2>, [<Xn|SP>], #<imm>" [32-bit (opc == 00)]
;;; "LDP <Xt1>, <Xt2>, [<Xn|SP>], #<imm>" [64-bit (opc == 10)]
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  x  0    1  0  1    0    0    1  1    1           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "LDP <Wt1>, <Wt2>, [<Xn|SP>, #<imm>]!" [32-bit (opc == 00)]
;;; "LDP <Xt1>, <Xt2>, [<Xn|SP>, #<imm>]!" [64-bit (opc == 10)]
;;
;;;; Signed offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  x  0    1  0  1    0    0    1  0    1           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "LDP <Wt1>, <Wt2>, [<Xn|SP>{, #<imm>}]" [32-bit (opc == 00)]
;;; "LDP <Xt1>, <Xt2>, [<Xn|SP>{, #<imm>}]" [64-bit (opc == 10)]
;;
;;;; Assembler Symbols
;;
;;; <Wt1>       Is the 32-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Wt2>       Is the 32-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xt1>       Is the 64-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt2>       Is the 64-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <imm>       For the 32-bit post-index and 32-bit pre-index variant: is the signed immediate byte offset, a multiple of
;;;             4 in the range -256 to 252, encoded in the "imm7" field as <imm>/4.
;;;             For the 32-bit signed offset variant: is the optional signed immediate byte offset, a multiple of 4 in the
;;;             range -256 to 252, defaulting to 0 and encoded in the "imm7" field as <imm>/4.
;;;             For the 64-bit post-index and 64-bit pre-index variant: is the signed immediate byte offset, a multiple of
;;;             8 in the range -512 to 504, encoded in the "imm7" field as <imm>/8.
;;;             For the 64-bit signed offset variant: is the optional signed immediate byte offset, a multiple of 8 in the
;;;             range -512 to 504, defaulting to 0 and encoded in the "imm7" field as <imm>/8.

(defn ldp! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(7)" imm7 (at I 21 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        [#_"boolean" wback?, #_"boolean" postindex?]
            (case 
                Post-index [true, true]
                Pre-index [true, false]
                Signed offset [false, false]
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" t2 (unsigned Rt2)
        _
            (when (or (= (cat L (at opc 0)) (bits '01)) (= opc (bits '11)))
                (throw! "UNDEFINED")
            )

        #_"boolean" signed? (not= (at opc 0) (bits '0))
        #_"integer" scale (+ 2 (unsigned (at opc 1)))
        #_"integer" width (<< 8 scale)
        #_"bits(64)" offset (lsl (sign-extend imm7, 64), scale)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)
        #_"integer" dbytes (quot width 8)

        #_"bits(width)" data1 (mem address, dbytes)
        #_"bits(width)" data2 (mem (+ address dbytes), dbytes)
    ]
        (if signed?
        (do
            (X! t (sign-extend data1, 64))
            (X! t2 (sign-extend data2, 64))
        )
        (do
            (X! t data1)
            (X! t2 data2)
        )
        )

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)

;;;; LDPSW
;;
;;; Load Pair of Registers Signed Word calculates an address from a base register value and an immediate offset, loads
;;; two 32-bit words from memory, sign-extends them, and writes them to two registers.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Signed offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  0  1    0    0    0  1    1           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "LDPSW <Xt1>, <Xt2>, [<Xn|SP>], #<imm>"
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  0  1    0    0    1  1    1           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "LDPSW <Xt1>, <Xt2>, [<Xn|SP>, #<imm>]!"
;;
;;;; Signed offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 | 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  0  1    0    0    1  0    1           imm7                Rt2               Rn               Rt
;;;  opc                                  L
;;
;;; "LDPSW <Xt1>, <Xt2>, [<Xn|SP>{, #<imm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Xt1>       Is the 64-bit name of the first general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt2>       Is the 64-bit name of the second general-purpose register to be transferred, encoded in the "Rt2" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <imm>       For the post-index and pre-index variant: is the signed immediate byte offset,
;;;             a multiple of 4 in the range -256 to 252, encoded in the "imm7" field as <imm>/4.
;;;             For the signed offset variant: is the optional signed immediate byte offset,
;;;             a multiple of 4 in the range -256 to 252, defaulting to 0 and encoded in the "imm7" field as <imm>/4.

(defn ldpsw! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 31 30)
        #_"bits(1)" L (at I 22)
        #_"bits(7)" imm7 (at I 21 15)
        #_"bits(5)" Rt2 (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        [#_"boolean" wback?, #_"boolean" postindex?]
            (case 
                Post-index [true, true]
                Pre-index [true, false]
                Signed offset [false, false]
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" t2 (unsigned Rt2)
        #_"bits(64)" offset (lsl (sign-extend imm7, 64), 2)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)

        #_"bits(32)" data1 (mem address, 4)
        #_"bits(32)" data2 (mem (+ address 4), 4)
    ]
        (X! t (sign-extend data1, 64))
        (X! t2 (sign-extend data2, 64))

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)


;;;; Load/store register (unscaled immediate)
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5  4  3  2  1  0
;;;  size    1  1  1  0  0  0    opc     0                                 0  0

(defn load-store-register-unscaled-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
    ]
        (case (cat size    opc)
            (bits '00      00) (sturb! I)
            (bits '00      01) (ldurb! I)
            (bits '00      1x) (ldursb! I)
            (bits '01      00) (sturh! I)
            (bits '01      01) (ldurh! I)
            (bits '01      1x) (ldursh! I)
            (bits '1x      00) (stur! I)
            (bits '1x      01) (ldur! I)
            (bits '10      10) (ldursw! I)
        )
    )
)

;;;; STURB
;;
;;; Store Register Byte (unscaled) calculates an address from a base register value and an immediate offset, and stores a
;;; byte to the calculated address, from a 32-bit register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    0  0    0              imm9               0  0         Rn               Rt
;;;  size                            opc
;;
;;; "STURB <Wt>, [<Xn|SP>{, #<simm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the optional signed immediate byte offset, in the range -256 to 255, defaulting to 0 and encoded in the "imm9" field.

(defn sturb! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(9)" imm9 (at I 20 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"bits(64)" offset (sign-extend imm9, 64)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)

        #_"bits(8)" data (X t)
    ]
        (mem! address, 1, data)
    )
)

;;;; LDURB
;;
;;; Load Register Byte (unscaled) calculates an address from a base register and an immediate offset, loads a byte from
;;; memory, zero-extends it, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    0  1    0              imm9               0  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDURB <Wt>, [<Xn|SP>{, #<simm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the optional signed immediate byte offset, in the range -256 to 255, defaulting to 0 and encoded in the "imm9" field.

(defn ldurb! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(9)" imm9 (at I 20 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"bits(64)" offset (sign-extend imm9, 64)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)

        #_"bits(8)" data (mem address, 1)
    ]
        (X! t (zero-extend data, 32))
    )
)

(defn #_"void" -load-store-register! [#_"bits(2)" opc, #_"integer" t, #_"bits(64)" address, #_"integer" width]
    (let [
        [#_"boolean" store?, #_"integer" regsize, #_"boolean" signed?]
            (if (= (at opc 1) (bits '0))
            [
                ;; store or zero-extending load
                (not (= (at opc 0) (bits '1)))
                32
                false
            ]
            [
                ;; sign-extending load
                false
                (if (= (at opc 0) (bits '1)) 32 64)
                true
            ]
            )
    ]
        (if store?
            (let [
                #_"bits(width)" data (X t)
            ]
                (mem! address, (quot width 8), data)
            )
            (let [
                #_"bits(width)" data (mem address, (quot width 8))
            ]
                (X! t ((if signed? sign-extend zero-extend) data, regsize))
            )
        )
    )
    nil
)

;;;; LDURSB
;;
;;; Load Register Signed Byte (unscaled) calculates an address from a base register and an immediate offset, loads a
;;; signed byte from memory, sign-extends it, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    1  x    0              imm9               0  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDURSB <Wt>, [<Xn|SP>{, #<simm>}]" [32-bit (opc == 11)]
;;; "LDURSB <Xt>, [<Xn|SP>{, #<simm>}]" [64-bit (opc == 10)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the optional signed immediate byte offset, in the range -256 to 255, defaulting to 0 and encoded in the "imm9" field.

(defn ldursb! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(9)" imm9 (at I 20 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"bits(64)" offset (sign-extend imm9, 64)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)
    ]
        (-load-store-register! opc, t, address, 8)
    )
)

;;;; STURH
;;
;;; Store Register Halfword (unscaled) calculates an address from a base register value and an immediate offset, and
;;; stores a halfword to the calculated address, from a 32-bit register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    0  0    0              imm9               0  0         Rn               Rt
;;;  size                            opc
;;
;;; "STURH <Wt>, [<Xn|SP>{, #<simm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the optional signed immediate byte offset, in the range -256 to 255, defaulting to 0 and encoded in the "imm9" field.

(defn sturh! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(9)" imm9 (at I 20 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"bits(64)" offset (sign-extend imm9, 64)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)

        #_"bits(16)" data (X t)
    ]
        (mem! address, 2, data)
    )
)

;;;; LDURH
;;
;;; Load Register Halfword (unscaled) calculates an address from a base register and an immediate offset, loads a
;;; halfword from memory, zero-extends it, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    0  1    0              imm9               0  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDURH <Wt>, [<Xn|SP>{, #<simm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the optional signed immediate byte offset, in the range -256 to 255, defaulting to 0 and encoded in the "imm9" field.

(defn ldurh! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(9)" imm9 (at I 20 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"bits(64)" offset (sign-extend imm9, 64)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)

        #_"bits(16)" data (mem address, 2)
    ]
        (X! t (zero-extend data, 32))
    )
)

;;;; LDURSH
;;
;;; Load Register Signed Halfword (unscaled) calculates an address from a base register and an immediate offset, loads a
;;; signed halfword from memory, sign-extends it, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    1  x    0              imm9               0  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDURSH <Wt>, [<Xn|SP>{, #<simm>}]" [32-bit (opc == 11)]
;;; "LDURSH <Xt>, [<Xn|SP>{, #<simm>}]" [64-bit (opc == 10)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the optional signed immediate byte offset, in the range -256 to 255, defaulting to 0 and encoded in the "imm9" field.

(defn ldursh! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(9)" imm9 (at I 20 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"bits(64)" offset (sign-extend imm9, 64)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)
    ]
        (-load-store-register! opc, t, address, 16)
    )
)

;;;; STUR
;;
;;; Store Register (unscaled) calculates an address from a base register value and an immediate offset, and stores a 32-bit
;;; word or a 64-bit doubleword to the calculated address, from a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    1  1  1    0    0  0    0  0    0              imm9               0  0         Rn               Rt
;;;  size                            opc
;;
;;; "STUR <Wt>, [<Xn|SP>{, #<simm>}]" [32-bit (size == 10)]
;;; "STUR <Xt>, [<Xn|SP>{, #<simm>}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the optional signed immediate byte offset, in the range -256 to 255, defaulting to 0 and encoded in the "imm9" field.

(defn stur! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(9)" imm9 (at I 20 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" scale (unsigned size)
        #_"bits(64)" offset (sign-extend imm9, 64)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"integer" width (<< 8 scale)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)

        #_"bits(width)" data (X t)
    ]
        (mem! address, (quot width 8), data)
    )
)

;;;; LDUR
;;
;;; Load Register (unscaled) calculates an address from a base register and an immediate offset, loads a 32-bit word or
;;; 64-bit doubleword from memory, zero-extends it, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    1  1  1    0    0  0    0  1    0              imm9               0  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDUR <Wt>, [<Xn|SP>{, #<simm>}]" [32-bit (size == 10)]
;;; "LDUR <Xt>, [<Xn|SP>{, #<simm>}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the optional signed immediate byte offset, in the range -256 to 255, defaulting to 0 and encoded in the "imm9" field.

(defn ldur! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(9)" imm9 (at I 20 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" scale (unsigned size)
        #_"bits(64)" offset (sign-extend imm9, 64)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" regsize (if (= size (bits '11)) 64 32)
        #_"integer" width (<< 8 scale)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)

        #_"bits(width)" data (mem address, (quot width 8))
    ]
        (X! t (zero-extend data, regsize))
    )
)

;;;; LDURSW
;;
;;; Load Register Signed Word (unscaled) calculates an address from a base register and an immediate offset, loads a
;;; signed word from memory, sign-extends it, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  0    1  1  1    0    0  0    1  0    0              imm9               0  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDURSW <Xt>, [<Xn|SP>{, #<simm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the optional signed immediate byte offset, in the range -256 to 255, defaulting to 0 and encoded in the "imm9" field.

(defn ldursw! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(9)" imm9 (at I 20 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"bits(64)" offset (sign-extend imm9, 64)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (+ address offset)
        #_"bits(32)" data (mem address, 4)
    ]
        (X! t (sign-extend data, 64))
    )
)


;;;; Load/store register (immediate indexed)
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5  4  3  2  1  0
;;;  size    1  1  1  0  0  0    opc     0                                 x  1

(defn load-store-register-immediate-indexed! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
    ]
        (case (cat size    opc)
            (bits '00      00) (strb-immediate! I)
            (bits '00      01) (ldrb-immediate! I)
            (bits '00      1x) (ldrsb-immediate! I)
            (bits '01      00) (strh-immediate! I)
            (bits '01      01) (ldrh-immediate! I)
            (bits '01      1x) (ldrsh-immediate! I)
            (bits '1x      00) (str-immediate! I)
            (bits '1x      01) (ldr-immediate! I)
            (bits '10      10) (ldrsw-immediate! I)
        )
    )
)

;;;; Load/store register (unsigned immediate)
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;  size    1  1  1  0  0  1    opc

(defn load-store-register-unsigned-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
    ]
        (case (cat size    opc)
            (bits '00      00) (strb-immediate! I)
            (bits '00      01) (ldrb-immediate! I)
            (bits '00      1x) (ldrsb-immediate! I)
            (bits '01      00) (strh-immediate! I)
            (bits '01      01) (ldrh-immediate! I)
            (bits '01      1x) (ldrsh-immediate! I)
            (bits '1x      00) (str-immediate! I)
            (bits '1x      01) (ldr-immediate! I)
            (bits '10      10) (ldrsw-immediate! I)
        )
    )
)

;;;; STRB (immediate)
;;
;;; Store Register Byte (immediate) stores the least significant byte of a 32-bit register to memory. The address that is
;;; used for the store is calculated from a base register and an immediate offset.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Unsigned offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    0  0    0              imm9               0  1         Rn               Rt
;;;  size                            opc
;;
;;; "STRB <Wt>, [<Xn|SP>], #<simm>"
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    0  0    0              imm9               1  1         Rn               Rt
;;;  size                            opc
;;
;;; "STRB <Wt>, [<Xn|SP>, #<simm>]!"
;;
;;;; Unsigned offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  1    0  0                  imm12                        Rn               Rt
;;;  size                            opc
;;
;;; "STRB <Wt>, [<Xn|SP>{, #<pimm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.
;;; <pimm>      Is the optional positive immediate byte offset, in the range 0 to 4095, defaulting to 0
;;;             and encoded in the "imm12" field.

(defn strb-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        [#_"boolean" wback?, #_"boolean" postindex?, #_"bits(64)" offset]
            (case 
                Post-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, true, (sign-extend imm9, 64)])
                Pre-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, false, (sign-extend imm9, 64)])
                Unsigned offset (let [#_"bits(12)" imm12 (at I 21 10)] [false, false, (lsl (zero-extend imm12, 64), 0)])
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)

        #_"bits(8)" data (X t)
    ]
        (mem! address, 1, data)

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)

;;;; LDRB (immediate)
;;
;;; Load Register Byte (immediate) loads a byte from memory, zero-extends it, and writes the result to a register. The
;;; address that is used for the load is calculated from a base register and an immediate offset.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Unsigned offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    0  1    0              imm9               0  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDRB <Wt>, [<Xn|SP>], #<simm>"
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    0  1    0              imm9               1  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDRB <Wt>, [<Xn|SP>, #<simm>]!"
;;
;;;; Unsigned offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  1    0  1                  imm12                        Rn               Rt
;;;  size                            opc
;;
;;; "LDRB <Wt>, [<Xn|SP>{, #<pimm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.
;;; <pimm>      Is the optional positive immediate byte offset, in the range 0 to 4095, defaulting to 0
;;;             and encoded in the "imm12" field.

(defn ldrb-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        [#_"boolean" wback?, #_"boolean" postindex?, #_"bits(64)" offset]
            (case 
                Post-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, true, (sign-extend imm9, 64)])
                Pre-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, false, (sign-extend imm9, 64)])
                Unsigned offset (let [#_"bits(12)" imm12 (at I 21 10)] [false, false, (lsl (zero-extend imm12, 64), 0)])
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)

        #_"bits(8)" data (mem address, 1)
    ]
        (X! t (zero-extend data, 32))

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)

;;;; LDRSB (immediate)
;;
;;; Load Register Signed Byte (immediate) loads a byte from memory, sign-extends it to either 32 bits or 64 bits, and
;;; writes the result to a register. The address that is used for the load is calculated from a base register and an
;;; immediate offset.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Unsigned offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    1  x    0              imm9               0  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDRSB <Wt>, [<Xn|SP>], #<simm>" [32-bit (opc == 11)]
;;; "LDRSB <Xt>, [<Xn|SP>], #<simm>" [64-bit (opc == 10)]
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    1  x    0              imm9               1  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDRSB <Wt>, [<Xn|SP>, #<simm>]!" [32-bit (opc == 11)]
;;; "LDRSB <Xt>, [<Xn|SP>, #<simm>]!" [64-bit (opc == 10)]
;;
;;;; Unsigned offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  1    1  x                  imm12                        Rn               Rt
;;;  size                            opc
;;
;;; "LDRSB <Wt>, [<Xn|SP>{, #<pimm>}]" [32-bit (opc == 11)]
;;; "LDRSB <Xt>, [<Xn|SP>{, #<pimm>}]" [64-bit (opc == 10)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.
;;; <pimm>      Is the optional positive immediate byte offset, in the range 0 to 4095, defaulting to 0
;;;             and encoded in the "imm12" field.

(defn ldrsb-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        [#_"boolean" wback?, #_"boolean" postindex?, #_"bits(64)" offset]
            (case 
                Post-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, true, (sign-extend imm9, 64)])
                Pre-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, false, (sign-extend imm9, 64)])
                Unsigned offset (let [#_"bits(12)" imm12 (at I 21 10)] [false, false, (lsl (zero-extend imm12, 64), 0)])
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)
    ]
        (-load-store-register! opc, t, address, 8)

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)

;;;; STRH (immediate)
;;
;;; Store Register Halfword (immediate) stores the least significant halfword of a 32-bit register to memory. The address
;;; that is used for the store is calculated from a base register and an immediate offset.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Unsigned offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    0  0    0              imm9               0  1         Rn               Rt
;;;  size                            opc
;;
;;; "STRH <Wt>, [<Xn|SP>], #<simm>"
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    0  0    0              imm9               1  1         Rn               Rt
;;;  size                            opc
;;
;;; "STRH <Wt>, [<Xn|SP>, #<simm>]!"
;;
;;;; Unsigned offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  1    0  0                  imm12                        Rn               Rt
;;;  size                            opc
;;
;;; "STRH <Wt>, [<Xn|SP>{, #<pimm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.
;;; <pimm>      Is the optional positive immediate byte offset, a multiple of 2 in the range 0 to 8190, defaulting to 0
;;;             and encoded in the "imm12" field as <pimm>/2.

(defn strh-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        [#_"boolean" wback?, #_"boolean" postindex?, #_"bits(64)" offset]
            (case 
                Post-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, true, (sign-extend imm9, 64)])
                Pre-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, false, (sign-extend imm9, 64)])
                Unsigned offset (let [#_"bits(12)" imm12 (at I 21 10)] [false, false, (lsl (zero-extend imm12, 64), 1)])
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)

        #_"bits(16)" data (X t)
    ]
        (mem! address, 2, data)

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)

;;;; LDRH (immediate)
;;
;;; Load Register Halfword (immediate) loads a halfword from memory, zero-extends it, and writes the result to a register.
;;; The address that is used for the load is calculated from a base register and an immediate offset.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Unsigned offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    0  1    0              imm9               0  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDRH <Wt>, [<Xn|SP>], #<simm>"
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    0  1    0              imm9               1  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDRH <Wt>, [<Xn|SP>, #<simm>]!"
;;
;;;; Unsigned offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  1    0  1                  imm12                        Rn               Rt
;;;  size                            opc
;;
;;; "LDRH <Wt>, [<Xn|SP>{, #<pimm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.
;;; <pimm>      Is the optional positive immediate byte offset, a multiple of 2 in the range 0 to 8190, defaulting to 0
;;;             and encoded in the "imm12" field as <pimm>/2.

(defn ldrh-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        [#_"boolean" wback?, #_"boolean" postindex?, #_"bits(64)" offset]
            (case 
                Post-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, true, (sign-extend imm9, 64)])
                Pre-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, false, (sign-extend imm9, 64)])
                Unsigned offset (let [#_"bits(12)" imm12 (at I 21 10)] [false, false, (lsl (zero-extend imm12, 64), 1)])
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)

        #_"bits(16)" data (mem address, 2)
    ]
        (X! t (zero-extend data, 32))

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)

;;;; LDRSH (immediate)
;;
;;; Load Register Signed Halfword (immediate) loads a halfword from memory, sign-extends it to 32 bits or 64 bits, and
;;; writes the result to a register. The address that is used for the load is calculated from a base register and an
;;; immediate offset.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Unsigned offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    1  x    0              imm9               0  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDRSH <Wt>, [<Xn|SP>], #<simm>" [32-bit (opc == 11)]
;;; "LDRSH <Xt>, [<Xn|SP>], #<simm>" [64-bit (opc == 10)]
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    1  x    0              imm9               1  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDRSH <Wt>, [<Xn|SP>, #<simm>]!" [32-bit (opc == 11)]
;;; "LDRSH <Xt>, [<Xn|SP>, #<simm>]!" [64-bit (opc == 10)]
;;
;;;; Unsigned offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  1    1  x                  imm12                        Rn               Rt
;;;  size                            opc
;;
;;; "LDRSH <Wt>, [<Xn|SP>{, #<pimm>}]" [32-bit (opc == 11)]
;;; "LDRSH <Xt>, [<Xn|SP>{, #<pimm>}]" [64-bit (opc == 10)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.
;;; <pimm>      Is the optional positive immediate byte offset, a multiple of 2 in the range 0 to 8190, defaulting to 0
;;;             and encoded in the "imm12" field as <pimm>/2.

(defn ldrsh-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        [#_"boolean" wback?, #_"boolean" postindex?, #_"bits(64)" offset]
            (case 
                Post-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, true, (sign-extend imm9, 64)])
                Pre-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, false, (sign-extend imm9, 64)])
                Unsigned offset (let [#_"bits(12)" imm12 (at I 21 10)] [false, false, (lsl (zero-extend imm12, 64), 1)])
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)
    ]
        (-load-store-register! opc, t, address, 16)

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)

;;;; STR (immediate)
;;
;;; Store Register (immediate) stores a word or a doubleword from a register to memory. The address that is used for the
;;; store is calculated from a base register and an immediate offset.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Unsigned offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    1  1  1    0    0  0    0  0    0              imm9               0  1         Rn               Rt
;;;  size                            opc
;;
;;; "STR <Wt>, [<Xn|SP>], #<simm>" [32-bit (size == 10)]
;;; "STR <Xt>, [<Xn|SP>], #<simm>" [64-bit (size == 11)]
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    1  1  1    0    0  0    0  0    0              imm9               1  1         Rn               Rt
;;;  size                            opc
;;
;;; "STR <Wt>, [<Xn|SP>, #<simm>]!" [32-bit (size == 10)]
;;; "STR <Xt>, [<Xn|SP>, #<simm>]!" [64-bit (size == 11)]
;;
;;;; Unsigned offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    1  1  1    0    0  1    0  0                  imm12                        Rn               Rt
;;;  size                            opc
;;
;;; "STR <Wt>, [<Xn|SP>{, #<pimm>}]" [32-bit (size == 10)]
;;; "STR <Xt>, [<Xn|SP>{, #<pimm>}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.
;;; <pimm>      For the 32-bit variant: is the optional positive immediate byte offset,
;;;             a multiple of 4 in the range 0 to 16380, defaulting to 0 and encoded in the "imm12" field as <pimm>/4.
;;;             For the 64-bit variant: is the optional positive immediate byte offset,
;;;             a multiple of 8 in the range 0 to 32760, defaulting to 0 and encoded in the "imm12" field as <pimm>/8.

(defn str-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" scale (unsigned size)

        [#_"boolean" wback?, #_"boolean" postindex?, #_"bits(64)" offset]
            (case 
                Post-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, true, (sign-extend imm9, 64)])
                Pre-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, false, (sign-extend imm9, 64)])
                Unsigned offset (let [#_"bits(12)" imm12 (at I 21 10)] [false, false, (lsl (zero-extend imm12, 64), scale)])
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" width (<< 8 scale)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)

        #_"bits(width)" data (X t)
    ]
        (mem! address, (quot width 8), data)

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)

;;;; LDR (immediate)
;;
;;; Load Register (immediate) loads a word or doubleword from memory and writes it to a register. The address that is
;;; used for the load is calculated from a base register and an immediate offset. The Unsigned offset variant scales
;;; the immediate offset value by the size of the value accessed before adding it to the base register value.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Unsigned offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    1  1  1    0    0  0    0  1    0              imm9               0  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDR <Wt>, [<Xn|SP>], #<simm>" [32-bit (size == 10)]
;;; "LDR <Xt>, [<Xn|SP>], #<simm>" [64-bit (size == 11)]
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    1  1  1    0    0  0    0  1    0              imm9               1  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDR <Wt>, [<Xn|SP>, #<simm>]!" [32-bit (size == 10)]
;;; "LDR <Xt>, [<Xn|SP>, #<simm>]!" [64-bit (size == 11)]
;;
;;;; Unsigned offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    1  1  1    0    0  1    0  1                  imm12                        Rn               Rt
;;;  size                            opc
;;
;;; "LDR <Wt>, [<Xn|SP>{, #<pimm>}]" [32-bit (size == 10)]
;;; "LDR <Xt>, [<Xn|SP>{, #<pimm>}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.
;;; <pimm>      For the 32-bit variant: is the optional positive immediate byte offset,
;;;             a multiple of 4 in the range 0 to 16380, defaulting to 0 and encoded in the "imm12" field as <pimm>/4.
;;;             For the 64-bit variant: is the optional positive immediate byte offset,
;;;             a multiple of 8 in the range 0 to 32760, defaulting to 0 and encoded in the "imm12" field as <pimm>/8.

(defn ldr-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" scale (unsigned size)

        [#_"boolean" wback?, #_"boolean" postindex?, #_"bits(64)" offset]
            (case 
                Post-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, true, (sign-extend imm9, 64)])
                Pre-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, false, (sign-extend imm9, 64)])
                Unsigned offset (let [#_"bits(12)" imm12 (at I 21 10)] [false, false, (lsl (zero-extend imm12, 64), scale)])
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" regsize (if (= size (bits '11)) 64 32)
        #_"integer" width (<< 8 scale)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)

        #_"bits(width)" data (mem address, (quot width 8))
    ]
        (X! t (zero-extend data, regsize))

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)

;;;; LDRSW (immediate)
;;
;;; Load Register Signed Word (immediate) loads a word from memory, sign-extends it to 64 bits, and writes the result to
;;; a register. The address that is used for the load is calculated from a base register and an immediate offset.
;;
;;; It has encodings from 3 classes: Post-index, Pre-index and Unsigned offset
;;
;;;; Post-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  0    1  1  1    0    0  0    1  0    0              imm9               0  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDRSW <Xt>, [<Xn|SP>], #<simm>"
;;
;;;; Pre-index
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  0    1  1  1    0    0  0    1  0    0              imm9               1  1         Rn               Rt
;;;  size                            opc
;;
;;; "LDRSW <Xt>, [<Xn|SP>, #<simm>]!"
;;
;;;; Unsigned offset
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  0    1  1  1    0    0  1    1  0                  imm12                        Rn               Rt
;;;  size                            opc
;;
;;; "LDRSW <Xt>, [<Xn|SP>{, #<pimm>}]"
;;
;;;; Assembler Symbols
;;
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <simm>      Is the signed immediate byte offset, in the range -256 to 255, encoded in the "imm9" field.
;;; <pimm>      Is the optional positive immediate byte offset, a multiple of 4 in the range 0 to 16380, defaulting to 0
;;;             and encoded in the "imm12" field as <pimm>/4.

(defn ldrsw-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        [#_"boolean" wback?, #_"boolean" postindex?, #_"bits(64)" offset]
            (case 
                Post-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, true, (sign-extend imm9, 64)])
                Pre-index (let [#_"bits(9)" imm9 (at I 20 12)] [true, false, (sign-extend imm9, 64)])
                Unsigned offset (let [#_"bits(12)" imm12 (at I 21 10)] [false, false, (lsl (zero-extend imm12, 64), 2)])
                ?
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        address (if (not postindex?) (+ address offset) address)

        #_"bits(32)" data (mem address, 4)
    ]
        (X! t (sign-extend data, 64))

        (when wback?
            (let [
                address (if postindex? (+ address offset) address)
            ]
                (if (= n 31)
                    (sp! address)
                    (X! n address)
                )
            )
        )
    )
)


;;;; Load/store register (register offset)
;;
;;; 31 30 | 29 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5  4  3  2  1  0
;;;  size    1  1  1  0  0  0    opc     1                                 1  0

(defn load-store-register-register-offset! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
    ]
        (case (cat size    opc)
            (bits '00      00) (strb-register! I)
            (bits '00      01) (ldrb-register! I)
            (bits '00      1x) (ldrsb-register! I)
            (bits '01      00) (strh-register! I)
            (bits '01      01) (ldrh-register! I)
            (bits '01      1x) (ldrsh-register! I)
            (bits '1x      00) (str-register! I)
            (bits '1x      01) (ldr-register! I)
            (bits '10      10) (ldrsw-register! I)
        )
    )
)

;;;; STRB (register)
;;
;;; Store Register Byte (register) calculates an address from a base register value and an offset register value, and stores
;;; a byte from a 32-bit register to the calculated address.
;;; The instruction uses an offset addressing mode, that calculates the address used for the memory access from a base
;;; register value and an offset register value. The offset can be optionally shifted and extended.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    0  0    1         Rm          option     S    1  0         Rn               Rt
;;;  size                            opc
;;
;;;; Extended register (option != 011)
;;
;;; "STRB <Wt>, [<Xn|SP>, (<Wm>|<Xm>), <extend> {<amount>}]"
;;
;;;; Shifted register (option == 011)
;;
;;; "STRB <Wt>, [<Xn|SP>, <Xm>{, LSL <amount>}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        When option<0> is set to 0, is the 32-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <Xm>        When option<0> is set to 1, is the 64-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <extend>    Is the index extend specifier, encoded in “option”:
;;;                option   <extend>
;;;                 010       UXTW
;;;                 110       SXTW
;;;                 111       SXTX
;;; <amount>    Is the index shift amount, it must be #0, encoded in "S" as 0 if omitted, or as 1 if present.

(defn strb-register! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(1)" S (at I 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)
        _
            (when (= (at option 1) (bits '0))       ;; sub-word index
                (throw! "UNDEFINED")
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" m (unsigned Rm)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(64)" offset (extend-reg m, option, 0)
        address (+ address offset)

        #_"bits(8)" data (X t)
    ]
        (mem! address, 1, data)
    )
)

;;;; LDRB (register)
;;
;;; Load Register Byte (register) calculates an address from a base register value and an offset register value, loads a
;;; byte from memory, zero-extends it, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    0  1    1         Rm          option     S    1  0         Rn               Rt
;;;  size                            opc
;;
;;;; Extended register (option != 011)
;;
;;; "LDRB <Wt>, [<Xn|SP>, (<Wm>|<Xm>), <extend> {<amount>}]"
;;
;;;; Shifted register (option == 011)
;;
;;; "LDRB <Wt>, [<Xn|SP>, <Xm>{, LSL <amount>}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        When option<0> is set to 0, is the 32-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <Xm>        When option<0> is set to 1, is the 64-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <extend>    Is the index extend specifier, encoded in “option”:
;;;                option   <extend>
;;;                 010       UXTW
;;;                 110       SXTW
;;;                 111       SXTX
;;; <amount>    Is the index shift amount, it must be #0, encoded in "S" as 0 if omitted, or as 1 if present.

(defn ldrb-register! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(1)" S (at I 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)
        _
            (when (= (at option 1) (bits '0))       ;; sub-word index
                (throw! "UNDEFINED")
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" m (unsigned Rm)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(64)" offset (extend-reg m, option, 0)
        address (+ address offset)

        #_"bits(8)" data (mem address, 1)
    ]
        (X! t (zero-extend data, 32))
    )
)

;;;; LDRSB (register)
;;
;;; Load Register Signed Byte (register) calculates an address from a base register value and an offset register value,
;;; loads a byte from memory, sign-extends it, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  0    1  1  1    0    0  0    1  x    1         Rm          option     S    1  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDRSB <Wt>, [<Xn|SP>, (<Wm>|<Xm>), <extend> {<amount>}]" [32-bit with extended register offset (opc == 11 && option != 011)]
;;
;;; "LDRSB <Wt>, [<Xn|SP>, <Xm>{, LSL <amount>}]" [32-bit with shifted register offset (opc == 11 && option == 011)]
;;; "LDRSB <Xt>, [<Xn|SP>, (<Wm>|<Xm>), <extend> {<amount>}]" [64-bit with extended register offset (opc == 10 && option != 011)]
;;
;;; "LDRSB <Xt>, [<Xn|SP>, <Xm>{, LSL <amount>}]" [64-bit with shifted register offset (opc == 10 && option == 011)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        When option<0> is set to 0, is the 32-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <Xm>        When option<0> is set to 1, is the 64-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <extend>    Is the index extend specifier, encoded in “option”:
;;;                option   <extend>
;;;                 010       UXTW
;;;                 110       SXTW
;;;                 111       SXTX
;;; <amount>    Is the index shift amount, it must be #0, encoded in "S" as 0 if omitted, or as 1 if present.

(defn ldrsb-register! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(1)" S (at I 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)
        _
            (when (= (at option 1) (bits '0))       ;; sub-word index
                (throw! "UNDEFINED")
            )

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" m (unsigned Rm)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(64)" offset (extend-reg m, option, 0)
        address (+ address offset)
    ]
        (-load-store-register! opc, t, address, 8)
    )
)

;;;; STRH (register)
;;
;;; Store Register Halfword (register) calculates an address from a base register value and an offset register value, and
;;; stores a halfword from a 32-bit register to the calculated address.
;;; The instruction uses an offset addressing mode, that calculates the address used for the memory access from a base
;;; register value and an offset register value. The offset can be optionally shifted and extended.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    0  0    1         Rm          option     S    1  0         Rn               Rt
;;;  size                            opc
;;
;;; "STRH <Wt>, [<Xn|SP>, (<Wm>|<Xm>){, <extend> {<amount>}}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        When option<0> is set to 0, is the 32-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <Xm>        When option<0> is set to 1, is the 64-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <extend>    Is the index extend/shift specifier, defaulting to LSL, and which must be omitted for the LSL option
;;;             when <amount> is omitted. It is encoded in “option”:
;;;                option   <extend>
;;;                 010       UXTW
;;;                 011       LSL
;;;                 110       SXTW
;;;                 111       SXTX
;;; <amount>    Is the index shift amount, optional only when <extend> is not LSL. Where it is permitted to be optional,
;;;             it defaults to #0. It is encoded in “S”:
;;;                 S       <amount>
;;;                 0         #0
;;;                 1         #1

(defn strh-register! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(1)" S (at I 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)
        _
            (when (= (at option 1) (bits '0))       ;; sub-word index
                (throw! "UNDEFINED")
            )

        #_"integer" shift (if (= S (bits '1)) 1 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" m (unsigned Rm)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(64)" offset (extend-reg m, option, shift)
        address (+ address offset)

        #_"bits(16)" data (X t)
    ]
        (mem! address, 2, data)
    )
)

;;;; LDRH (register)
;;
;;; Load Register Halfword (register) calculates an address from a base register value and an offset register value, loads a
;;; halfword from memory, zero-extends it, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    0  1    1         Rm          option     S    1  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDRH <Wt>, [<Xn|SP>, (<Wm>|<Xm>){, <extend> {<amount>}}]"
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        When option<0> is set to 0, is the 32-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <Xm>        When option<0> is set to 1, is the 64-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <extend>    Is the index extend/shift specifier, defaulting to LSL, and which must be omitted for the LSL option
;;;             when <amount> is omitted. It is encoded in “option”:
;;;                option   <extend>
;;;                 010       UXTW
;;;                 011       LSL
;;;                 110       SXTW
;;;                 111       SXTX
;;; <amount>    Is the index shift amount, optional only when <extend> is not LSL. Where it is permitted to be optional,
;;;             it defaults to #0. It is encoded in “S”:
;;;                 S       <amount>
;;;                 0         #0
;;;                 1         #1

(defn ldrh-register! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(1)" S (at I 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)
        _
            (when (= (at option 1) (bits '0))       ;; sub-word index
                (throw! "UNDEFINED")
            )

        #_"integer" shift (if (= S (bits '1)) 1 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" m (unsigned Rm)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(64)" offset (extend-reg m, option, shift)
        address (+ address offset)

        #_"bits(16)" data (mem address, 2)
    ]
        (X! t (zero-extend data, 32))
    )
)

;;;; LDRSH (register)
;;
;;; Load Register Signed Halfword (register) calculates an address from a base register value and an offset register value,
;;; loads a halfword from memory, sign-extends it, and writes it to a register.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  0  1    1  1  1    0    0  0    1  x    1         Rm          option     S    1  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDRSH <Wt>, [<Xn|SP>, (<Wm>|<Xm>){, <extend> {<amount>}}]" [32-bit (opc == 11)]
;;; "LDRSH <Xt>, [<Xn|SP>, (<Wm>|<Xm>){, <extend> {<amount>}}]" [64-bit (opc == 10)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        When option<0> is set to 0, is the 32-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <Xm>        When option<0> is set to 1, is the 64-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <extend>    Is the index extend/shift specifier, defaulting to LSL, and which must be omitted for the LSL option
;;;             when <amount> is omitted. It is encoded in “option”:
;;;                option   <extend>
;;;                 010       UXTW
;;;                 011       LSL
;;;                 110       SXTW
;;;                 111       SXTX
;;; <amount>    Is the index shift amount, optional only when <extend> is not LSL. Where it is permitted to be optional,
;;;             it defaults to #0. It is encoded in “S”:
;;;                 S       <amount>
;;;                 0         #0
;;;                 1         #1

(defn ldrsh-register! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(1)" S (at I 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)
        _
            (when (= (at option 1) (bits '0))       ;; sub-word index
                (throw! "UNDEFINED")
            )

        #_"integer" shift (if (= S (bits '1)) 1 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" m (unsigned Rm)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(64)" offset (extend-reg m, option, shift)
        address (+ address offset)
    ]
        (-load-store-register! opc, t, address, 16)
    )
)

;;;; STR (register)
;;
;;; Store Register (register) calculates an address from a base register value and an offset register value, and stores a
;;; 32-bit word or a 64-bit doubleword to the calculated address, from a register.
;;; The instruction uses an offset addressing mode, that calculates the address used for the memory access from a base
;;; register value and an offset register value. The offset can be optionally shifted and extended.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    1  1  1    0    0  0    0  0    1         Rm          option     S    1  0         Rn               Rt
;;;  size                            opc
;;
;;; "STR <Wt>, [<Xn|SP>, (<Wm>|<Xm>){, <extend> {<amount>}}]" [32-bit (size == 10)]
;;; "STR <Xt>, [<Xn|SP>, (<Wm>|<Xm>){, <extend> {<amount>}}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        When option<0> is set to 0, is the 32-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <Xm>        When option<0> is set to 1, is the 64-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <extend>    Is the index extend/shift specifier, defaulting to LSL, and which must be omitted for the LSL option
;;;             when <amount> is omitted. It is encoded in “option”:
;;;                option   <extend>
;;;                 010       UXTW
;;;                 011       LSL
;;;                 110       SXTW
;;;                 111       SXTX
;;; <amount>    For the 32-bit variant: is the index shift amount, optional only when <extend> is not LSL.
;;;             Where it is permitted to be optional, it defaults to #0. It is encoded in “S”:
;;;                 S       <amount>
;;;                 0         #0
;;;                 1         #2
;;;             For the 64-bit variant: is the index shift amount, optional only when <extend> is not LSL.
;;;             Where it is permitted to be optional, it defaults to #0. It is encoded in “S”:
;;;                 S       <amount>
;;;                 0         #0
;;;                 1         #3

(defn str-register! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(1)" S (at I 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" scale (unsigned size)
        _
            (when (= (at option 1) (bits '0))       ;; sub-word index
                (throw! "UNDEFINED")
            )

        #_"integer" shift (if (= S (bits '1)) scale 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" m (unsigned Rm)

        #_"integer" width (<< 8 scale)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(64)" offset (extend-reg m, option, shift)
        address (+ address offset)

        #_"bits(width)" data (X t)
    ]
        (mem! address, (quot width 8), data)
    )
)

;;;; LDR (register)
;;
;;; Load Register (register) calculates an address from a base register value and an offset register value, loads a word
;;; from memory, and writes it to a register. The offset register value can optionally be shifted and extended.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  x    1  1  1    0    0  0    0  1    1         Rm          option     S    1  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDR <Wt>, [<Xn|SP>, (<Wm>|<Xm>){, <extend> {<amount>}}]" [32-bit (size == 10)]
;;; "LDR <Xt>, [<Xn|SP>, (<Wm>|<Xm>){, <extend> {<amount>}}]" [64-bit (size == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wt>        Is the 32-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        When option<0> is set to 0, is the 32-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <Xm>        When option<0> is set to 1, is the 64-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <extend>    Is the index extend/shift specifier, defaulting to LSL, and which must be omitted for the LSL option
;;;             when <amount> is omitted. It is encoded in “option”:
;;;                option   <extend>
;;;                 010       UXTW
;;;                 011       LSL
;;;                 110       SXTW
;;;                 111       SXTX
;;; <amount>    For the 32-bit variant: is the index shift amount, optional only when <extend> is not LSL.
;;;             Where it is permitted to be optional, it defaults to #0. It is encoded in “S”:
;;;                 S       <amount>
;;;                 0         #0
;;;                 1         #2
;;;             For the 64-bit variant: is the index shift amount, optional only when <extend> is not LSL.
;;;             Where it is permitted to be optional, it defaults to #0. It is encoded in “S”:
;;;                 S       <amount>
;;;                 0         #0
;;;                 1         #3

(defn ldr-register! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(1)" S (at I 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)

        #_"integer" scale (unsigned size)
        _
            (when (= (at option 1) (bits '0))       ;; sub-word index
                (throw! "UNDEFINED")
            )

        #_"integer" shift (if (= S (bits '1)) scale 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" m (unsigned Rm)
        #_"integer" regsize (if (= size (bits '11)) 64 32)
        #_"integer" width (<< 8 scale)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(64)" offset (extend-reg m, option, shift)
        address (+ address offset)

        #_"bits(width)" data (mem address, (quot width 8))
    ]
        (X! t (zero-extend data, regsize))
    )
)

;;;; LDRSW (register)
;;
;;; Load Register Signed Word (register) calculates an address from a base register value and an offset register value,
;;; loads a word from memory, sign-extends it to form a 64-bit value, and writes it to a register. The offset register value
;;; can be shifted left by 0 or 2 bits.
;;
;;; 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1  0    1  1  1    0    0  0    1  0    1         Rm          option     S    1  0         Rn               Rt
;;;  size                            opc
;;
;;; "LDRSW <Xt>, [<Xn|SP>, (<Wm>|<Xm>){, <extend> {<amount>}}]"
;;
;;;; Assembler Symbols
;;
;;; <Xt>        Is the 64-bit name of the general-purpose register to be transferred, encoded in the "Rt" field.
;;; <Xn|SP>     Is the 64-bit name of the general-purpose base register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        When option<0> is set to 0, is the 32-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <Xm>        When option<0> is set to 1, is the 64-bit name of the general-purpose index register, encoded in the "Rm" field.
;;; <extend>    Is the index extend/shift specifier, defaulting to LSL, and which must be omitted for the LSL option
;;;             when <amount> is omitted. It is encoded in “option”:
;;;                option   <extend>
;;;                 010       UXTW
;;;                 011       LSL
;;;                 110       SXTW
;;;                 111       SXTX
;;; <amount>    Is the index shift amount, optional only when <extend> is not LSL. Where it is permitted to be optional,
;;;             it defaults to #0. It is encoded in “S”:
;;;                 S       <amount>
;;;                 0         #0
;;;                 1         #2

(defn ldrsw-register! [#_"bits(32)" I]
    (let [
        #_"bits(2)" size (at I 31 30)
        #_"bits(2)" opc (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(1)" S (at I 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rt (at I 4 0)
        _
            (when (= (at option 1) (bits '0))       ;; sub-word index
                (throw! "UNDEFINED")
            )

        #_"integer" shift (if (= S (bits '1)) 2 0)

        #_"integer" n (unsigned Rn)
        #_"integer" t (unsigned Rt)
        #_"integer" m (unsigned Rm)

        #_"bits(64)" address (if (= n 31) (sp) (X n))
        #_"bits(64)" offset (extend-reg m, option, shift)
        address (+ address offset)
        #_"bits(32)" data (mem address, 4)
    ]
        (X! t (sign-extend data, 64))
    )
)


;;;; Data Processing -- Register
;;
;;; 31 | 30 | 29 | 28 | 27 26 25 | 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 9  8  7  6  5  4  3  2  1  0
;;;      op0       op1   1  0  1       op2                               op3

(defn data-processing-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" op0 (at I 30)
        #_"bits(1)" op1 (at I 28)
        #_"bits(4)" op2 (at I 24 21)
        #_"bits(6)" op3 (at I 15 10)
    ]
        (case (cat op0 op1 op2     op3)
            (bits '0   1   0110    xxxxxx) (data-processing-2-source! I)
            (bits '1   1   0110    xxxxxx) (data-processing-1-source! I)
            (bits 'x   0   0xxx    xxxxxx) (logical-shifted-register! I)
            (bits 'x   0   1xx0    xxxxxx) (add-subtract-shifted-register! I)
            (bits 'x   0   1xx1    xxxxxx) (add-subtract-extended-register! I)
            (bits 'x   1   0000    000000) (add-subtract-with-carry! I)
            (bits 'x   1   0010    xxxx0x) (conditional-compare-register! I)
            (bits 'x   1   0010    xxxx1x) (conditional-compare-immediate! I)
            (bits 'x   1   0100    xxxxxx) (conditional-select! I)
            (bits 'x   1   1xxx    xxxxxx) (data-processing-3-source! I)
        )
    )
)


;;;; Data-processing (2 source)
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5  4  3  2  1  0
;;;       0    S    1  1  0  1  0  1  1  0                          opcode

(defn data-processing-2-source! [#_"bits(32)" I]
    (let [
        #_"bits(1)" S (at I 29)
        #_"bits(6)" opcode (at I 15 10)
    ]
        (when (= S (bits '0))
            (case opcode
                (bits '000010) (udiv! I)
                (bits '000011) (sdiv! I)
                (bits '001000) (lslv! I)
                (bits '001001) (lsrv! I)
                (bits '001010) (asrv! I)
                (bits '001011) (rorv! I)
            )
        )
    )
)

;;;; UDIV
;;
;;; Unsigned Divide divides an unsigned integer register value by another unsigned integer register value, and writes the
;;; result to the destination register. The condition flags are not affected.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    1  1  0  1  0  1  1  0         Rm          0  0  0  0  1    0         Rn               Rd
;;;                                                                            o1
;;
;;; "UDIV <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "UDIV <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.

(defn udiv! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(1)" o1 (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        #_"integer" result (if (is-zero? operand2) 0 (quot (unsigned operand1) (unsigned operand2)))
    ]
        (X! d (at result (- width 1) 0))
    )
)

;;;; SDIV
;;
;;; Signed Divide divides a signed integer register value by another signed integer register value, and writes the result to
;;; the destination register. The condition flags are not affected.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    1  1  0  1  0  1  1  0         Rm          0  0  0  0  1    1         Rn               Rd
;;;                                                                            o1
;;
;;; "SDIV <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "SDIV <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.

(defn sdiv! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(1)" o1 (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        #_"integer" result (if (is-zero? operand2) 0 (quot (signed operand1) (signed operand2)))
    ]
        (X! d (at result (- width 1) 0))
    )
)

;;;; LSLV
;;
;;; Logical Shift Left Variable shifts a register value left by a variable number of bits, shifting in zeros, and writes the
;;; result to the destination register. The remainder obtained by dividing the second source register by the data size
;;; defines the number of bits by which the first source register is left-shifted.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    1  1  0  1  0  1  1  0         Rm          0  0  1  0    0  0         Rn               Rd
;;;                                                                          op2
;;
;;; "LSLV <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "LSLV <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register holding a shift amount from 0 to 31 in its bottom 5 bits,
;;;         encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register holding a shift amount from 0 to 63 in its bottom 6 bits,
;;;         encoded in the "Rm" field.

;;;; LSL (register) [alias]
;;
;;; Logical Shift Left (register) shifts a register value left by a variable number of bits, shifting in zeros, and writes the
;;; result to the destination register. The remainder obtained by dividing the second source register by the data size
;;; defines the number of bits by which the first source register is left-shifted.
;;
;;; "LSL <Wd>, <Wn>, <Wm>" === "LSLV <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "LSL <Xd>, <Xn>, <Xm>" === "LSLV <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]

(defn lslv! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(2)" op2 (at I 11 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand2 (X m)
        #_"bits(width)" result (shift-reg n, op2, (MOD (unsigned operand2) width))
    ]
        (X! d result)
    )
)

(def #_"alias" lsl-register! lslv!)

;;;; LSRV
;;
;;; Logical Shift Right Variable shifts a register value right by a variable number of bits, shifting in zeros, and writes the
;;; result to the destination register. The remainder obtained by dividing the second source register by the data size
;;; defines the number of bits by which the first source register is right-shifted.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    1  1  0  1  0  1  1  0         Rm          0  0  1  0    0  1         Rn               Rd
;;;                                                                          op2
;;
;;; "LSRV <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "LSRV <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register holding a shift amount from 0 to 31 in its bottom 5 bits,
;;;         encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register holding a shift amount from 0 to 63 in its bottom 6 bits,
;;;         encoded in the "Rm" field.

;;;; LSR (register) [alias]
;;
;;; Logical Shift Right (register) shifts a register value right by a variable number of bits, shifting in zeros, and writes the
;;; result to the destination register. The remainder obtained by dividing the second source register by the data size
;;; defines the number of bits by which the first source register is right-shifted.
;;
;;; "LSR <Wd>, <Wn>, <Wm>" === "LSRV <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "LSR <Xd>, <Xn>, <Xm>" === "LSRV <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]

(defn lsrv! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(2)" op2 (at I 11 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand2 (X m)
        #_"bits(width)" result (shift-reg n, op2, (MOD (unsigned operand2) width))
    ]
        (X! d result)
    )
)

(def #_"alias" lsr-register! lsrv!)

;;;; ASRV
;;
;;; Arithmetic Shift Right Variable shifts a register value right by a variable number of bits, shifting in copies of its sign
;;; bit, and writes the result to the destination register. The remainder obtained by dividing the second source register by
;;; the data size defines the number of bits by which the first source register is right-shifted.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    1  1  0  1  0  1  1  0         Rm          0  0  1  0    1  0         Rn               Rd
;;;                                                                          op2
;;
;;; "ASRV <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "ASRV <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register holding a shift amount from 0 to 31 in its bottom 5 bits,
;;;         encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register holding a shift amount from 0 to 63 in its bottom 6 bits,
;;;         encoded in the "Rm" field.

;;;; ASR (register) [alias]
;;
;;; Arithmetic Shift Right (register) shifts a register value right by a variable number of bits, shifting in copies of its sign
;;; bit, and writes the result to the destination register. The remainder obtained by dividing the second source register by
;;; the data size defines the number of bits by which the first source register is right-shifted.
;;
;;; "ASR <Wd>, <Wn>, <Wm>" === "ASRV <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "ASR <Xd>, <Xn>, <Xm>" === "ASRV <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]

(defn asrv! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(2)" op2 (at I 11 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand2 (X m)
        #_"bits(width)" result (shift-reg n, op2, (MOD (unsigned operand2) width))
    ]
        (X! d result)
    )
)

(def #_"alias" asr-register! asrv!)

;;;; RORV
;;
;;; Rotate Right Variable provides the value of the contents of a register rotated by a variable number of bits. The bits
;;; that are rotated off the right end are inserted into the vacated bit positions on the left. The remainder obtained by
;;; dividing the second source register by the data size defines the number of bits by which the first source register is
;;; right-shifted.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    1  1  0  1  0  1  1  0         Rm          0  0  1  0    1  1         Rn               Rd
;;;                                                                          op2
;;
;;; "RORV <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "RORV <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register holding a shift amount from 0 to 31 in its bottom 5 bits,
;;;         encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register holding a shift amount from 0 to 63 in its bottom 6 bits,
;;;         encoded in the "Rm" field.

;;;; ROR (register) [alias]
;;
;;; Rotate Right (register) provides the value of the contents of a register rotated by a variable number of bits. The bits
;;; that are rotated off the right end are inserted into the vacated bit positions on the left. The remainder obtained by
;;; dividing the second source register by the data size defines the number of bits by which the first source register is
;;; right-shifted.
;;
;;; "ROR <Wd>, <Wn>, <Wm>" === "RORV <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "ROR <Xd>, <Xn>, <Xm>" === "RORV <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]

(defn rorv! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(2)" op2 (at I 11 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand2 (X m)
        #_"bits(width)" result (shift-reg n, op2, (MOD (unsigned operand2) width))
    ]
        (X! d result)
    )
)

(def #_"alias" ror-register! rorv!)


;;;; Data-processing (1 source)
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5  4  3  2  1  0
;;; sf    1    S    1  1  0  1  0  1  1  0       opcode2            opcode

(defn data-processing-1-source! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" S (at I 29)
        #_"bits(5)" opcode2 (at I 20 16)
        #_"bits(6)" opcode (at I 15 10)
    ]
        (when (and (= S (bits '0)) (= opcode2 (bits '00000)))
            (case (cat sf  opcode)
                (bits 'x   000000) (rbit! I)
                (bits 'x   000001) (rev16! I)
                (bits '0   000010) (rev! I)
                (bits '1   000010) (rev32! I)
                (bits '1   000011) (rev! I)
                (bits 'x   000100) (clz! I)
                (bits 'x   000101) (cls! I)
            )
        )
    )
)

;;;; RBIT
;;
;;; Reverse Bits reverses the bit order in a register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    1  1  0  1  0  1  1  0    0  0  0  0  0    0  0  0  0    0  0         Rn               Rd
;;
;;; "RBIT <Wd>, <Wn>" [32-bit (sf == 0)]
;;; "RBIT <Xd>, <Xn>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.

(defn rbit! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand (X n)
        #_"bits(width)" result 
    ]
        (loop [i 0]
            (when (<= i (- width 1))
                ( ass (at result (- width 1 i)) (at operand i))
                (recur (inc i))
            )
        )

        (X! d result)
    )
)

(defn #_"void" -rev! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 11 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"integer" container_size
            (case opc
                (bits '00) (throw! "UNREACHABLE")
                (bits '01) 16
                (bits '10) 32
                (bits '11) (if (= sf (bits '0)) (throw! "UNDEFINED") 64)
            )

        #_"integer" cs (quot width container_size)
        #_"integer" es (quot container_size 8)

        #_"bits(width)" operand (X n)
        #_"bits(width)" result 
        _
            (loop [#_"integer" i 0 #_"integer" c 0]
                (when (< c cs)
                    (loop [#_"integer" r (+ i (* (- es 1) 8)) #_"integer" e 0]
                        (when (< e es)
                            ( ass (at result (+ r 7) r) (at operand (+ i 7) i))
                            ( ass i (+ i 8))
                            (recur (- r 8) (inc e))
                        )
                    )
                    (recur i (inc c))
                )
            )
    ]
        (X! d result)
    )
)

;;;; REV16
;;
;;; Reverse bytes in 16-bit halfwords reverses the byte order in each 16-bit halfword of a register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    1  1  0  1  0  1  1  0    0  0  0  0  0    0  0  0  0    0  1         Rn               Rd
;;;                                                                          opc
;;
;;; "REV16 <Wd>, <Wn>" [32-bit (sf == 0)]
;;; "REV16 <Xd>, <Xn>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.

(defn rev16! [#_"bits(32)" I]
    (-rev! I)
)

;;;; REV32
;;
;;; Reverse bytes in 32-bit words reverses the byte order in each 32-bit word of a register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1    1    0    1  1  0  1  0  1  1  0    0  0  0  0  0    0  0  0  0    1  0         Rn               Rd
;;; sf                                                                       opc
;;
;;; "REV32 <Xd>, <Xn>"
;;
;;;; Assembler Symbols
;;
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.

(defn rev32! [#_"bits(32)" I]
    (-rev! I)
)

;;;; REV
;;
;;; Reverse Bytes reverses the byte order in a register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    1  1  0  1  0  1  1  0    0  0  0  0  0    0  0  0  0    1  x         Rn               Rd
;;;                                                                          opc
;;
;;; "REV <Wd>, <Wn>" [32-bit (sf == 0 && opc == 10)]
;;; "REV <Xd>, <Xn>" [64-bit (sf == 1 && opc == 11)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.

;;;; REV64 [pseudo]
;;
;;; Reverse Bytes reverses the byte order in a 64-bit general-purpose register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1    1    0    1  1  0  1  0  1  1  0    0  0  0  0  0    0  0  0  0    1  1         Rn               Rd
;;; sf                                                                       opc
;;
;;; "REV64 <Xd>, <Xn>" === "REV <Xd>, <Xn>" [64-bit (sf == 1 && opc == 11)]

(defn rev! [#_"bits(32)" I]
    (-rev! I)
)

(def #_"pseudo" rev64! rev!)

;;;; CLZ
;;
;;; Count Leading Zeros counts the number of binary zero bits before the first binary one bit in the value of the source
;;; register, and writes the result to the destination register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    1  1  0  1  0  1  1  0    0  0  0  0  0    0  0  0  1  0    0         Rn               Rd
;;;                                                                            op
;;
;;; "CLZ <Wd>, <Wn>" [32-bit (sf == 0)]
;;; "CLZ <Xd>, <Xn>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.

(defn clz! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"integer" result (count-leading-zero-bits operand1)
    ]
        (X! d (at result (- width 1) 0))
    )
)

;;;; CLS
;;
;;; Count Leading Sign bits counts the number of leading bits of the source register that have the same value as the most
;;; significant bit of the register, and writes the result to the destination register. This count does not include the most
;;; significant bit of the source register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    1  1  0  1  0  1  1  0    0  0  0  0  0    0  0  0  1  0    1         Rn               Rd
;;;                                                                            op
;;
;;; "CLS <Wd>, <Wn>" [32-bit (sf == 0)]
;;; "CLS <Xd>, <Xn>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the general-purpose source register, encoded in the "Rn" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the general-purpose source register, encoded in the "Rn" field.

(defn cls! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"integer" result (count-leading-sign-bits operand1)
    ]
        (X! d (at result (- width 1) 0))
    )
)


;;;; Logical (shifted register)
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;       opc     0  1  0  1  0            N

(defn logical-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(2)" opc (at I 30 29)
        #_"bits(1)" N (at I 21)
    ]
        (case (cat opc N)
            (bits '000) (and-shifted-register! I)
            (bits '001) (bic-shifted-register! I)
            (bits '010) (orr-shifted-register! I)
            (bits '011) (orn-shifted-register! I)
            (bits '100) (eor-shifted-register! I)
            (bits '101) (eon-shifted-register! I)
            (bits '110) (ands-shifted-register! I)
            (bits '111) (bics-shifted-register! I)
        )
    )
)

;;;; AND (shifted register)
;;
;;; Bitwise AND (shifted register) performs a bitwise AND of a register value and an optionally-shifted register value, and
;;; writes the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  0    0  1  0  1  0   shift    0         Rm                imm6               Rn               Rd
;;;       opc                              N
;;
;;; "AND <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "AND <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift to be applied to the final source, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11      ROR
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field,

(defn and-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(1)" N (at I 21)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at imm6 5) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
    ]
        (X! d (AND operand1 operand2))
    )
)

;;;; BIC (shifted register)
;;
;;; Bitwise Bit Clear (shifted register) performs a bitwise AND of a register value and the complement of an optionally-
;;; shifted register value, and writes the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  0    0  1  0  1  0   shift    1         Rm                imm6               Rn               Rd
;;;       opc                              N
;;
;;; "BIC <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "BIC <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift to be applied to the final source, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11      ROR
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field,

(defn bic-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(1)" N (at I 21)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at imm6 5) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
    ]
        (X! d (AND operand1 (NOT operand2)))
    )
)

;;;; ORR (shifted register)
;;
;;; Bitwise OR (shifted register) performs a bitwise (inclusive) OR of a register value and an optionally-shifted register
;;; value, and writes the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  1    0  1  0  1  0   shift    0         Rm                imm6               Rn               Rd
;;;       opc                              N
;;
;;; "ORR <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "ORR <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift to be applied to the final source, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11      ROR
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field,

;;;; MOV (register) [alias for (shift == '00' && imm6 == '000000' && Rn == '11111')]
;;
;;; Move (register) copies the value in a source register to the destination register.
;;
;;; "MOV <Wd>, <Wm>" === "ORR <Wd>, WZR, <Wm>" [32-bit (sf == 0)]
;;; "MOV <Xd>, <Xm>" === "ORR <Xd>, XZR, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wm>    Is the 32-bit name of the general-purpose source register, encoded in the "Rm" field.
;;; <Xm>    Is the 64-bit name of the general-purpose source register, encoded in the "Rm" field.

(defn orr-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(1)" N (at I 21)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at imm6 5) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
    ]
        (X! d (OR operand1 operand2))
    )
)

(def #_"alias" mov-register! orr-shifted-register!)

;;;; ORN (shifted register)
;;
;;; Bitwise OR NOT (shifted register) performs a bitwise (inclusive) OR of a register value and the complement of an
;;; optionally-shifted register value, and writes the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  1    0  1  0  1  0   shift    1         Rm                imm6               Rn               Rd
;;;       opc                              N
;;
;;; "ORN <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "ORN <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift to be applied to the final source, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11      ROR
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field,

;;;; MVN [alias for (Rn == '11111')]
;;
;;; Bitwise NOT writes the bitwise inverse of a register value to the destination register.
;;
;;; "MVN <Wd>, <Wm>{, <shift> #<amount>}" === "ORN <Wd>, WZR, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "MVN <Xd>, <Xm>{, <shift> #<amount>}" === "ORN <Xd>, XZR, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wm>        Is the 32-bit name of the general-purpose source register, encoded in the "Rm" field.
;;; <Xm>        Is the 64-bit name of the general-purpose source register, encoded in the "Rm" field.

(defn orn-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(1)" N (at I 21)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at imm6 5) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
    ]
        (X! d (OR operand1 (NOT operand2)))
    )
)

(def #_"alias" mvn! orn-shifted-register!)

;;;; EOR (shifted register)
;;
;;; Bitwise Exclusive OR (shifted register) performs a bitwise Exclusive OR of a register value and an optionally-shifted register
;;; value, and writes the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1  0    0  1  0  1  0   shift    0         Rm                imm6               Rn               Rd
;;;       opc                              N
;;
;;; "EOR <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "EOR <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift to be applied to the final source, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11      ROR
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field,

(defn eor-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(1)" N (at I 21)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at imm6 5) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
    ]
        (X! d (EOR operand1 operand2))
    )
)

;;;; EON (shifted register)
;;
;;; Bitwise Exclusive OR NOT (shifted register) performs a bitwise Exclusive OR NOT of a register value and an
;;; optionally-shifted register value, and writes the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1  0    0  1  0  1  0   shift    1         Rm                imm6               Rn               Rd
;;;       opc                              N
;;
;;; "EON <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "EON <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift to be applied to the final source, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11      ROR
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field,

(defn eon-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(1)" N (at I 21)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at imm6 5) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
    ]
        (X! d (EOR operand1 (NOT operand2)))
    )
)

;;;; ANDS (shifted register)
;;
;;; Bitwise AND (shifted register), setting flags, performs a bitwise AND of a register value and an optionally-shifted
;;; register value, and writes the result to the destination register. It updates the condition flags based on the result.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1  1    0  1  0  1  0   shift    0         Rm                imm6               Rn               Rd
;;;       opc                              N
;;
;;; "ANDS <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "ANDS <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift to be applied to the final source, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11      ROR
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field,

;;;; TST (shifted register) [alias for (Rd == '11111')]
;;
;;; Test (shifted register) performs a bitwise AND operation on a register value and an optionally-shifted register value. It
;;; updates the condition flags based on the result, and discards the result.
;;
;;; "TST <Wn>, <Wm>{, <shift> #<amount>}" === "ANDS WZR, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "TST <Xn>, <Xm>{, <shift> #<amount>}" === "ANDS XZR, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]

(defn ands-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(1)" N (at I 21)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at imm6 5) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
        #_"bits(width)" result (AND operand1 operand2)
    ]
        ( ass PSTATE'NZCV (cat (at result (- width 1)) (is-zero-bit? result) (bits '00)))
        (X! d result)
    )
)

(def #_"alias" tst-shifted-register! ands-shifted-register!)

;;;; BICS (shifted register)
;;
;;; Bitwise Bit Clear (shifted register), setting flags, performs a bitwise AND of a register value and the complement of an
;;; optionally-shifted register value, and writes the result to the destination register. It updates the condition flags based
;;; on the result.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1  1    0  1  0  1  0   shift    1         Rm                imm6               Rn               Rd
;;;       opc                              N
;;
;;; "BICS <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "BICS <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift to be applied to the final source, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11      ROR
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field,

(defn bics-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" opc (at I 30 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(1)" N (at I 21)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (when (and (= sf (bits '0)) (= (at imm6 5) (bits '1)))
                (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
        #_"bits(width)" result (AND operand1 (NOT operand2))
    ]
        ( ass PSTATE'NZCV (cat (at result (- width 1)) (is-zero-bit? result) (bits '00)))
        (X! d result)
    )
)


;;;; Add/subtract (shifted register)
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;       N    S    0  1  0  1  1            0

(defn add-subtract-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
    ]
        (case (cat N S)
            (bits '00) (add-shifted-register! I)
            (bits '01) (adds-shifted-register! I)
            (bits '10) (sub-shifted-register! I)
            (bits '11) (subs-shifted-register! I)
        )
    )
)

;;;; ADD (shifted register)
;;
;;; Add (shifted register) adds a register value and an optionally-shifted register value,
;;; and writes the result to the destination register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    0  1  0  1  1   shift    0         Rm                imm6               Rn               Rd
;;;       N    S
;;
;;; "ADD <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "ADD <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift type to be applied to the second source operand, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11    RESERVED
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field.

(defn add-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (cond
                (= shift (bits '11))                             (throw! "UNDEFINED")
                (and (= sf (bits '0)) (= (at imm6 5) (bits '1))) (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
        [#_"bits(width)" result, _] (add-with-carry operand1, operand2, (bits '0))
    ]
        (X! d result)
    )
)

;;;; ADDS (shifted register)
;;
;;; Add (shifted register), setting flags, adds a register value and an optionally-shifted register value, and writes the result
;;; to the destination register. It updates the condition flags based on the result.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    1    0  1  0  1  1   shift    0         Rm                imm6               Rn               Rd
;;;       N    S
;;
;;; "ADDS <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "ADDS <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift type to be applied to the second source operand, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11    RESERVED
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field.

;;;; CMN (shifted register) [alias for (Rd == '11111')]
;;
;;; Compare Negative (shifted register) adds a register value and an optionally-shifted register value.
;;; It updates the condition flags based on the result, and discards the result.
;;
;;; "CMN <Wn>, <Wm>{, <shift> #<amount>}" === "ADDS WZR, <Wn>, <Wm> {, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "CMN <Xn>, <Xm>{, <shift> #<amount>}" === "ADDS XZR, <Xn>, <Xm> {, <shift> #<amount>}" [64-bit (sf == 1)]

(defn adds-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (cond
                (= shift (bits '11))                             (throw! "UNDEFINED")
                (and (= sf (bits '0)) (= (at imm6 5) (bits '1))) (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
        [#_"bits(width)" result, #_"bits(4)" nzcv] (add-with-carry operand1, operand2, (bits '0))
    ]
        ( ass PSTATE'NZCV nzcv)
        (X! d result)
    )
)

(def #_"alias" cmn-shifted-register! adds-shifted-register!)

;;;; SUB (shifted register)
;;
;;; Subtract (shifted register) subtracts an optionally-shifted register value from a register value,
;;; and writes the result to the destination register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    0  1  0  1  1   shift    0         Rm                imm6               Rn               Rd
;;;       N    S
;;
;;; "SUB <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "SUB <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift type to be applied to the second source operand, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11    RESERVED
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field.

;;;; NEG (shifted register) [alias for (Rn == '11111')]
;;
;;; Negate (shifted register) negates an optionally-shifted register value, and writes the result to the destination register.
;;
;;; "NEG <Wd>, <Wm>{, <shift> #<amount>}" === "SUB <Wd>, WZR, <Wm> {, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "NEG <Xd>, <Xm>{, <shift> #<amount>}" === "SUB <Xd>, XZR, <Xm> {, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wm>        Is the 32-bit name of the general-purpose source register, encoded in the "Rm" field.
;;; <Xm>        Is the 64-bit name of the general-purpose source register, encoded in the "Rm" field.

(defn sub-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (cond
                (= shift (bits '11))                             (throw! "UNDEFINED")
                (and (= sf (bits '0)) (= (at imm6 5) (bits '1))) (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
        [#_"bits(width)" result, _] (add-with-carry operand1, (NOT operand2), (bits '1))
    ]
        (X! d result)
    )
)

(def #_"alias" neg-shifted-register! sub-shifted-register!)

;;;; SUBS (shifted register)
;;
;;; Subtract (shifted register), setting flags, subtracts an optionally-shifted register value from a register value, and writes
;;; the result to the destination register. It updates the condition flags based on the result.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    1    0  1  0  1  1   shift    0         Rm                imm6               Rn               Rd
;;;       N    S
;;
;;; "SUBS <Wd>, <Wn>, <Wm>{, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "SUBS <Xd>, <Xn>, <Xm>{, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <shift>     Is the optional shift type to be applied to the second source operand, defaulting to LSL and encoded in “shift”:
;;;                     shift  <shift>
;;;                      00      LSL
;;;                      01      LSR
;;;                      10      ASR
;;;                      11    RESERVED
;;; <amount>    For the 32-bit variant: is the shift amount, in the range 0 to 31, defaulting to 0 and encoded in the "imm6" field.
;;;             For the 64-bit variant: is the shift amount, in the range 0 to 63, defaulting to 0 and encoded in the "imm6" field.

;;;; CMP (shifted register) [alias for (Rd == '11111')]
;;
;;; Compare (shifted register) subtracts an optionally-shifted register value from a register value. It updates the condition
;;; flags based on the result, and discards the result.
;;
;;; "CMP <Wn>, <Wm>{, <shift> #<amount>}" === "SUBS WZR, <Wn>, <Wm> {, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "CMP <Xn>, <Xm>{, <shift> #<amount>}" === "SUBS XZR, <Xn>, <Xm> {, <shift> #<amount>}" [64-bit (sf == 1)]

;;;; NEGS [alias for (Rn == '11111' && Rd != '11111')]
;;
;;; Negate, setting flags, negates an optionally-shifted register value, and writes the result to the destination register.
;;; It updates the condition flags based on the result.
;;
;;; "NEGS <Wd>, <Wm>{, <shift> #<amount>}" === "SUBS <Wd>, WZR, <Wm> {, <shift> #<amount>}" [32-bit (sf == 0)]
;;; "NEGS <Xd>, <Xm>{, <shift> #<amount>}" === "SUBS <Xd>, XZR, <Xm> {, <shift> #<amount>}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wm>        Is the 32-bit name of the general-purpose source register, encoded in the "Rm" field.
;;; <Xm>        Is the 64-bit name of the general-purpose source register, encoded in the "Rm" field.

(defn subs-shifted-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(2)" shift (at I 23 22)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(6)" imm6 (at I 15 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        _
            (cond
                (= shift (bits '11))                             (throw! "UNDEFINED")
                (and (= sf (bits '0)) (= (at imm6 5) (bits '1))) (throw! "UNDEFINED")
            )

        #_"integer" amount (unsigned imm6)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (shift-reg m, shift, amount)
        [#_"bits(width)" result, #_"bits(4)" nzcv] (add-with-carry operand1, (NOT operand2), (bits '1))
    ]
        ( ass PSTATE'NZCV nzcv)
        (X! d result)
    )
)

(def #_"alias" cmp-shifted-register! subs-shifted-register!)

(def #_"alias" negs! subs-shifted-register!)


;;;; Add/subtract (extended register)
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;;       N    S    0  1  0  1  1    opt     1

(defn add-subtract-extended-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(2)" opt (at I 23 22)
    ]
        (when (= opt (bits '00))
            (case (cat N S)
                (bits '00) (add-extended-register! I)
                (bits '01) (adds-extended-register! I)
                (bits '10) (sub-extended-register! I)
                (bits '11) (subs-extended-register! I)
            )
        )
    )
)

;;;; ADD (extended register)
;;
;;; Add (extended register) adds a register value and a sign or zero-extended register value, followed by an optional left
;;; shift amount, and writes the result to the destination register. The argument that is extended from the <Rm> register
;;; can be a byte, halfword, word, or doubleword.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    0  1  0  1  1    0  0    1         Rm          option      imm3           Rn               Rd
;;;       N    S
;;
;;; "ADD <Wd|WSP>, <Wn|WSP>, <Wm>{, <extend> {#<amount>}}" [32-bit (sf == 0)]
;;; "ADD <Xd|SP>, <Xn|SP>, <R><m>{, <extend> {#<amount>}}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd|WSP>    Is the 32-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Wn|WSP>    Is the 32-bit name of the first source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd|SP>     Is the 64-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Xn|SP>     Is the 64-bit name of the first source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <R>         Is a width specifier, encoded in “option”:
;;;                    option   <R>
;;;                     00x      W
;;;                     010      W
;;;                     x11      X
;;;                     10x      W
;;;                     110      W
;;; <m>         Is the number [0-30] of the second general-purpose source register or the name ZR (31), encoded in the "Rm" field.
;;; <extend>    For the 32-bit variant: is the extension to be applied to the second source operand, encoded in “option”:
;;;                    option  <extend>
;;;                     000      UXTB
;;;                     001      UXTH
;;;                     010    LSL|UXTW
;;;                     011      UXTX
;;;                     100      SXTB
;;;                     101      SXTH
;;;                     110      SXTW
;;;                     111      SXTX
;;;             If "Rd" or "Rn" is '11111' (WSP) and "option" is '010' then LSL is preferred, but may be omitted when
;;;             "imm3" is '000'. In all other cases <extend> is required and must be UXTW when "option" is '010'.
;;;             For the 64-bit variant: is the extension to be applied to the second source operand, encoded in “option”:
;;;                    option  <extend>
;;;                     000      UXTB
;;;                     001      UXTH
;;;                     010      UXTW
;;;                     011    LSL|UXTX
;;;                     100      SXTB
;;;                     101      SXTH
;;;                     110      SXTW
;;;                     111      SXTX
;;;             If "Rd" or "Rn" is '11111' (SP) and "option" is '011' then LSL is preferred, but may be omitted when
;;;             "imm3" is '000'. In all other cases <extend> is required and must be UXTX when "option" is '011'.
;;; <amount>    Is the left shift amount to be applied after extension in the range 0 to 4, defaulting to 0, encoded in the
;;;             "imm3" field. It must be absent when <extend> is absent, is required when <extend> is LSL, and is
;;;             optional when <extend> is present but not LSL.

(defn add-extended-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(3)" imm3 (at I 12 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"integer" shift (unsigned imm3)
        _
            (when (< 4 shift)
                (throw! "UNDEFINED")
            )

        #_"bits(width)" operand1 (if (= n 31) (sp) (X n))
        #_"bits(width)" operand2 (extend-reg m, option, shift)
        [#_"bits(width)" result, _] (add-with-carry operand1, operand2, (bits '0))
    ]
        (if (= d 31)
            (sp! result)
            (X! d result)
        )
    )
)

;;;; ADDS (extended register)
;;
;;; Add (extended register), setting flags, adds a register value and a sign or zero-extended register value, followed by an
;;; optional left shift amount, and writes the result to the destination register. The argument that is extended from the
;;; <Rm> register can be a byte, halfword, word, or doubleword. It updates the condition flags based on the result.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    1    0  1  0  1  1    0  0    1         Rm          option      imm3           Rn               Rd
;;;       N    S
;;
;;; "ADDS <Wd>, <Wn|WSP>, <Wm>{, <extend> {#<amount>}}" [32-bit (sf == 0)]
;;; "ADDS <Xd>, <Xn|SP>, <R><m>{, <extend> {#<amount>}}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn|WSP>    Is the 32-bit name of the first source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn|SP>     Is the 64-bit name of the first source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <R>         Is a width specifier, encoded in “option”:
;;;                    option   <R>
;;;                     00x      W
;;;                     010      W
;;;                     x11      X
;;;                     10x      W
;;;                     110      W
;;; <m>         Is the number [0-30] of the second general-purpose source register or the name ZR (31), encoded in the "Rm" field.
;;; <extend>    For the 32-bit variant: is the extension to be applied to the second source operand, encoded in “option”:
;;;                    option  <extend>
;;;                     000      UXTB
;;;                     001      UXTH
;;;                     010    LSL|UXTW
;;;                     011      UXTX
;;;                     100      SXTB
;;;                     101      SXTH
;;;                     110      SXTW
;;;                     111      SXTX
;;;             If "Rn" is '11111' (WSP) and "option" is '010' then LSL is preferred, but may be omitted when "imm3" is
;;;             '000'. In all other cases <extend> is required and must be UXTW when "option" is '010'.
;;;             For the 64-bit variant: is the extension to be applied to the second source operand, encoded in “option”:
;;;                    option  <extend>
;;;                     000      UXTB
;;;                     001      UXTH
;;;                     010      UXTW
;;;                     011    LSL|UXTX
;;;                     100      SXTB
;;;                     101      SXTH
;;;                     110      SXTW
;;;                     111      SXTX
;;;             If "Rn" is '11111' (SP) and "option" is '011' then LSL is preferred, but may be omitted when "imm3" is
;;;             '000'. In all other cases <extend> is required and must be UXTX when "option" is '011'.
;;; <amount>    Is the left shift amount to be applied after extension in the range 0 to 4, defaulting to 0, encoded in the
;;;             "imm3" field. It must be absent when <extend> is absent, is required when <extend> is LSL, and is
;;;             optional when <extend> is present but not LSL.

;;;; CMN (extended register) [alias for (Rd == '11111')]
;;
;;; Compare Negative (extended register) adds a register value and a sign or zero-extended register value, followed by an
;;; optional left shift amount. The argument that is extended from the <Rm> register can be a byte, halfword, word, or
;;; doubleword. It updates the condition flags based on the result, and discards the result.
;;
;;; "CMN <Wn|WSP>, <Wm>{, <extend> {#<amount>}}" === "ADDS WZR, <Wn|WSP>, <Wm>{, <extend> {#<amount>}}" [32-bit (sf == 0)]
;;; "CMN <Xn|SP>, <R><m>{, <extend> {#<amount>}}" === "ADDS XZR, <Xn|SP>, <R><m>{, <extend> {#<amount>}}" [64-bit (sf == 1)]

(defn adds-extended-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(3)" imm3 (at I 12 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"integer" shift (unsigned imm3)
        _
            (when (< 4 shift)
                (throw! "UNDEFINED")
            )

        #_"bits(width)" operand1 (if (= n 31) (sp) (X n))
        #_"bits(width)" operand2 (extend-reg m, option, shift)
        [#_"bits(width)" result, #_"bits(4)" nzcv] (add-with-carry operand1, operand2, (bits '0))
    ]
        ( ass PSTATE'NZCV nzcv)
        (X! d result)
    )
)

(def #_"alias" cmn-extended-register! adds-extended-register!)

;;;; SUB (extended register)
;;
;;; Subtract (extended register) subtracts a sign or zero-extended register value, followed by an optional left shift
;;; amount, from a register value, and writes the result to the destination register. The argument that is extended from
;;; the <Rm> register can be a byte, halfword, word, or doubleword.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    0  1  0  1  1    0  0    1         Rm          option      imm3           Rn               Rd
;;;       N    S
;;
;;; "SUB <Wd|WSP>, <Wn|WSP>, <Wm>{, <extend> {#<amount>}}" [32-bit (sf == 0)]
;;; "SUB <Xd|SP>, <Xn|SP>, <R><m>{, <extend> {#<amount>}}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd|WSP>    Is the 32-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Wn|WSP>    Is the 32-bit name of the first source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd|SP>     Is the 64-bit name of the destination general-purpose register or stack pointer, encoded in the "Rd" field.
;;; <Xn|SP>     Is the 64-bit name of the first source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <R>         Is a width specifier, encoded in “option”:
;;;                    option   <R>
;;;                     00x      W
;;;                     010      W
;;;                     x11      X
;;;                     10x      W
;;;                     110      W
;;; <m>         Is the number [0-30] of the second general-purpose source register or the name ZR (31), encoded in the "Rm" field.
;;; <extend>    For the 32-bit variant: is the extension to be applied to the second source operand, encoded in “option”:
;;;                    option  <extend>
;;;                     000      UXTB
;;;                     001      UXTH
;;;                     010    LSL|UXTW
;;;                     011      UXTX
;;;                     100      SXTB
;;;                     101      SXTH
;;;                     110      SXTW
;;;                     111      SXTX
;;;             If "Rd" or "Rn" is '11111' (WSP) and "option" is '010' then LSL is preferred, but may be omitted when
;;;             "imm3" is '000'. In all other cases <extend> is required and must be UXTW when "option" is '010'.
;;;             For the 64-bit variant: is the extension to be applied to the second source operand, encoded in “option”:
;;;                    option  <extend>
;;;                     000      UXTB
;;;                     001      UXTH
;;;                     010      UXTW
;;;                     011    LSL|UXTX
;;;                     100      SXTB
;;;                     101      SXTH
;;;                     110      SXTW
;;;                     111      SXTX
;;;             If "Rd" or "Rn" is '11111' (SP) and "option" is '011' then LSL is preferred, but may be omitted when
;;;             "imm3" is '000'. In all other cases <extend> is required and must be UXTX when "option" is '011'.
;;; <amount>    Is the left shift amount to be applied after extension in the range 0 to 4, defaulting to 0, encoded in the
;;;             "imm3" field. It must be absent when <extend> is absent, is required when <extend> is LSL, and is optional
;;;             when <extend> is present but not LSL.

(defn sub-extended-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(3)" imm3 (at I 12 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"integer" shift (unsigned imm3)
        _
            (when (< 4 shift)
                (throw! "UNDEFINED")
            )

        #_"bits(width)" operand1 (if (= n 31) (sp) (X n))
        #_"bits(width)" operand2 (extend-reg m, option, shift)
        [#_"bits(width)" result, _] (add-with-carry operand1, (NOT operand2), (bits '1))
    ]
        (if (= d 31)
            (sp! result)
            (X! d result)
        )
    )
)

;;;; SUBS (extended register)
;;
;;; Subtract (extended register), setting flags, subtracts a sign or zero-extended register value, followed by an optional
;;; left shift amount, from a register value, and writes the result to the destination register. The argument that is
;;; extended from the <Rm> register can be a byte, halfword, word, or doubleword. It updates the condition flags based on the result.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    1    0  1  0  1  1    0  0    1         Rm          option      imm3           Rn               Rd
;;;       N    S
;;
;;; "SUBS <Wd>, <Wn|WSP>, <Wm>{, <extend> {#<amount>}}" [32-bit (sf == 0)]
;;; "SUBS <Xd>, <Xn|SP>, <R><m>{, <extend> {#<amount>}}" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>        Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn|WSP>    Is the 32-bit name of the first source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>        Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn|SP>     Is the 64-bit name of the first source general-purpose register or stack pointer, encoded in the "Rn" field.
;;; <R>         Is a width specifier, encoded in “option”:
;;;                    option   <R>
;;;                     00x      W
;;;                     010      W
;;;                     x11      X
;;;                     10x      W
;;;                     110      W
;;; <m>         Is the number [0-30] of the second general-purpose source register or the name ZR (31), encoded in the "Rm" field.
;;; <extend>    For the 32-bit variant: is the extension to be applied to the second source operand, encoded in “option”:
;;;                    option  <extend>
;;;                     000      UXTB
;;;                     001      UXTH
;;;                     010    LSL|UXTW
;;;                     011      UXTX
;;;                     100      SXTB
;;;                     101      SXTH
;;;                     110      SXTW
;;;                     111      SXTX
;;;             If "Rn" is '11111' (WSP) and "option" is '010' then LSL is preferred, but may be omitted when "imm3" is
;;;             '000'. In all other cases <extend> is required and must be UXTW when "option" is '010'.
;;;             For the 64-bit variant: is the extension to be applied to the second source operand, encoded in “option”:
;;;                    option  <extend>
;;;                     000      UXTB
;;;                     001      UXTH
;;;                     010      UXTW
;;;                     011    LSL|UXTX
;;;                     100      SXTB
;;;                     101      SXTH
;;;                     110      SXTW
;;;                     111      SXTX
;;;             If "Rn" is '11111' (SP) and "option" is '011' then LSL is preferred, but may be omitted when "imm3" is
;;;             '000'. In all other cases <extend> is required and must be UXTX when "option" is '011'.
;;; <amount>    Is the left shift amount to be applied after extension in the range 0 to 4, defaulting to 0, encoded in the
;;;             "imm3" field. It must be absent when <extend> is absent, is required when <extend> is LSL, and is optional
;;;             when <extend> is present but not LSL.

;;;; CMP (extended register) [alias for (Rd == '11111')]
;;
;;; Compare (extended register) subtracts a sign or zero-extended register value, followed by an optional left shift
;;; amount, from a register value. The argument that is extended from the <Rm> register can be a byte, halfword, word,
;;; or doubleword. It updates the condition flags based on the result, and discards the result.
;;
;;; "CMP <Wn|WSP>, <Wm>{, <extend> {#<amount>}}" === "SUBS WZR, <Wn|WSP>, <Wm>{, <extend> {#<amount>}}" [32-bit (sf == 0)]
;;; "CMP <Xn|SP>, <R><m>{, <extend> {#<amount>}}" === "SUBS XZR, <Xn|SP>, <R><m>{, <extend> {#<amount>}}" [64-bit (sf == 1)]

(defn subs-extended-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(3)" option (at I 15 13)
        #_"bits(3)" imm3 (at I 12 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"integer" shift (unsigned imm3)
        _
            (when (< 4 shift)
                (throw! "UNDEFINED")
            )

        #_"bits(width)" operand1 (if (= n 31) (sp) (X n))
        #_"bits(width)" operand2 (extend-reg m, option, shift)
        [#_"bits(width)" result, #_"bits(4)" nzcv] (add-with-carry operand1, (NOT operand2), (bits '1))
    ]
        ( ass PSTATE'NZCV nzcv)
        (X! d result)
    )
)

(def #_"alias" cmp-extended-register! subs-extended-register!)


;;;; Add/subtract (with carry)
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5  4  3  2  1  0
;;;       N    S    1  1  0  1  0  0  0  0                     0  0  0  0  0  0

(defn add-subtract-with-carry! [#_"bits(32)" I]
    (let [
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
    ]
        (case (cat N S)
            (bits '00) (adc! I)
            (bits '01) (adcs! I)
            (bits '10) (sbc! I)
            (bits '11) (sbcs! I)
        )
    )
)

;;;; ADC
;;
;;; Add with Carry adds two register values and the Carry flag value, and writes the result to the destination register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    1  1  0  1  0  0  0  0         Rm          0  0  0  0  0  0         Rn               Rd
;;;       N    S
;;
;;; "ADC <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "ADC <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.

(defn adc! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        [#_"bits(width)" result, _] (add-with-carry operand1, operand2, PSTATE'C)
    ]
        (X! d result)
    )
)

;;;; ADCS
;;
;;; Add with Carry, setting flags, adds two register values and the Carry flag value, and writes the result to the destination
;;; register. It updates the condition flags based on the result.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    1    1  1  0  1  0  0  0  0         Rm          0  0  0  0  0  0         Rn               Rd
;;;       N    S
;;
;;; "ADCS <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "ADCS <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.

(defn adcs! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        [#_"bits(width)" result, #_"bits(4)" nzcv] (add-with-carry operand1, operand2, PSTATE'C)
    ]
        ( ass PSTATE'NZCV nzcv)
        (X! d result)
    )
)

;;;; SBC
;;
;;; Subtract with Carry subtracts a register value and the value of NOT (Carry flag) from a register value,
;;; and writes the result to the destination register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    1  1  0  1  0  0  0  0         Rm          0  0  0  0  0  0         Rn               Rd
;;;       N    S
;;
;;; "SBC <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "SBC <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.

;;;; NGC [alias for (Rn == '11111')]
;;
;;; Negate with Carry negates the sum of a register value and the value of NOT (Carry flag),
;;; and writes the result to the destination register.
;;
;;; "NGC <Wd>, <Wm>" === "SBC <Wd>, WZR, <Wm>" [32-bit (sf == 0)]
;;; "NGC <Xd>, <Xm>" === "SBC <Xd>, XZR, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wm>    Is the 32-bit name of the general-purpose source register, encoded in the "Rm" field.
;;; <Xm>    Is the 64-bit name of the general-purpose source register, encoded in the "Rm" field.

(defn sbc! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        [#_"bits(width)" result, _] (add-with-carry operand1, (NOT operand2), PSTATE'C)
    ]
        (X! d result)
    )
)

(def #_"alias" ngc! sbc!)

;;;; SBCS
;;
;;; Subtract with Carry, setting flags, subtracts a register value and the value of NOT (Carry flag) from a register value,
;;; and writes the result to the destination register. It updates the condition flags based on the result.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    1    1  1  0  1  0  0  0  0         Rm          0  0  0  0  0  0         Rn               Rd
;;;       N    S
;;
;;; "SBCS <Wd>, <Wn>, <Wm>" [32-bit (sf == 0)]
;;; "SBCS <Xd>, <Xn>, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.

;;;; NGCS [alias for (Rn == '11111')]
;;
;;; Negate with Carry, setting flags, negates the sum of a register value and the value of NOT (Carry flag), and writes the
;;; result to the destination register. It updates the condition flags based on the result.
;;
;;; "NGCS <Wd>, <Wm>" === "SBCS <Wd>, WZR, <Wm>" [32-bit (sf == 0)]
;;; "NGCS <Xd>, <Xm>" === "SBCS <Xd>, XZR, <Xm>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wm>    Is the 32-bit name of the general-purpose source register, encoded in the "Rm" field.
;;; <Xm>    Is the 64-bit name of the general-purpose source register, encoded in the "Rm" field.

(defn sbcs! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" N (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        [#_"bits(width)" result, #_"bits(4)" nzcv] (add-with-carry operand1, (NOT operand2), PSTATE'C)
    ]
        ( ass PSTATE'NZCV nzcv)
        (X! d result)
    )
)

(def #_"alias" ngcs! sbcs!)


;;;; Conditional compare (register)
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4 |  3  2  1  0
;;;      op    S    1  1  0  1  0  0  1  0                                 0   o2                    o3

(defn conditional-compare-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" op (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(1)" o2 (at I 10)
        #_"bits(1)" o3 (at I 4)
    ]
        (when (and (= S (bits '1)) (= o2 (bits '0)) (= o3 (bits '0)))
            (case op
                (bits '0) (ccmn-register! I)
                (bits '1) (ccmp-register! I)
            )
        )
    )
)

;;;; CCMN (register)
;;
;;; Conditional Compare Negative (register) sets the value of the condition flags to the result of the comparison of a
;;; register value and the inverse of another register value if the condition is true, and an immediate value otherwise.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4 |  3  2  1  0
;;; sf    0    1    1  1  0  1  0  0  1  0         Rm             cond       0    0         Rn          0       nzcv
;;;      op
;;
;;; "CCMN <Wn>, <Wm>, #<nzcv>, <cond>" [32-bit (sf == 0)]
;;; "CCMN <Xn>, <Xm>, #<nzcv>, <cond>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <nzcv>      Is the flag bit specifier, an immediate in the range 0 to 15, giving the alternative state for the 4-bit NZCV
;;;             condition flags, encoded in the "nzcv" field.
;;; <cond>      Is one of the standard conditions, encoded in the "cond" field in the standard way.

(defn ccmn-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 30)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(4)" cond (at I 15 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(4)" nzcv (at I 3 0)

        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"bits(4)" flags nzcv

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
    ]
        (when (condition-holds? cond)
            ( ass [_, flags] (add-with-carry operand1, operand2, (bits '0)))
        )
        ( ass PSTATE'NZCV flags)
    )
)

;;;; CCMP (register)
;;
;;; Conditional Compare (register) sets the value of the condition flags to the result of the comparison of two registers if
;;; the condition is true, and an immediate value otherwise.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4 |  3  2  1  0
;;; sf    1    1    1  1  0  1  0  0  1  0         Rm             cond       0    0         Rn          0       nzcv
;;;      op
;;
;;; "CCMP <Wn>, <Wm>, #<nzcv>, <cond>" [32-bit (sf == 0)]
;;; "CCMP <Xn>, <Xm>, #<nzcv>, <cond>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>        Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>        Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <nzcv>      Is the flag bit specifier, an immediate in the range 0 to 15, giving the alternative state for the 4-bit NZCV
;;;             condition flags, encoded in the "nzcv" field.
;;; <cond>      Is one of the standard conditions, encoded in the "cond" field in the standard way.

(defn ccmp-register! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 30)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(4)" cond (at I 15 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(4)" nzcv (at I 3 0)

        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"bits(4)" flags nzcv

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
    ]
        (when (condition-holds? cond)
            ( ass [_, flags] (add-with-carry operand1, (NOT operand2), (bits '1)))
        )
        ( ass PSTATE'NZCV flags)
    )
)


;;;; Conditional compare (immediate)
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4 |  3  2  1  0
;;;      op    S    1  1  0  1  0  0  1  0                                 1   o2                    o3

(defn conditional-compare-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" op (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(1)" o2 (at I 10)
        #_"bits(1)" o3 (at I 4)
    ]
        (when (and (= S (bits '1)) (= o2 (bits '0)) (= o3 (bits '0)))
            (case op
                (bits '0) (ccmn-immediate! I)
                (bits '1) (ccmp-immediate! I)
            )
        )
    )
)

;;;; CCMN (immediate)
;;
;;; Conditional Compare Negative (immediate) sets the value of the condition flags to the result of the comparison of a
;;; register value and a negated immediate value if the condition is true, and an immediate value otherwise.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5  | 4 |  3  2  1  0
;;; sf    0    1    1  1  0  1  0  0  1  0        imm5            cond       1    0         Rn          0       nzcv
;;;      op
;;
;;; "CCMN <Wn>, #<imm>, #<nzcv>, <cond>" [32-bit (sf == 0)]
;;; "CCMN <Xn>, #<imm>, #<nzcv>, <cond>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <imm>       Is a five bit unsigned (positive) immediate encoded in the "imm5" field.
;;; <nzcv>      Is the flag bit specifier, an immediate in the range 0 to 15, giving the alternative state for the 4-bit NZCV
;;;             condition flags, encoded in the "nzcv" field.
;;; <cond>      Is one of the standard conditions, encoded in the "cond" field in the standard way.

(defn ccmn-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 30)
        #_"bits(5)" imm5 (at I 20 16)
        #_"bits(4)" cond (at I 15 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(4)" nzcv (at I 3 0)

        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"bits(4)" flags nzcv
        #_"bits(width)" imm (zero-extend imm5, width)

        #_"bits(width)" operand1 (X n)
    ]
        (when (condition-holds? cond)
            ( ass [_, flags] (add-with-carry operand1, imm, (bits '0)))
        )
        ( ass PSTATE'NZCV flags)
    )
)

;;;; CCMP (immediate)
;;
;;; Conditional Compare (immediate) sets the value of the condition flags to the result of the comparison of a register
;;; value and an immediate value if the condition is true, and an immediate value otherwise.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4 |  3  2  1  0
;;; sf    1    1    1  1  0  1  0  0  1  0        imm5            cond       1    0         Rn          0       nzcv
;;;      op
;;
;;; "CCMP <Wn>, #<imm>, #<nzcv>, <cond>" [32-bit (sf == 0)]
;;; "CCMP <Xn>, #<imm>, #<nzcv>, <cond>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wn>        Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xn>        Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <imm>       Is a five bit unsigned (positive) immediate encoded in the "imm5" field.
;;; <nzcv>      Is the flag bit specifier, an immediate in the range 0 to 15, giving the alternative state for the 4-bit NZCV
;;;             condition flags, encoded in the "nzcv" field.
;;; <cond>      Is one of the standard conditions, encoded in the "cond" field in the standard way.

(defn ccmp-immediate! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 30)
        #_"bits(5)" imm5 (at I 20 16)
        #_"bits(4)" cond (at I 15 12)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(4)" nzcv (at I 3 0)

        #_"integer" n (unsigned Rn)
        #_"integer" width (if (= sf (bits '1)) 64 32)
        #_"bits(4)" flags nzcv
        #_"bits(width)" imm (zero-extend imm5, width)
    ]
        (when (condition-holds? cond)
            (let [
                #_"bits(width)" operand1 (X n)
                #_"bits(width)" operand2 (NOT imm)
            ]
                ( ass [_, flags] (add-with-carry operand1, operand2, (bits '1)))
            )
        )
        ( ass PSTATE'NZCV flags)
    )
)


;;;; Conditional select
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 15 14 13 12 | 11 10 |  9  8  7  6  5  4  3  2  1  0
;;;      op    S    1  1  0  1  0  1  0  0                                 op2

(defn conditional-select! [#_"bits(32)" I]
    (let [
        #_"bits(1)" op (at I 30)
        #_"bits(1)" S (at I 29)
        #_"bits(2)" op2 (at I 11 10)
    ]
        (when (= S (bits '0))
            (case (cat op op2)
                (bits '000) (csel! I)
                (bits '001) (csinc! I)
                (bits '100) (csinv! I)
                (bits '101) (csneg! I)
            )
        )
    )
)

;;;; CSEL
;;
;;; If the condition is true, Conditional Select writes the value of the first source register to the destination register. If the
;;; condition is false, it writes the value of the second source register to the destination register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    1  1  0  1  0  1  0  0         Rm             cond       0    0         Rn               Rd
;;;      op                                                                      o2
;;
;;; "CSEL <Wd>, <Wn>, <Wm>, <cond>" [32-bit (sf == 0)]
;;; "CSEL <Xd>, <Xn>, <Xm>, <cond>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <cond>  Is one of the standard conditions, encoded in the "cond" field in the standard way.

(defn csel! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 30)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(4)" cond (at I 15 12)
        #_"bits(1)" o2 (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        #_"bits(width)" result (if (condition-holds? cond) operand1 operand2)
    ]
        (X! d result)
    )
)

;;;; CSINC
;;
;;; Conditional Select Increment returns, in the destination register, the value of the first source register if the condition
;;; is true, and otherwise returns the value of the second source register incremented by 1.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0    0    1  1  0  1  0  1  0  0         Rm             cond       0    1         Rn               Rd
;;;      op                                                                      o2
;;
;;; "CSINC <Wd>, <Wn>, <Wm>, <cond>" [32-bit (sf == 0)]
;;; "CSINC <Xd>, <Xn>, <Xm>, <cond>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <cond>  Is one of the standard conditions, encoded in the "cond" field in the standard way.

;;;; CINC [alias for (Rm != '11111' && cond != '111x' && Rn != '11111' && Rn == Rm)]
;;
;;; Conditional Increment returns, in the destination register, the value of the source register incremented by 1 if the
;;; condition is true, and otherwise returns the value of the source register.
;;
;;; "CINC <Wd>, <Wn>, <cond>" === "CSINC <Wd>, <Wn>, <Wn>, invert(<cond>)" [32-bit (sf == 0)]
;;; "CINC <Xd>, <Xn>, <cond>" === "CSINC <Xd>, <Xn>, <Xn>, invert(<cond>)" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wn>    Is the 32-bit name of the general-purpose source register, encoded in the "Rn" and "Rm" fields.
;;; <Xn>    Is the 64-bit name of the general-purpose source register, encoded in the "Rn" and "Rm" fields.
;;; <cond>  Is one of the standard conditions, excluding AL and NV, encoded in the "cond" field with its least significant bit inverted.

;;;; CSET [alias for (Rm == '11111' && cond != '111x' && Rn == '11111')]
;;
;;; Conditional Set sets the destination register to 1 if the condition is true, and otherwise sets it to 0.
;;
;;; "CSET <Wd>, <cond>" === "CSINC <Wd>, WZR, WZR, invert(<cond>)" [32-bit (sf == 0)]
;;; "CSET <Xd>, <cond>" === "CSINC <Xd>, XZR, XZR, invert(<cond>)" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <cond>  Is one of the standard conditions, excluding AL and NV, encoded in the "cond" field with its least significant bit inverted.

(defn csinc! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 30)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(4)" cond (at I 15 12)
        #_"bits(1)" o2 (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        #_"bits(width)" result (if (condition-holds? cond) operand1 (+ operand2 1))
    ]
        (X! d result)
    )
)

(def #_"alias" cinc! csinc!)

(def #_"alias" cset! csinc!)

;;;; CSINV
;;
;;; Conditional Select Invert returns, in the destination register, the value of the first source register if the condition is
;;; true, and otherwise returns the bitwise inversion value of the second source register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    1  1  0  1  0  1  0  0         Rm             cond       0    0         Rn               Rd
;;;      op                                                                      o2
;;
;;; "CSINV <Wd>, <Wn>, <Wm>, <cond>" [32-bit (sf == 0)]
;;; "CSINV <Xd>, <Xn>, <Xm>, <cond>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <cond>  Is one of the standard conditions, encoded in the "cond" field in the standard way.

;;;; CINV [alias for (Rm != '11111' && cond != '111x' && Rn != '11111' && Rn == Rm)]
;;
;;; Conditional Invert returns, in the destination register, the bitwise inversion of the value of the source register if the
;;; condition is true, and otherwise returns the value of the source register.
;;
;;; "CINV <Wd>, <Wn>, <cond>" === "CSINV <Wd>, <Wn>, <Wn>, invert(<cond>)" [32-bit (sf == 0)]
;;; "CINV <Xd>, <Xn>, <cond>" === "CSINV <Xd>, <Xn>, <Xn>, invert(<cond>)" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wn>        Is the 32-bit name of the general-purpose source register, encoded in the "Rn" and "Rm" fields.
;;; <Xn>        Is the 64-bit name of the general-purpose source register, encoded in the "Rn" and "Rm" fields.
;;; <cond>      Is one of the standard conditions, excluding AL and NV, encoded in the "cond" field with its least significant bit inverted.

;;;; CSETM [alias for (Rm == '11111' && cond != '111x' && Rn == '11111')]
;;
;;; Conditional Set Mask sets all bits of the destination register to 1 if the condition is true, and otherwise sets all bits to 0.
;;
;;; "CSETM <Wd>, <cond>" === "CSINV <Wd>, WZR, WZR, invert(<cond>)" [32-bit (sf == 0)]
;;; "CSETM <Xd>, <cond>" === "CSINV <Xd>, XZR, XZR, invert(<cond>)" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <cond>  Is one of the standard conditions, excluding AL and NV, encoded in the "cond" field with its least significant bit inverted.

(defn csinv! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 30)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(4)" cond (at I 15 12)
        #_"bits(1)" o2 (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        #_"bits(width)" result (if (condition-holds? cond) operand1 (NOT operand2))
    ]
        (X! d result)
    )
)

(def #_"alias" cinv! csinv!)

(def #_"alias" csetm! csinv!)

;;;; CSNEG
;;
;;; Conditional Select Negation returns, in the destination register, the value of the first source register if the condition is
;;; true, and otherwise returns the negated value of the second source register.
;;
;;; 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    1    0    1  1  0  1  0  1  0  0         Rm             cond       0    1         Rn               Rd
;;;      op                                                                      o2
;;
;;; "CSNEG <Wd>, <Wn>, <Wm>, <cond>" [32-bit (sf == 0)]
;;; "CSNEG <Xd>, <Xn>, <Xm>, <cond>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register, encoded in the "Rm" field.
;;; <cond>  Is one of the standard conditions, encoded in the "cond" field in the standard way.

;;;; CNEG [alias for (cond != '111x' && Rn == Rm)]
;;
;;; Conditional Negate returns, in the destination register, the negated value of the source register if the condition is
;;; true, and otherwise returns the value of the source register.
;;
;;; "CNEG <Wd>, <Wn>, <cond>" === "CSNEG <Wd>, <Wn>, <Wn>, invert(<cond>)" [32-bit (sf == 0)]
;;; "CNEG <Xd>, <Xn>, <cond>" === "CSNEG <Xd>, <Xn>, <Xn>, invert(<cond>)" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wn>    Is the 32-bit name of the general-purpose source register, encoded in the "Rn" and "Rm" fields.
;;; <Xn>    Is the 64-bit name of the general-purpose source register, encoded in the "Rn" and "Rm" fields.
;;; <cond>  Is one of the standard conditions, excluding AL and NV, encoded in the "cond" field with its least significant bit inverted.

(defn csneg! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(1)" op (at I 30)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(4)" cond (at I 15 12)
        #_"bits(1)" o2 (at I 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" width (if (= sf (bits '1)) 64 32)

        #_"bits(width)" operand1 (X n)
        #_"bits(width)" operand2 (X m)
        #_"bits(width)" result (if (condition-holds? cond) operand1 (+ (NOT operand2) 1))
    ]
        (X! d result)
    )
)

(def #_"alias" cneg! csneg!)


;;;; Data-processing (3 source)
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
;;; sf    op54    1  1  0  1  1     op31                      o0

(defn data-processing-3-source! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(2)" op54 (at I 30 29)
        #_"bits(3)" op31 (at I 23 21)
        #_"bits(1)" o0 (at I 15)
    ]
        (when (= op54 (bits '00))
            (case (cat sf  op31    o0)
                (bits 'x   000     0) (madd! I)
                (bits 'x   000     1) (msub! I)
                (bits '1   001     0) (smaddl! I)
                (bits '1   001     1) (smsubl! I)
                (bits '1   010     0) (smulh! I)
                (bits '1   101     0) (umaddl! I)
                (bits '1   101     1) (umsubl! I)
                (bits '1   110     0) (umulh! I)
            )
        )
    )
)

;;;; MADD
;;
;;; Multiply-Add multiplies two register values, adds a third register value, and writes the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  0    1  1  0  1  1    0  0  0         Rm          0         Ra               Rn               Rd
;;;                                                           o0
;;
;;; "MADD <Wd>, <Wn>, <Wm>, <Wa>" [32-bit (sf == 0)]
;;; "MADD <Xd>, <Xn>, <Xm>, <Xa>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register holding the multiplicand, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register holding the multiplier, encoded in the "Rm" field.
;;; <Wa>    Is the 32-bit name of the third general-purpose source register holding the addend, encoded in the "Ra" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register holding the multiplicand, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register holding the multiplier, encoded in the "Rm" field.
;;; <Xa>    Is the 64-bit name of the third general-purpose source register holding the addend, encoded in the "Ra" field.

;;;; MUL [alias for (Ra == '11111')]
;;
;;; Multiply: Rd = Rn * Rm.
;;
;;; "MUL <Wd>, <Wn>, <Wm>" === "MADD <Wd>, <Wn>, <Wm>, WZR" [32-bit (sf == 0)]
;;; "MUL <Xd>, <Xn>, <Xm>" === "MADD <Xd>, <Xn>, <Xm>, XZR" [64-bit (sf == 1)]

(defn madd! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Ra (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" a (unsigned Ra)
        #_"integer" destsize (if (= sf (bits '1)) 64 32)

        #_"bits(destsize)" operand1 (X n)
        #_"bits(destsize)" operand2 (X m)
        #_"bits(destsize)" operand3 (X a)
        #_"integer" result (+ (unsigned operand3) (* (unsigned operand1) (unsigned operand2)))
    ]
        (X! d (at result (- destsize 1) 0))
    )
)

(def #_"alias" mul! madd!)

;;;; MSUB
;;
;;; Multiply-Subtract multiplies two register values, subtracts the product from a third register value,
;;; and writes the result to the destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 22 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;; sf    0  0    1  1  0  1  1    0  0  0         Rm          1         Ra               Rn               Rd
;;;                                                           o0
;;
;;; "MSUB <Wd>, <Wn>, <Wm>, <Wa>" [32-bit (sf == 0)]
;;; "MSUB <Xd>, <Xn>, <Xm>, <Xa>" [64-bit (sf == 1)]
;;
;;;; Assembler Symbols
;;
;;; <Wd>    Is the 32-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register holding the multiplicand, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register holding the multiplier, encoded in the "Rm" field.
;;; <Wa>    Is the 32-bit name of the third general-purpose source register holding the minuend, encoded in the "Ra" field.
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register holding the multiplicand, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register holding the multiplier, encoded in the "Rm" field.
;;; <Xa>    Is the 64-bit name of the third general-purpose source register holding the minuend, encoded in the "Ra" field.

;;;; MNEG [alias for (Ra == '11111')]
;;
;;; Multiply-Negate multiplies two register values, negates the product, and writes the result to the destination register.
;;
;;; "MNEG <Wd>, <Wn>, <Wm>" === "MSUB <Wd>, <Wn>, <Wm>, WZR" [32-bit (sf == 0)]
;;; "MNEG <Xd>, <Xn>, <Xm>" === "MSUB <Xd>, <Xn>, <Xm>, XZR" [64-bit (sf == 1)]

(defn msub! [#_"bits(32)" I]
    (let [
        #_"bits(1)" sf (at I 31)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Ra (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" a (unsigned Ra)
        #_"integer" destsize (if (= sf (bits '1)) 64 32)

        #_"bits(destsize)" operand1 (X n)
        #_"bits(destsize)" operand2 (X m)
        #_"bits(destsize)" operand3 (X a)
        #_"integer" result (- (unsigned operand3) (* (unsigned operand1) (unsigned operand2)))
    ]
        (X! d (at result (- destsize 1) 0))
    )
)

(def #_"alias" mneg! msub!)

;;;; SMADDL
;;
;;; Signed Multiply-Add Long multiplies two 32-bit register values, adds a 64-bit register value,
;;; and writes the result to the 64-bit destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 | 22 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1    0  0    1  1  0  1  1    0    0  1         Rm          0         Ra               Rn               Rd
;;;                                U                            o0
;;
;;; "SMADDL <Xd>, <Wn>, <Wm>, <Xa>"
;;
;;;; Assembler Symbols
;;
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register holding the multiplicand, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register holding the multiplier, encoded in the "Rm" field.
;;; <Xa>    Is the 64-bit name of the third general-purpose source register holding the addend, encoded in the "Ra" field.

;;;; SMULL [alias for (Ra == '11111')]
;;
;;; Signed Multiply Long multiplies two 32-bit register values, and writes the result to the 64-bit destination register.
;;
;;; "SMULL <Xd>, <Wn>, <Wm>" === "SMADDL <Xd>, <Wn>, <Wm>, XZR"

(defn smaddl! [#_"bits(32)" I]
    (let [
        #_"bits(1)" U (at I 23)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Ra (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" a (unsigned Ra)

        #_"bits(32)" operand1 (X n)
        #_"bits(32)" operand2 (X m)
        #_"bits(64)" operand3 (X a)
        #_"integer" result (+ (signed operand3) (* (signed operand1) (signed operand2)))
    ]
        (X! d (at result 63 0))
    )
)

(def #_"alias" smull! smaddl!)

;;;; SMSUBL
;;
;;; Signed Multiply-Subtract Long multiplies two 32-bit register values, subtracts the product from a 64-bit register value,
;;; and writes the result to the 64-bit destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 | 22 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1    0  0    1  1  0  1  1    0    0  1         Rm          1         Ra               Rn               Rd
;;;                                U                            o0
;;
;;; "SMSUBL <Xd>, <Wn>, <Wm>, <Xa>"
;;
;;;; Assembler Symbols
;;
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register holding the multiplicand, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register holding the multiplier, encoded in the "Rm" field.
;;; <Xa>    Is the 64-bit name of the third general-purpose source register holding the minuend, encoded in the "Ra" field.

;;;; SMNEGL [alias for (Ra == '11111')]
;;
;;; Signed Multiply-Negate Long multiplies two 32-bit register values, negates the product,
;;; and writes the result to the 64-bit destination register.
;;
;;; "SMNEGL <Xd>, <Wn>, <Wm>" === "SMSUBL <Xd>, <Wn>, <Wm>, XZR"

(defn smsubl! [#_"bits(32)" I]
    (let [
        #_"bits(1)" U (at I 23)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Ra (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" a (unsigned Ra)

        #_"bits(32)" operand1 (X n)
        #_"bits(32)" operand2 (X m)
        #_"bits(64)" operand3 (X a)
        #_"integer" result (- (signed operand3) (* (signed operand1) (signed operand2)))
    ]
        (X! d (at result 63 0))
    )
)

(def #_"alias" smnegl! smsubl!)

;;;; SMULH
;;
;;; Signed Multiply High multiplies two 64-bit register values,
;;; and writes bits[127:64] of the 128-bit result to the 64-bit destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 | 22 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1    0  0    1  1  0  1  1    0    1  0         Rm          0 (1) (1) (1) (1) (1)      Rn               Rd
;;;                                U                                       Ra
;;
;;; "SMULH <Xd>, <Xn>, <Xm>"
;;
;;;; Assembler Symbols
;;
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register holding the multiplicand, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register holding the multiplier, encoded in the "Rm" field.

(defn smulh! [#_"bits(32)" I]
    (let [
        #_"bits(1)" U (at I 23)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(5)" Ra (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)

        #_"bits(64)" operand1 (X n)
        #_"bits(64)" operand2 (X m)
        #_"integer" result (* (signed operand1) (signed operand2))
    ]
        (X! d (at result 127 64))
    )
)

;;;; UMADDL
;;
;;; Unsigned Multiply-Add Long multiplies two 32-bit register values, adds a 64-bit register value,
;;; and writes the result to the 64-bit destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 | 22 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1    0  0    1  1  0  1  1    1    0  1         Rm          0         Ra               Rn               Rd
;;;                                U                            o0
;;
;;; "UMADDL <Xd>, <Wn>, <Wm>, <Xa>"
;;
;;;; Assembler Symbols
;;
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register holding the multiplicand, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register holding the multiplier, encoded in the "Rm" field.
;;; <Xa>    Is the 64-bit name of the third general-purpose source register holding the addend, encoded in the "Ra" field.

;;;; UMULL [alias for (Ra == '11111')]
;;
;;; Unsigned Multiply Long multiplies two 32-bit register values, and writes the result to the 64-bit destination register.
;;
;;; "UMULL <Xd>, <Wn>, <Wm>" === "UMADDL <Xd>, <Wn>, <Wm>, XZR"

(defn umaddl! [#_"bits(32)" I]
    (let [
        #_"bits(1)" U (at I 23)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Ra (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" a (unsigned Ra)

        #_"bits(32)" operand1 (X n)
        #_"bits(32)" operand2 (X m)
        #_"bits(64)" operand3 (X a)
        #_"integer" result (+ (unsigned operand3) (* (unsigned operand1) (unsigned operand2)))
    ]
        (X! d (at result 63 0))
    )
)

(def #_"alias" umull! umaddl!)

;;;; UMSUBL
;;
;;; Unsigned Multiply-Subtract Long multiplies two 32-bit register values, subtracts the product from a 64-bit register value,
;;; and writes the result to the 64-bit destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 | 22 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1    0  0    1  1  0  1  1    1    0  1         Rm          1         Ra               Rn               Rd
;;;                                U                            o0
;;
;;; "UMSUBL <Xd>, <Wn>, <Wm>, <Xa>"
;;
;;;; Assembler Symbols
;;
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Wn>    Is the 32-bit name of the first general-purpose source register holding the multiplicand, encoded in the "Rn" field.
;;; <Wm>    Is the 32-bit name of the second general-purpose source register holding the multiplier, encoded in the "Rm" field.
;;; <Xa>    Is the 64-bit name of the third general-purpose source register holding the minuend, encoded in the "Ra" field.

;;;; UMNEGL [alias for (Ra == '11111')]
;;
;;; Unsigned Multiply-Negate Long multiplies two 32-bit register values, negates the product,
;;; and writes the result to the 64-bit destination register.
;;
;;; "UMNEGL <Xd>, <Wn>, <Wm>" === "UMSUBL <Xd>, <Wn>, <Wm>, XZR"

(defn umsubl! [#_"bits(32)" I]
    (let [
        #_"bits(1)" U (at I 23)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(1)" o0 (at I 15)
        #_"bits(5)" Ra (at I 14 10)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)
        #_"integer" a (unsigned Ra)

        #_"bits(32)" operand1 (X n)
        #_"bits(32)" operand2 (X m)
        #_"bits(64)" operand3 (X a)
        #_"integer" result (- (unsigned operand3) (* (unsigned operand1) (unsigned operand2)))
    ]
        (X! d (at result 63 0))
    )
)

(def #_"alias" umnegl! umsubl!)

;;;; UMULH
;;
;;; Unsigned Multiply High multiplies two 64-bit register values,
;;; and writes bits[127:64] of the 128-bit result to the 64-bit destination register.
;;
;;; 31 | 30 29 | 28 27 26 25 24 | 23 | 22 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 |  9  8  7  6  5 |  4  3  2  1  0
;;;  1    0  0    1  1  0  1  1    1    1  0         Rm          0 (1) (1) (1) (1) (1)      Rn               Rd
;;;                                U                                       Ra
;;
;;; "UMULH <Xd>, <Xn>, <Xm>"
;;
;;;; Assembler Symbols
;;
;;; <Xd>    Is the 64-bit name of the general-purpose destination register, encoded in the "Rd" field.
;;; <Xn>    Is the 64-bit name of the first general-purpose source register holding the multiplicand, encoded in the "Rn" field.
;;; <Xm>    Is the 64-bit name of the second general-purpose source register holding the multiplier, encoded in the "Rm" field.

(defn umulh! [#_"bits(32)" I]
    (let [
        #_"bits(1)" U (at I 23)
        #_"bits(5)" Rm (at I 20 16)
        #_"bits(5)" Rn (at I 9 5)
        #_"bits(5)" Rd (at I 4 0)

        #_"integer" d (unsigned Rd)
        #_"integer" n (unsigned Rn)
        #_"integer" m (unsigned Rm)

        #_"bits(64)" operand1 (X n)
        #_"bits(64)" operand2 (X m)
        #_"integer" result (* (unsigned operand1) (unsigned operand2))
    ]
        (X! d (at result 127 64))
    )
)

