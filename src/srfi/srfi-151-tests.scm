(library (srfi srfi-151-tests)

  (export
   test-001 test-002 test-003 test-004 test-005 test-006 test-007 test-008 test-009
   test-010 test-011 test-012 test-013 test-014 test-015 test-016 test-017 test-018 test-019
   test-020 test-021 test-022 test-023 test-024 test-025 test-026 test-027 test-028 test-029
   test-030 test-031 test-032 test-033 test-034 test-035 test-036 test-037 test-038 test-039
   test-040 test-041 test-042 test-043 test-044 test-045 test-046 test-047 test-048 test-049
   test-050 test-051 test-052 test-053 test-054 test-055 test-056 test-057 test-058 test-059
   test-060 test-061 test-062 test-063 test-064 test-065 test-066 test-067 test-068 test-069
   test-070 test-071 test-072 test-073 test-074 test-075 test-076 test-077 test-078 test-079
   test-080 test-081 test-082 test-083 test-084 test-085 test-086 test-087 test-088 test-089
   test-090 test-091 test-092 test-093 test-094 test-095 test-096 test-097 test-098 test-099


   test-100 test-101 test-102 test-103 test-104 test-105 test-106 test-107 test-108 test-109
   test-110 test-111 test-112 test-113 test-114 test-115 test-116 test-117 test-118 test-119
   test-120 test-121 test-122 test-123 test-124 test-125 test-126 test-127 test-128 test-129
   test-130 test-131 test-132 test-133 test-134 test-135 test-136 test-137 test-138 test-139
   test-140 test-141 test-142 test-143 test-144 test-145 test-146 test-147 test-148 test-149
   test-150 test-151 test-152 test-153 test-154 test-155 test-156 test-157 test-158 test-159
   test-160 test-161 test-162 test-163 test-164 test-165 test-166 test-167 test-168 test-169
   test-170 test-171 test-172 test-173 test-174 test-175 test-176 test-177 test-178 test-179
   test-180 test-181 test-182 test-183 test-184 test-185 test-186 test-187 #;test-188 test-189
   test-190 test-191 test-192 #;test-193 test-194 test-195 test-196 test-197 test-198 test-199

   test-200 test-201 test-202 test-203 test-204 test-205 test-206 test-207 test-208 test-209
   test-210 test-211 test-212 test-213 test-214 test-215 test-216 test-217 test-218 test-219
   test-220 test-221 test-222 test-223 test-224 test-225 test-226 test-227 test-228 test-229
   test-230 test-231 test-232 test-233 test-234 test-235 test-236 test-237 test-238 test-239
   test-240 test-241 test-242 test-243)

  (import (scheme base)
          (tests)
          (srfi srfi-151))

  (define test-001
    (test -1 (bitwise-not 0)))

  (define test-002
    (test 0 (bitwise-not -1)))

  (define test-003
    (test -11 (bitwise-not 10)))

  (define test-004
    (test 36 (bitwise-not -37)))

  (define test-005
    (test 0 (bitwise-and #b0 #b1)))

  (define test-006
    (test 1680869008 (bitwise-and -193073517 1689392892)))

  (define test-007
    (test 3769478 (bitwise-and 1694076839 -4290775858)))

  (define test-008
    (test 6 (bitwise-and 14 6)))

  (define test-009
    (test 10 (bitwise-and 11 26)))

  (define test-010
    (test 4 (bitwise-and 37 12)))

  (define test-011
    (test 1 (bitwise-and #b1 #b1)))

  (define test-012
    (test 0 (bitwise-and #b1 #b10)))

  (define test-013
    (test #b10 (bitwise-and #b11 #b10)))

  (define test-014
    (test #b101 (bitwise-and #b101 #b111)))

  (define test-015
    (test #b111 (bitwise-and -1 #b111)))

  (define test-016
    (test #b110 (bitwise-and -2 #b111)))

  (define test-017
    (test 3769478 (bitwise-and -4290775858 1694076839)))

  (define test-018
    (test -4294967295 (bitwise-ior 1 (- -1 #xffffffff))))

  (define test-019
    (test -18446744073709551615 (bitwise-ior 1 (- -1 #xffffffffffffffff))))

  (define test-020
    (test 14 (bitwise-ior 10 12)))

  (define test-021
    (test 11 (bitwise-ior 3  10)))

  (define test-022
    (test -4294967126 (bitwise-xor #b10101010 (- -1 #xffffffff))))

  (define test-023
    (test -18446744073709551446 (bitwise-xor #b10101010 (- -1 #xffffffffffffffff))))

  (define test-024
    (test -2600468497 (bitwise-ior 1694076839 -4290775858)))

  (define test-025
    (test -184549633 (bitwise-ior -193073517 1689392892)))

  (define test-026
    (test -2604237975 (bitwise-xor 1694076839 -4290775858)))

  (define test-027
    (test -1865418641 (bitwise-xor -193073517 1689392892)))

  (define test-028
    (test 6 (bitwise-xor 10 12)))

  (define test-029
    (test 9 (bitwise-xor 3 10)))

  (define test-030
    (test (bitwise-not -4294967126) (bitwise-eqv #b10101010 (- -1 #xffffffff))))

  (define test-031
    (test -42 (bitwise-eqv 37 12)))

  (define test-032
    (test -1 (bitwise-nand 0 0)))

  (define test-033
    (test -1 (bitwise-nand 0 -1)))

  (define test-034
    (test -124 (bitwise-nand -1 123)))

  (define test-035
    (test -11 (bitwise-nand 11 26)))

  (define test-036
    (test -28 (bitwise-nor  11 26)))

  (define test-037
    (test 0 (bitwise-nor -1 123)))

  (define test-038
    (test 16 (bitwise-andc1 11 26)))

  (define test-039
    (test 1 (bitwise-andc2 11 26)))

  (define test-040
    (test -2 (bitwise-orc1 11 26)))

  (define test-041
    (test -1 (bitwise-nor 0 0)))

  (define test-042
    (test 0 (bitwise-nor 0 -1)))

  (define test-043
    (test 0 (bitwise-andc1 0 0)))

  (define test-044
    (test -1 (bitwise-andc1 0 -1)))

  (define test-045
    (test 123 (bitwise-andc1 0 123)))

  (define test-046
    (test 0 (bitwise-andc2 0 0)))

  (define test-047
    (test -1 (bitwise-andc2 -1 0)))

  (define test-048
    (test -1 (bitwise-orc1 0 0)))

  (define test-049
    (test -1 (bitwise-orc1 0 -1)))

  (define test-050
    (test 0 (bitwise-orc1 -1 0)))

  (define test-051
    (test -124 (bitwise-orc1 123 0)))

  (define test-052
    (test -1 (bitwise-orc2 0 0)))

  (define test-053
    (test -1 (bitwise-orc2 -1 0)))

  (define test-054
    (test 0 (bitwise-orc2 0 -1)))

  (define test-055
    (test -124 (bitwise-orc2 0 123)))

  ;; bitwise/integer

  (define test-056
    (test #x1000000000000000100000000000000000000000000000000
          (arithmetic-shift #x100000000000000010000000000000000 64)))

  (define test-057
    (test #x8e73b0f7da0e6452c810f32b809079e5
          (arithmetic-shift #x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b -64)))

  (define test-058
    (test 2 (arithmetic-shift 1 1)))

  (define test-059
    (test 0 (arithmetic-shift 1 -1)))

  (define test-060
    (test 1 (arithmetic-shift 1 0)))

  (define test-061
    (test 4 (arithmetic-shift 1 2)))

  (define test-062
    (test 8 (arithmetic-shift 1 3)))

  (define test-063
    (test 16 (arithmetic-shift 1 4)))

  (define test-064
    (test (expt 2 31) (arithmetic-shift 1 31)))

  (define test-065
    (test (expt 2 32) (arithmetic-shift 1 32)))

  (define test-066
    (test (expt 2 33) (arithmetic-shift 1 33)))

  (define test-067
    (test (expt 2 63) (arithmetic-shift 1 63)))

  (define test-068
    (test (expt 2 64) (arithmetic-shift 1 64)))

  (define test-069
    (test (expt 2 65) (arithmetic-shift 1 65)))

  (define test-070
    (test (expt 2 127) (arithmetic-shift 1 127)))

  (define test-071
    (test (expt 2 128) (arithmetic-shift 1 128)))

  (define test-072
    (test (expt 2 129) (arithmetic-shift 1 129)))

  (define test-073
    (test 3028397001194014464 (arithmetic-shift 11829675785914119 8)))

  (define test-074
    (test -1 (arithmetic-shift -1 0)))

  (define test-075
    (test -2 (arithmetic-shift -1 1)))

  (define test-076
    (test -4 (arithmetic-shift -1 2)))

  (define test-077
    (test -8 (arithmetic-shift -1 3)))

  (define test-078
    (test -16 (arithmetic-shift -1 4)))

  (define test-079
    (test (- (expt 2 31)) (arithmetic-shift -1 31)))

  (define test-080
    (test (- (expt 2 32)) (arithmetic-shift -1 32)))

  (define test-081
    (test (- (expt 2 33)) (arithmetic-shift -1 33)))

  (define test-082
    (test (- (expt 2 63)) (arithmetic-shift -1 63)))

  (define test-083
    (test (- (expt 2 64)) (arithmetic-shift -1 64)))

  (define test-084
    (test (- (expt 2 65)) (arithmetic-shift -1 65)))

  (define test-085
    (test (- (expt 2 127)) (arithmetic-shift -1 127)))

  (define test-086
    (test (- (expt 2 128)) (arithmetic-shift -1 128)))

  (define test-087
    (test (- (expt 2 129)) (arithmetic-shift -1 129)))

  (define test-088
    (test 0 (arithmetic-shift 1 -63)))

  (define test-089
    (test 0 (arithmetic-shift 1 -64)))

  (define test-090
    (test 0 (arithmetic-shift 1 -65)))

  (define test-091
    (test 32 (arithmetic-shift 8 2)))

  (define test-092
    (test 4 (arithmetic-shift 4 0)))

  (define test-093
    (test 4 (arithmetic-shift 8 -1)))

  (define test-094
    (test -79 (arithmetic-shift -100000000000000000000000000000000 -100)))

  (define test-095
    (test 2 (bit-count 12)))

  (define test-096
    (test 0 (integer-length  0)))

  (define test-097
    (test 1 (integer-length  1)))

  (define test-098
    (test 0 (integer-length -1)))

  (define test-099
    (test 3 (integer-length  7)))

  (define test-100
    (test 3 (integer-length -7)))

  (define test-101
    (test 4 (integer-length  8)))

  (define test-102
    (test 3 (integer-length -8)))

  (define test-103
    (test 9 (bitwise-if 3 1 8)))

  (define test-104
    (test 0 (bitwise-if 3 8 1)))

  (define test-105
    (test 3 (bitwise-if 1 1 2)))

  (define test-106
    (test #b00110011 (bitwise-if #b00111100 #b11110000 #b00001111)))

  ;; bitwise/single

  (define test-107
    (test #t (bit-set? 0 1)))

  (define test-108
    (test #f (bit-set? 1 1)))

  (define test-109
    (test #f (bit-set? 1 8)))

  (define test-110
    (test #t (bit-set? 10000 -1)))

  (define test-111
    (test #t (bit-set? 1000 -1)))

  (define test-112
    (test #t (bit-set? 64 #x10000000000000000)))

  (define test-113
    (test #f (bit-set? 64 1)))

  (define test-114
    (test #t (bit-set? 3 10)))

  (define test-115
    (test #t (bit-set? 2 6)))

  (define test-116
    (test #f (bit-set? 0 6)))

  (define test-117
    (test 0 (copy-bit 0 0 #f)))

  (define test-118
    (test 0 (copy-bit 30 0 #f)))

  (define test-119
    (test 0 (copy-bit 31 0 #f)))

  (define test-120
    (test 0 (copy-bit 62 0 #f)))

  (define test-121
    (test 0 (copy-bit 63 0 #f)))

  (define test-122
    (test 0 (copy-bit 128 0 #f)))

  (define test-123
    (test -1 (copy-bit 0 -1 #t)))

  (define test-124
    (test -1 (copy-bit 30 -1 #t)))

  (define test-125
    (test -1 (copy-bit 31 -1 #t)))

  (define test-126
    (test -1 (copy-bit 62 -1 #t)))

  (define test-127
    (test -1 (copy-bit 63 -1 #t)))

  (define test-128
    (test -1 (copy-bit 128 -1 #t)))

  (define test-129
    (test 1 (copy-bit 0 0 #t)))

  (define test-130
    (test #x106 (copy-bit 8 6 #t)))

  (define test-131
    (test 6 (copy-bit 8 6 #f)))

  (define test-132
    (test -2 (copy-bit 0 -1 #f)))

  (define test-133
    (test 0 (copy-bit 128 #x100000000000000000000000000000000 #f)))

  (define test-134
    (test #x100000000000000000000000000000000
	  (copy-bit 128 #x100000000000000000000000000000000 #t)))

  (define test-135
    (test #x100000000000000000000000000000000
	  (copy-bit 64 #x100000000000000000000000000000000 #f)))

  (define test-136
    (test #x-100000000000000000000000000000000
	  (copy-bit 64 #x-100000000000000000000000000000000 #f)))

  (define test-137
    (test #x-100000000000000000000000000000000
	  (copy-bit 256 #x-100000000000000000000000000000000 #t)))

  (define test-138
    (test #b100 (copy-bit 2 0 #t)))

  (define test-139
    (test #b1011 (copy-bit 2 #b1111 #f)))

  (define test-140
    (test #b1 (copy-bit 0 0 #t)))

  (define test-141
    (test #b1011 (bit-swap 1 2 #b1101)))

  (define test-142
    (test #b1011 (bit-swap 2 1 #b1101)))

  (define test-143
    (test #b1110 (bit-swap 0 1 #b1101)))

  (define test-144
    (test #b10000000101 (bit-swap 3 10 #b1101)))

  (define test-145
    (test 1 (bit-swap 0 2 4)))

  (define test-146
    (test #t (any-bit-set? 3 6)))

  (define test-147
    (test #f (any-bit-set? 3 12)))

  (define test-148
    (test #t (every-bit-set? 4 6)))

  (define test-149
    (test #f (every-bit-set? 7 6)))

  (define test-150
    (test -1 (first-set-bit 0)))

  (define test-151
    (test 0 (first-set-bit 1)))

  (define test-152
    (test 0 (first-set-bit 3)))

  (define test-153
    (test 2 (first-set-bit 4)))

  (define test-154
    (test 1 (first-set-bit 6)))

  (define test-155
    (test 0 (first-set-bit -1)))

  (define test-156
    (test 1 (first-set-bit -2)))

  (define test-157
    (test 0 (first-set-bit -3)))

  (define test-158
    (test 2 (first-set-bit -4)))

  (define test-159
    (test 128 (first-set-bit #x100000000000000000000000000000000)))

  (define test-160
    (test 1 (first-set-bit 2)))

  (define test-161
    (test 3 (first-set-bit 40)))

  (define test-162
    (test 2 (first-set-bit -28)))

  (define test-163
    (test 99 (first-set-bit (expt  2 99))))

  (define test-164
    (test 99 (first-set-bit (expt -2 99))))

  ;; bitwise/field

  (define test-165
    (test 0 (bit-field 6 0 1)))

  (define test-166
    (test 3 (bit-field 6 1 3)))

  (define test-167
    (test 1 (bit-field 6 2 999)))

  (define test-168
    (test 1 (bit-field #x100000000000000000000000000000000 128 129)))

  (define test-169
    (test #b1010 (bit-field #b1101101010 0 4)))

  (define test-170
    (test #b101101 (bit-field #b1101101010 3 9)))

  (define test-171
    (test #b10110 (bit-field #b1101101010 4 9)))

  (define test-172
    (test #b110110 (bit-field #b1101101010 4 10)))

  (define test-173
    (test #t (bit-field-any? #b101101 0 2)))

  (define test-174
    (test #t (bit-field-any? #b101101 2 4)))

  (define test-175
    (test #f (bit-field-any? #b101101 1 2)))

  (define test-176
    (test #f (bit-field-every? #b101101 0 2)))

  (define test-177
    (test #t (bit-field-every? #b101101 2 4)))

  (define test-178
    (test #t (bit-field-every? #b101101 0 1)))

  (define test-179
    (test #b100000 (bit-field-clear #b101010 1 4)))

  (define test-180
    (test #b101110 (bit-field-set #b101010 1 4)))

  (define test-181
    (test #b111 (bit-field-replace #b110 1 0 1)))

  (define test-182
    (test #b110 (bit-field-replace #b110 1 1 2)))

  (define test-183
    (test #b010 (bit-field-replace #b110 1 1 3)))

  (define test-184
    (test #b100100 (bit-field-replace #b101010 #b010 1 4)))

  (define test-185
    (test #b1001 (bit-field-replace-same #b1111 #b0000 1 3)))

  (define test-186
    (test #b110  (bit-field-rotate #b110 1 1 2)))

  (define test-187
    (test #b1010 (bit-field-rotate #b110 1 2 4)))

  ;; TODO: FIXME: negative rotation is invalid according to chez
  ;; (define test-188
  ;;   (test #b1011 (bit-field-rotate #b0111 -1 1 4)))

  (define test-189
    (test #b0  (bit-field-rotate #b0 128 0 256)))

  (define test-190
    (test #b1  (bit-field-rotate #b1 128 1 256)))

  (define test-191
    (test #x100000000000000000000000000000000
	  (bit-field-rotate #x100000000000000000000000000000000 128 0 64)))

  (define test-192
    (test #x100000000000000000000000000000008
	  (bit-field-rotate #x100000000000000000000000000000001 3 0 64)))

  ;; TODO: FIXME: negative rotation is invalid according to chez
  ;; (define test-193
  ;;  (test #x100000000000000002000000000000000
  ;;        (bit-field-rotate #x100000000000000000000000000000001 -3 0 64)))

  (define test-194
    (test #b110 (bit-field-rotate #b110 0 0 10)))

  (define test-195
    (test #b110 (bit-field-rotate #b110 0 0 256)))

  (define test-196
    (test 1 (bit-field-rotate #x100000000000000000000000000000000 1 0 129)))

  (define test-197
    (test 6 (bit-field-reverse 6 1 3)))

  (define test-198
    (test 12 (bit-field-reverse 6 1 4)))

  (define test-199
    (test #x80000000 (bit-field-reverse 1 0 32)))

  (define test-200
    (test #x40000000 (bit-field-reverse 1 0 31)))

  (define test-201
    (test #x20000000 (bit-field-reverse 1 0 30)))

  (define test-202
    (test (bitwise-ior (arithmetic-shift -1 32) #xFBFFFFFF)
	  (bit-field-reverse -2 0 27)))

  (define test-203
    (test (bitwise-ior (arithmetic-shift -1 32) #xF7FFFFFF)
	  (bit-field-reverse -2 0 28)))

  (define test-204
    (test (bitwise-ior (arithmetic-shift -1 32) #xEFFFFFFF)
	  (bit-field-reverse -2 0 29)))

  (define test-205
    (test (bitwise-ior (arithmetic-shift -1 32) #xDFFFFFFF)
	  (bit-field-reverse -2 0 30)))

  (define test-206
    (test (bitwise-ior (arithmetic-shift -1 32) #xBFFFFFFF)
	  (bit-field-reverse -2 0 31)))

  (define test-207
    (test (bitwise-ior (arithmetic-shift -1 32) #x7FFFFFFF)
	  (bit-field-reverse -2 0 32)))

  (define test-208
    (test 5 (bit-field-reverse #x140000000000000000000000000000000 0 129)))

  ;; bitwise/conversion

  (define test-209
    (test '(#t #f #t #f #t #t #t) (bits->list #b1110101)))

  (define test-210
    (test '(#f #t #f #t) (bits->list #b111010 4)))

  (define test-211
    (test #b1110101 (list->bits '(#t #f #t #f #t #t #t))))

  (define test-212
    (test #b111010100 (list->bits '(#f #f #t #f #t #f #t #t #t))))

  (define test-213
    (test '(#t #t) (bits->list 3)))

  (define test-214
    (test '(#f #t #t #f) (bits->list 6 4)))

  (define test-215
    (test '(#f #t) (bits->list 6 2)))

  (define test-216
    (test '(#t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
	  (bits->list 1 128)))

  (define test-217
    (test '(#f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t)
	  (bits->list #x100000000000000000000000000000000)))

  (define test-218
    (test 6 (list->bits '(#f #t #t))))

  (define test-219
    (test 12 (list->bits '(#f #f #t #t))))

  (define test-220
    (test 6 (list->bits '(#f #t #t #f))))

  (define test-221
    (test 2 (list->bits '(#f #t))))

  (define test-222
    (test 1 (list->bits
	     '(#t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))))

  (define test-223
    (test #x100000000000000000000000000000000
	  (list->bits
	   '(#f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t))))

  (define test-224
    (test #x03FFFFFF (list->bits '(#t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define test-225
    (test #x07FFFFFF (list->bits '(#t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define test-226
    (test #x0FFFFFFF (list->bits '(#t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define test-227
    (test #x1FFFFFFF (list->bits '(#t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define test-228
    (test #x3FFFFFFF (list->bits '(#t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define test-229
    (test #x7FFFFFFF (list->bits '(#t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))
  (define test-230
    (test #xFFFFFFFF (list->bits '(#t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define test-231
    (test #x1FFFFFFFF (list->bits '(#t
				    #t #t #t #t #t #t #t #t
				    #t #t #t #t #t #t #t #t
				    #t #t #t #t #t #t #t #t
				    #t #t #t #t #t #t #t #t))))

  (define test-232
    (test 1 (list->bits '(#t #f))))

  (define test-233
    (test #b1110101 (vector->bits '#(#t #f #t #f #t #t #t))))

  (define test-234
    (test #b00011010100 (vector->bits '#(#f #f #t #f #t #f #t #t))))

  (define test-235
    (test '#(#t #t #t #f #t #f #t #f #f) (bits->vector #b1010111 9)))

  (define test-236
    (test '#(#t #t #t #f #t #f #t #f #f) (bits->vector #b1010111 9)))

  (define test-237
    (test #b1110101 (bits #t #f #t #f #t #t #t)))

  (define test-238
    (test 0 (bits)))

  (define test-239
    (test #b111010100 (bits #f #f #t #f #t #f #t #t #t)))

  ;; bitwise/fold

  (define test-240
    (test '(#t #f #t #f #t #t #t) (bitwise-fold cons '() #b1010111)))

  (define test-241
    (test 5
          (let ((count 0))
            (bitwise-for-each (lambda (b) (if b (set! count (+ count 1))))
                              #b1010111)
            count)))

  (define test-242
    (test #b101010101
          (bitwise-unfold (lambda (i) (= i 10)) even? (lambda (i) (+ i 1)) 0)))

  (define test-243
    (test #t
          (let ((g (make-bitwise-generator #b110)))
            (and
             (equal? #f (g))
             (equal? #t (g))
             (equal? #t (g))
             (equal? #f (g)))))))
