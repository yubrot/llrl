# adcb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| adcb r8 r/m8 | `12 /r [reg=0, rm=1]` | Add with carry r/m8 to byte register. |
| adcb r/m8 r8 | `10 /r [reg=1, rm=0]` | Add with carry byte register to r/m8. |
| adcb r/m8 imm8 | `80 /2 ib [rm=0, i=1]` | Add with carry imm8 to r/m8. |
| adcb AL imm8 | `14 ib [i=1]` | Add with carry imm8 to AL |

# adcl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| adcl r32 r/m32 | `13 /r [reg=0, rm=1]` | Add with CF r/m32 to r32. |
| adcl r/m32 r32 | `11 /r [reg=1, rm=0]` | Add with CF r32 to r/m32. |
| adcl r/m32 imm8 | `83 /2 ib [rm=0, i=1]` | Add with CF sign-extended imm8 into r/m32. |
| adcl r/m32 imm32 | `81 /2 id [rm=0, i=1]` | Add with CF imm32 to r/m32. |
| adcl EAX imm32 | `15 id [i=1]` | Add with carry imm32 to EAX. |

# adcq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| adcq r64 r/m64 | `REX.W+ 13 /r [reg=0, rm=1]` | Add with CF r/m64 to r64. |
| adcq r/m64 r64 | `REX.W+ 11 /r [reg=1, rm=0]` | Add with CF r64 to r/m64. |
| adcq r/m64 imm8 | `REX.W+ 83 /2 ib [rm=0, i=1]` | Add with CF sign-extended imm8 into r/m64. |
| adcq r/m64 imm32 | `REX.W+ 81 /2 id [rm=0, i=1]` | Add with CF imm32 sign extended to 64-bits to r/m64. |
| adcq RAX imm32 | `REX.W+ 15 id [i=1]` | Add with carry imm32 sign extended to 64- bits to RAX. |

# adcw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| adcw r16 r/m16 | `66 13 /r [reg=0, rm=1]` | Add with carry r/m16 to r16. |
| adcw r/m16 r16 | `66 11 /r [reg=1, rm=0]` | Add with carry r16 to r/m16. |
| adcw r/m16 imm8 | `66 83 /2 ib [rm=0, i=1]` | Add with CF sign-extended imm8 to r/m16. |
| adcw r/m16 imm16 | `66 81 /2 iw [rm=0, i=1]` | Add with carry imm16 to r/m16. |
| adcw AX imm16 | `66 15 iw [i=1]` | Add with carry imm16 to AX. |

# addb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| addb r8 r/m8 | `02 /r [reg=0, rm=1]` | Add r/m8 to r8. |
| addb r/m8 r8 | `00 /r [reg=1, rm=0]` | Add r8 to r/m8. |
| addb r/m8 imm8 | `80 /0 ib [rm=0, i=1]` | Add imm8 to r/m8. |
| addb AL imm8 | `04 ib [i=1]` | Add imm8 to AL. |

# addl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| addl r32 r/m32 | `03 /r [reg=0, rm=1]` | Add r/m32 to r32. |
| addl r/m32 r32 | `01 /r [reg=1, rm=0]` | Add r32 to r/m32. |
| addl r/m32 imm8 | `83 /0 ib [rm=0, i=1]` | Add sign-extended imm8 to r/m32. |
| addl r/m32 imm32 | `81 /0 id [rm=0, i=1]` | Add imm32 to r/m32. |
| addl EAX imm32 | `05 id [i=1]` | Add imm32 to EAX. |

# addpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| addpd xmm xmm/m128 | `66 0F 58 /r [reg=0, rm=1]` | Add packed double-precision floating-point values from xmm2/m128 to xmm1. |

# addps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| addps xmm xmm/m128 | `0F 58 /r [reg=0, rm=1]` | Add packed single-precision floating-point values from xmm2/m128 to xmm1 and stores result in xmm1. |

# addq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| addq r64 r/m64 | `REX.W+ 03 /r [reg=0, rm=1]` | Add r/m64 to r64. |
| addq r/m64 r64 | `REX.W+ 01 /r [reg=1, rm=0]` | Add r64 to r/m64. |
| addq r/m64 imm8 | `REX.W+ 83 /0 ib [rm=0, i=1]` | Add sign-extended imm8 to r/m64. |
| addq r/m64 imm32 | `REX.W+ 81 /0 id [rm=0, i=1]` | Add imm32 sign-extended to 64-bits to r/m64. |
| addq RAX imm32 | `REX.W+ 05 id [i=1]` | Add imm32 sign-extended to 64-bits to RAX. |

# addsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| addsd xmm xmm/m64 | `F2 0F 58 /r [reg=0, rm=1]` | Add the low double-precision floating-point value from xmm2/m64 to xmm1. |

# addss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| addss xmm xmm/m32 | `F3 0F 58 /r [reg=0, rm=1]` | Add the low single-precision floating-point value from xmm2/m32 to xmm1. |

# addw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| addw r16 r/m16 | `66 03 /r [reg=0, rm=1]` | Add r/m16 to r16. |
| addw r/m16 r16 | `66 01 /r [reg=1, rm=0]` | Add r16 to r/m16. |
| addw r/m16 imm8 | `66 83 /0 ib [rm=0, i=1]` | Add sign-extended imm8 to r/m16. |
| addw r/m16 imm16 | `66 81 /0 iw [rm=0, i=1]` | Add imm16 to r/m16. |
| addw AX imm16 | `66 05 iw [i=1]` | Add imm16 to AX. |

# andb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| andb r8 r/m8 | `22 /r [reg=0, rm=1]` | r8 AND r/m8. |
| andb r/m8 r8 | `20 /r [reg=1, rm=0]` | r/m8 AND r8. |
| andb r/m8 imm8 | `80 /4 ib [rm=0, i=1]` | r/m8 AND imm8. |
| andb AL imm8 | `24 ib [i=1]` | AL AND imm8. |

# andl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| andl r32 r/m32 | `23 /r [reg=0, rm=1]` | r32 AND r/m32. |
| andl r/m32 r32 | `21 /r [reg=1, rm=0]` | r/m32 AND r32. |
| andl r/m32 imm8 | `83 /4 ib [rm=0, i=1]` | r/m32 AND imm8 (sign-extended). |
| andl r/m32 imm32 | `81 /4 id [rm=0, i=1]` | r/m32 AND imm32. |
| andl EAX imm32 | `25 id [i=1]` | EAX AND imm32. |

# andnpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| andnpd xmm xmm/m128 | `66 0F 55 /r [reg=0, rm=1]` | Bitwise logical AND NOT of xmm2/m128 and xmm1. |

# andnps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| andnps xmm xmm/m128 | `0F 55 /r [reg=0, rm=1]` | Bitwise logical AND NOT of xmm2/m128 and xmm1. |

# andpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| andpd xmm xmm/m128 | `66 0F 54 /r [reg=0, rm=1]` | Return the bitwise logical AND of packed double-precision floating-point values in xmm1 and xmm2/m128. |

# andps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| andps xmm xmm/m128 | `0F 54 /r [reg=0, rm=1]` | Bitwise logical AND of xmm2/m128 and xmm1. |

# andq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| andq r64 r/m64 | `REX.W+ 23 /r [reg=0, rm=1]` | r64 AND r/m64. |
| andq r/m64 r64 | `REX.W+ 21 /r [reg=1, rm=0]` | r/m64 AND r32. |
| andq r/m64 imm8 | `REX.W+ 83 /4 ib [rm=0, i=1]` | r/m64 AND imm8 (sign-extended). |
| andq r/m64 imm32 | `REX.W+ 81 /4 id [rm=0, i=1]` | r/m64 AND imm32 sign extended to 64-bits. |
| andq RAX imm32 | `REX.W+ 25 id [i=1]` | RAX AND imm32 sign-extended to 64-bits. |

# andw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| andw r16 r/m16 | `66 23 /r [reg=0, rm=1]` | r16 AND r/m16. |
| andw r/m16 r16 | `66 21 /r [reg=1, rm=0]` | r/m16 AND r16. |
| andw r/m16 imm8 | `66 83 /4 ib [rm=0, i=1]` | r/m16 AND imm8 (sign-extended). |
| andw r/m16 imm16 | `66 81 /4 iw [rm=0, i=1]` | r/m16 AND imm16. |
| andw AX imm16 | `66 25 iw [i=1]` | AX AND imm16. |

# blendpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| blendpd xmm xmm/m128 imm8 | `66 0F 3A 0D /r ib [reg=0, rm=1, i=2]` | Select packed DP-FP values from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. |

# blendps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| blendps xmm xmm/m128 imm8 | `66 0F 3A 0C /r ib [reg=0, rm=1, i=2]` | Select packed single precision floating-point values from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. |

# blendvpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| blendvpd xmm xmm/m128 <XMM0> | `66 0F 38 15 /r [reg=0, rm=1]` | Select packed DP FP values from xmm1 and xmm2 from mask specified in XMM0 and store the values in xmm1. |

# blendvps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| blendvps xmm xmm/m128 <XMM0> | `66 0F 38 14 /r [reg=0, rm=1]` | Select packed single precision floating-point values from xmm1 and xmm2/m128 from mask specified in XMM0 and store the values into xmm1. |

# bsfl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| bsfl r32 r/m32 | `0F BC /r [reg=0, rm=1]` | Bit scan forward on r/m32. |

# bsfq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| bsfq r64 r/m64 | `REX.W+ 0F BC /r [reg=0, rm=1]` | Bit scan forward on r/m64. |

# bsfw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| bsfw r16 r/m16 | `66 0F BC /r [reg=0, rm=1]` | Bit scan forward on r/m16. |

# bsrl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| bsrl r32 r/m32 | `0F BD /r [reg=0, rm=1]` | Bit scan reverse on r/m32. |

# bsrq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| bsrq r64 r/m64 | `REX.W+ 0F BD /r [reg=0, rm=1]` | Bit scan reverse on r/m64. |

# bsrw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| bsrw r16 r/m16 | `66 0F BD /r [reg=0, rm=1]` | Bit scan reverse on r/m16. |

# bswap
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| bswap r32 | `0F C8 +rd [r=0]` | Reverses the byte order of a 32-bit register. |
| bswap r64 | `REX.W+ 0F C8 +ro [r=0]` | Reverses the byte order of a 64-bit register. |

# btcl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btcl r/m32 r32 | `0F BB /r [reg=1, rm=0]` | Store selected bit in CF flag and complement. |
| btcl r/m32 imm8 | `0F BA /7 ib [rm=0, i=1]` | Store selected bit in CF flag and complement. |

# btcq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btcq r/m64 r64 | `REX.W+ 0F BB /r [reg=1, rm=0]` | Store selected bit in CF flag and complement. |
| btcq r/m64 imm8 | `REX.W+ 0F BA /7 ib [rm=0, i=1]` | Store selected bit in CF flag and complement. |

# btcw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btcw r/m16 r16 | `66 0F BB /r [reg=1, rm=0]` | Store selected bit in CF flag and complement. |
| btcw r/m16 imm8 | `66 0F BA /7 ib [rm=0, i=1]` | Store selected bit in CF flag and complement. |

# btl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btl r/m32 r32 | `0F A3 /r [reg=1, rm=0]` | Store selected bit in CF flag. |
| btl r/m32 imm8 | `0F BA /4 ib [rm=0, i=1]` | Store selected bit in CF flag. |

# btq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btq r/m64 r64 | `REX.W+ 0F A3 /r [reg=1, rm=0]` | Store selected bit in CF flag. |
| btq r/m64 imm8 | `REX.W+ 0F BA /4 ib [rm=0, i=1]` | Store selected bit in CF flag. |

# btrl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btrl r/m32 r32 | `0F B3 /r [reg=1, rm=0]` | Store selected bit in CF flag and clear. |
| btrl r/m32 imm8 | `0F BA /6 ib [rm=0, i=1]` | Store selected bit in CF flag and clear. |

# btrq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btrq r/m64 r64 | `REX.W+ 0F B3 /r [reg=1, rm=0]` | Store selected bit in CF flag and clear. |
| btrq r/m64 imm8 | `REX.W+ 0F BA /6 ib [rm=0, i=1]` | Store selected bit in CF flag and clear. |

# btrw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btrw r/m16 r16 | `66 0F B3 /r [reg=1, rm=0]` | Store selected bit in CF flag and clear. |
| btrw r/m16 imm8 | `66 0F BA /6 ib [rm=0, i=1]` | Store selected bit in CF flag and clear. |

# btsl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btsl r/m32 r32 | `0F AB /r [reg=1, rm=0]` | Store selected bit in CF flag and set. |
| btsl r/m32 imm8 | `0F BA /5 ib [rm=0, i=1]` | Store selected bit in CF flag and set. |

# btsq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btsq r/m64 r64 | `REX.W+ 0F AB /r [reg=1, rm=0]` | Store selected bit in CF flag and set. |
| btsq r/m64 imm8 | `REX.W+ 0F BA /5 ib [rm=0, i=1]` | Store selected bit in CF flag and set. |

# btsw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btsw r/m16 r16 | `66 0F AB /r [reg=1, rm=0]` | Store selected bit in CF flag and set. |
| btsw r/m16 imm8 | `66 0F BA /5 ib [rm=0, i=1]` | Store selected bit in CF flag and set. |

# btw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| btw r/m16 r16 | `66 0F A3 /r [reg=1, rm=0]` | Store selected bit in CF flag. |
| btw r/m16 imm8 | `66 0F BA /4 ib [rm=0, i=1]` | Store selected bit in CF flag. |

# callq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| callq rel32 | `E8 cd [c=0]` | Call near, relative, displacement relative to next instruction. 32-bit displacement sign extended to 64-bits in 64-bit mode. |
| callq r/m64 | `FF /2 [rm=0]` | Call near, absolute indirect, address given in r/m64. |

# cbtw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cbtw | `66 98 []` | AX = sign-extend of AL. |

# clc
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| clc | `F8 []` | Clear CF flag. |

# cld
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cld | `FC []` | Clear DF flag. |

# cli
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cli | `FA []` | Clear interrupt flag; interrupts disabled when interrupt flag cleared. |

# cltd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cltd | `99 []` | EDX:EAX = sign-extend of EAX. |

# cltq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cltq | `REX.W+ 98 []` | RAX = sign-extend of EAX. |

# clts
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| clts | `0F 06 []` | Clears TS flag in CR0. |

# cmc
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmc | `F5 []` | Complement CF flag. |

# cmpb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpb r8 r/m8 | `3A /r [reg=0, rm=1]` | Compare r/m8 with r8. |
| cmpb r/m8 r8 | `38 /r [reg=1, rm=0]` | Compare r8 with r/m8. |
| cmpb r/m8 imm8 | `80 /7 ib [rm=0, i=1]` | Compare imm8 with r/m8. |
| cmpb AL imm8 | `3C ib [i=1]` | Compare imm8 with AL. |

# cmpl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpl r32 r/m32 | `3B /r [reg=0, rm=1]` | Compare r/m32 with r32. |
| cmpl r/m32 r32 | `39 /r [reg=1, rm=0]` | Compare r32 with r/m32. |
| cmpl r/m32 imm8 | `83 /7 ib [rm=0, i=1]` | Compare imm8 with r/m32. |
| cmpl r/m32 imm32 | `81 /7 id [rm=0, i=1]` | Compare imm32 with r/m32. |
| cmpl EAX imm32 | `3D id [i=1]` | Compare imm32 with EAX. |

# cmppd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmppd xmm xmm/m128 imm8 | `66 0F C2 /r ib [reg=0, rm=1, i=2]` | Compare packed double-precision floating- point values in xmm2/m128 and xmm1 using imm8 as comparison predicate. |

# cmpps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpps xmm xmm/m128 imm8 | `0F C2 /r ib [reg=0, rm=1, i=2]` | Compare packed single-precision floating- point values in xmm2/mem and xmm1 using imm8 as comparison predicate. |

# cmpq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpq r64 r/m64 | `REX.W+ 3B /r [reg=0, rm=1]` | Compare r/m64 with r64. |
| cmpq r/m64 r64 | `REX.W+ 39 /r [reg=1, rm=0]` | Compare r64 with r/m64. |
| cmpq r/m64 imm8 | `REX.W+ 83 /7 ib [rm=0, i=1]` | Compare imm8 with r/m64. |
| cmpq r/m64 imm32 | `REX.W+ 81 /7 id [rm=0, i=1]` | Compare imm32 sign-extended to 64-bits with r/m64. |
| cmpq RAX imm32 | `REX.W+ 3D id [i=1]` | Compare imm32 sign-extended to 64-bits with RAX. |

# cmpsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpsd xmm xmm/m64 imm8 | `F2 0F C2 /r ib [reg=0, rm=1, i=2]` | Compare low double-precision floating-point value in xmm2/m64 and xmm1 using imm8 as comparison predicate. |

# cmpsl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpsl | `A7 []` | For legacy mode, compare dword at address DS:(E)SI with dword at address ES:(E)DI; For 64-bit mode compare dword at address (R\|E)SI with dword at address (R\|E)DI. The status flags are set accordingly. |

# cmpw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpw r16 r/m16 | `66 3B /r [reg=0, rm=1]` | Compare r/m16 with r16. |
| cmpw r/m16 r16 | `66 39 /r [reg=1, rm=0]` | Compare r16 with r/m16. |
| cmpw r/m16 imm8 | `66 83 /7 ib [rm=0, i=1]` | Compare imm8 with r/m16. |
| cmpw r/m16 imm16 | `66 81 /7 iw [rm=0, i=1]` | Compare imm16 with r/m16. |
| cmpw AX imm16 | `66 3D iw [i=1]` | Compare imm16 with AX. |

# cmpxchgb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpxchgb r/m8 r8 | `0F B0 /r [reg=1, rm=0]` | Compare AL with r/m8. If equal, ZF is set and r8 is loaded into r/m8. Else, clear ZF and load r/m8 into AL. |

# cmpxchgl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpxchgl r/m32 r32 | `0F B1 /r [reg=1, rm=0]` | Compare EAX with r/m32. If equal, ZF is set and r32 is loaded into r/m32. Else, clear ZF and load r/m32 into EAX. |

# cmpxchgq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpxchgq r/m64 r64 | `REX.W+ 0F B1 /r [reg=1, rm=0]` | Compare RAX with r/m64. If equal, ZF is set and r64 is loaded into r/m64. Else, clear ZF and load r/m64 into RAX. |

# cmpxchgw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cmpxchgw r/m16 r16 | `66 0F B1 /r [reg=1, rm=0]` | Compare AX with r/m16. If equal, ZF is set and r16 is loaded into r/m16. Else, clear ZF and load r/m16 into AX. |

# comisd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| comisd xmm xmm/m64 | `66 0F 2F /r [reg=0, rm=1]` | Compare low double-precision floating-point values in xmm1 and xmm2/mem64 and set the EFLAGS flags accordingly. |

# comiss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| comiss xmm xmm/m32 | `0F 2F /r [reg=0, rm=1]` | Compare low single-precision floating-point values in xmm1 and xmm2/mem32 and set the EFLAGS flags accordingly. |

# cpuid
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cpuid | `0F A2 []` | Returns processor identification and feature information to the EAX, EBX, ECX, and EDX registers, as determined by input entered in EAX (in some cases, ECX as well). |

# cqto
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cqto | `REX.W+ 99 []` | RDX:RAX = sign-extend of RAX. |

# crc32b
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| crc32b r32 r/m8 | `F2 0F 38 F0 /r [reg=0, rm=1]` | Accumulate CRC32 on r/m8. |
| crc32b r64 r/m8 | `F2 REX.W+ 0F 38 F0 /r [reg=0, rm=1]` | Accumulate CRC32 on r/m8. |

# crc32l
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| crc32l r32 r/m32 | `F2 0F 38 F1 /r [reg=0, rm=1]` | Accumulate CRC32 on r/m32. |

# crc32q
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| crc32q r64 r/m64 | `F2 REX.W+ 0F 38 F1 /r [reg=0, rm=1]` | Accumulate CRC32 on r/m64. |

# crc32w
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| crc32w r32 r/m16 | `F2 66 0F 38 F1 /r [reg=0, rm=1]` | Accumulate CRC32 on r/m16. |

# cvtdq2pd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtdq2pd xmm xmm/m64 | `F3 0F E6 /r [reg=0, rm=1]` | Convert two packed signed doubleword integers from xmm2/m128 to two packed double-precision floating-point values in xmm1. |

# cvtdq2ps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtdq2ps xmm xmm/m128 | `0F 5B /r [reg=0, rm=1]` | Convert four packed signed doubleword integers from xmm2/m128 to four packed single-precision floating-point values in xmm1. |

# cvtpd2dq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtpd2dq xmm xmm/m128 | `F2 0F E6 /r [reg=0, rm=1]` | Convert two packed double-precision floating- point values from xmm2/m128 to two packed signed doubleword integers in xmm1. |

# cvtpd2ps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtpd2ps xmm xmm/m128 | `66 0F 5A /r [reg=0, rm=1]` | Convert two packed double-precision floating- point values in xmm2/m128 to two packed single-precision floating-point values in xmm1. |

# cvtps2dq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtps2dq xmm xmm/m128 | `66 0F 5B /r [reg=0, rm=1]` | Convert four packed single-precision floating- point values from xmm2/m128 to four packed signed doubleword integers in xmm1. |

# cvtps2pd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtps2pd xmm xmm/m64 | `0F 5A /r [reg=0, rm=1]` | Convert two packed single-precision floating- point values in xmm2/m64 to two packed double-precision floating-point values in xmm1. |

# cvtsd2si
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtsd2si r32 xmm/m64 | `F2 0F 2D /r [reg=0, rm=1]` | Convert one double-precision floating-point value from xmm/m64 to one signed doubleword integer r32. |
| cvtsd2si r64 xmm/m64 | `F2 REX.W+ 0F 2D /r [reg=0, rm=1]` | Convert one double-precision floating-point value from xmm/m64 to one signed quadword integer sign-extended into r64. |

# cvtsd2ss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtsd2ss xmm xmm/m64 | `F2 0F 5A /r [reg=0, rm=1]` | Convert one double-precision floating-point value in xmm2/m64 to one single-precision floating-point value in xmm1. |

# cvtsi2sdl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtsi2sdl xmm r/m32 | `F2 0F 2A /r [reg=0, rm=1]` | Convert one signed doubleword integer from r/m32 to one double-precision floating-point value in xmm. |

# cvtsi2sdq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtsi2sdq xmm r/m64 | `F2 REX.W+ 0F 2A /r [reg=0, rm=1]` | Convert one signed quadword integer from r/m64 to one double-precision floating-point value in xmm. |

# cvtsi2ssl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtsi2ssl xmm r/m32 | `F3 0F 2A /r [reg=0, rm=1]` | Convert one signed doubleword integer from r/m32 to one single-precision floating-point value in xmm. |

# cvtsi2ssq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtsi2ssq xmm r/m64 | `F3 REX.W+ 0F 2A /r [reg=0, rm=1]` | Convert one signed quadword integer from r/m64 to one single-precision floating-point value in xmm. |

# cvtss2sd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtss2sd xmm xmm/m32 | `F3 0F 5A /r [reg=0, rm=1]` | Convert one single-precision floating-point value in xmm2/m32 to one double-precision floating-point value in xmm1. |

# cvtss2si
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvtss2si r32 xmm/m32 | `F3 0F 2D /r [reg=0, rm=1]` | Convert one single-precision floating-point value from xmm/m32 to one signed doubleword integer in r32. |
| cvtss2si r64 xmm/m32 | `F3 REX.W+ 0F 2D /r [reg=0, rm=1]` | Convert one single-precision floating-point value from xmm/m32 to one signed quadword integer in r64. |

# cvttpd2dq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvttpd2dq xmm xmm/m128 | `66 0F E6 /r [reg=0, rm=1]` | Convert two packed double-precision floating- point values from xmm2/m128 to two packed signed doubleword integers in xmm1 using truncation. |

# cvttps2dq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvttps2dq xmm xmm/m128 | `F3 0F 5B /r [reg=0, rm=1]` | Convert four single-precision floating-point values from xmm2/m128 to four signed doubleword integers in xmm1 using truncation. |

# cvttsd2si
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvttsd2si r32 xmm/m64 | `F2 0F 2C /r [reg=0, rm=1]` | Convert one double-precision floating-point value from xmm/m64 to one signed doubleword integer in r32 using truncation. |
| cvttsd2si r64 xmm/m64 | `F2 REX.W+ 0F 2C /r [reg=0, rm=1]` | Convert one double precision floating-point value from xmm/m64 to one signedquadword integer in r64 using truncation. |

# cvttss2si
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cvttss2si r32 xmm/m32 | `F3 0F 2C /r [reg=0, rm=1]` | Convert one single-precision floating-point value from xmm/m32 to one signed doubleword integer in r32 using truncation. |
| cvttss2si r64 xmm/m32 | `F3 REX.W+ 0F 2C /r [reg=0, rm=1]` | Convert one single-precision floating-point value from xmm/m32 to one signed quadword integer in r64 using truncation. |

# cwtd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cwtd | `66 99 []` | DX:AX = sign-extend of AX. |

# cwtl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| cwtl | `98 []` | EAX = sign-extend of AX. |

# decb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| decb r/m8 | `FE /1 [rm=0]` | Decrement r/m8 by 1. |

# decl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| decl r/m32 | `FF /1 [rm=0]` | Decrement r/m32 by 1. |

# decq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| decq r/m64 | `REX.W+ FF /1 [rm=0]` | Decrement r/m64 by 1. |

# decw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| decw r/m16 | `66 FF /1 [rm=0]` | Decrement r/m16 by 1. |

# divb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| divb r/m8 | `F6 /6 [rm=0]` | Unsigned divide AX by r/m8, with result stored in AL = Quotient, AH = Remainder. |

# divl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| divl r/m32 | `F7 /6 [rm=0]` | Unsigned divide EDX:EAX by r/m32, with result stored in EAX = Quotient, EDX = Remainder. |

# divpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| divpd xmm xmm/m128 | `66 0F 5E /r [reg=0, rm=1]` | Divide packed double-precision floating-point values in xmm1 by packed double-precision floating-point values xmm2/m128. |

# divps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| divps xmm xmm/m128 | `0F 5E /r [reg=0, rm=1]` | Divide packed single-precision floating-point values in xmm1 by packed single-precision floating-point values xmm2/m128. |

# divq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| divq r/m64 | `REX.W+ F7 /6 [rm=0]` | Unsigned divide RDX:RAX by r/m64, with result stored in RAX = Quotient, RDX = Remainder. |

# divsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| divsd xmm xmm/m64 | `F2 0F 5E /r [reg=0, rm=1]` | Divide low double-precision floating-point value in xmm1 by low double-precision floating-point value in xmm2/mem64. |

# divss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| divss xmm xmm/m32 | `F3 0F 5E /r [reg=0, rm=1]` | Divide low single-precision floating-point value in xmm1 by low single-precision floating-point value in xmm2/m32. |

# divw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| divw r/m16 | `66 F7 /6 [rm=0]` | Unsigned divide DX:AX by r/m16, with result stored in AX = Quotient, DX = Remainder. |

# dppd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| dppd xmm xmm/m128 imm8 | `66 0F 3A 41 /r ib [reg=0, rm=1, i=2]` | Selectively multiply packed DP floating-point values from xmm1 with packed DP floating- point values from xmm2, add and selectively store the packed DP floating-point values to xmm1. |

# dpps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| dpps xmm xmm/m128 imm8 | `66 0F 3A 40 /r ib [reg=0, rm=1, i=2]` | Selectively multiply packed SP floating-point values from xmm1 with packed SP floating- point values from xmm2, add and selectively store the packed SP floating-point values or zero values to xmm1. |

# emms
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| emms | `0F 77 []` | Set the x87 FPU tag word to empty. |

# extractps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| extractps reg/m32 xmm imm8 | `66 0F 3A 17 /r ib [reg=1, rm=0, i=2]` | Extract a single-precision floating-point value from xmm2 at the source offset specified by imm8 and store the result to reg or m32. The upper 32 bits of r64 is zeroed if reg is r64. |

# hlt
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| hlt | `F4 []` | Halt |

# idivb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| idivb r/m8 | `F6 /7 [rm=0]` | Signed divide AX by r/m8, with result stored in: AL = Quotient, AH = Remainder. |

# idivl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| idivl r/m32 | `F7 /7 [rm=0]` | Signed divide EDX:EAX by r/m32, with result stored in EAX = Quotient, EDX = Remainder. |

# idivq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| idivq r/m64 | `REX.W+ F7 /7 [rm=0]` | Signed divide RDX:RAX by r/m64, with result stored in RAX = Quotient, RDX = Remainder. |

# idivw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| idivw r/m16 | `66 F7 /7 [rm=0]` | Signed divide DX:AX by r/m16, with result stored in AX = Quotient, DX = Remainder. |

# imulb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| imulb r/m8 | `F6 /5 [rm=0]` | AX= AL * r/m byte. |

# imull
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| imull r32 r/m32 | `0F AF /r [reg=0, rm=1]` | doubleword register = doubleword register *  r/m32. |
| imull r32 r/m32 imm8 | `6B /r ib [reg=0, rm=1, i=2]` | doubleword register = r/m32 * sign- extended immediate byte. |
| imull r32 r/m32 imm32 | `69 /r id [reg=0, rm=1, i=2]` | doubleword register = r/m32 * immediate doubleword. |
| imull r/m32 | `F7 /5 [rm=0]` | EDX:EAX = EAX * r/m32. |

# imulq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| imulq r64 r/m64 | `REX.W+ 0F AF /r [reg=0, rm=1]` | Quadword register = Quadword register *  r/m64. |
| imulq r64 r/m64 imm8 | `REX.W+ 6B /r ib [reg=0, rm=1, i=2]` | Quadword register = r/m64 * sign-extended  immediate byte. |
| imulq r64 r/m64 imm32 | `REX.W+ 69 /r id [reg=0, rm=1, i=2]` | Quadword register = r/m64 * immediate doubleword. |
| imulq r/m64 | `REX.W+ F7 /5 [rm=0]` | RDX:RAX = RAX * r/m64. |

# imulw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| imulw r16 r/m16 | `66 0F AF /r [reg=0, rm=1]` | word register = word register * r/m16. |
| imulw r16 r/m16 imm8 | `66 6B /r ib [reg=0, rm=1, i=2]` | word register = r/m16 * sign-extended immediate byte. |
| imulw r16 r/m16 imm16 | `66 69 /r iw [reg=0, rm=1, i=2]` | word register = r/m16 * immediate word. |
| imulw r/m16 | `66 F7 /5 [rm=0]` | DX:AX = AX * r/m word. |

# inb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| inb AL imm8 | `E4 ib [i=1]` | Input byte from imm8 I/O port address into AL. |
| inb AL DX | `EC []` | Input byte from I/O port in DX into AL. |

# incb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| incb r/m8 | `FE /0 [rm=0]` | Increment r/m byte by 1. |

# incl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| incl r/m32 | `FF /0 [rm=0]` | Increment r/m doubleword by 1. |

# incq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| incq r/m64 | `REX.W+ FF /0 [rm=0]` | Increment r/m quadword by 1. |

# incw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| incw r/m16 | `66 FF /0 [rm=0]` | Increment r/m word by 1. |

# inl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| inl EAX imm8 | `E5 ib [i=1]` | Input dword from imm8 I/O port address into EAX. |
| inl EAX DX | `ED []` | Input doubleword from I/O port in DX into EAX. |

# int
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| int imm8 | `CD ib [i=0]` | Interrupt vector number specified by immediate byte. |
| int 3 | `CC []` | Interrupt 3-trap to debugger. |

# invd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| invd | `0F 08 []` | Flush internal caches; initiate flushing of external caches. |

# invlpg
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| invlpg m | `0F 01 /7 [rm=0]` | Invalidate TLB Entry for page that contains m. |

# inw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| inw AX imm8 | `66 E5 ib [i=1]` | Input word from imm8 I/O port address into AX. |
| inw AX DX | `66 ED []` | Input word from I/O port in DX into AX. |

# iretl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| iretl | `CF []` | Interrupt return (32-bit operand size). |

# iretq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| iretq | `REX.W+ CF []` | Interrupt return (64-bit operand size). |

# iretw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| iretw | `66 CF []` | Interrupt return (16-bit operand size). |

# ja
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| ja rel8 | `77 cb [c=0]` | Jump short if above (CF=0 and ZF=0). |
| ja rel32 | `0F 87 cd [c=0]` | Jump near if above (CF=0 and ZF=0). |

# jae
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jae rel8 | `73 cb [c=0]` | Jump short if above or equal (CF=0). |
| jae rel32 | `0F 83 cd [c=0]` | Jump near if above or equal (CF=0). |

# jb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jb rel8 | `72 cb [c=0]` | Jump short if below (CF=1). |
| jb rel32 | `0F 82 cd [c=0]` | Jump near if below (CF=1). |

# jbe
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jbe rel8 | `76 cb [c=0]` | Jump short if below or equal (CF=1 or ZF=1). |
| jbe rel32 | `0F 86 cd [c=0]` | Jump near if below or equal (CF=1 or ZF=1). |

# jc
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jc rel8 | `72 cb [c=0]` | Jump short if carry (CF=1). |
| jc rel32 | `0F 82 cd [c=0]` | Jump near if carry (CF=1). |

# je
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| je rel8 | `74 cb [c=0]` | Jump short if equal (ZF=1). |
| je rel32 | `0F 84 cd [c=0]` | Jump near if 0 (ZF=1). |

# jecxz
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jecxz rel8 | `E3 cb [c=0]` | Jump short if ECX register is 0. |

# jg
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jg rel8 | `7F cb [c=0]` | Jump short if greater (ZF=0 and SF=OF). |
| jg rel32 | `0F 8F cd [c=0]` | Jump near if greater (ZF=0 and SF=OF). |

# jge
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jge rel8 | `7D cb [c=0]` | Jump short if greater or equal (SF=OF). |
| jge rel32 | `0F 8D cd [c=0]` | Jump near if greater or equal (SF=OF). |

# jl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jl rel8 | `7C cb [c=0]` | Jump short if less (SF!= OF). |
| jl rel32 | `0F 8C cd [c=0]` | Jump near if less (SF!= OF). |

# jle
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jle rel8 | `7E cb [c=0]` | Jump short if less or equal (ZF=1 or SF!= OF). |
| jle rel32 | `0F 8E cd [c=0]` | Jump near if less or equal (ZF=1 or SF!= OF). |

# jmpq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jmpq rel8 | `EB cb [c=0]` | Jump short, RIP = RIP + 8-bit displacement sign extended to 64-bits |
| jmpq rel32 | `E9 cd [c=0]` | Jump near, relative, RIP = RIP + 32-bit displacement sign extended to 64-bits |
| jmpq r/m64 | `FF /4 [rm=0]` | Jump near, absolute indirect, RIP = 64-Bit offset from register or memory |

# jna
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jna rel8 | `76 cb [c=0]` | Jump short if not above (CF=1 or ZF=1). |
| jna rel32 | `0F 86 cd [c=0]` | Jump near if not above (CF=1 or ZF=1). |

# jnae
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jnae rel8 | `72 cb [c=0]` | Jump short if not above or equal (CF=1). |
| jnae rel32 | `0F 82 cd [c=0]` | Jump near if not above or equal (CF=1). |

# jnb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jnb rel8 | `73 cb [c=0]` | Jump short if not below (CF=0). |
| jnb rel32 | `0F 83 cd [c=0]` | Jump near if not below (CF=0). |

# jnbe
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jnbe rel8 | `77 cb [c=0]` | Jump short if not below or equal (CF=0 and ZF=0). |
| jnbe rel32 | `0F 87 cd [c=0]` | Jump near if not below or equal (CF=0 and ZF=0). |

# jnc
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jnc rel8 | `73 cb [c=0]` | Jump short if not carry (CF=0). |
| jnc rel32 | `0F 83 cd [c=0]` | Jump near if not carry (CF=0). |

# jne
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jne rel8 | `75 cb [c=0]` | Jump short if not equal (ZF=0). |
| jne rel32 | `0F 85 cd [c=0]` | Jump near if not equal (ZF=0). |

# jng
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jng rel8 | `7E cb [c=0]` | Jump short if not greater (ZF=1 or SF!= OF). |
| jng rel32 | `0F 8E cd [c=0]` | Jump near if not greater (ZF=1 or SF != OF). |

# jnge
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jnge rel8 | `7C cb [c=0]` | Jump short if not greater or equal (SF!= OF). |
| jnge rel32 | `0F 8C cd [c=0]` | Jump near if not greater or equal (SF != OF). |

# jnl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jnl rel8 | `7D cb [c=0]` | Jump short if not less (SF=OF). |
| jnl rel32 | `0F 8D cd [c=0]` | Jump near if not less (SF=OF). |

# jnle
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jnle rel8 | `7F cb [c=0]` | Jump short if not less or equal (ZF=0 and SF=OF). |
| jnle rel32 | `0F 8F cd [c=0]` | Jump near if not less or equal (ZF=0 and SF=OF). |

# jno
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jno rel8 | `71 cb [c=0]` | Jump short if not overflow (OF=0). |
| jno rel32 | `0F 81 cd [c=0]` | Jump near if not overflow (OF=0). |

# jnp
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jnp rel8 | `7B cb [c=0]` | Jump short if not parity (PF=0). |
| jnp rel32 | `0F 8B cd [c=0]` | Jump near if not parity (PF=0). |

# jns
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jns rel8 | `79 cb [c=0]` | Jump short if not sign (SF=0). |
| jns rel32 | `0F 89 cd [c=0]` | Jump near if not sign (SF=0). |

# jnz
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jnz rel8 | `75 cb [c=0]` | Jump short if not zero (ZF=0). |
| jnz rel32 | `0F 85 cd [c=0]` | Jump near if not zero (ZF=0). |

# jo
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jo rel8 | `70 cb [c=0]` | Jump short if overflow (OF=1). |
| jo rel32 | `0F 80 cd [c=0]` | Jump near if overflow (OF=1). |

# jp
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jp rel8 | `7A cb [c=0]` | Jump short if parity (PF=1). |
| jp rel32 | `0F 8A cd [c=0]` | Jump near if parity (PF=1). |

# jpe
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jpe rel8 | `7A cb [c=0]` | Jump short if parity even (PF=1). |
| jpe rel32 | `0F 8A cd [c=0]` | Jump near if parity even (PF=1). |

# jpo
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jpo rel8 | `7B cb [c=0]` | Jump short if parity odd (PF=0). |
| jpo rel32 | `0F 8B cd [c=0]` | Jump near if parity odd (PF=0). |

# jrcxz
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jrcxz rel8 | `E3 cb [c=0]` | Jump short if RCX register is 0. |

# js
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| js rel8 | `78 cb [c=0]` | Jump short if sign (SF=1). |
| js rel32 | `0F 88 cd [c=0]` | Jump near if sign (SF=1). |

# jz
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| jz rel8 | `74 cb [c=0]` | Jump short if zero (ZF = 1). |
| jz rel32 | `0F 84 cd [c=0]` | Jump near if 0 (ZF=1). |

# ldmxcsr
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| ldmxcsr m32 | `0F AE /2 [rm=0]` | Load MXCSR register from m32. |

# leal
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| leal r32 m | `8D /r [reg=0, rm=1]` | Store effective address for m in register r32. |

# leaq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| leaq r64 m | `REX.W+ 8D /r [reg=0, rm=1]` | Store effective address for m in register r64. |

# leaveq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| leaveq | `C9 []` | Set RSP to RBP, then pop RBP. |

# leavew
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| leavew p66 | `66 C9 []` | Set SP to BP, then pop BP. |

# leaw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| leaw r16 m | `66 8D /r [reg=0, rm=1]` | Store effective address for m in register r16. |

# lfence
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| lfence | `0F AE E8 []` | Serializes load operations. |

# lmsw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| lmsw r/m16 | `0F 01 /6 [rm=0]` | Loads r/m16 in machine status word of CR0. |

# lock
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| lock | `F0 []` | Asserts LOCK# signal for duration of the accompanying instruction. |

# maskmovdqu
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| maskmovdqu xmm xmm | `66 0F F7 /r [reg=0, rm=1]` | Selectively write bytes from xmm1 to memory location using the byte mask in xmm2. The default memory location is specified by DS:DI/EDI/RDI. |

# maxpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| maxpd xmm xmm/m128 | `66 0F 5F /r [reg=0, rm=1]` | Return the maximum double-precision floating-point values between xmm2/m128 and xmm1. |

# maxps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| maxps xmm xmm/m128 | `0F 5F /r [reg=0, rm=1]` | Return the maximum single-precision floating-point values between xmm2/m128 and xmm1. |

# maxsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| maxsd xmm xmm/m64 | `F2 0F 5F /r [reg=0, rm=1]` | Return the maximum scalar double-precision floating-point value between xmm2/mem64 and xmm1. |

# maxss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| maxss xmm xmm/m32 | `F3 0F 5F /r [reg=0, rm=1]` | Return the maximum scalar single-precision floating-point value between xmm2/mem32 and xmm1. |

# mfence
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| mfence | `0F AE F0 []` | Serializes load and store operations. |

# minpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| minpd xmm xmm/m128 | `66 0F 5D /r [reg=0, rm=1]` | Return the minimum double-precision floating-point values between xmm2/m128 and xmm1. |

# minps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| minps xmm xmm/m128 | `0F 5D /r [reg=0, rm=1]` | Return the minimum single-precision floating-point values between xmm2/m128 and xmm1. |

# minsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| minsd xmm xmm/m64 | `F2 0F 5D /r [reg=0, rm=1]` | Return the minimum scalar double-precision floating-point value between xmm2/mem64 and xmm1. |

# minss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| minss xmm xmm/m32 | `F3 0F 5D /r [reg=0, rm=1]` | Return the minimum scalar single-precision floating-point value between xmm2/mem32 and xmm1. |

# movapd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movapd xmm xmm/m128 | `66 0F 28 /r [reg=0, rm=1]` | Move packed double-precision floating-point values from xmm2/m128 to xmm1. |
| movapd xmm/m128 xmm | `66 0F 29 /r [reg=1, rm=0]` | Move packed double-precision floating-point values from xmm1 to xmm2/m128. |

# movaps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movaps xmm xmm/m128 | `0F 28 /r [reg=0, rm=1]` | Move packed single-precision floating-point values from xmm2/m128 to xmm1. |
| movaps xmm/m128 xmm | `0F 29 /r [reg=1, rm=0]` | Move packed single-precision floating-point values from xmm1 to xmm2/m128. |

# movb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movb r8 imm8 | `B0 +rb ib [i=1, r=0]` | Move imm8 to r8. |
| movb r8 r/m8 | `8A /r [reg=0, rm=1]` | Move r/m8 to r8. |
| movb r/m8 r8 | `88 /r [reg=1, rm=0]` | Move r8 to r/m8. |
| movb r/m8 imm8 | `C6 /0 ib [rm=0, i=1]` | Move imm8 to r/m8. |

# movd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movd r/m32 xmm | `66 0F 7E /r [reg=1, rm=0]` | Move doubleword from xmm register to r/m32. |
| movd xmm r/m32 | `66 0F 6E /r [reg=0, rm=1]` | Move doubleword from r/m32 to xmm. |

# movdqa
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movdqa xmm xmm/m128 | `66 0F 6F /r [reg=0, rm=1]` | Move aligned double quadword from xmm2/m128 to xmm1. |
| movdqa xmm/m128 xmm | `66 0F 7F /r [reg=1, rm=0]` | Move aligned double quadword from xmm1 to xmm2/m128. |

# movdqu
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movdqu xmm xmm/m128 | `F3 0F 6F /r [reg=0, rm=1]` | Move unaligned double quadword from xmm2/m128 to xmm1. |
| movdqu xmm/m128 xmm | `F3 0F 7F /r [reg=1, rm=0]` | Move unaligned double quadword from xmm1 to xmm2/m128. |

# movhlps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movhlps xmm xmm | `0F 12 /r [reg=0, rm=1]` | Move two packed single-precision floating-point values from high quadword of xmm2 to low quadword of xmm1. |

# movhpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movhpd m64 xmm | `66 0F 17 /r [reg=1, rm=0]` | Move double-precision floating-point value from high quadword of xmm to m64. |
| movhpd xmm m64 | `66 0F 16 /r [reg=0, rm=1]` | Move double-precision floating-point value from m64 to high quadword of xmm. |

# movhps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movhps m64 xmm | `0F 17 /r [reg=1, rm=0]` | Move two packed single-precision floating-point values from high quadword of xmm to m64. |
| movhps xmm m64 | `0F 16 /r [reg=0, rm=1]` | Move two packed single-precision floating-point values from m64 to high quadword of xmm. |

# movl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movl r32 imm32 | `B8 +rd id [i=1, r=0]` | Move imm32 to r32. |
| movl r32 r/m32 | `8B /r [reg=0, rm=1]` | Move r/m32 to r32. |
| movl r/m32 r32 | `89 /r [reg=1, rm=0]` | Move r32 to r/m32. |
| movl r/m32 imm32 | `C7 /0 id [rm=0, i=1]` | Move imm32 to r/m32. |

# movlhps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movlhps xmm xmm | `0F 16 /r [reg=0, rm=1]` | Move two packed single-precision floating-point values from low quadword of xmm2 to high quadword of xmm1. |

# movlpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movlpd m64 xmm | `66 0F 13 /r [reg=1, rm=0]` | Move double-precision floating-point nvalue from low quadword of xmm register to m64. |
| movlpd xmm m64 | `66 0F 12 /r [reg=0, rm=1]` | Move double-precision floating-point value from m64 to low quadword of xmm register. |

# movlps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movlps m64 xmm | `0F 13 /r [reg=1, rm=0]` | Move two packed single-precision floating-point values from low quadword of xmm to m64. |
| movlps xmm m64 | `0F 12 /r [reg=0, rm=1]` | Move two packed single-precision floating-point values from m64 to low quadword of xmm. |

# movmskpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movmskpd reg xmm | `66 0F 50 /r [reg=0, rm=1]` | Extract 2-bit sign mask from xmm and store in reg. The upper bits of r32 or r64 are filled with zeros. |

# movmskps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movmskps reg xmm | `0F 50 /r [reg=0, rm=1]` | Extract 4-bit sign mask from xmm and store in reg. The upper bits of r32 or r64 are filled with zeros. |

# movntdq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movntdq m128 xmm | `66 0F E7 /r [reg=1, rm=0]` | Move double quadword from xmm to m128 using non-temporal hint. |

# movntdqa
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movntdqa xmm m128 | `66 0F 38 2A /r [reg=0, rm=1]` | Move double quadword from m128 to xmm using non-temporal hint if WC memory type. |

# movnti
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movnti m32 r32 | `0F C3 /r [reg=1, rm=0]` | Move doubleword from r32 to m32 using non-temporal hint. |
| movnti m64 r64 | `REX.W+ 0F C3 /r [reg=1, rm=0]` | Move quadword from r64 to m64 using non-temporal hint. |

# movntpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movntpd m128 xmm | `66 0F 2B /r [reg=1, rm=0]` | Move packed double-precision floating-point values from xmm to m128 using non-temporal hint. |

# movntps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movntps m128 xmm | `0F 2B /r [reg=1, rm=0]` | Move packed single-precision floating-point values from xmm to m128 using non-temporal hint. |

# movq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movq r64 imm64 | `REX.W+ B8 +ro io [i=1, r=0]` | Move imm64 to r64. |
| movq r64 r/m64 | `REX.W+ 8B /r [reg=0, rm=1]` | Move r/m64 to r64. |
| movq r/m64 r64 | `REX.W+ 89 /r [reg=1, rm=0]` | Move r64 to r/m64. |
| movq r/m64 imm32 | `REX.W+ C7 /0 id [rm=0, i=1]` | Move imm32 sign extended to 64-bits to r/m64. |
| movq r/m64 xmm | `66 REX.W+ 0F 7E /r [reg=1, rm=0]` | Move quadword from xmm register to r/m64. |
| movq xmm r/m64 | `66 REX.W+ 0F 6E /r [reg=0, rm=1]` | Move quadword from r/m64 to xmm. |
| movq xmm xmm/m64 | `F3 0F 7E /r [reg=0, rm=1]` | Move quadword from xmm2/mem64 to xmm1. |
| movq xmm/m64 xmm | `66 0F D6 /r [reg=1, rm=0]` | Move quadword from xmm1 to xmm2/mem64. |

# movsbl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movsbl r32 r/m8 | `0F BE /r [reg=0, rm=1]` | Move byte to doubleword with sign-extension. |

# movsbq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movsbq r64 r/m8 | `REX.W+ 0F BE /r [reg=0, rm=1]` | Move byte to quadword with sign-extension. |

# movsbw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movsbw r16 r/m8 | `66 0F BE /r [reg=0, rm=1]` | Move byte to word with sign-extension. |

# movsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movsd xmm xmm/m64 | `F2 0F 10 /r [reg=0, rm=1]` | Move scalar double-precision floating-point value from xmm2/m64 to xmm1 register. |
| movsd xmm/m64 xmm | `F2 0F 11 /r [reg=1, rm=0]` | Move scalar double-precision floating-point value from xmm1 register to xmm2/m64. |

# movsl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movsl | `A5 []` | For legacy mode, move dword from address DS:(E)SI to ES:(E)DI. For 64-bit mode move dword from address (R\|E)SI to (R\|E)DI. |

# movswl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movswl r32 r/m16 | `0F BF /r [reg=0, rm=1]` | Move word to doubleword, with sign-extension. |

# movswq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movswq r64 r/m16 | `REX.W+ 0F BF /r [reg=0, rm=1]` | Move word to quadword with sign-extension. |

# movupd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movupd xmm xmm/m128 | `66 0F 10 /r [reg=0, rm=1]` | Move packed double-precision floating-point values from xmm2/m128 to xmm1. |
| movupd xmm/m128 xmm | `66 0F 11 /r [reg=1, rm=0]` | Move packed double-precision floating-point values from xmm1 to xmm2/m128. |

# movups
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movups xmm xmm/m128 | `0F 10 /r [reg=0, rm=1]` | Move packed single-precision floating-point values from xmm2/m128 to xmm1. |
| movups xmm/m128 xmm | `0F 11 /r [reg=1, rm=0]` | Move packed single-precision floating-point values from xmm1 to xmm2/m128. |

# movw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movw r16 imm16 | `66 B8 +rw iw [i=1, r=0]` | Move imm16 to r16. |
| movw r16 r/m16 | `66 8B /r [reg=0, rm=1]` | Move r/m16 to r16. |
| movw r/m16 r16 | `66 89 /r [reg=1, rm=0]` | Move r16 to r/m16. |
| movw r/m16 imm16 | `66 C7 /0 iw [rm=0, i=1]` | Move imm16 to r/m16. |

# movzbl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movzbl r32 r/m8 | `0F B6 /r [reg=0, rm=1]` | Move byte to doubleword, zero-extension. |

# movzbq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movzbq r64 r/m8 | `REX.W+ 0F B6 /r [reg=0, rm=1]` | Move byte to quadword, zero-extension. |

# movzbw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movzbw r16 r/m8 | `66 0F B6 /r [reg=0, rm=1]` | Move byte to word with zero-extension. |

# movzwl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movzwl r32 r/m16 | `0F B7 /r [reg=0, rm=1]` | Move word to doubleword, zero-extension. |

# movzwq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| movzwq r64 r/m16 | `REX.W+ 0F B7 /r [reg=0, rm=1]` | Move word to quadword, zero-extension. |

# mpsadbw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| mpsadbw xmm xmm/m128 imm8 | `66 0F 3A 42 /r ib [reg=0, rm=1, i=2]` | Sums absolute 8-bit integer difference of adjacent groups of 4 byte integers in xmm1 and xmm2/m128 and writes the results in xmm1. Starting offsets within xmm1 and xmm2/m128 are determined by imm8. |

# mulb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| mulb r/m8 | `F6 /4 [rm=0]` | Unsigned multiply (AX = AL * r/m8). |

# mull
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| mull r/m32 | `F7 /4 [rm=0]` | Unsigned multiply (EDX:EAX = EAX * r/m32). |

# mulpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| mulpd xmm xmm/m128 | `66 0F 59 /r [reg=0, rm=1]` | Multiply packed double-precision floating-point values in xmm2/m128 by xmm1. |

# mulps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| mulps xmm xmm/m128 | `0F 59 /r [reg=0, rm=1]` | Multiply packed single-precision floating-point values in xmm2/mem by xmm1. |

# mulq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| mulq r/m64 | `REX.W+ F7 /4 [rm=0]` | Unsigned multiply (RDX:RAX = RAX * r/m64. |

# mulsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| mulsd xmm xmm/m64 | `F2 0F 59 /r [reg=0, rm=1]` | Multiply the low double-precision floating-point value in xmm2/mem64 by low double-precision floating-point value in xmm1. |

# mulss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| mulss xmm xmm/m32 | `F3 0F 59 /r [reg=0, rm=1]` | Multiply the low single-precision floating-point value in xmm2/mem by the low single-precision floating-point value in xmm1. |

# mulw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| mulw r/m16 | `66 F7 /4 [rm=0]` | Unsigned multiply (DX:AX = AX * r/m16). |

# negb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| negb r/m8 | `F6 /3 [rm=0]` | Two's complement negate r/m8. |

# negl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| negl r/m32 | `F7 /3 [rm=0]` | Two's complement negate r/m32. |

# negq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| negq r/m64 | `REX.W+ F7 /3 [rm=0]` | Two's complement negate r/m64. |

# negw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| negw r/m16 | `66 F7 /3 [rm=0]` | Two's complement negate r/m16. |

# nop
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| nop | `90 []` | One byte no-operation instruction. |

# notb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| notb r/m8 | `F6 /2 [rm=0]` | Reverse each bit of r/m8. |

# notl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| notl r/m32 | `F7 /2 [rm=0]` | Reverse each bit of r/m32. |

# notq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| notq r/m64 | `REX.W+ F7 /2 [rm=0]` | Reverse each bit of r/m64. |

# notw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| notw r/m16 | `66 F7 /2 [rm=0]` | Reverse each bit of r/m16. |

# orb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| orb r8 r/m8 | `0A /r [reg=0, rm=1]` | r8 OR r/m8. |
| orb r/m8 r8 | `08 /r [reg=1, rm=0]` | r/m8 OR r8. |
| orb r/m8 imm8 | `80 /1 ib [rm=0, i=1]` | r/m8 OR imm8. |
| orb AL imm8 | `0C ib [i=1]` | AL OR imm8. |

# orl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| orl r32 r/m32 | `0B /r [reg=0, rm=1]` | r32 OR r/m32. |
| orl r/m32 r32 | `09 /r [reg=1, rm=0]` | r/m32 OR r32. |
| orl r/m32 imm8 | `83 /1 ib [rm=0, i=1]` | r/m32 OR imm8 (sign-extended). |
| orl r/m32 imm32 | `81 /1 id [rm=0, i=1]` | r/m32 OR imm32. |
| orl EAX imm32 | `0D id [i=1]` | EAX OR imm32. |

# orpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| orpd xmm xmm/m128 | `66 0F 56 /r [reg=0, rm=1]` | Bitwise OR of xmm2/m128 and xmm1. |

# orps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| orps xmm xmm/m128 | `0F 56 /r [reg=0, rm=1]` | Bitwise OR of xmm1 and xmm2/m128. |

# orq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| orq r64 r/m64 | `REX.W+ 0B /r [reg=0, rm=1]` | r64 OR r/m64. |
| orq r/m64 r64 | `REX.W+ 09 /r [reg=1, rm=0]` | r/m64 OR r64. |
| orq r/m64 imm8 | `REX.W+ 83 /1 ib [rm=0, i=1]` | r/m64 OR imm8 (sign-extended). |
| orq r/m64 imm32 | `REX.W+ 81 /1 id [rm=0, i=1]` | r/m64 OR imm32 (sign-extended). |
| orq RAX imm32 | `REX.W+ 0D id [i=1]` | RAX OR imm32 (sign-extended). |

# orw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| orw r16 r/m16 | `66 0B /r [reg=0, rm=1]` | r16 OR r/m16. |
| orw r/m16 r16 | `66 09 /r [reg=1, rm=0]` | r/m16 OR r16. |
| orw r/m16 imm8 | `66 83 /1 ib [rm=0, i=1]` | r/m16 OR imm8 (sign-extended). |
| orw r/m16 imm16 | `66 81 /1 iw [rm=0, i=1]` | r/m16 OR imm16. |
| orw AX imm16 | `66 0D iw [i=1]` | AX OR imm16. |

# outb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| outb imm8 AL | `E6 ib [i=0]` | Output byte in AL to I/O port address imm8. |
| outb DX AL | `EE []` | Output byte in AL to I/O port address in DX. |

# outl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| outl imm8 EAX | `E7 ib [i=0]` | Output doubleword in EAX to I/O port address imm8. |
| outl DX EAX | `EF []` | Output doubleword in EAX to I/O port address in DX. |

# outw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| outw imm8 AX | `66 E7 ib [i=0]` | Output word in AX to I/O port address imm8. |
| outw DX AX | `66 EF []` | Output word in AX to I/O port address in DX. |

# packssdw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| packssdw xmm xmm/m128 | `66 0F 6B /r [reg=0, rm=1]` | Converts 4 packed signed doubleword integers from xmm1 and from xxm2/m128 into 8 packed signed word integers in xxm1 using signed saturation. |

# packsswb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| packsswb xmm xmm/m128 | `66 0F 63 /r [reg=0, rm=1]` | Converts 8 packed signed word integers from xmm1 and from xxm2/m128 into 16 packed signed byte integers in xxm1 using signed saturation. |

# packusdw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| packusdw xmm xmm/m128 | `66 0F 38 2B /r [reg=0, rm=1]` | Convert 4 packed signed doubleword integers from xmm1 and 4 packed signed doubleword integers from xmm2/m128 into 8 packed unsigned word integers in xmm1 using unsigned saturation. |

# packuswb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| packuswb xmm xmm/m128 | `66 0F 67 /r [reg=0, rm=1]` | Converts 8 signed word integers from xmm1 and 8 signed word integers from xmm2/m128 into 16 unsigned byte integers in xmm1 using unsigned saturation. |

# paddb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| paddb xmm xmm/m128 | `66 0F FC /r [reg=0, rm=1]` | Add packed byte integers from xmm2/m128 and xmm1. |

# paddd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| paddd xmm xmm/m128 | `66 0F FE /r [reg=0, rm=1]` | Add packed doubleword integers from xmm2/m128 and xmm1. |

# paddq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| paddq xmm xmm/m128 | `66 0F D4 /r [reg=0, rm=1]` | Add packed quadword integers xmm2/m128 to xmm1. |

# paddsb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| paddsb xmm xmm/m128 | `66 0F EC /r [reg=0, rm=1]` | Add packed signed byte integers from xmm2/m128 and xmm1 saturate the results. |

# paddsw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| paddsw xmm xmm/m128 | `66 0F ED /r [reg=0, rm=1]` | Add packed signed word integers from xmm2/m128 and xmm1 and saturate the results. |

# paddusb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| paddusb xmm xmm/m128 | `66 0F DC /r [reg=0, rm=1]` | Add packed unsigned byte integers from xmm2/m128 and xmm1 saturate the results. |

# paddusw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| paddusw xmm xmm/m128 | `66 0F DD /r [reg=0, rm=1]` | Add packed unsigned word integers from xmm2/m128 to xmm1 and saturate the results. |

# paddw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| paddw xmm xmm/m128 | `66 0F FD /r [reg=0, rm=1]` | Add packed word integers from xmm2/m128 and xmm1. |

# pand
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pand xmm xmm/m128 | `66 0F DB /r [reg=0, rm=1]` | Bitwise AND of xmm2/m128 and xmm1. |

# pandn
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pandn xmm xmm/m128 | `66 0F DF /r [reg=0, rm=1]` | Bitwise AND NOT of xmm2/m128 and xmm1. |

# pause
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pause | `F3 90 []` | Gives hint to processor that improves performance of spin-wait loops. |

# pavgb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pavgb xmm xmm/m128 | `66 0F E0 /r [reg=0, rm=1]` | Average packed unsigned byte integers from xmm2/m128 and xmm1 with rounding. |

# pavgw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pavgw xmm xmm/m128 | `66 0F E3 /r [reg=0, rm=1]` | Average packed unsigned word integers from xmm2/m128 and xmm1 with rounding. |

# pblendvb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pblendvb xmm xmm/m128 <XMM0> | `66 0F 38 10 /r [reg=0, rm=1]` | Select byte values from xmm1 and xmm2/m128 from mask specified in the high bit of each byte in XMM0 and store the values into xmm1. |

# pblendw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pblendw xmm xmm/m128 imm8 | `66 0F 3A 0E /r ib [reg=0, rm=1, i=2]` | Select words from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. |

# pcmpeqb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpeqb xmm xmm/m128 | `66 0F 74 /r [reg=0, rm=1]` | Compare packed bytes in xmm2/m128 and xmm1 for equality. |

# pcmpeqd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpeqd xmm xmm/m128 | `66 0F 76 /r [reg=0, rm=1]` | Compare packed doublewords in xmm2/m128 and xmm1 for equality. |

# pcmpeqq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpeqq xmm xmm/m128 | `66 0F 38 29 /r [reg=0, rm=1]` | Compare packed qwords in xmm2/m128 and xmm1 for equality. |

# pcmpeqw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpeqw xmm xmm/m128 | `66 0F 75 /r [reg=0, rm=1]` | Compare packed words in xmm2/m128 and xmm1 for equality. |

# pcmpestri
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpestri xmm xmm/m128 imm8 | `66 0F 3A 61 /r ib [reg=0, rm=1, i=2]` | Perform a packed comparison of string data with explicit lengths, generating an index, and storing the result in ECX. |

# pcmpestrm
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpestrm xmm xmm/m128 imm8 | `66 0F 3A 60 /r ib [reg=0, rm=1, i=2]` | Perform a packed comparison of string data with explicit lengths, generating a mask, and storing the result in XMM0 |

# pcmpgtb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpgtb xmm xmm/m128 | `66 0F 64 /r [reg=0, rm=1]` | Compare packed signed byte integers in xmm1 and xmm2/m128 for greater than. |

# pcmpgtd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpgtd xmm xmm/m128 | `66 0F 66 /r [reg=0, rm=1]` | Compare packed signed doubleword integers in xmm1 and xmm2/m128 for greater than. |

# pcmpgtq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpgtq xmm xmm/m128 | `66 0F 38 37 /r [reg=0, rm=1]` | Compare packed signed qwords in xmm2/m128 and xmm1 for greater than. |

# pcmpgtw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpgtw xmm xmm/m128 | `66 0F 65 /r [reg=0, rm=1]` | Compare packed signed word integers in xmm1 and xmm2/m128 for greater than. |

# pcmpistri
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpistri xmm xmm/m128 imm8 | `66 0F 3A 63 /r ib [reg=0, rm=1, i=2]` | Perform a packed comparison of string data with implicit lengths, generating an index, and storing the result in ECX. |

# pcmpistrm
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pcmpistrm xmm xmm/m128 imm8 | `66 0F 3A 62 /r ib [reg=0, rm=1, i=2]` | Perform a packed comparison of string data with implicit lengths, generating a mask, and storing the result in XMM0. |

# pextrb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pextrb reg/m8 xmm imm8 | `66 0F 3A 14 /r ib [reg=1, rm=0, i=2]` | Extract a byte integer value from xmm2 at the source byte offset specified by imm8 into rreg or m8. The upper bits of r32 or r64 are zeroed. |

# pextrd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pextrd r/m32 xmm imm8 | `66 0F 3A 16 /r ib [reg=1, rm=0, i=2]` | Extract a dword integer value from xmm2 at the source dword offset specified by imm8 into r/m32. |

# pextrq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pextrq r/m64 xmm imm8 | `66 REX.W+ 0F 3A 16 /r ib [reg=1, rm=0, i=2]` | Extract a qword integer value from xmm2 at the source qword offset specified by imm8 into r/m64. |

# pextrw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pextrw reg xmm imm8 | `66 0F C5 /r ib [reg=0, rm=1, i=2]` | Extract the word specified by imm8 from xmm and move it to reg, bits 15-0. The upper bits of r32 or r64 is zeroed. |
| pextrw reg/m16 xmm imm8 | `66 0F 3A 15 /r ib [reg=1, rm=0, i=2]` | Extract the word specified by imm8 from xmm and copy it to lowest 16 bits of reg or m16. Zero-extend the result in the destination, r32 or r64. |

# phminposuw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| phminposuw xmm xmm/m128 | `66 0F 38 41 /r [reg=0, rm=1]` | Find the minimum unsigned word in xmm2/m128 and place its value in the low word of xmm1 and its index in the second-lowest word of xmm1. |

# pinsrb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pinsrb xmm r32/m8 imm8 | `66 0F 3A 20 /r ib [reg=0, rm=1, i=2]` | Insert a byte integer value from r32/m8 into xmm1 at the destination element in xmm1 specified by imm8. |

# pinsrd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pinsrd xmm r/m32 imm8 | `66 0F 3A 22 /r ib [reg=0, rm=1, i=2]` | Insert a dword integer value from r/m32 into the xmm1 at the destination element specified by imm8. |

# pinsrw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pinsrw xmm r32/m16 imm8 | `66 0F C4 /r ib [reg=0, rm=1, i=2]` | Move the low word of r32 or from m16 into xmm at the word position specified by imm8. |

# pmaddwd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmaddwd xmm xmm/m128 | `66 0F F5 /r [reg=0, rm=1]` | Multiply the packed word integers in xmm1 by the packed word integers in xmm2/m128, add adjacent doubleword results, and store in xmm1. |

# pmaxsb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmaxsb xmm xmm/m128 | `66 0F 38 3C /r [reg=0, rm=1]` | Compare packed signed byte integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. |

# pmaxsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmaxsd xmm xmm/m128 | `66 0F 38 3D /r [reg=0, rm=1]` | Compare packed signed dword integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. |

# pmaxsw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmaxsw xmm xmm/m128 | `66 0F EE /r [reg=0, rm=1]` | Compare signed word integers in xmm2/m128 and xmm1 and return maximum values. |

# pmaxub
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmaxub xmm xmm/m128 | `66 0F DE /r [reg=0, rm=1]` | Compare unsigned byte integers in xmm2/m128 and xmm1 and returns maximum values. |

# pmaxud
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmaxud xmm xmm/m128 | `66 0F 38 3F /r [reg=0, rm=1]` | Compare packed unsigned dword integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. |

# pmaxuw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmaxuw xmm xmm/m128 | `66 0F 38 3E /r [reg=0, rm=1]` | Compare packed unsigned word integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. |

# pminsb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pminsb xmm xmm/m128 | `66 0F 38 /r [reg=0, rm=1]` | Compare packed signed byte integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. |

# pminsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pminsd xmm xmm/m128 | `66 0F 38 39 /r [reg=0, rm=1]` | Compare packed signed dword integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. |

# pminsw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pminsw xmm xmm/m128 | `66 0F EA /r [reg=0, rm=1]` | Compare signed word integers in xmm2/m128 and xmm1 and return minimum values. |

# pminub
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pminub xmm xmm/m128 | `66 0F DA /r [reg=0, rm=1]` | Compare unsigned byte integers in xmm2/m128 and xmm1 and returns minimum values. |

# pminud
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pminud xmm xmm/m128 | `66 0F 38 3B /r [reg=0, rm=1]` | Compare packed unsigned dword integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. |

# pminuw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pminuw xmm xmm/m128 | `66 0F 38 3A /r [reg=0, rm=1]` | Compare packed unsigned word integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. |

# pmovmskb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovmskb reg xmm | `66 0F D7 /r [reg=0, rm=1]` | Move a byte mask of xmm to reg. The upper bits of r32 or r64 are zeroed |

# pmovsxbd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovsxbd xmm xmm/m32 | `66 0F 38 21 /r [reg=0, rm=1]` | Sign extend 4 packed signed 8-bit integers in the low 4 bytes of xmm2/m32 to 4 packed signed 32-bit integers in xmm1. |

# pmovsxbq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovsxbq xmm xmm/m16 | `66 0F 38 22 /r [reg=0, rm=1]` | Sign extend 2 packed signed 8-bit integers in the low 2 bytes of xmm2/m16 to 2 packed signed 64-bit integers in xmm1. |

# pmovsxbw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovsxbw xmm xmm/m64 | `66 0F 38 20 /r [reg=0, rm=1]` | Sign extend 8 packed signed 8-bit integers in the low 8 bytes of xmm2/m64 to 8 packed signed 16-bit integers in xmm1. |

# pmovsxdq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovsxdq xmm xmm/m64 | `66 0F 38 25 /r [reg=0, rm=1]` | Sign extend 2 packed signed 32-bit integers in the low 8 bytes of xmm2/m64 to 2 packed signed 64-bit integers in xmm1. |

# pmovsxwd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovsxwd xmm xmm/m64 | `66 0F 38 23 /r [reg=0, rm=1]` | Sign extend 4 packed signed 16-bit integers in the low 8 bytes of xmm2/m64 to 4 packed signed 32-bit integers in xmm1. |

# pmovsxwq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovsxwq xmm xmm/m32 | `66 0F 38 24 /r [reg=0, rm=1]` | Sign extend 2 packed signed 16-bit integers in the low 4 bytes of xmm2/m32 to 2 packed signed 64-bit integers in xmm1. |

# pmovzxbd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovzxbd xmm xmm/m32 | `66 0F 38 31 /r [reg=0, rm=1]` | Zero extend 4 packed 8-bit integers in the low 4 bytes of xmm2/m32 to 4 packed 32-bit integers in xmm1. |

# pmovzxbq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovzxbq xmm xmm/m16 | `66 0F 38 32 /r [reg=0, rm=1]` | Zero extend 2 packed 8-bit integers in the low 2 bytes of xmm2/m16 to 2 packed 64-bit integers in xmm1. |

# pmovzxbw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovzxbw xmm xmm/m64 | `66 0F 38 30 /r [reg=0, rm=1]` | Zero extend 8 packed 8-bit integers in the low 8 bytes of xmm2/m64 to 8 packed 16-bit integers in xmm1. |

# pmovzxdq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovzxdq xmm xmm/m64 | `66 0F 38 35 /r [reg=0, rm=1]` | Zero extend 2 packed 32-bit integers in the low 8 bytes of xmm2/m64 to 2 packed 64-bit integers in xmm1. |

# pmovzxwd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovzxwd xmm xmm/m64 | `66 0F 38 33 /r [reg=0, rm=1]` | Zero extend 4 packed 16-bit integers in the low 8 bytes of xmm2/m64 to 4 packed 32-bit integers in xmm1. |

# pmovzxwq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmovzxwq xmm xmm/m32 | `66 0F 38 34 /r [reg=0, rm=1]` | Zero extend 2 packed 16-bit integers in the low 4 bytes of xmm2/m32 to 2 packed 64-bit integers in xmm1. |

# pmuldq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmuldq xmm xmm/m128 | `66 0F 38 28 /r [reg=0, rm=1]` | Multiply the packed signed dword integers in xmm1 and xmm2/m128 and store the quadword product in xmm1. |

# pmulhuw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmulhuw xmm xmm/m128 | `66 0F E4 /r [reg=0, rm=1]` | Multiply the packed unsigned word integers in xmm1 and xmm2/m128, and store the high 16 bits of the results in xmm1. |

# pmulhw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmulhw xmm xmm/m128 | `66 0F E5 /r [reg=0, rm=1]` | Multiply the packed signed word integers in xmm1 and xmm2/m128, and store the high 16 bits of the results in xmm1. |

# pmulld
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmulld xmm xmm/m128 | `66 0F 38 40 /r [reg=0, rm=1]` | Multiply the packed dword signed integers in xmm1 and xmm2/m128 and store the low 32 bits of each product in xmm1. |

# pmullw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmullw xmm xmm/m128 | `66 0F D5 /r [reg=0, rm=1]` | Multiply the packed signed word integers in xmm1 and xmm2/m128, and store the low 16 bits of the results in xmm1. |

# pmuludq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pmuludq xmm xmm/m128 | `66 0F F4 /r [reg=0, rm=1]` | Multiply packed unsigned doubleword integers in xmm1 by packed unsigned doubleword integers in xmm2/m128, and store the quadword results in xmm1. |

# popfq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| popfq | `9D []` | Pop top of stack and zero-extend into RFLAGS. |

# popfw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| popfw | `66 9D []` | Pop top of stack into lower 16 bits of EFLAGS. |

# popq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| popq r64 | `58 +ro [r=0]` | Pop top of stack into r64; increment stack pointer. Cannot encode 32-bit operand size. |
| popq r/m64 | `8F /0 [rm=0]` | Pop top of stack into m64; increment stack pointer. Cannot encode 32-bit operand size. |

# popw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| popw r16 | `66 58 +rw [r=0]` | Pop top of stack into r16; increment stack pointer. |
| popw r/m16 | `66 8F /0 [rm=0]` | Pop top of stack into m16; increment stack pointer. |

# por
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| por xmm xmm/m128 | `66 0F EB /r [reg=0, rm=1]` | Bitwise OR of xmm2/m128 and xmm1. |

# psadbw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psadbw xmm xmm/m128 | `66 0F F6 /r [reg=0, rm=1]` | Computes the absolute differences of the packed unsigned byte integers from xmm2 /m128 and xmm1; the 8 low differences and 8 high differences are then summed separately to produce two unsigned word integer results. |

# pshufd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pshufd xmm xmm/m128 imm8 | `66 0F 70 /r ib [reg=0, rm=1, i=2]` | Shuffle the doublewords in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. |

# pshufhw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pshufhw xmm xmm/m128 imm8 | `F3 0F 70 /r ib [reg=0, rm=1, i=2]` | Shuffle the high words in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. |

# pshuflw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pshuflw xmm xmm/m128 imm8 | `F2 0F 70 /r ib [reg=0, rm=1, i=2]` | Shuffle the low words in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. |

# pslld
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pslld xmm imm8 | `66 0F 72 /6 ib [rm=0, i=1]` | Shift doublewords in xmm1 left by imm8 while shifting in 0s. |
| pslld xmm xmm/m128 | `66 0F F2 /r [reg=0, rm=1]` | Shift doublewords in xmm1 left by xmm2/m128 while shifting in 0s. |

# pslldq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pslldq xmm imm8 | `66 0F 73 /7 ib [rm=0, i=1]` | Shift xmm1 left by imm8 bytes while shifting in 0s. |

# psllq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psllq xmm imm8 | `66 0F 73 /6 ib [rm=0, i=1]` | Shift quadwords in xmm1 left by imm8 while shifting in 0s. |
| psllq xmm xmm/m128 | `66 0F F3 /r [reg=0, rm=1]` | Shift quadwords in xmm1 left by xmm2/m128 while shifting in 0s. |

# psllw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psllw xmm imm8 | `66 0F 71 /6 ib [rm=0, i=1]` | Shift words in xmm1 left by imm8 while shifting in 0s. |
| psllw xmm xmm/m128 | `66 0F F1 /r [reg=0, rm=1]` | Shift words in xmm1 left by xmm2/m128 while shifting in 0s. |

# psrad
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psrad xmm imm8 | `66 0F 72 /4 ib [rm=0, i=1]` | Shift doublewords in xmm1 right by imm8 while shifting in sign bits. |
| psrad xmm xmm/m128 | `66 0F E2 /r [reg=0, rm=1]` | Shift doubleword in xmm1 right by xmm2 /m128 while shifting in sign bits. |

# psraw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psraw xmm imm8 | `66 0F 71 /4 ib [rm=0, i=1]` | Shift words in xmm1 right by imm8 while shifting in sign bits |
| psraw xmm xmm/m128 | `66 0F E1 /r [reg=0, rm=1]` | Shift words in xmm1 right by xmm2/m128 while shifting in sign bits. |

# psrld
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psrld xmm imm8 | `66 0F 72 /2 ib [rm=0, i=1]` | Shift doublewords in xmm1 right by imm8 while shifting in 0s. |
| psrld xmm xmm/m128 | `66 0F D2 /r [reg=0, rm=1]` | Shift doublewords in xmm1 right by amount specified in xmm2 /m128 while shifting in 0s. |

# psrldq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psrldq xmm imm8 | `66 0F 73 /3 ib [rm=0, i=1]` | Shift xmm1 right by imm8 while shifting in 0s. |

# psrlq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psrlq xmm imm8 | `66 0F 73 /2 ib [rm=0, i=1]` | Shift quadwords in xmm1 right by imm8 while shifting in 0s. |
| psrlq xmm xmm/m128 | `66 0F D3 /r [reg=0, rm=1]` | Shift quadwords in xmm1 right by amount specified in xmm2/m128 while shifting in 0s. |

# psrlw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psrlw xmm imm8 | `66 0F 71 /2 ib [rm=0, i=1]` | Shift words in xmm1 right by imm8 while shifting in 0s. |
| psrlw xmm xmm/m128 | `66 0F D1 /r [reg=0, rm=1]` | Shift words in xmm1 right by amount specified in xmm2/m128 while shifting in 0s. |

# psubb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psubb xmm xmm/m128 | `66 0F F8 /r [reg=0, rm=1]` | Subtract packed byte integers in xmm2/m128 from packed byte integers in xmm1. |

# psubd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psubd xmm xmm/m128 | `66 0F FA /r [reg=0, rm=1]` | Subtract packed doubleword integers in xmm2/mem128 from packed doubleword integers in xmm1. |

# psubq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psubq xmm xmm/m128 | `66 0F FB /r [reg=0, rm=1]` | Subtract packed quadword integers in xmm1 from xmm2 /m128. |

# psubsb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psubsb xmm xmm/m128 | `66 0F E8 /r [reg=0, rm=1]` | Subtract packed signed byte integers in xmm2/m128 from packed signed byte integers in xmm1 and saturate results. |

# psubsw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psubsw xmm xmm/m128 | `66 0F E9 /r [reg=0, rm=1]` | Subtract packed signed word integers in xmm2/m128 from packed signed word integers in xmm1 and saturate results. |

# psubusb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psubusb xmm xmm/m128 | `66 0F D8 /r [reg=0, rm=1]` | Subtract packed unsigned byte integers in xmm2/m128 from packed unsigned byte integers in xmm1 and saturate result. |

# psubusw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psubusw xmm xmm/m128 | `66 0F D9 /r [reg=0, rm=1]` | Subtract packed unsigned word integers in xmm2/m128 from packed unsigned word integers in xmm1 and saturate result. |

# psubw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| psubw xmm xmm/m128 | `66 0F F9 /r [reg=0, rm=1]` | Subtract packed word integers in xmm2/m128 from packed word integers in xmm1. |

# ptest
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| ptest xmm xmm/m128 | `66 0F 38 17 /r [reg=0, rm=1]` | Set ZF if xmm2/m128 AND xmm1 result is all 0s. Set CF if xmm2/m128 AND NOT xmm1 result is all 0s. |

# punpckhbw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| punpckhbw xmm xmm/m128 | `66 0F 68 /r [reg=0, rm=1]` | Unpack and interleave high-order bytes from xmm1 and xmm2/m128 into xmm1. |

# punpckhdq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| punpckhdq xmm xmm/m128 | `66 0F 6A /r [reg=0, rm=1]` | Unpack and interleave high-order doublewords from xmm1 and xmm2/m128 into xmm1. |

# punpckhqdq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| punpckhqdq xmm xmm/m128 | `66 0F 6D /r [reg=0, rm=1]` | Unpack and interleave high-order quadwords from xmm1 and xmm2/m128 into xmm1. |

# punpckhwd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| punpckhwd xmm xmm/m128 | `66 0F 69 /r [reg=0, rm=1]` | Unpack and interleave high-order words from xmm1 and xmm2/m128 into xmm1. |

# punpcklbw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| punpcklbw xmm xmm/m128 | `66 0F 60 /r [reg=0, rm=1]` | Interleave low-order bytes from xmm1 and xmm2/m128 into xmm1. |

# punpckldq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| punpckldq xmm xmm/m128 | `66 0F 62 /r [reg=0, rm=1]` | Interleave low-order doublewords from xmm1 and xmm2/m128 into xmm1. |

# punpcklqdq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| punpcklqdq xmm xmm/m128 | `66 0F 6C /r [reg=0, rm=1]` | Interleave low-order quadword from xmm1 and xmm2/m128 into xmm1 register. |

# punpcklwd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| punpcklwd xmm xmm/m128 | `66 0F 61 /r [reg=0, rm=1]` | Interleave low-order words from xmm1 and xmm2/m128 into xmm1. |

# pushfq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pushfq | `9C []` | Push RFLAGS. |

# pushfw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pushfw | `66 9C []` | Push lower 16 bits of EFLAGS. |

# pushq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pushq r64 | `50 +ro [r=0]` | Push r64. |
| pushq imm8 | `6A ib [i=0]` | Push imm8 (sign-extended to 64-bits). |
| pushq imm16 | `66 68 iw [i=0]` | Push imm16 (sign-extended to 64-bits). |
| pushq imm32 | `68 id [i=0]` | Push imm32 (sign-extended to 64-bits). |
| pushq r/m64 | `FF /6 [rm=0]` | Push r/m64. |

# pushw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pushw r16 | `66 50 +rw [r=0]` | Push r16. |
| pushw r/m16 | `66 FF /6 [rm=0]` | Push r/m16. |

# pxor
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| pxor xmm xmm/m128 | `66 0F EF /r [reg=0, rm=1]` | Bitwise XOR of xmm2/m128 and xmm1. |

# rclb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rclb r/m8 imm8 | `C0 /2 ib [rm=0, i=1]` | Rotate 9 bits (CF, r/m8) left imm8 times. |
| rclb r/m8 1 | `D0 /2 [rm=0]` | Rotate 9 bits (CF, r/m8) left once. |
| rclb r/m8 CL | `D2 /2 [rm=0]` | Rotate 9 bits (CF, r/m8) left CL times. |

# rcll
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rcll r/m32 imm8 | `C1 /2 ib [rm=0, i=1]` | Rotate 33 bits (CF, r/m32) left imm8 times. |
| rcll r/m32 1 | `D1 /2 [rm=0]` | Rotate 33 bits (CF, r/m32) left once. |
| rcll r/m32 CL | `D3 /2 [rm=0]` | Rotate 33 bits (CF, r/m32) left CL times. |

# rclq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rclq r/m64 imm8 | `REX.W+ C1 /2 ib [rm=0, i=1]` | Rotate 65 bits (CF, r/m64) left imm8 times. Uses a 6 bit count. |
| rclq r/m64 1 | `REX.W+ D1 /2 [rm=0]` | Rotate 65 bits (CF, r/m64) left once. Uses a 6 bit count. |
| rclq r/m64 CL | `REX.W+ D3 /2 [rm=0]` | Rotate 65 bits (CF, r/m64) left CL times. Uses a 6 bit count. |

# rclw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rclw r/m16 imm8 | `66 C1 /2 ib [rm=0, i=1]` | Rotate 17 bits (CF, r/m16) left imm8 times. |
| rclw r/m16 1 | `66 D1 /2 [rm=0]` | Rotate 17 bits (CF, r/m16) left once. |
| rclw r/m16 CL | `66 D3 /2 [rm=0]` | Rotate 17 bits (CF, r/m16) left CL times. |

# rcpps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rcpps xmm xmm/m128 | `0F 53 /r [reg=0, rm=1]` | Computes the approximate reciprocals of the packed single-precision floating-point values in xmm2/m128 and stores the results in xmm1. |

# rcpss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rcpss xmm xmm/m32 | `F3 0F 53 /r [reg=0, rm=1]` | Computes the approximate reciprocal of the scalar single-precision floating-point value in xmm2/m32 and stores the result in xmm1. |

# rcrb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rcrb r/m8 imm8 | `C0 /3 ib [rm=0, i=1]` | Rotate 9 bits (CF, r/m8) right imm8 times. |
| rcrb r/m8 1 | `D0 /3 [rm=0]` | Rotate 9 bits (CF, r/m8) right once. |
| rcrb r/m8 CL | `D2 /3 [rm=0]` | Rotate 9 bits (CF, r/m8) right CL times. |

# rcrl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rcrl r/m32 imm8 | `C1 /3 ib [rm=0, i=1]` | Rotate 33 bits (CF, r/m32) right imm8 times. |
| rcrl r/m32 1 | `D1 /3 [rm=0]` | Rotate 33 bits (CF, r/m32) right once. Uses a 6 bit count. |
| rcrl r/m32 CL | `D3 /3 [rm=0]` | Rotate 33 bits (CF, r/m32) right CL times. |

# rcrq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rcrq r/m64 imm8 | `REX.W+ C1 /3 ib [rm=0, i=1]` | Rotate 65 bits (CF, r/m64) right imm8 times. Uses a 6 bit count. |
| rcrq r/m64 1 | `REX.W+ D1 /3 [rm=0]` | Rotate 65 bits (CF, r/m64) right once. Uses a 6 bit count. |
| rcrq r/m64 CL | `REX.W+ D3 /3 [rm=0]` | Rotate 65 bits (CF, r/m64) right CL times. Uses a 6 bit count. |

# rcrw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rcrw r/m16 imm8 | `66 C1 /3 ib [rm=0, i=1]` | Rotate 17 bits (CF, r/m16) right imm8 times. |
| rcrw r/m16 1 | `66 D1 /3 [rm=0]` | Rotate 17 bits (CF, r/m16) right once. |
| rcrw r/m16 CL | `66 D3 /3 [rm=0]` | Rotate 17 bits (CF, r/m16) right CL times. |

# retq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| retq | `C3 []` | Near return to calling procedure. |
| retq imm16 | `C2 iw [i=0]` | Near return to calling procedure and pop imm16 bytes from stack. |

# rolb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rolb r/m8 imm8 | `C0 /0 ib [rm=0, i=1]` | Rotate 8 bits r/m8 left imm8 times. |
| rolb r/m8 1 | `D0 /0 [rm=0]` | Rotate 8 bits r/m8 left once. |
| rolb r/m8 CL | `D2 /0 [rm=0]` | Rotate 8 bits r/m8 left CL times. |

# roll
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| roll r/m32 imm8 | `C1 /0 ib [rm=0, i=1]` | Rotate 32 bits r/m32 left imm8 times. |
| roll r/m32 1 | `D1 /0 [rm=0]` | Rotate 32 bits r/m32 left once. |
| roll r/m32 CL | `D3 /0 [rm=0]` | Rotate 32 bits r/m32 left CL times. |

# rolq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rolq r/m64 imm8 | `REX.W+ C1 /0 ib [rm=0, i=1]` | Rotate 64 bits r/m64 left imm8 times. Uses a 6 bit count. |
| rolq r/m64 1 | `REX.W+ D1 /0 [rm=0]` | Rotate 64 bits r/m64 left once. Uses a 6 bit count. |
| rolq r/m64 CL | `REX.W+ D3 /0 [rm=0]` | Rotate 64 bits r/m64 left CL times. Uses a 6 bit count. |

# rolw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rolw r/m16 imm8 | `66 C1 /0 ib [rm=0, i=1]` | Rotate 16 bits r/m16 left imm8 times. |
| rolw r/m16 1 | `66 D1 /0 [rm=0]` | Rotate 16 bits r/m16 left once. |
| rolw r/m16 CL | `66 D3 /0 [rm=0]` | Rotate 16 bits r/m16 left CL times. |

# rorb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rorb r/m8 imm8 | `C0 /1 ib [rm=0, i=1]` | Rotate 8 bits r/m16 right imm8 times. |
| rorb r/m8 1 | `D0 /1 [rm=0]` | Rotate 8 bits r/m8 right once. |
| rorb r/m8 CL | `D2 /1 [rm=0]` | Rotate 8 bits r/m8 right CL times. |

# rorl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rorl r/m32 imm8 | `C1 /1 ib [rm=0, i=1]` | Rotate 32 bits r/m32 right imm8 times. |
| rorl r/m32 1 | `D1 /1 [rm=0]` | Rotate 32 bits r/m32 right once. |
| rorl r/m32 CL | `D3 /1 [rm=0]` | Rotate 32 bits r/m32 right CL times. |

# rorq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rorq r/m64 imm8 | `REX.W+ C1 /1 ib [rm=0, i=1]` | Rotate 64 bits r/m64 right imm8 times. Uses a 6 bit count. |
| rorq r/m64 1 | `REX.W+ D1 /1 [rm=0]` | Rotate 64 bits r/m64 right once. Uses a 6 bit count. |
| rorq r/m64 CL | `REX.W+ D3 /1 [rm=0]` | Rotate 64 bits r/m64 right CL times. Uses a 6 bit count. |

# rorw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rorw r/m16 imm8 | `66 C1 /1 ib [rm=0, i=1]` | Rotate 16 bits r/m16 right imm8 times. |
| rorw r/m16 1 | `66 D1 /1 [rm=0]` | Rotate 16 bits r/m16 right once. |
| rorw r/m16 CL | `66 D3 /1 [rm=0]` | Rotate 16 bits r/m16 right CL times. |

# roundpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| roundpd xmm xmm/m128 imm8 | `66 0F 3A 09 /r ib [reg=0, rm=1, i=2]` | Round packed double precision floating-point values in xmm2/m128 and place the result in xmm1. The rounding mode is determined by imm8. |

# roundps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| roundps xmm xmm/m128 imm8 | `66 0F 3A 08 /r ib [reg=0, rm=1, i=2]` | Round packed single precision floating-point values in xmm2/m128 and place the result in xmm1. The rounding mode is determined by imm8. |

# roundsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| roundsd xmm xmm/m64 imm8 | `66 0F 3A 0B /r ib [reg=0, rm=1, i=2]` | Round the low packed double precision floating-point value in xmm2/m64 and place the result in xmm1. The rounding mode is determined by imm8. |

# roundss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| roundss xmm xmm/m32 imm8 | `66 0F 3A 0A /r ib [reg=0, rm=1, i=2]` | Round the low packed single precision floating-point value in xmm2/m32 and place the result in xmm1. The rounding mode is determined by imm8. |

# rsqrtps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rsqrtps xmm xmm/m128 | `0F 52 /r [reg=0, rm=1]` | Computes the approximate reciprocals of the square roots of the packed single-precision floating-point values in xmm2/m128 and stores the results in xmm1. |

# rsqrtss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| rsqrtss xmm xmm/m32 | `F3 0F 52 /r [reg=0, rm=1]` | Computes the approximate reciprocal of the square root of the low single-precision floating-point value in xmm2/m32 and stores the results in xmm1. |

# salb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| salb r/m8 imm8 | `C0 /4 ib [rm=0, i=1]` | Multiply r/m8 by 2, imm8 times. |
| salb r/m8 1 | `D0 /4 [rm=0]` | Multiply r/m8 by 2, once. |
| salb r/m8 CL | `D2 /4 [rm=0]` | Multiply r/m8 by 2, CL times. |

# sall
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sall r/m32 imm8 | `C1 /4 ib [rm=0, i=1]` | Multiply r/m32 by 2, imm8 times. |
| sall r/m32 1 | `D1 /4 [rm=0]` | Multiply r/m32 by 2, once. |
| sall r/m32 CL | `D3 /4 [rm=0]` | Multiply r/m32 by 2, CL times. |

# salq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| salq r/m64 imm8 | `REX.W+ C1 /4 ib [rm=0, i=1]` | Multiply r/m64 by 2, imm8 times. |
| salq r/m64 1 | `REX.W+ D1 /4 [rm=0]` | Multiply r/m64 by 2, once. |
| salq r/m64 CL | `REX.W+ D3 /4 [rm=0]` | Multiply r/m64 by 2, CL times. |

# salw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| salw r/m16 imm8 | `66 C1 /4 ib [rm=0, i=1]` | Multiply r/m16 by 2, imm8 times. |
| salw r/m16 1 | `66 D1 /4 [rm=0]` | Multiply r/m16 by 2, once. |
| salw r/m16 CL | `66 D3 /4 [rm=0]` | Multiply r/m16 by 2, CL times. |

# sarb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sarb r/m8 imm8 | `C0 /7 ib [rm=0, i=1]` | Signed divide r/m8 by 2, imm8 time. |
| sarb r/m8 1 | `D0 /7 [rm=0]` | Signed divide r/m8 by 2, once. |
| sarb r/m8 CL | `D2 /7 [rm=0]` | Signed divide r/m8 by 2, CL times. |

# sarl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sarl r/m32 imm8 | `C1 /7 ib [rm=0, i=1]` | Signed divide r/m32 by 2, imm8 times. |
| sarl r/m32 1 | `D1 /7 [rm=0]` | Signed divide r/m32 by 2, once. |
| sarl r/m32 CL | `D3 /7 [rm=0]` | Signed divide r/m32 by 2, CL times. |

# sarq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sarq r/m64 imm8 | `REX.W+ C1 /7 ib [rm=0, i=1]` | Signed divide r/m32 by 2, imm8 times. |
| sarq r/m64 1 | `REX.W+ D1 /7 [rm=0]` | Signed divide r/m32 by 2, once. |
| sarq r/m64 CL | `REX.W+ D3 /7 [rm=0]` | Signed divide r/m32 by 2, CL times. |

# sarw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sarw r/m16 imm8 | `66 C1 /7 ib [rm=0, i=1]` | Signed divide r/m16 by 2, imm8 times. |
| sarw r/m16 1 | `66 D1 /7 [rm=0]` | Signed divide r/m16 by 2, once. |
| sarw r/m16 CL | `66 D3 /7 [rm=0]` | Signed divide r/m16 by 2, CL times. |

# sbbb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sbbb r8 r/m8 | `1A /r [reg=0, rm=1]` | Subtract with borrow r/m8 from r8. |
| sbbb r/m8 r8 | `18 /r [reg=1, rm=0]` | Subtract with borrow r8 from r/m8. |
| sbbb r/m8 imm8 | `80 /3 ib [rm=0, i=1]` | Subtract with borrow imm8 from r/m8. |
| sbbb AL imm8 | `1C ib [i=1]` | Subtract with borrow imm8 from AL. |

# sbbl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sbbl r32 r/m32 | `1B /r [reg=0, rm=1]` | Subtract with borrow r/m32 from r32. |
| sbbl r/m32 r32 | `19 /r [reg=1, rm=0]` | Subtract with borrow r32 from r/m32. |
| sbbl r/m32 imm8 | `83 /3 ib [rm=0, i=1]` | Subtract with borrow sign-extended imm8 from r/m32. |
| sbbl r/m32 imm32 | `81 /3 id [rm=0, i=1]` | Subtract with borrow imm32 from r/m32. |
| sbbl EAX imm32 | `1D id [i=1]` | Subtract with borrow imm32 from EAX. |

# sbbq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sbbq r64 r/m64 | `REX.W+ 1B /r [reg=0, rm=1]` | Subtract with borrow r/m64 from r64. |
| sbbq r/m64 r64 | `REX.W+ 19 /r [reg=1, rm=0]` | Subtract with borrow r64 from r/m64. |
| sbbq r/m64 imm8 | `REX.W+ 83 /3 ib [rm=0, i=1]` | Subtract with borrow sign-extended imm8 from r/m64. |
| sbbq r/m64 imm32 | `REX.W+ 81 /3 id [rm=0, i=1]` | Subtract with borrow sign-extended imm32 to 64-bits from r/m64. |
| sbbq RAX imm32 | `REX.W+ 1D id [i=1]` | Subtract with borrow sign-extended imm.32 to 64-bits from RAX. |

# sbbw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sbbw r16 r/m16 | `66 1B /r [reg=0, rm=1]` | Subtract with borrow r/m16 from r16. |
| sbbw r/m16 r16 | `66 19 /r [reg=1, rm=0]` | Subtract with borrow r16 from r/m16. |
| sbbw r/m16 imm8 | `66 83 /3 ib [rm=0, i=1]` | Subtract with borrow sign-extended imm8 from r/m16. |
| sbbw r/m16 imm16 | `66 81 /3 iw [rm=0, i=1]` | Subtract with borrow imm16 from r/m16. |
| sbbw AX imm16 | `66 1D iw [i=1]` | Subtract with borrow imm16 from AX. |

# seta
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| seta r/m8 | `0F 97 /0 [rm=0]` | Set byte if above (CF=0 and ZF=0). |

# setae
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setae r/m8 | `0F 93 /0 [rm=0]` | Set byte if above or equal (CF=0). |

# setb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setb r/m8 | `0F 92 /0 [rm=0]` | Set byte if below (CF=1). |

# setbe
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setbe r/m8 | `0F 96 /0 [rm=0]` | Set byte if below or equal (CF=1 or ZF=1). |

# setc
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setc r/m8 | `0F 92 /0 [rm=0]` | Set byte if carry (CF=1). |

# sete
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sete r/m8 | `0F 94 /0 [rm=0]` | Set byte if equal (ZF=1). |

# setg
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setg r/m8 | `0F 9F /0 [rm=0]` | Set byte if greater (ZF=0 and SF=OF). |

# setge
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setge r/m8 | `0F 9D /0 [rm=0]` | Set byte if greater or equal (SF=OF). |

# setl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setl r/m8 | `0F 9C /0 [rm=0]` | Set byte if less (SF!= OF). |

# setle
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setle r/m8 | `0F 9E /0 [rm=0]` | Set byte if less or equal (ZF=1 or SF!= OF). |

# setna
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setna r/m8 | `0F 96 /0 [rm=0]` | Set byte if not above (CF=1 or ZF=1). |

# setnae
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setnae r/m8 | `0F 92 /0 [rm=0]` | Set byte if not above or equal (CF=1). |

# setnb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setnb r/m8 | `0F 93 /0 [rm=0]` | Set byte if not below (CF=0). |

# setnbe
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setnbe r/m8 | `0F 97 /0 [rm=0]` | Set byte if not below or equal (CF=0 and ZF=0). |

# setnc
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setnc r/m8 | `0F 93 /0 [rm=0]` | Set byte if not carry (CF=0). |

# setne
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setne r/m8 | `0F 95 /0 [rm=0]` | Set byte if not equal (ZF=0). |

# setng
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setng r/m8 | `0F 9E /0 [rm=0]` | Set byte if not greater (ZF=1 or SF!= OF) |

# setnge
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setnge r/m8 | `0F 9C /0 [rm=0]` | Set byte if not greater or equal (SF!= OF). |

# setnl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setnl r/m8 | `0F 9D /0 [rm=0]` | Set byte if not less (SF=OF). |

# setnle
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setnle r/m8 | `0F 9F /0 [rm=0]` | Set byte if not less or equal (ZF=0 and SF=OF). |

# setno
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setno r/m8 | `0F 91 /0 [rm=0]` | Set byte if not overflow (OF=0). |

# setnp
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setnp r/m8 | `0F 9B /0 [rm=0]` | Set byte if not parity (PF=0). |

# setns
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setns r/m8 | `0F 99 /0 [rm=0]` | Set byte if not sign (SF=0). |

# setnz
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setnz r/m8 | `0F 95 /0 [rm=0]` | Set byte if not zero (ZF=0). |

# seto
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| seto r/m8 | `0F 90 /0 [rm=0]` | Set byte if overflow (OF=1) |

# setp
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setp r/m8 | `0F 9A /0 [rm=0]` | Set byte if parity (PF=1). |

# setpe
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setpe r/m8 | `0F 9A /0 [rm=0]` | Set byte if parity even (PF=1). |

# setpo
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setpo r/m8 | `0F 9B /0 [rm=0]` | Set byte if parity odd (PF=0). |

# sets
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sets r/m8 | `0F 98 /0 [rm=0]` | Set byte if sign (SF=1). |

# setz
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| setz r/m8 | `0F 94 /0 [rm=0]` | Set byte if zero (ZF=1). |

# sfence
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sfence | `0F AE F8 []` | Serializes store operations. |

# sgdt
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sgdt m | `0F 01 /0 [rm=0]` | Store GDTR to m. |

# shlb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shlb r/m8 imm8 | `C0 /4 ib [rm=0, i=1]` | Multiply r/m8 by 2, imm8 times. |
| shlb r/m8 1 | `D0 /4 [rm=0]` | Multiply r/m8 by 2, once. |
| shlb r/m8 CL | `D2 /4 [rm=0]` | Multiply r/m8 by 2, CL times. |

# shldl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shldl r/m32 r32 imm8 | `0F A4 /r ib [reg=1, rm=0, i=2]` | Shift r/m32 to left imm8 places while shifting bits from r32 in from the right. |
| shldl r/m32 r32 CL | `0F A5 /r [reg=1, rm=0]` | Shift r/m32 to left CL places while shifting bits from r32 in from the right. |

# shldq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shldq r/m64 r64 imm8 | `REX.W+ 0F A4 /r ib [reg=1, rm=0, i=2]` | Shift r/m64 to left imm8 places while shifting bits from r64 in from the right. |
| shldq r/m64 r64 CL | `REX.W+ 0F A5 /r [reg=1, rm=0]` | Shift r/m64 to left CL places while shifting bits from r64 in from the right. |

# shldw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shldw r/m16 r16 imm8 | `66 0F A4 /r ib [reg=1, rm=0, i=2]` | Shift r/m16 to left imm8 places while shifting bits from r16 in from the right. |
| shldw r/m16 r16 CL | `66 0F A5 /r [reg=1, rm=0]` | Shift r/m16 to left CL places while shifting bits from r16 in from the right. |

# shll
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shll r/m32 imm8 | `C1 /4 ib [rm=0, i=1]` | Multiply r/m32 by 2, imm8 times. |
| shll r/m32 1 | `D1 /4 [rm=0]` | Multiply r/m32 by 2, once. |
| shll r/m32 CL | `D3 /4 [rm=0]` | Multiply r/m32 by 2, CL times. |

# shlq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shlq r/m64 imm8 | `REX.W+ C1 /4 ib [rm=0, i=1]` | Multiply r/m32 by 2, imm8 times. |
| shlq r/m64 1 | `REX.W+ D1 /4 [rm=0]` | Multiply r/m64 by 2, once. |
| shlq r/m64 CL | `REX.W+ D3 /4 [rm=0]` | Multiply r/m32 by 2, CL times. |

# shlw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shlw r/m16 imm8 | `66 C1 /4 ib [rm=0, i=1]` | Multiply r/m16 by 2, imm8 times. |
| shlw r/m16 1 | `66 D1 /4 [rm=0]` | Multiply r/m16 by 2, once. |
| shlw r/m16 CL | `66 D3 /4 [rm=0]` | Multiply r/m16 by 2, CL times. |

# shrb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shrb r/m8 imm8 | `C0 /5 ib [rm=0, i=1]` | Unsigned divide r/m8 by 2, imm8 times. |
| shrb r/m8 1 | `D0 /5 [rm=0]` | Unsigned divide r/m8 by 2, once. |
| shrb r/m8 CL | `D2 /5 [rm=0]` | Unsigned divide r/m8 by 2, CL times. |

# shrdl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shrdl r/m32 r32 imm8 | `0F AC /r ib [reg=1, rm=0, i=2]` | Shift r/m32 to right imm8 places while shifting bits from r32 in from the left. |
| shrdl r/m32 r32 CL | `0F AD /r [reg=1, rm=0]` | Shift r/m32 to right CL places while shifting bits from r32 in from the left. |

# shrdq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shrdq r/m64 r64 imm8 | `REX.W+ 0F AC /r ib [reg=1, rm=0, i=2]` | Shift r/m64 to right imm8 places while shifting bits from r64 in from the left. |
| shrdq r/m64 r64 CL | `REX.W+ 0F AD /r [reg=1, rm=0]` | Shift r/m64 to right CL places while shifting bits from r64 in from the left. |

# shrdw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shrdw r/m16 r16 imm8 | `66 0F AC /r ib [reg=1, rm=0, i=2]` | Shift r/m16 to right imm8 places while shifting bits from r16 in from the left. |
| shrdw r/m16 r16 CL | `66 0F AD /r [reg=1, rm=0]` | Shift r/m16 to right CL places while shifting bits from r16 in from the left. |

# shrl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shrl r/m32 imm8 | `C1 /5 ib [rm=0, i=1]` | Unsigned divide r/m32 by 2, imm8 times. |
| shrl r/m32 1 | `D1 /5 [rm=0]` | Unsigned divide r/m32 by 2, once. |
| shrl r/m32 CL | `D3 /5 [rm=0]` | Unsigned divide r/m32 by 2, CL times. |

# shrq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shrq r/m64 imm8 | `REX.W+ C1 /5 ib [rm=0, i=1]` | Unsigned divide r/m32 by 2, imm8 times. |
| shrq r/m64 1 | `REX.W+ D1 /5 [rm=0]` | Unsigned divide r/m32 by 2, once. |
| shrq r/m64 CL | `REX.W+ D3 /5 [rm=0]` | Unsigned divide r/m32 by 2, CL times. |

# shrw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shrw r/m16 imm8 | `66 C1 /5 ib [rm=0, i=1]` | Unsigned divide r/m16 by 2, imm8 times. |
| shrw r/m16 1 | `66 D1 /5 [rm=0]` | Unsigned divide r/m16 by 2, once. |
| shrw r/m16 CL | `66 D3 /5 [rm=0]` | Unsigned divide r/m16 by 2, CL times |

# shufpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shufpd xmm xmm/m128 imm8 | `66 0F C6 /r ib [reg=0, rm=1, i=2]` | Shuffle packed double-precision floating- point values selected by imm8 from xmm1 and xmm2/m128 to xmm1. |

# shufps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| shufps xmm xmm/m128 imm8 | `0F C6 /r ib [reg=0, rm=1, i=2]` | Shuffle packed single-precision floating-point values selected by imm8 from xmm1 and xmm1/m128 to xmm1. |

# sidt
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sidt m | `0F 01 /1 [rm=0]` | Store IDTR to m. |

# sqrtpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sqrtpd xmm xmm/m128 | `66 0F 51 /r [reg=0, rm=1]` | Computes square roots of the packed double- precision floating-point values in xmm2/m128 and stores the results in xmm1. |

# sqrtps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sqrtps xmm xmm/m128 | `0F 51 /r [reg=0, rm=1]` | Computes square roots of the packed single- precision floating-point values in xmm2/m128 and stores the results in xmm1. |

# sqrtsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sqrtsd xmm xmm/m64 | `F2 0F 51 /r [reg=0, rm=1]` | Computes square root of the low double- precision floating-point value in xmm2/m64 and stores the results in xmm1. |

# sqrtss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sqrtss xmm xmm/m32 | `F3 0F 51 /r [reg=0, rm=1]` | Computes square root of the low single- precision floating-point value in xmm2/m32 and stores the results in xmm1. |

# stc
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| stc | `F9 []` | Set CF flag. |

# std
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| std | `FD []` | Set DF flag. |

# sti
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| sti | `FB []` | Set interrupt flag; external, maskable interrupts enabled at the end of the next instruction. |

# stmxcsr
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| stmxcsr m32 | `0F AE /3 [rm=0]` | Store contents of MXCSR register to m32. |

# str
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| str r/m16 | `66 0F 00 /1 [rm=0]` | Stores segment selector from TR in r/m16. |

# subb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| subb r8 r/m8 | `2A /r [reg=0, rm=1]` | Subtract r/m8 from r8. |
| subb r/m8 r8 | `28 /r [reg=1, rm=0]` | Subtract r8 from r/m8. |
| subb r/m8 imm8 | `80 /5 ib [rm=0, i=1]` | Subtract imm8 from r/m8. |
| subb AL imm8 | `2C ib [i=1]` | Subtract imm8 from AL. |

# subl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| subl r32 r/m32 | `2B /r [reg=0, rm=1]` | Subtract r/m32 from r32. |
| subl r/m32 r32 | `29 /r [reg=1, rm=0]` | Subtract r32 from r/m32. |
| subl r/m32 imm8 | `83 /5 ib [rm=0, i=1]` | Subtract sign-extended imm8 from r/m32. |
| subl r/m32 imm32 | `81 /5 id [rm=0, i=1]` | Subtract imm32 from r/m32. |
| subl EAX imm32 | `2D id [i=1]` | Subtract imm32 from EAX. |

# subpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| subpd xmm xmm/m128 | `66 0F 5C /r [reg=0, rm=1]` | Subtract packed double-precision floating- point values in xmm2/m128 from xmm1. |

# subps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| subps xmm xmm/m128 | `0F 5C /r [reg=0, rm=1]` | Subtract packed single-precision floating-point values in xmm2/mem from xmm1. |

# subq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| subq r64 r/m64 | `REX.W+ 2B /r [reg=0, rm=1]` | Subtract r/m64 from r64. |
| subq r/m64 r64 | `REX.W+ 29 /r [reg=1, rm=0]` | Subtract r64 from r/m64. |
| subq r/m64 imm8 | `REX.W+ 83 /5 ib [rm=0, i=1]` | Subtract sign-extended imm8 from r/m64. |
| subq r/m64 imm32 | `REX.W+ 81 /5 id [rm=0, i=1]` | Subtract imm32 sign-extended to 64-bits from r/m64. |
| subq RAX imm32 | `REX.W+ 2D id [i=1]` | Subtract imm32 sign-extended to 64-bits from RAX. |

# subsd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| subsd xmm xmm/m64 | `F2 0F 5C /r [reg=0, rm=1]` | Subtracts the low double-precision floating- point values in xmm2/mem64 from xmm1. |

# subss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| subss xmm xmm/m32 | `F3 0F 5C /r [reg=0, rm=1]` | Subtract the lower single-precision floating- point values in xmm2/m32 from xmm1. |

# subw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| subw r16 r/m16 | `66 2B /r [reg=0, rm=1]` | Subtract r/m16 from r16. |
| subw r/m16 r16 | `66 29 /r [reg=1, rm=0]` | Subtract r16 from r/m16. |
| subw r/m16 imm8 | `66 83 /5 ib [rm=0, i=1]` | Subtract sign-extended imm8 from r/m16. |
| subw r/m16 imm16 | `66 81 /5 iw [rm=0, i=1]` | Subtract imm16 from r/m16. |
| subw AX imm16 | `66 2D iw [i=1]` | Subtract imm16 from AX. |

# swapgs
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| swapgs | `0F 01 F8 []` | Exchanges the current GS base register value with the value contained in MSR address C0000102H. |

# testb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| testb r/m8 r8 | `84 /r [reg=1, rm=0]` | AND r8 with r/m8; set SF, ZF, PF according to result. |
| testb r/m8 imm8 | `F6 /0 ib [rm=0, i=1]` | AND imm8 with r/m8; set SF, ZF, PF according to result. |
| testb AL imm8 | `A8 ib [i=1]` | AND imm8 with AL; set SF, ZF, PF according to result. |

# testl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| testl r/m32 r32 | `85 /r [reg=1, rm=0]` | AND r32 with r/m32; set SF, ZF, PF according to result. |
| testl r/m32 imm32 | `F7 /0 id [rm=0, i=1]` | AND imm32 with r/m32; set SF, ZF, PF according to result. |
| testl EAX imm32 | `A9 id [i=1]` | AND imm32 with EAX; set SF, ZF, PF according to result. |

# testq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| testq r/m64 r64 | `REX.W+ 85 /r [reg=1, rm=0]` | AND r64 with r/m64; set SF, ZF, PF according to result. |
| testq r/m64 imm32 | `REX.W+ F7 /0 id [rm=0, i=1]` | AND imm32 sign-extended to 64-bits with r/m64; set SF, ZF, PF according to result. |
| testq RAX imm32 | `REX.W+ A9 id [i=1]` | AND imm32 sign-extended to 64-bits with RAX; set SF, ZF, PF according to result. |

# testw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| testw r/m16 r16 | `66 85 /r [reg=1, rm=0]` | AND r16 with r/m16; set SF, ZF, PF according to result. |
| testw r/m16 imm16 | `66 F7 /0 iw [rm=0, i=1]` | AND imm16 with r/m16; set SF, ZF, PF according to result. |
| testw AX imm16 | `66 A9 iw [i=1]` | AND imm16 with AX; set SF, ZF, PF according to result. |

# ucomisd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| ucomisd xmm xmm/m64 | `66 0F 2E /r [reg=0, rm=1]` | Compares (unordered) the low double- precision floating-point values in xmm1 and xmm2/m64 and set the EFLAGS accordingly. |

# ucomiss
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| ucomiss xmm xmm/m32 | `0F 2E /r [reg=0, rm=1]` | Compare lower single-precision floating-point value in xmm1 register with lower single- precision floating-point value in xmm2/mem and set the status flags accordingly. |

# ud2
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| ud2 | `0F 0B []` | Raise invalid opcode exception. |

# unpckhpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| unpckhpd xmm xmm/m128 | `66 0F 15 /r [reg=0, rm=1]` | Unpacks and Interleaves double-precision floating-point values from high quadwords of xmm1 and xmm2/m128. |

# unpckhps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| unpckhps xmm xmm/m128 | `0F 15 /r [reg=0, rm=1]` | Unpacks and Interleaves single-precision floating-point values from high quadwords of xmm1 and xmm2/mem into xmm1. |

# unpcklpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| unpcklpd xmm xmm/m128 | `66 0F 14 /r [reg=0, rm=1]` | Unpacks and Interleaves double-precision floating-point values from low quadwords of xmm1 and xmm2/m128. |

# unpcklps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| unpcklps xmm xmm/m128 | `0F 14 /r [reg=0, rm=1]` | Unpacks and Interleaves single-precision floating-point values from low quadwords of xmm1 and xmm2/mem into xmm1. |

# verr
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| verr r/m16 | `0F 00 /4 [rm=0]` | Set ZF=1 if segment specified with r/m16 can be read. |

# verw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| verw r/m16 | `0F 00 /5 [rm=0]` | Set ZF=1 if segment specified with r/m16 can be written. |

# wbinvd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| wbinvd | `0F 09 []` | Write back and flush Internal caches; initiate writing-back and flushing of external caches. |

# wrmsr
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| wrmsr | `0F 30 []` | Write the value in EDX:EAX to MSR specified by ECX. |

# xaddb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xaddb r/m8 r8 | `0F C0 /r [reg=1, rm=0]` | Exchange r8 and r/m8; load sum into r/m8. |

# xaddl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xaddl r/m32 r32 | `0F C1 /r [reg=1, rm=0]` | Exchange r32 and r/m32; load sum into r/m32. |

# xaddq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xaddq r/m64 r64 | `REX.W+ 0F C1 /r [reg=1, rm=0]` | Exchange r64 and r/m64; load sum into r/m64. |

# xaddw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xaddw r/m16 r16 | `66 0F C1 /r [reg=1, rm=0]` | Exchange r16 and r/m16; load sum into r/m16. |

# xchgb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xchgb r8 r/m8 | `86 /r [reg=0, rm=1]` | Exchange byte from r/m8 with r8 (byte register). |
| xchgb r/m8 r8 | `86 /r [reg=1, rm=0]` | Exchange r8 (byte register) with byte from r/m8. |

# xchgl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xchgl r32 r/m32 | `87 /r [reg=0, rm=1]` | Exchange doubleword from r/m32 with r32. |
| xchgl r32 EAX | `90 +rd [r=0]` | Exchange EAX with r32. |
| xchgl r/m32 r32 | `87 /r [reg=1, rm=0]` | Exchange r32 with doubleword from r/m32. |
| xchgl EAX r32 | `90 +rd [r=1]` | Exchange r32 with EAX. |

# xchgq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xchgq r64 r/m64 | `REX.W+ 87 /r [reg=0, rm=1]` | Exchange quadword from r/m64 with r64. |
| xchgq r64 RAX | `REX.W+ 90 +ro [r=0]` | Exchange RAX with r64. |
| xchgq r/m64 r64 | `REX.W+ 87 /r [reg=1, rm=0]` | Exchange r64 with quadword from r/m64. |
| xchgq RAX r64 | `REX.W+ 90 +ro [r=1]` | Exchange r64 with RAX. |

# xchgw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xchgw r16 r/m16 | `66 87 /r [reg=0, rm=1]` | Exchange word from r/m16 with r16. |
| xchgw r16 AX | `66 90 +rw [r=0]` | Exchange AX with r16. |
| xchgw r/m16 r16 | `66 87 /r [reg=1, rm=0]` | Exchange r16 with word from r/m16. |
| xchgw AX r16 | `66 90 +rw [r=1]` | Exchange r16 with AX. |

# xorb
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xorb r8 r/m8 | `32 /r [reg=0, rm=1]` | r8 XOR r/m8. |
| xorb r/m8 r8 | `30 /r [reg=1, rm=0]` | r/m8 XOR r8. |
| xorb r/m8 imm8 | `80 /6 ib [rm=0, i=1]` | r/m8 XOR imm8. |
| xorb AL imm8 | `34 ib [i=1]` | AL XOR imm8. |

# xorl
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xorl r32 r/m32 | `33 /r [reg=0, rm=1]` | r32 XOR r/m32. |
| xorl r/m32 r32 | `31 /r [reg=1, rm=0]` | r/m32 XOR r32. |
| xorl r/m32 imm8 | `83 /6 ib [rm=0, i=1]` | r/m32 XOR imm8 (sign-extended). |
| xorl r/m32 imm32 | `81 /6 id [rm=0, i=1]` | r/m32 XOR imm32. |
| xorl EAX imm32 | `35 id [i=1]` | EAX XOR imm32. |

# xorpd
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xorpd xmm xmm/m128 | `66 0F 57 /r [reg=0, rm=1]` | Bitwise exclusive-OR of xmm2/m128 and xmm1. |

# xorps
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xorps xmm xmm/m128 | `0F 57 /r [reg=0, rm=1]` | Bitwise exclusive-OR of xmm2/m128 and xmm1. |

# xorq
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xorq r64 r/m64 | `REX.W+ 33 /r [reg=0, rm=1]` | r64 XOR r/m64. |
| xorq r/m64 r64 | `REX.W+ 31 /r [reg=1, rm=0]` | r/m64 XOR r64. |
| xorq r/m64 imm8 | `REX.W+ 83 /6 ib [rm=0, i=1]` | r/m64 XOR imm8 (sign-extended). |
| xorq r/m64 imm32 | `REX.W+ 81 /6 id [rm=0, i=1]` | r/m64 XOR imm32 (sign-extended). |
| xorq RAX imm32 | `REX.W+ 35 id [i=1]` | RAX XOR imm32 (sign-extended). |

# xorw
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xorw r16 r/m16 | `66 33 /r [reg=0, rm=1]` | r16 XOR r/m16. |
| xorw r/m16 r16 | `66 31 /r [reg=1, rm=0]` | r/m16 XOR r16. |
| xorw r/m16 imm8 | `66 83 /6 ib [rm=0, i=1]` | r/m16 XOR imm8 (sign-extended). |
| xorw r/m16 imm16 | `66 81 /6 iw [rm=0, i=1]` | r/m16 XOR imm16. |
| xorw AX imm16 | `66 35 iw [i=1]` | AX XOR imm16. |

# xsetbv
| Instruction | Encoding | Description |
| ----------- | -------- | ----------- |
| xsetbv | `0F 01 D1 []` | Write the value in EDX:EAX to the XCR specified by ECX. |

---
Total 491 mnemonics.
