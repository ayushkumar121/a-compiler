.text
.global _start

_start:
   bl _main
   movz x16, #0x1
   movk x16, #0x200, lsl #16
   svc #0
; ins_label
_foo:
; ins_func_start
   stp x29, x30, [sp, #-16]!
   mov x29, sp
   sub sp, sp, #0
; ins_ret
   movz x0, #10
   b .Lreturn_foo
; ins_func_end
.Lreturn_foo:
   add sp, sp, #0
   ldp x29, x30, [sp], #16
   ret
; ins_label
_add:
; ins_func_start
   stp x29, x30, [sp, #-16]!
   mov x29, sp
   sub sp, sp, #48
; op_load_param
    str w0, [x29, #-16]
; op_load_param
    str w1, [x29, #-24]
; op_add
   ldr w0, [x29, #-16]
   ldr w1, [x29, #-24]
   add x0, x0, x1
   str w0, [x29, #-32]
; ins_func_call
   bl _foo
   str w0, [x29, #-40]
; op_add
   ldr w0, [x29, #-32]
   ldr w1, [x29, #-40]
   add x0, x0, x1
   str w0, [x29, #-48]
; ins_ret
   ldr w0, [x29, #-48]
   b .Lreturn_add
; ins_func_end
.Lreturn_add:
   add sp, sp, #48
   ldp x29, x30, [sp], #16
   ret
; ins_label
_main:
; ins_func_start
   stp x29, x30, [sp, #-16]!
   mov x29, sp
   sub sp, sp, #64
; op_addrof
   sub x0, x29, #16
   str x0, [x29, #-24]
; op_add
   ldr x0, [x29, #-24]
   movz x1, #0
   add x0, x0, x1
   str x0, [x29, #-24]
; op_store_indirect
   movz x0, #10
   ldr x1, [x29, #-24]
   str x0, [x1]
; op_addrof
   sub x0, x29, #16
   str x0, [x29, #-32]
; op_add
   ldr x0, [x29, #-32]
   movz x1, #4
   add x0, x0, x1
   str x0, [x29, #-32]
; op_store_indirect
   movz x0, #20
   ldr x1, [x29, #-32]
   str x0, [x1]
; op_addrof
   sub x0, x29, #16
   str x0, [x29, #-56]
; op_add
   ldr x0, [x29, #-56]
   movz x1, #0
   add x0, x0, x1
   str x0, [x29, #-56]
; op_load_indirect
   ldr x0, [x29, #-56]
   ldr w0, [x0]
   str w0, [x29, #-48]
; op_addrof
   sub x0, x29, #16
   str x0, [x29, #-72]
; op_add
   ldr x0, [x29, #-72]
   movz x1, #4
   add x0, x0, x1
   str x0, [x29, #-72]
; op_load_indirect
   ldr x0, [x29, #-72]
   ldr w0, [x0]
   str w0, [x29, #-64]
; ins_func_call
   ldr w0, [x29, #-48]
   ldr w1, [x29, #-64]
   bl _add
   str w0, [x29, #-40]
; ins_ret
   ldr w0, [x29, #-40]
   b .Lreturn_main
; ins_func_end
.Lreturn_main:
   add sp, sp, #64
   ldp x29, x30, [sp], #16
   ret
