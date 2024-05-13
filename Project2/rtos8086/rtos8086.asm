.model small

__RTOS8086_ASM equ 1
include rtos8086.inc

;; -------------------------------------
;   RTOS Dev Log
;; -------------------------------------

; Heap Management
; Currently we don't implement a heap management
; Thus all thing we do is about static / stack

; Task Management
; Since clock frequency of 8086 is low
; We only implemnt non-preemptive scheduler here
; Idle task is used in case external clock frequency is much lower (It should be an endless loop with vTaskSuspend called at the end of the loop)

; Timer
; Currently we don't have a native timer support
; But it can be achieved using vTaskDelay

;; -------------------------------------
;   RTOS Data
;; -------------------------------------
.stack portRTOS_STACK_SIZE
.data
    intt_intype_addr dw 0, 0
    
    ; ticks serves as ticktime, ticks could be used to determine the loop time 
    ticks dw 0 ; the current ticks
    ticks_lst dw 0 ; update before task execute by ticks
    ticks_lst_lst dw 0 ; update after task execute by ticks_lst

    ; task_table from LSB to MSB: 2 IP, 2 CS, 2 lst_execute, 1 pri (ff is highest), 1 task status
    ; pri with 0 and pri with ff should be reserved
    task_table RTOS_TASK_TCB 3 dup (<>)
    task_table_idx_end dw 0
    soft_exit db 0
    keyboard_signal db 0ffh

    idle_task_table RTOS_FUNC_TAB 3 dup (<>)
    idle_task_table_idx_end db 0

    rtos_stack_top dw 0
    rtos_stack_bottom dw 0
    task_current dw 0
    ; Personal Hobby, reversed for x64
    r8 dq 0
    r9 dq 0
    r10 dq 0
    r11 dq 0
    r12 dq 0
    r13 dq 0
    r14 dq 0
    r15 dq 0
.const
    configCPU_FREQ_KHZ equ 1000
    configTICK_RATE_HZ equ 1000
    portTICK_RATE_MS equ 1000 / configTICK_RATE_HZ
    intype equ 0ah
.code
;; -------------------------------------
;   RTOS Code - Internal
;; -------------------------------------

; Context Management
; IP, CS
; AX, CX, DX, BX, SP, BP, SI, DI
; Flag Register

; store the context of the caller of the calling function
; this should be execute by the calling function immediately after the calling function is called 
portSAVE_CONTEXT macro
    pushf
    pusha
endm

; restore the context of the called function before calling
; this should be the end of the calling function
portRESTORE_CONTEXT macro
    popa
    popf
    ret RTOS_FUNC_TYPE
endm

switch_context_to_task macro
    mov [task_current], bx
    mov [rtos_stack_bottom], bp
    mov [rtos_stack_top], sp
    mov bp, [bx].task_stack_bottom
    mov sp, [bx].task_stack_top
endm

switch_context_to_rtos macro
    mov bx, [task_current]
    mov [bx].task_stack_bottom, bp
    mov [bx].task_stack_top, sp
    mov bp, [rtos_stack_bottom]
    mov sp, [rtos_stack_top]
endm

; target TCB at [bx]
switch_to_task proc near
    portSAVE_CONTEXT
    switch_context_to_task
    portRESTORE_CONTEXT
switch_to_task endp

ticks_intt proc
    push ds
    mov ax, @data
    mov ds, ax
    inc word ptr [ticks]
    pop ds

    ; sti
    iret
ticks_intt endp

ticks_intt_setup proc near
    push es

    mov ax, 0
    mov es, ax
    ; backup interrupt address
    mov bx, word ptr es:[intype*4]
    mov dx, word ptr es:[intype*4+2]
    mov word ptr [intt_intype_addr], bx
    mov word ptr [intt_intype_addr+2], dx
    ; replace intype interrupt
    mov word ptr es:[intype*4], @code
    mov word ptr es:[intype*4+2], offset ticks_intt
    sti ; enable interrupt
    pop es
    ret
ticks_intt_setup endp

ticks_intt_restore proc near
    push es

    mov ax, 0
    mov es, ax
    ; restore intype interrupt
    mov bx, word ptr [intt_intype_addr]
    mov dx, word ptr [intt_intype_addr+2]
    mov word ptr es:[intype*4], bx
    mov word ptr es:[intype*4+2], dx

    pop es
    ret
ticks_intt_restore endp

cli_setup proc near
    lea cx, cli_task
    call add_idle_task_table

    ret
cli_setup endp

cli_task proc RTOS_FUNC_TYPE
    ; mov dl, byte ptr '-'
    ; mov ah, 2h
    ; int 21h

    ; detect if the user has input q
    mov ah, 0bh
    int 21h
    test al, al
    jnz if_detect_input_else

    if_detect_input:
        mov ah, 1h
        int 21h
        cmp al, byte ptr 'q'
        jne if_detect_input_else
        call exit_program
        cmp al, byte ptr '0'
        jb if_detect_input_else
        cmp al, byte ptr '9'
        ja if_detect_input_else
        mov byte ptr [keyboard_signal], al
    if_detect_input_else:
        nop

    ret
cli_task endp

scheduler_routine proc near
    ; Cooperative Scheduling, Non-preemptive
    sche_main_loop: ; main loop

        ; Update states of each tasks when timer increases
        xor si, si
        sche_time_loop:
            cmp si, task_table_idx_end
            je sche_cmp_loop_end

            mov al, byte ptr task_table[si].task_stat
            and al, 00000111b
            cmp al, eBlocked
            jne if_sche_cmp_task_is_blocked_not
            je if_sche_cmp_task_is_blocked
            cmp al, eDeleted
            jne if_sche_cmp_task_is_blocked_not

            if_sche_cmp_task_is_deleted:
                mov cx, task_table[si].func.fip
                call del_task_table
                jmp if_sche_cmp_task_is_blocked_not

            if_sche_cmp_task_is_blocked:
                test byte ptr task_table[si].task_stat, 08h
                jz sche_cmp_loop_nxt
                cmp word ptr task_table[si].task_timer, 0
                jz if_sche_cmp_task_is_still_blocked_not
                if_sche_cmp_task_is_still_blocked:
                    dec word ptr task_table[si].task_timer
                    jmp if_sche_cmp_task_is_blocked_not
                if_sche_cmp_task_is_still_blocked_not:
                    and byte ptr task_table[si].task_stat, 0f8h
                    or byte ptr task_table[si].task_stat, eReady
            if_sche_cmp_task_is_blocked_not:
                nop
            jmp sche_time_loop

        ; loop until tick increases
        sche_inside_loop:
            mov ax, word ptr [ticks]
            cmp ax, word ptr [ticks_lst]
            jne sche_inside_loop_end

            sche_task_loop: ; execute all ready tasks

                xor si, si
                xor di, di
                mov word ptr [r8], 0h

                sche_cmp_loop:
                    cmp si, task_table_idx_end
                    je sche_cmp_loop_end

                    mov al, byte ptr task_table[si].task_stat
                    and al, 00000111b
                    cmp al, eReady
                    jne sche_cmp_loop_nxt

                    inc word ptr [r8]
                    cmp word ptr [r8], 1
                    je sche_cmp_loop_nxt ; first valid

                    cmp word ptr task_table[si].task_pri, dx
                    jb sche_cmp_loop_nxt
                    ja sche_cmp_loop_swp
                    cmp word ptr task_table[si].lst_exec, bx
                    ja sche_cmp_loop_nxt

                    sche_cmp_loop_swp: ; swap when find better
                        mov di, si
                        mov bx, word ptr task_table[si].lst_exec
                        mov dx, word ptr task_table[si].task_pri
                    sche_cmp_loop_nxt: ; next
                        add si, size RTOS_TASK_TCB
                        jmp sche_cmp_loop
                sche_cmp_loop_end:
                    nop
                
                cmp word ptr [r8], 0
                jz sche_task_loop_end

                ; execute target function
                mov ax, word ptr [ticks_lst]
                mov word ptr task_table[di].lst_exec, ax
                and task_table[di].task_stat, 11111000b
                or task_table[di].task_stat, eRunning

                lea bx, task_table[di]
                call switch_to_task
                
                ; update ticks_lst_lst
                mov ax, word ptr [ticks_lst]
                mov word ptr [ticks_lst_lst], ax

                jmp sche_task_loop
            sche_task_loop_end:
                nop
            
            xor si, si
            idle_task_func_loop: 
                cmp si, word ptr [idle_task_func_loop_end]
                je idle_task_func_loop_end
                
                call dword ptr idle_task_func_loop[si]
                add si, size RTOS_FUNC_TAB
            idle_task_func_loop_end: 
                nop
        sche_inside_loop_end:
            nop
        
        mov word ptr [ticks_lst], ax

        cmp [soft_exit], 0
        jnz sche_main_loop

    ret
scheduler_routine endp

main_setup proc RTOS_FUNC_TYPE
    lea cx, main_task
    mov dl, 0fh
    call add_task_table
    call cli_setup

    ret
main_setup endp

main_task proc RTOS_FUNC_TYPE
    call app_main

    lea cx, main_task
    call vTaskDelete

    ret
main_task endp

main proc far
    mov ax, @data
    mov ds, ax
    mov es, ax
    mov ss, ax
    
    push bp
    mov bp, sp

    call start_rtos
    call stop_rtos ; will go here when soft_exit is not zero
    
    mov sp, bp
    pop bp

    ret
main endp

;; -------------------------------------
;   RTOS Code - Exported
;; -------------------------------------

start_rtos proc near
    mov [rtos_stack_bottom], sp
    mov [rtos_stack_top], bp
    
    call pre_main_setup
    call main_setup
    call post_main_setup
    call ticks_intt_setup
    call scheduler_routine

    ret
start_rtos endp

stop_rtos proc near
    call exit_program

    ret
stop_rtos endp

exit_program proc near
    call ticks_intt_restore
    mov ah, 4ch
    int 21h

    ret
exit_program endp

; si is &table[0], di is &table_idx_end, ax is the elem size
; cx is the function offset, dl is the priority
add_table proc near
    mov bx, word ptr [di]
    add word ptr [di], ax
    mov word ptr [si + bx].func.fip, cx
    mov ax, @code
    mov word ptr [si + bx].func.fcs, ax
    mov byte ptr [si + bx].task_pri, dl
    mov byte ptr [si + bx].task_stat, eReady
    or byte ptr [si + bx].task_stat, 08h
    add bx, si

    ret
add_table endp

add_task_table proc near ; cx is the function offset, dl is the priority, bx is the returned handle
    lea si, task_table
    lea di, task_table_idx_end
    mov ax, size RTOS_TASK_TCB
    call add_table

    ret
add_task_table endp

add_idle_task_table proc near ; cx is the function offset, bx is the returned handle
    lea si, idle_task_table
    lea di, idle_task_table_idx_end
    mov ax, size RTOS_FUNC_TAB
    call add_table

    ret
add_idle_task_table endp

; si is &table[0], di is &table_idx_end, ax is the elem size
; cx is the function offset
del_table proc near
    del_table_loop:
        cmp si, [di]
        je del_table_loop_end
        cmp word ptr [si].func.fip, cx
        jne del_table_loop_nxt

        mov bx, di
        sub [bx], ax
        mov di, si
        add di, ax
        xchg si, di
        cld
        mov_tab_loop:
            cmp di, [bx]
            je del_table_loop_end

            lodsb
            stosb
            jmp mov_tab_loop
        mov_tab_loop_end:
            nop

        jmp del_table_loop_end
        del_table_loop_nxt:
            add si, ax
            jmp del_table_loop
    del_table_loop_end:
        nop

    ret
del_table endp

del_task_table proc near ; cx is the function offset
    lea si, task_table
    lea di, task_table_idx_end
    mov ax, size RTOS_TASK_TCB
    call del_table

    ret
del_task_table endp

del_idle_task_table proc near ; cx is the function offset
    lea si, idle_task_table
    lea di, idle_task_table_idx_end
    mov ax, size RTOS_TASK_TCB
    call del_table

    ret
del_idle_task_table endp

vTaskDelay proc RTOS_FUNC_TYPE ; ax is the ticks to block
    portSAVE_CONTEXT
    switch_context_to_rtos

    mov [bx].task_timer, ax
    and [bx].task_stat, 11111000b
    or [bx].task_stat, eBlocked

    portRESTORE_CONTEXT
vTaskDelay endp

vTaskDelayUntil proc RTOS_FUNC_TYPE ; ax is the target ticks (note that the function will not validate input)
    portSAVE_CONTEXT
    switch_context_to_rtos

    mov cx, [ticks]
    sub cx, ax
    mov [bx].task_timer, cx
    and [bx].task_stat, 11111000b
    or [bx].task_stat, eBlocked

    portRESTORE_CONTEXT
vTaskDelayUntil endp

vTaskDelete proc RTOS_FUNC_TYPE ; bx is the target TCB
    portSAVE_CONTEXT
    switch_context_to_rtos

    and [bx].task_stat, 11111000b
    or [bx].task_stat, eDeleted
    
    portRESTORE_CONTEXT
vTaskDelete endp

vTaskSuspend proc RTOS_FUNC_TYPE ; bx is the target TCB
    portSAVE_CONTEXT
    switch_context_to_rtos

    and [bx].task_stat, 11111000b
    or [bx].task_stat, eSuspended
    
    portRESTORE_CONTEXT
vTaskSuspend endp

vTaskResume proc RTOS_FUNC_TYPE ; bx is the target TCB
    portSAVE_CONTEXT
    switch_context_to_rtos

    and [bx].task_stat, 11111000b
    or [bx].task_stat, eReady
    
    portRESTORE_CONTEXT
vTaskResume endp

get_keyboard_signal proc near ; return at al
    mov al, byte ptr [keyboard_signal]
get_keyboard_signal endp

set_keyboard_signal proc near ; parameter al
    mov byte ptr [keyboard_signal], al
set_keyboard_signal endp

get_task_current proc near ; return at bx
    mov bx, word ptr [task_current]
get_task_current endp

set_task_current proc near ; parameter bx
    mov word ptr [task_current], bx
set_task_current endp


end